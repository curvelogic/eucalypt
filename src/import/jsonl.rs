//! Import JSON Lines (JSONL) format where each line is a JSON value
use crate::common::sourcemap::SourceMap;
use crate::core::expr::*;
use crate::import::error::SourceError;
use codespan::Span;
use codespan_reporting::files::SimpleFiles;

/// Read JSONL (JSON Lines) into a core expression as a list of values
///
/// Each non-empty line is parsed as a JSON value. Empty lines and
/// whitespace-only lines are skipped.
pub fn read_jsonl<'smap>(
    files: &'smap mut SimpleFiles<String, String>,
    source_map: &'smap mut SourceMap,
    file_id: usize,
    text: &'smap str,
) -> Result<RcExpr, SourceError> {
    let mut items = Vec::new();

    for (line_num, line) in text.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let line_file_id = files.add(format!("jsonl:line:{}", line_num + 1), trimmed.to_string());
        match super::yaml::read_yaml(files, source_map, line_file_id, trimmed) {
            Ok(expr) => items.push(expr),
            Err(_) => {
                return Err(SourceError::InvalidJsonl(
                    format!("invalid JSON on line {}", line_num + 1),
                    file_id,
                    Span::default(),
                ));
            }
        }
    }

    Ok(acore::list(items))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::expr::acore;
    use codespan_reporting::files::SimpleFiles;
    use moniker::assert_term_eq;

    fn parse(text: &str) -> RcExpr {
        let mut sm = SourceMap::new();
        let mut files = SimpleFiles::new();
        let file_id = files.add("test.jsonl".to_string(), text.to_string());
        read_jsonl(&mut files, &mut sm, file_id, text).unwrap()
    }

    #[test]
    fn test_basic_jsonl() {
        let input = r#"{"name": "Alice"}
{"name": "Bob"}"#;
        let result = parse(input);
        let expected = acore::list(vec![
            acore::default_let(vec![(
                moniker::FreeVar::fresh_named("name"),
                acore::str("Alice"),
            )]),
            acore::default_let(vec![(
                moniker::FreeVar::fresh_named("name"),
                acore::str("Bob"),
            )]),
        ]);
        assert_term_eq!(result, expected);
    }

    #[test]
    fn test_empty_lines_skipped() {
        let input = r#"{"a": 1}

{"b": 2}"#;
        let result = parse(input);
        let expected = acore::list(vec![
            acore::default_let(vec![(moniker::FreeVar::fresh_named("a"), acore::num(1))]),
            acore::default_let(vec![(moniker::FreeVar::fresh_named("b"), acore::num(2))]),
        ]);
        assert_term_eq!(result, expected);
    }

    #[test]
    fn test_whitespace_lines_skipped() {
        let input = "1\n   \n2";
        let result = parse(input);
        let expected = acore::list(vec![acore::num(1), acore::num(2)]);
        assert_term_eq!(result, expected);
    }

    #[test]
    fn test_mixed_json_types() {
        let input = r#"{"obj": true}
[1, 2, 3]
"string"
42
null"#;
        let result = parse(input);
        let expected = acore::list(vec![
            acore::default_let(vec![(
                moniker::FreeVar::fresh_named("obj"),
                acore::bool_(true),
            )]),
            acore::list(vec![acore::num(1), acore::num(2), acore::num(3)]),
            acore::str("string"),
            acore::num(42),
            acore::null(),
        ]);
        assert_term_eq!(result, expected);
    }

    #[test]
    fn test_single_line() {
        let input = r#"{"single": "value"}"#;
        let result = parse(input);
        let expected = acore::list(vec![acore::default_let(vec![(
            moniker::FreeVar::fresh_named("single"),
            acore::str("value"),
        )])]);
        assert_term_eq!(result, expected);
    }

    #[test]
    fn test_trailing_newline() {
        let input = "1\n2\n";
        let result = parse(input);
        let expected = acore::list(vec![acore::num(1), acore::num(2)]);
        assert_term_eq!(result, expected);
    }
}
