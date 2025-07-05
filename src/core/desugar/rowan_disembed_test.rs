//! Tests for Rowan core embedding functionality

#[cfg(test)]
mod tests {
    use crate::syntax::rowan::parse_expr;

    // Test helper to create minimal working test setup
    fn test_with_minimal_setup<F>(input: &str, test_fn: F)
    where
        F: FnOnce(&crate::syntax::rowan::ast::Soup) -> Result<(), String>,
    {
        let parse_result = parse_expr(input);
        assert!(parse_result.errors().is_empty(), "Parse should succeed");

        let soup = parse_result.tree();

        // Run the test function
        if let Err(msg) = test_fn(&soup) {
            panic!("{}", msg);
        }
    }

    #[test]
    fn test_parse_core_embedding_syntax() {
        // Just test that core embedding syntax parses correctly
        test_with_minimal_setup(r#"[:c-var, "x"]"#, |soup| {
            let elements: Vec<_> = soup.elements().collect();
            if elements.len() != 1 {
                return Err("Expected single element".to_string());
            }

            match &elements[0] {
                crate::syntax::rowan::ast::Element::List(_) => Ok(()),
                _ => Err("Expected list element".to_string()),
            }
        });

        test_with_minimal_setup(r#"[:c-bif, :__ADD]"#, |soup| {
            let elements: Vec<_> = soup.elements().collect();
            if elements.len() != 1 {
                return Err("Expected single element".to_string());
            }

            match &elements[0] {
                crate::syntax::rowan::ast::Element::List(_) => Ok(()),
                _ => Err("Expected list element".to_string()),
            }
        });

        test_with_minimal_setup(r#"[:c-name, "foo"]"#, |_| Ok(()));
        test_with_minimal_setup(r#"[:c-lit, "hello"]"#, |_| Ok(()));
    }
}
