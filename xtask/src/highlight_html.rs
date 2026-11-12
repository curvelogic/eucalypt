//! `cargo xtask highlight-html` — render eucalypt source to highlighted HTML.
//!
//! Highlighting comes from the tree-sitter grammar in
//! `editors/tree-sitter-eucalypt` and its `queries/highlights.scm`, through
//! `tree-sitter-highlight`'s `HtmlRenderer`. Each highlighted span carries
//! one class per level of its capture name, so `@string.escape` becomes
//! `class="ts-string ts-string-escape"` and a capture without a rule of its
//! own falls back to its parent's colour.
//!
//! ```text
//! cargo xtask highlight-html [-o OUT] [--title TITLE] [--fragment] FILE.eu...
//! ```
//!
//! Without `-o` the page is written to stdout. `--fragment` writes only the
//! `<pre>` blocks (no page, no stylesheet), for embedding in other pages.

use std::{fmt::Write as _, path::PathBuf};

use anyhow::{bail, Context, Result};
use tree_sitter_highlight::{HighlightConfiguration, Highlighter, HtmlRenderer};

/// Colours for each highlight name, as `(name, light, dark)`. Every
/// capture `highlights.scm` declares (other than `@_` internals) must have
/// an entry here; a test enforces it.
const PALETTE: &[(&str, &str, &str)] = &[
    ("attribute", "#8a3ffc", "#c4a3ff"),
    ("comment", "#6e7781", "#8b949e"),
    ("comment.documentation", "#57606a", "#a5b1bd"),
    ("constant", "#0550ae", "#79c0ff"),
    ("function", "#8250df", "#d2a8ff"),
    ("function.builtin", "#953800", "#ffa657"),
    ("function.call", "#6639ba", "#d2a8ff"),
    ("keyword", "#cf222e", "#ff7b72"),
    ("number", "#0550ae", "#79c0ff"),
    ("operator", "#cf222e", "#ff7b72"),
    ("parameter", "#953800", "#ffa657"),
    ("property", "#116329", "#7ee787"),
    ("punctuation", "#24292f", "#c9d1d9"),
    ("punctuation.bracket", "#57606a", "#8b949e"),
    ("punctuation.delimiter", "#57606a", "#8b949e"),
    ("punctuation.special", "#8a3ffc", "#c4a3ff"),
    ("string", "#0a3069", "#a5d6ff"),
    ("string.escape", "#0550ae", "#79c0ff"),
    ("string.special", "#1a7f37", "#56d364"),
    ("variable", "#24292f", "#c9d1d9"),
    ("variable.special", "#bf3989", "#ff9bce"),
];

/// Names whose styles are bold or italic, on top of their colour.
const EMPHASIS: &[(&str, &str)] = &[
    ("comment", "font-style: italic;"),
    ("keyword", "font-weight: 600;"),
    ("function", "font-weight: 600;"),
];

/// A CSS class for a dotted highlight name: `string.escape` → `ts-string-escape`.
fn class_of(name: &str) -> String {
    format!("ts-{}", name.replace('.', "-"))
}

/// The highlight names the query captures, less `@_` internals.
fn highlight_names(config: &HighlightConfiguration) -> Vec<String> {
    config
        .names()
        .iter()
        .filter(|n| !n.starts_with('_'))
        .map(|n| n.to_string())
        .collect()
}

/// The `class` attribute for each recognised name, with one class per
/// level of the dotted name.
fn class_attributes(names: &[String]) -> Vec<String> {
    names
        .iter()
        .map(|name| {
            let parts: Vec<&str> = name.split('.').collect();
            let classes: Vec<String> = (1..=parts.len())
                .map(|n| class_of(&parts[..n].join(".")))
                .collect();
            format!("class=\"{}\"", classes.join(" "))
        })
        .collect()
}

/// A highlighter for eucalypt, configured for every name the query captures.
pub struct EuHighlighter {
    config: HighlightConfiguration,
    attributes: Vec<String>,
}

impl EuHighlighter {
    pub fn new() -> Result<Self> {
        let mut config = HighlightConfiguration::new(
            tree_sitter_eucalypt::language(),
            "eucalypt",
            tree_sitter_eucalypt::HIGHLIGHTS_QUERY,
            "",
            "",
        )
        .context("highlights.scm does not compile against the grammar")?;
        let names = highlight_names(&config);
        config.configure(&names);
        let attributes = class_attributes(&names);
        Ok(EuHighlighter { config, attributes })
    }

    /// Render `source` as a `<pre class="eu"><code>` block.
    pub fn render(&self, source: &str) -> Result<String> {
        let mut highlighter = Highlighter::new();
        let events = highlighter
            .highlight(&self.config, source.as_bytes(), None, |_| None)
            .context("tree-sitter highlighting failed")?;
        let mut renderer = HtmlRenderer::new();
        renderer
            .render(events, source.as_bytes(), &|h, out: &mut Vec<u8>| {
                out.extend_from_slice(self.attributes[h.0].as_bytes())
            })
            .context("rendering highlighted HTML failed")?;
        let code = String::from_utf8(renderer.html).context("rendered HTML is not UTF-8")?;
        Ok(format!(
            "<pre class=\"eu\"><code>{}</code></pre>\n",
            code.trim_end_matches('\n')
        ))
    }
}

/// The stylesheet: custom properties for light and dark, and one rule per
/// highlight name. Dark applies under `prefers-color-scheme: dark` unless
/// the page sets `data-theme="light"`, or whenever it sets `data-theme="dark"`.
pub fn stylesheet() -> String {
    let var = |name: &str| format!("--eu-{}", name.replace('.', "-"));
    let vars = |dark: bool| -> String {
        PALETTE
            .iter()
            .fold(String::new(), |mut s, (name, light, dark_c)| {
                let _ = writeln!(s, "  {}: {};", var(name), if dark { dark_c } else { light });
                s
            })
    };
    let mut css = String::new();
    css.push_str(":root {\n  --eu-bg: #f6f8fa;\n  --eu-fg: #24292f;\n  --eu-page: #ffffff;\n");
    css.push_str(&vars(false));
    css.push_str("}\n");
    let dark_block = format!(
        "  --eu-bg: #161b22;\n  --eu-fg: #c9d1d9;\n  --eu-page: #0d1117;\n{}",
        vars(true)
    );
    let _ = write!(
        css,
        "@media (prefers-color-scheme: dark) {{\n:root:not([data-theme=\"light\"]) {{\n{dark_block}}}\n}}\n\
         :root[data-theme=\"dark\"] {{\n{dark_block}}}\n"
    );
    css.push_str(
        "body { background: var(--eu-page); color: var(--eu-fg); margin: 0; padding: 1rem; \
         font-family: system-ui, sans-serif; }\n\
         h2.eu-file { font-size: 1rem; font-weight: 600; margin: 1.5rem 0 0.5rem; }\n\
         pre.eu { background: var(--eu-bg); color: var(--eu-fg); padding: 1rem; \
         border-radius: 6px; overflow-x: auto; line-height: 1.45; \
         font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; \
         font-size: 0.875rem; tab-size: 2; }\n",
    );
    for (name, _, _) in PALETTE {
        let _ = write!(
            css,
            "pre.eu .{} {{ color: var({});",
            class_of(name),
            var(name)
        );
        if let Some((_, extra)) = EMPHASIS.iter().find(|(n, _)| n == name) {
            let _ = write!(css, " {extra}");
        }
        css.push_str(" }\n");
    }
    css
}

fn escape_text(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// A standalone HTML page holding each `(heading, block)` in order.
pub fn page(title: &str, blocks: &[(String, String)]) -> String {
    let mut html = String::new();
    let _ = write!(
        html,
        "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n\
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
         <title>{}</title>\n<style>\n{}</style>\n</head>\n<body>\n",
        escape_text(title),
        stylesheet()
    );
    for (heading, block) in blocks {
        if blocks.len() > 1 {
            let _ = writeln!(html, "<h2 class=\"eu-file\">{}</h2>", escape_text(heading));
        }
        html.push_str(block);
    }
    html.push_str("</body>\n</html>\n");
    html
}

pub fn run(args: &mut impl Iterator<Item = String>) -> Result<()> {
    let mut files = Vec::new();
    let mut out: Option<PathBuf> = None;
    let mut title: Option<String> = None;
    let mut fragment = false;
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-o" | "--output" => out = Some(args.next().context("-o needs a path")?.into()),
            "--title" => title = Some(args.next().context("--title needs a value")?),
            "--fragment" => fragment = true,
            flag if flag.starts_with('-') => bail!("unknown highlight-html option: {flag}"),
            file => files.push(PathBuf::from(file)),
        }
    }
    if files.is_empty() {
        bail!("usage: cargo xtask highlight-html [-o OUT] [--title T] [--fragment] FILE.eu...");
    }

    let highlighter = EuHighlighter::new()?;
    let mut blocks = Vec::new();
    for file in &files {
        let source =
            std::fs::read_to_string(file).with_context(|| format!("reading {}", file.display()))?;
        blocks.push((file.display().to_string(), highlighter.render(&source)?));
    }
    let html = if fragment {
        blocks.iter().map(|(_, b)| b.as_str()).collect()
    } else {
        let title = title.unwrap_or_else(|| blocks[0].0.clone());
        page(&title, &blocks)
    };
    match out {
        Some(path) => {
            std::fs::write(&path, html).with_context(|| format!("writing {}", path.display()))?
        }
        None => print!("{html}"),
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_capture_has_a_style() {
        let h = EuHighlighter::new().unwrap();
        let names = highlight_names(&h.config);
        assert!(!names.is_empty());
        let css = stylesheet();
        for name in names {
            let rule = format!("pre.eu .{} {{", class_of(&name));
            assert!(css.contains(&rule), "no style for capture @{name}");
        }
    }

    #[test]
    fn renders_spans_and_escapes() {
        let h = EuHighlighter::new().unwrap();
        let src = "# note <x>\n` \"double it\"\ndouble(n): n * 2\nx: double(3) & \"a<b\"\n";
        let html = h.render(src).unwrap();
        assert!(html.starts_with("<pre class=\"eu\"><code>"));
        assert!(html.contains("<span class=\"ts-comment\"># note &lt;x&gt;</span>"));
        assert!(html.contains("<span class=\"ts-function\">double</span>"));
        assert!(html.contains("<span class=\"ts-parameter\">n</span>"));
        assert!(html.contains("<span class=\"ts-number\">2</span>"));
        assert!(html.contains("<span class=\"ts-comment ts-comment-documentation\">"));
        assert!(html.contains("a&lt;b"));
        assert!(!html.contains("<b"), "unescaped source text in output");
    }

    #[test]
    fn renders_every_aoc25_example() {
        let h = EuHighlighter::new().unwrap();
        let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/../examples/aoc25");
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_eucalypt::language())
            .unwrap();
        let mut count = 0;
        for entry in std::fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.extension().is_some_and(|e| e == "eu") {
                let source = std::fs::read_to_string(&path).unwrap();
                let tree = parser.parse(&source, None).unwrap();
                assert!(
                    !tree.root_node().has_error(),
                    "{} has ERROR nodes",
                    path.display()
                );
                let html = h.render(&source).unwrap();
                assert!(
                    html.contains("<span class="),
                    "{} unhighlighted",
                    path.display()
                );
                count += 1;
            }
        }
        assert!(count > 0, "no examples found in {dir}");
    }

    #[test]
    fn page_is_standalone() {
        let html = page(
            "a <b>",
            &[("f.eu".into(), "<pre class=\"eu\"></pre>\n".into())],
        );
        assert!(html.starts_with("<!doctype html>"));
        assert!(html.contains("<title>a &lt;b&gt;</title>"));
        assert!(html.contains("prefers-color-scheme: dark"));
        assert!(html.contains(":root[data-theme=\"dark\"]"));
    }
}
