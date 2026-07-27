pub mod edn;
pub mod error;
pub mod eu;
pub mod html;
pub mod json;
pub mod markup;
pub mod table;
pub mod text;
pub mod toml;
pub mod yaml;

use self::html::HtmlMarkupSerialiser;

use super::export::toml::TomlEmitter;
use crate::eval::emit::Emitter;
use edn::EdnEmitter;
use eu::EuEmitter;
use html::HtmlEmitter;
use json::JsonEmitter;
use std::io::Write;
use text::TextEmitter;
use yaml::YamlEmitter;

/// Remediation shared by the formats whose integers are bounded by `i64`.
///
/// YAML and TOML both reject an integer above `i64::MAX`; the advice for
/// each is identical, so it lives here rather than being duplicated and
/// allowed to drift (eu-1tkk.7.20, eu-1tkk.7.23).
pub(crate) const INTEGER_RANGE_NOTES: [&str; 2] = [
    "render to a format that can carry the value — 'json', 'edn', 'text' and \
     'eu' output all keep integers of this magnitude",
    "to keep the exact digits in this format, convert the value to a string \
     first with 'str', e.g. 'n str'",
];

/// Create an emitter for the format specified
///
/// Return None if the format is not recognised.
pub fn create_emitter<'a, S: AsRef<str>>(
    format: S,
    output: &'a mut (dyn Write + 'a),
) -> Option<Box<dyn Emitter + 'a>> {
    match format.as_ref() {
        "yaml" => Some(Box::new(YamlEmitter::new(output))),
        "toml" => Some(Box::new(TomlEmitter::new(output))),
        "json" => Some(Box::new(JsonEmitter::new(output))),
        "text" => Some(Box::new(TextEmitter::new(output))),
        "edn" => Some(Box::new(EdnEmitter::new(output))),
        "eu" => Some(Box::new(EuEmitter::new(output))),
        "html" => Some(Box::new(HtmlEmitter::new(HtmlMarkupSerialiser::new(
            output,
        )))),
        _ => None,
    }
}
