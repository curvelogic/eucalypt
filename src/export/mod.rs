pub mod error;
pub mod html;
pub mod json;
pub mod markup;
pub mod text;
pub mod toml;
pub mod yaml;

use self::html::HtmlMarkupSerialiser;

use super::export::toml::TomlEmitter;
use crate::eval::emit::Emitter;
use html::HtmlEmitter;
use json::JsonEmitter;
use std::io::Write;
use text::TextEmitter;
use yaml::YamlEmitter;

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
        "html" => Some(Box::new(HtmlEmitter::new(HtmlMarkupSerialiser::new(
            output,
        )))),
        _ => None,
    }
}
