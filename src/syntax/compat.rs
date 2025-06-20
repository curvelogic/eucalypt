//! Compatibility layer between LALRPOP and Rowan parsers

use crate::syntax::{
    ast::{ArgTuple, Block, Expression},
    error::{ParserError, SyntaxError},
    rowan::{self, ast::Unit},
};
use codespan::{ByteIndex, Span};
use codespan_reporting::files::SimpleFiles;

/// Convert a Rowan parse result to the old LALRPOP interface for units
pub fn parse_unit_compat<N, T>(files: &SimpleFiles<N, T>, id: usize) -> Result<Block, ParserError>
where
    N: AsRef<str> + Clone + std::fmt::Display,
    T: AsRef<str>,
{
    let text = files.get(id).unwrap().source().as_ref();
    let parse = rowan::parse_unit(text);
    
    if !parse.errors().is_empty() {
        // Convert first error to ParserError
        return Err(ParserError::Syntax(SyntaxError::InvalidInputFormat(
            id,
            "Rowan parser error".to_string(),
        )));
    }
    
    // Convert Rowan AST to LALRPOP AST
    let unit = parse.tree();
    convert_unit_to_block(unit, id)
}

/// Convert a Rowan parse result to the old LALRPOP interface for expressions
pub fn parse_expression_compat<N, T>(
    files: &SimpleFiles<N, T>,
    id: usize,
) -> Result<Expression, ParserError>
where
    N: AsRef<str> + Clone + std::fmt::Display,
    T: AsRef<str>,
{
    let text = files.get(id).unwrap().source().as_ref();
    let parse = rowan::parse_expr(text);
    
    if !parse.errors().is_empty() {
        // Convert first error to ParserError
        return Err(ParserError::Syntax(SyntaxError::InvalidInputFormat(
            id,
            "Rowan parser error".to_string(),
        )));
    }
    
    // Convert Rowan AST to LALRPOP AST
    let soup = parse.tree();
    convert_soup_to_expression(soup, id)
}

/// Convert Rowan Unit to LALRPOP Block
fn convert_unit_to_block(_unit: Unit, _file_id: usize) -> Result<Block, ParserError> {
    // For now, create a placeholder conversion
    // This would need to be implemented based on the actual AST structure differences
    Ok(Block {
        span: Span::new(ByteIndex(0), ByteIndex(0)),
        metadata: None,
        declarations: Vec::new(),
    })
}

/// Convert a Rowan parse result to the old LALRPOP interface for embedded lambdas
pub fn parse_embedded_lambda_compat<N, T>(
    _files: &SimpleFiles<N, T>,
    id: usize,
) -> Result<(ArgTuple, Expression), ParserError>
where
    N: AsRef<str> + Clone + std::fmt::Display,
    T: AsRef<str>,
{
    // For now, return an error since embedded lambda parsing is not yet implemented in Rowan
    Err(ParserError::Syntax(SyntaxError::InvalidInputFormat(
        id,
        "Embedded lambda parsing not yet implemented in Rowan parser".to_string(),
    )))
}

/// Convert Rowan Soup to LALRPOP Expression
fn convert_soup_to_expression(
    _soup: rowan::ast::Soup,
    _file_id: usize,
) -> Result<Expression, ParserError> {
    // For now, create a placeholder conversion
    // This would need to be implemented based on the actual AST structure differences
    Ok(Expression::OpSoup(
        Span::new(ByteIndex(0), ByteIndex(0)),
        Vec::new(),
    ))
}