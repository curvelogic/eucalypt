//! Import analysis.
//!
//! We analyse imports at the AST stage so we can get binding right on
//! desugar into core syntax.
use crate::syntax::ast::*;
use crate::syntax::error::SyntaxError;
use crate::syntax::input::Input;
use crate::driver::source::ParsedAst;
use petgraph::algo;
use petgraph::graph::Graph;
use petgraph::graph::NodeIndex;
use petgraph::visit::{Bfs, Walker};
use std::str::FromStr;
use thiserror::Error;

/// Analyse and load an import graph
pub struct ImportGraph {
    /// Import analysis
    graph: Graph<Input, ()>,
}

impl Default for ImportGraph {
    fn default() -> Self {
        ImportGraph {
            graph: Graph::new(),
        }
    }
}

impl ImportGraph {
    /// Analyse the AST for imports and add to graph, returning
    /// inputs for further load and analysis
    pub fn analyse_ast(
        &mut self,
        input: Input,
        ast: &Expression,
    ) -> Result<Vec<Input>, ImportError> {
        let source_node = self.encounter_input(input);

        let mut imports: Vec<Input> = vec![];
        read_expression_imports(ast, &mut imports)?;

        for i in &imports {
            let target_node = self.encounter_input(i.clone());
            self.graph.add_edge(source_node, target_node, ());
        }
        Ok(imports)
    }

    /// Analyse the Rowan AST for imports and add to graph, returning
    /// inputs for further load and analysis
    pub fn analyse_rowan_ast(
        &mut self,
        input: Input,
        ast: &ParsedAst,
    ) -> Result<Vec<Input>, ImportError> {
        let source_node = self.encounter_input(input);

        let mut imports: Vec<Input> = vec![];
        read_rowan_ast_imports(ast, &mut imports)?;

        for i in &imports {
            let target_node = self.encounter_input(i.clone());
            self.graph.add_edge(source_node, target_node, ());
        }
        Ok(imports)
    }

    /// Add a input to the graph for a file which cannot import others
    /// (e.g. data formats like YAML, JSON...)
    pub fn add_leaf(&mut self, input: Input) -> Result<(), ImportError> {
        self.encounter_input(input);
        Ok(())
    }

    /// Check for cycles in the import graph
    pub fn check_for_cycles(&self) -> Result<(), ImportError> {
        match algo::toposort(&self.graph, None) {
            Ok(_) => Ok(()),
            Err(c) => Err(ImportError::Cycle(Box::new(
                self.graph[c.node_id()].clone(),
            ))),
        }
    }

    /// Retrieve all locators contributing to a translation unit
    pub fn unit_inputs(&self, input: &Input) -> Result<Vec<&Input>, ImportError> {
        if let Some(i) = self.find_index(input) {
            Ok(Bfs::new(&self.graph, i)
                .iter(&self.graph)
                .map(|id| &self.graph[id])
                .collect())
        } else {
            Err(ImportError::UnknownInput(Box::new(input.clone())))
        }
    }

    /// Find the node index for the specified input
    fn find_index(&self, input: &Input) -> Option<NodeIndex> {
        self.graph.node_indices().find(|&i| &self.graph[i] == input)
    }

    /// Add locator to the graph if it isn't already there and return
    /// node index of the node.
    fn encounter_input(&mut self, input: Input) -> NodeIndex {
        match self.find_index(&input) {
            Some(i) => i,
            None => self.graph.add_node(input),
        }
    }

    /// Reference to the petgraph Graph itself
    pub fn graph(&self) -> &Graph<Input, ()> {
        &self.graph
    }
}

/// An error in reading, loading or analysing the import graph.
#[derive(PartialEq, Debug, Clone, Error, Eq)]
pub enum ImportError {
    #[error("non-string import value")]
    BadImportValue(Expression),
    #[error("bad import syntax")]
    ImportSyntax(#[from] SyntaxError),
    #[error("load failure in test")]
    LoadFailure(),
    #[error("detected cycle involving {0}")]
    Cycle(Box<Input>),
    #[error("unknown input {0}")]
    UnknownInput(Box<Input>),
}

/// Read all imports specified in the expression and add them to imports
fn read_expression_imports(ast: &Expression, imports: &mut Vec<Input>) -> Result<(), ImportError> {
    match ast {
        Expression::Block(block) => {
            if let Some(ref meta) = block.metadata {
                for input in scrape_metadata(meta)? {
                    imports.push(input);
                }
            }
            for decl in &block.declarations {
                if let Some(ref meta) = decl.metadata() {
                    for input in scrape_metadata(meta)? {
                        imports.push(input);
                    }
                }
                read_expression_imports(decl.definition(), imports)?;
            }
        }
        Expression::List(_, xs) | Expression::OpSoup(_, xs) | Expression::ApplyTuple(_, xs) => {
            for x in xs {
                read_expression_imports(x, imports)?;
            }
        }
        _ => (),
    }
    Ok(())
}

/// Read import of list of imports from string or list of strings
fn read_import_values(import: &Expression) -> Result<Vec<Input>, ImportError> {
    match import {
        Expression::Lit(Literal::Str(_, input_str)) => Ok(vec![Input::from_str(input_str)?]),
        Expression::List(_, strs) => {
            let mut imports = Vec::new();
            for str in strs {
                imports.extend(read_import_values(str)?);
            }
            Ok(imports)
        }
        e => Err(ImportError::BadImportValue(e.clone())),
    }
}

/// If the metadata is a block, read an import key as an Input.
fn scrape_metadata(metadata: &Expression) -> Result<Vec<Input>, ImportError> {
    if let Expression::Block(block) = metadata {
        if let Some(decl) = block
            .declarations
            .iter()
            .find(|d| d.name().name() == "import")
        {
            read_import_values(decl.definition())
        } else {
            Ok(vec![])
        }
    } else {
        Ok(vec![])
    }
}

/// Read all imports specified in the Rowan AST and add them to imports
fn read_rowan_ast_imports(ast: &ParsedAst, imports: &mut Vec<Input>) -> Result<(), ImportError> {
    use crate::syntax::rowan::ast::HasSoup;
    
    
    match ast {
        ParsedAst::Unit(unit) => {
            
            // Check unit metadata for imports
            if let Some(meta) = unit.meta() {
                if let Some(meta_soup) = meta.soup() {
                    read_rowan_soup_imports(&meta_soup, imports)?;
                } else {
                }
            } else {
            }
            
            // Check each declaration for imports
            for (i, decl) in unit.declarations().enumerate() {
                
                // Check declaration metadata
                if let Some(meta) = decl.meta() {
                    if let Some(meta_soup) = meta.soup() {
                        read_rowan_soup_imports(&meta_soup, imports)?;
                    }
                }
                
                // Check declaration body for imports
                if let Some(body) = decl.body() {
                    if let Some(body_soup) = body.soup() {
                        read_rowan_soup_imports(&body_soup, imports)?;
                    }
                }
            }
        }
        ParsedAst::Soup(soup) => {
            read_rowan_soup_imports(soup, imports)?;
        }
    }
    
    Ok(())
}

/// Read imports from a Rowan Soup expression
fn read_rowan_soup_imports(soup: &crate::syntax::rowan::ast::Soup, imports: &mut Vec<Input>) -> Result<(), ImportError> {
    use crate::syntax::rowan::ast::{Element, HasSoup, AstToken};
    
    
    for (i, element) in soup.elements().enumerate() {
        
        match element {
            Element::Block(block) => {
                
                // Check if this block contains import declarations
                for decl in block.declarations() {
                    if let Some(head) = decl.head() {
                        let kind = head.classify_declaration();
                        match kind {
                            crate::syntax::rowan::ast::DeclarationKind::Property(prop) => {
                                let name = prop.text();
                                if name == "import" {
                                    if let Some(body) = decl.body() {
                                        if let Some(body_soup) = body.soup() {
                                            scrape_rowan_imports(&body_soup, imports)?;
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                
                // Check block metadata for imports (recursive)
                if let Some(meta) = block.meta() {
                    if let Some(meta_soup) = meta.soup() {
                        read_rowan_soup_imports(&meta_soup, imports)?;
                    }
                }
                
                // Check declaration metadata (recursive)
                for decl in block.declarations() {
                    if let Some(meta) = decl.meta() {
                        if let Some(meta_soup) = meta.soup() {
                            read_rowan_soup_imports(&meta_soup, imports)?;
                        }
                    }
                    
                    // Check declaration body for imports (recursive)
                    if let Some(body) = decl.body() {
                        if let Some(body_soup) = body.soup() {
                            read_rowan_soup_imports(&body_soup, imports)?;
                        }
                    }
                }
            }
            Element::List(list) => {
                for item in list.items() {
                    read_rowan_soup_imports(&item, imports)?;
                }
            }
            Element::ParenExpr(paren) => {
                if let Some(soup) = paren.soup() {
                    read_rowan_soup_imports(&soup, imports)?;
                }
            }
            Element::ApplyTuple(tuple) => {
                for item in tuple.items() {
                    read_rowan_soup_imports(&item, imports)?;
                }
            }
            _ => {
                // Other elements (literals, names, string patterns) don't contain imports
            }
        }
    }
    Ok(())
}

/// Extract import values from a Rowan soup (equivalent to read_import_values for legacy AST)
fn scrape_rowan_imports(soup: &crate::syntax::rowan::ast::Soup, imports: &mut Vec<Input>) -> Result<(), ImportError> {
    use crate::syntax::rowan::ast::Element;
    
    
    for element in soup.elements() {
        match element {
            Element::Lit(literal) => {
                if let Some(value) = literal.value() {
                    match value {
                        crate::syntax::rowan::ast::LiteralValue::Str(s) => {
                            if let Some(import_str) = s.value() {
                                match Input::from_str(&import_str) {
                                    Ok(input) => {
                                        imports.push(input);
                                    }
                                    Err(e) => {
                                        return Err(ImportError::ImportSyntax(e));
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            Element::List(list) => {
                // Handle list of imports
                for item in list.items() {
                    scrape_rowan_imports(&item, imports)?;
                }
            }
            _ => {
                // Other elements might contain imports recursively
            }
        }
    }
    Ok(())
}

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::syntax::input::Locator;
    use crate::syntax::parser;
    use crate::syntax::rowan::ast::{Unit, Soup, Element, HasSoup, AstToken};
    use crate::syntax::ast::*;
    use codespan_reporting::files::SimpleFiles;

    /// Convert Rowan Soup to legacy Expression for testing
    fn rowan_soup_to_legacy_expression(soup: &Soup) -> Expression {
        let elements: Vec<Element> = soup.elements().collect();
        
        // If single element, convert it
        if elements.len() == 1 {
            match &elements[0] {
                Element::Block(block) => {
                    return rowan_block_to_legacy_expression(block);
                }
                Element::Lit(literal) => {
                    return rowan_literal_to_legacy_expression(literal);
                }
                Element::List(list) => {
                    return rowan_list_to_legacy_expression(list);
                }
                _ => {
                    // Handle other element types as needed
                }
            }
        }
        
        // Otherwise return a simple placeholder
        Expression::Lit(Literal::Str(codespan::Span::default(), "soup".to_string()))
    }
    
    /// Convert Rowan Literal to legacy Expression for testing  
    fn rowan_literal_to_legacy_expression(literal: &crate::syntax::rowan::ast::Literal) -> Expression {
        if let Some(value) = literal.value() {
            match value {
                crate::syntax::rowan::ast::LiteralValue::Str(s) => {
                    if let Some(text) = s.value() {
                        return Expression::Lit(Literal::Str(codespan::Span::default(), text.to_string()));
                    }
                }
                crate::syntax::rowan::ast::LiteralValue::Sym(sym) => {
                    if let Some(text) = sym.value() {
                        return Expression::Lit(Literal::Sym(codespan::Span::default(), text.to_string()));
                    }
                }
                crate::syntax::rowan::ast::LiteralValue::Num(num) => {
                    if let Some(n) = num.value() {
                        return Expression::Lit(Literal::Num(codespan::Span::default(), n));
                    }
                }
            }
        }
        
        // Fallback
        Expression::Lit(Literal::Str(codespan::Span::default(), "literal".to_string()))
    }
    
    /// Convert Rowan List to legacy Expression for testing
    fn rowan_list_to_legacy_expression(list: &crate::syntax::rowan::ast::List) -> Expression {
        let mut items = Vec::new();
        
        for item in list.items() {
            let item_expr = rowan_soup_to_legacy_expression(&item);
            items.push(item_expr);
        }
        
        Expression::List(codespan::Span::default(), items)
    }
    
    /// Convert Rowan Unit to legacy Expression for testing
    fn rowan_unit_to_legacy_expression(unit: &Unit) -> Expression {
        // Convert declarations to legacy format
        let mut declarations = Vec::new();
        
        for decl in unit.declarations() {
            if let Some(head) = decl.head() {
                let kind = head.classify_declaration();
                match kind {
                    crate::syntax::rowan::ast::DeclarationKind::Property(prop) => {
                        let name = prop.text().to_string();
                        if let Some(body) = decl.body() {
                            if let Some(body_soup) = body.soup() {
                                let body_expr = rowan_soup_to_legacy_expression(&body_soup);
                                declarations.push(Declaration::PropertyDeclaration(
                                    codespan::Span::default(),
                                    None, // metadata
                                    normal(&name),
                                    body_expr,
                                ));
                            }
                        }
                    }
                    _ => {
                        // Skip other declaration types for now
                    }
                }
            }
        }
        
        // Check for unit metadata
        let metadata = unit.meta().and_then(|m| m.soup()).map(|soup| rowan_soup_to_legacy_expression(&soup));
        
        Expression::Block(Box::new(Block {
            span: codespan::Span::default(),
            metadata,
            declarations,
        }))
    }
    
    /// Convert Rowan Block to legacy Expression for testing
    fn rowan_block_to_legacy_expression(block: &crate::syntax::rowan::ast::Block) -> Expression {
        let mut declarations = Vec::new();
        
        for decl in block.declarations() {
            if let Some(head) = decl.head() {
                let kind = head.classify_declaration();
                match kind {
                    crate::syntax::rowan::ast::DeclarationKind::Property(prop) => {
                        let name = prop.text().to_string();
                        if let Some(body) = decl.body() {
                            if let Some(body_soup) = body.soup() {
                                let body_expr = rowan_soup_to_legacy_expression(&body_soup);
                                declarations.push(Declaration::PropertyDeclaration(
                                    codespan::Span::default(),
                                    None, // metadata
                                    normal(&name),
                                    body_expr,
                                ));
                            }
                        }
                    }
                    _ => {
                        // Skip other declaration types for now
                    }
                }
            }
        }
        
        // Check for block metadata  
        let metadata = block.meta().and_then(|m| m.soup()).map(|soup| rowan_soup_to_legacy_expression(&soup));
        
        Expression::Block(Box::new(Block {
            span: codespan::Span::default(),
            metadata,
            declarations,
        }))
    }

    fn parse_expr(text: &'static str) -> Expression {
        // Parse with Rowan and convert to legacy AST for testing
        let parse_result = crate::syntax::rowan::parse_expr(text);
        if parse_result.errors().is_empty() {
            rowan_soup_to_legacy_expression(&parse_result.tree())
        } else {
            // Return error placeholder if parse fails
            Expression::Lit(crate::syntax::ast::Literal::Str(codespan::Span::default(), "parse_error".to_string()))
        }
    }

    pub fn parse_unit(text: &'static str) -> Expression {
        // Parse with Rowan and convert to legacy AST for testing  
        let parse_result = crate::syntax::rowan::parse_unit(text);
        if parse_result.errors().is_empty() {
            rowan_unit_to_legacy_expression(&parse_result.tree())
        } else {
            // Return error placeholder if parse fails
            Expression::Block(Box::new(crate::syntax::ast::Block {
                span: codespan::Span::default(),
                metadata: None,
                declarations: Vec::new(),
            }))
        }
    }

    #[test]
    pub fn test_scrape_metadata() {
        let expr = parse_expr("{ import: \"blah.eu\" }");
        let meta = scrape_metadata(&expr);
        assert_eq!(meta, Ok(vec![Input::from_str("blah.eu").unwrap()]));
    }

    #[test]
    pub fn test_scrape_metadata_list() {
        let expr = parse_expr("{ import: [\"blah.eu\", \"blurgh.eu\"] }");
        let meta = scrape_metadata(&expr);
        assert_eq!(
            meta,
            Ok(vec![
                Input::from_str("blah.eu").unwrap(),
                Input::from_str("blurgh.eu").unwrap()
            ])
        );
    }

    #[test]
    pub fn test_graph_imports() {
        let mut g = ImportGraph::default();
        let inputs = g
            .analyse_ast(
                Input::from_str("[resource:a]").unwrap(),
                &parse_unit(" { import: \"[resource:b]\"} ` { import: \"[resource:c]\" } foo: bar"),
            )
            .unwrap();

        assert_eq!(
            inputs,
            vec![
                Input::from(Locator::Resource("b".to_string())),
                Input::from(Locator::Resource("c".to_string()))
            ]
        );
        assert_eq!(g.graph().node_count(), 3);

        let inputs = g
            .analyse_ast(
                Input::from_str("[resource:b]").unwrap(),
                &parse_unit(" ` { import: \"[resource:d]\" } x: y"),
            )
            .unwrap();

        assert_eq!(
            inputs,
            vec![Input::from(Locator::Resource("d".to_string()))]
        );
        assert_eq!(g.graph().node_count(), 4);
    }
}
