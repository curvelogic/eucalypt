//! Import analysis.
//!
//! We analyse imports at the AST stage so we can get binding right on
//! desugar into core syntax.
use crate::syntax::ast::*;
use crate::syntax::error::SyntaxError;
use crate::syntax::input::Input;
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
            Err(c) => Err(ImportError::Cycle(self.graph[c.node_id()].clone())),
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
            Err(ImportError::UnknownInput(input.clone()))
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
    Cycle(Input),
    #[error("unknown input {0}")]
    UnknownInput(Input),
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

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::syntax::input::Locator;
    use crate::syntax::parser::tests::ParseTester;

    fn parse_expr(text: &'static str) -> Expression {
        let mut parser = ParseTester::new();
        parser.parse_expr(text)
    }

    pub fn parse_unit(text: &'static str) -> Expression {
        let mut parser = ParseTester::new();
        Expression::Block(Box::new(parser.parse_unit(text)))
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
