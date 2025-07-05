//! Import analysis.
//!
//! We analyse imports at the AST stage so we can get binding right on
//! desugar into core syntax.
use crate::driver::source::ParsedAst;
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
    #[error("bad import syntax")]
    ImportSyntax(#[from] SyntaxError),
    #[error("load failure in test")]
    LoadFailure(),
    #[error("detected cycle involving {0}")]
    Cycle(Box<Input>),
    #[error("unknown input {0}")]
    UnknownInput(Box<Input>),
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
                }
            }

            // Check each declaration for imports
            for decl in unit.declarations() {
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
fn read_rowan_soup_imports(
    soup: &crate::syntax::rowan::ast::Soup,
    imports: &mut Vec<Input>,
) -> Result<(), ImportError> {
    use crate::syntax::rowan::ast::{AstToken, Element, HasSoup};

    for element in soup.elements() {
        match element {
            Element::Block(block) => {
                // Check if this block contains import declarations
                for decl in block.declarations() {
                    if let Some(head) = decl.head() {
                        let kind = head.classify_declaration();
                        if let crate::syntax::rowan::ast::DeclarationKind::Property(prop) = kind {
                            let name = prop.text();
                            if name == "import" {
                                if let Some(body) = decl.body() {
                                    if let Some(body_soup) = body.soup() {
                                        scrape_rowan_imports(&body_soup, imports)?;
                                    }
                                }
                            }
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

/// Extract import values from a Rowan soup
fn scrape_rowan_imports(
    soup: &crate::syntax::rowan::ast::Soup,
    imports: &mut Vec<Input>,
) -> Result<(), ImportError> {
    use crate::syntax::rowan::ast::Element;

    for element in soup.elements() {
        match element {
            Element::Lit(literal) => {
                if let Some(crate::syntax::rowan::ast::LiteralValue::Str(s)) = literal.value() {
                    if let Some(import_str) = s.value() {
                        match Input::from_str(import_str) {
                            Ok(input) => {
                                imports.push(input);
                            }
                            Err(e) => {
                                return Err(ImportError::ImportSyntax(e));
                            }
                        }
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

    #[test]
    pub fn test_import_graph_basic() {
        let mut g = ImportGraph::default();

        // Test adding a leaf (data file)
        let input = Input::from(Locator::Resource("test.yaml".to_string()));
        g.add_leaf(input.clone()).unwrap();

        assert_eq!(g.graph().node_count(), 1);

        // Test that the input is found
        assert!(g.find_index(&input).is_some());
    }

    #[test]
    pub fn test_cycle_detection() {
        let g = ImportGraph::default();

        // Empty graph should have no cycles
        assert!(g.check_for_cycles().is_ok());
    }
}
