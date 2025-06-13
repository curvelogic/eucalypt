#![allow(non_local_definitions)]
extern crate regex;
extern crate serde_json;
#[macro_use]
extern crate lalrpop_util;
extern crate codespan;
extern crate codespan_reporting;
extern crate indexmap;
extern crate itertools;
extern crate moniker;
extern crate pretty;
extern crate structopt;
extern crate thiserror;
extern crate unic_ucd_category;
extern crate url;
#[macro_use]
extern crate lazy_static;

pub mod common;
pub mod core;
pub mod driver;
pub mod eval;
pub mod export;
pub mod import;
pub mod syntax;
