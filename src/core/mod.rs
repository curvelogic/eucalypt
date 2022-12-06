//! The core expression representation and various processing phases
#![allow(clippy::result_large_err)]
pub mod analyse;
pub mod anaphora;
pub mod cook;
pub mod desugar;
pub mod doc;
pub mod error;
pub mod export;
#[macro_use]
pub mod expr;
pub mod inline;
pub mod metadata;
pub mod rt;
pub mod simplify;
pub mod target;
pub mod transform;
pub mod unit;
pub mod verify;
