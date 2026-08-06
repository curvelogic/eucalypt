//! STG machine
//!
//! Shared infrastructure (environments, intrinsic ABI, metrics, crash
//! diagnostics, the SIGINT flag) used by the bytecode engine
//! (`crate::eval::bytecode`), the sole execution engine since the Phase 4
//! collapse (eu-oufc) deleted the HeapSyn tree-walk machine this module used
//! to also house (`Initialiser`/`standard_machine`, built on `vm::Machine`).

pub mod crash;
pub mod env;
pub mod intrinsic;
pub mod metrics;
pub mod vm;
