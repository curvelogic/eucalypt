#![cfg(test)]
//! Test support
//!
//! Builds a [`BytecodeMachine`] from synthetic STG syntax for unit tests
//! across the `stg::*` intrinsic modules. Used to build on the deleted
//! HeapSyn `vm::Machine` (`standard_machine`); the bytecode engine is the
//! sole execution engine since the Phase 4 collapse (eu-oufc).

use std::rc::Rc;

use crate::{
    common::sourcemap::SourceMap,
    eval::{
        bytecode::{encode, BytecodeMachine},
        emit::CapturingEmitter,
        machine::intrinsic::StgIntrinsic,
        memory::mutator::{Mutator, MutatorHeapView},
    },
};

use super::{
    runtime::{self, Runtime},
    syntax::StgSyn,
};

/// Create a runtime from the specified intrinsics and a blank sourcemap
pub fn runtime(mut bifs: Vec<Box<dyn StgIntrinsic>>) -> Box<dyn Runtime> {
    let mut rt = runtime::StandardRuntime::default();
    for bif in bifs.drain(..) {
        rt.add(bif);
    }
    rt.prepare(&mut SourceMap::default());
    Box::new(rt)
}

/// Create a bytecode machine for standard unit tests, equipped with the
/// specified runtime. `syntax` is encoded fresh against `runtime`'s globals
/// (no prelude blob — these are self-contained synthetic programs).
pub fn machine(runtime: &dyn Runtime, syntax: Rc<StgSyn>) -> BytecodeMachine<'_> {
    let globals = runtime.globals();
    let (prog, root, gforms) = encode(&syntax, &globals);
    BytecodeMachine::new(
        prog,
        root,
        &gforms,
        runtime.intrinsics(),
        Box::new(CapturingEmitter::default()),
        2, // heap_limit_mib, matching the deleted vm.rs SETTINGS
        false,
    )
    .expect("build bytecode machine for test")
}

impl<F> Mutator for F
where
    F: Fn(&MutatorHeapView),
{
    type Input = ();
    type Output = ();

    fn run(
        &self,
        view: &crate::eval::memory::mutator::MutatorHeapView,
        _input: Self::Input,
    ) -> Result<Self::Output, crate::eval::error::ExecutionError> {
        self(view);
        Ok(())
    }
}
