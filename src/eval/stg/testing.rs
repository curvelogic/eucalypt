#![cfg(test)]
//! Test support

use std::rc::Rc;

use crate::{
    common::sourcemap::SourceMap,
    eval::{
        emit::CapturingEmitter,
        machine::{intrinsic::StgIntrinsic, standard_machine, vm::Machine},
        memory::mutator::{Mutator, MutatorHeapView},
        stg::StgSettings,
    },
};

use super::{
    runtime::{self, Runtime},
    syntax::StgSyn,
};

lazy_static! {
    static ref SETTINGS: StgSettings = StgSettings {
        generate_annotations: true,
        trace_steps: true,
        render_type: Default::default(),
        suppress_updates: false,
        suppress_inlining: false,
        suppress_optimiser: false
    };
}

/// Create a runtime from the specified intrinsics and a blank sourcemap
pub fn runtime(mut bifs: Vec<Box<dyn StgIntrinsic>>) -> Box<dyn Runtime> {
    let mut rt = runtime::StandardRuntime::default();
    for bif in bifs.drain(..) {
        rt.add(bif);
    }
    rt.prepare(&mut SourceMap::default());
    Box::new(rt)
}

/// Create a machine for standard unit tests equipped with the
/// specified runtime
pub fn machine(runtime: &dyn Runtime, syntax: Rc<StgSyn>) -> Machine {
    standard_machine(
        &SETTINGS,
        syntax,
        Box::new(CapturingEmitter::default()),
        runtime,
    )
    .unwrap()
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
