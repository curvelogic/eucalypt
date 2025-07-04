//! The runtime provides intrinsic implementations and corresponding
//! global wrapper STG forms to allow the machine to do real work.
use crate::{
    common::{
        prettify::{prettify, ToPretty},
        sourcemap::SourceMap,
    },
    eval::{
        emit::Emitter,
        machine::intrinsic::{IntrinsicMachine, StgIntrinsic},
        memory::{mutator::MutatorHeapView, syntax::Ref},
        stg::support::call,
    },
};

use pretty::{DocAllocator, DocBuilder};

use super::syntax::{
    dsl::{self},
    LambdaForm,
};
use crate::{
    common::sourcemap::Smid,
    eval::{error::ExecutionError, intrinsics},
};

/// The STG-machine can be run with different sets of intrinsics
/// available (mainly for testing). A runtime packages together the
/// intrinsics and their STG-syntax wrappers.
pub trait Runtime: Sync {
    /// Initialise a runtime, registering source map info for any
    /// annotations
    fn prepare(&mut self, source_map: &mut SourceMap);

    /// Provide an environment of globals wrappers
    fn globals(&self) -> Vec<LambdaForm>;

    /// Provide the (immutable) intrinsic implementations
    fn intrinsics(&self) -> Vec<&dyn StgIntrinsic>;
}

pub enum NativeVariant {
    Boxed,
    Unboxed,
}

pub struct StandardRuntime {
    /// Intrinsic implementations
    impls: Vec<Box<dyn StgIntrinsic>>,
    /// Annotation SMIDs to apply to globals
    annotations: Option<Vec<Smid>>,
}

impl Default for StandardRuntime {
    fn default() -> Self {
        let impls = intrinsics::catalogue()
            .iter()
            .map(|i| -> Box<dyn StgIntrinsic> { Box::new(Unimplemented::new(i.name())) })
            .collect();
        StandardRuntime {
            impls,
            annotations: None,
        }
    }
}

impl StandardRuntime {
    pub fn add(&mut self, imp: Box<dyn StgIntrinsic>) {
        let index = imp.index();
        self.impls[index] = imp;
    }
}

impl ToPretty for StandardRuntime {
    fn pretty<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        let docs = self.lambdas().into_iter().enumerate().map(|(i, lam)| {
            let name = intrinsics::intrinsic(i).name();

            allocator
                .text(format!("(⊗{i}) "))
                .append(name)
                .append(":")
                .append(allocator.space())
                .append(allocator.line())
                .append(allocator.text(prettify(&lam)))
                .append(allocator.line())
        });

        allocator.intersperse(docs, allocator.line())
    }
}

impl StandardRuntime {
    /// Generate the wrappers to populate the global environment
    fn lambdas(&self) -> Vec<LambdaForm> {
        let smids = self.annotations.as_ref().expect("runtime not initialised");

        self.impls
            .iter()
            .zip(smids)
            .map(|(g, ann)| g.wrapper(*ann))
            .collect()
    }
}

impl Runtime for StandardRuntime {
    /// Generate the STG wrappers for all the intrinsics
    fn prepare(&mut self, source_map: &mut SourceMap) {
        self.annotations = Some(
            self.impls
                .iter()
                .map(|bif| source_map.add_synthetic(bif.name()))
                .collect(),
        )
    }

    /// Provide all global STG wrappers for the machine
    ///
    /// Must not be called until globals have been generated
    fn globals(&self) -> Vec<LambdaForm> {
        self.lambdas()
    }

    /// Provide reference to intrinsic implementations for the machine
    fn intrinsics(&self) -> Vec<&dyn StgIntrinsic> {
        self.impls.iter().map(|b| b.as_ref()).collect()
    }
}

#[derive(Clone)]
pub struct Unimplemented {
    name: &'static str,
}

impl Unimplemented {
    fn new(name: &'static str) -> Self {
        Unimplemented { name }
    }
}

impl StgIntrinsic for Unimplemented {
    fn name(&self) -> &str {
        self.name
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        dsl::value(call::bif::panic(dsl::str(format!(
            "unimplemented intrinsic wrapper {}",
            self.name()
        ))))
    }

    fn execute(
        &self,
        _machine: &mut dyn IntrinsicMachine,
        _view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        panic!("executing unimplemented intrinsic {}", self.name())
    }
}
