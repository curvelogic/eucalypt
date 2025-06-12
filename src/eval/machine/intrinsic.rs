//! Intrinsic traits

use std::rc::Rc;

use lru::LruCache;
use regex::Regex;

use crate::{
    common::sourcemap::Smid,
    eval::stg::wrap::wrap,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        intrinsics,
        memory::{
            mutator::MutatorHeapView,
            syntax::{Ref, RefPtr},
        },
        stg::syntax::{dsl, StgSyn},
    },
};

use super::{
    env::{EnvFrame, SynClosure},
    vm::HeapNavigator,
};

/// Machine interface exposed to intrinsic implementations
pub trait IntrinsicMachine {
    /// Access the regex cache
    fn rcache(&mut self) -> &mut LruCache<String, Regex>;

    /// Update closure after completion
    fn set_closure(&mut self, closure: SynClosure) -> Result<(), ExecutionError>;

    /// Get a navigator for resolving references
    fn nav<'guard>(&'guard self, view: MutatorHeapView<'guard>) -> HeapNavigator<'guard>;

    /// Constant empty environment
    fn root_env(&self) -> RefPtr<EnvFrame>;

    /// Environment of current closure
    fn env(&self, view: MutatorHeapView) -> RefPtr<EnvFrame>;
}

/// All intrinsics have an STG syntax wrapper
pub trait StgIntrinsic: Sync {
    /// The name of the intrinsic
    fn name(&self) -> &str;

    /// The STG wrapper for calling the intrinsic
    fn wrapper(&self, annotation: Smid) -> crate::eval::stg::syntax::LambdaForm {
        wrap(self.index(), self.info(), annotation)
    }

    /// Whether the compiler should inline the wrapper
    fn inlinable(&self) -> bool {
        true
    }

    /// Index of the intrinsic
    fn index(&self) -> usize {
        intrinsics::index(self.name()).unwrap()
    }

    /// Type and arity information for the intrinsic
    fn info(&self) -> &intrinsics::Intrinsic {
        intrinsics::intrinsic(self.index())
    }

    /// An intrinsic has mutable access to the machine
    ///
    /// A call to an intrinsic may assume that its strict arguments are
    /// already evaluated (by the corresponding global wrapper) but must
    /// take care of updating the machine's closure and stack as
    /// appropriate to constitute a return.
    fn execute(
        &self,
        _machine: &mut dyn IntrinsicMachine,
        _heap: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        panic!("{} is STG-only", self.name());
    }

    /// A Ref to this global
    fn gref(&self) -> crate::eval::stg::syntax::Ref {
        dsl::gref(self.index())
    }
}

pub trait Const: StgIntrinsic {
    fn global(&self) -> Rc<StgSyn> {
        dsl::global(self.index())
    }
}

pub trait CallGlobal0: StgIntrinsic {
    fn global(&self) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![])
    }
}

pub trait CallGlobal1: StgIntrinsic {
    fn global(&self, x: crate::eval::stg::syntax::Ref) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x])
    }
}

pub trait CallGlobal2: StgIntrinsic {
    fn global(
        &self,
        x: crate::eval::stg::syntax::Ref,
        y: crate::eval::stg::syntax::Ref,
    ) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x, y])
    }
}

pub trait CallGlobal3: StgIntrinsic {
    fn global(
        &self,
        x: crate::eval::stg::syntax::Ref,
        y: crate::eval::stg::syntax::Ref,
        z: crate::eval::stg::syntax::Ref,
    ) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x, y, z])
    }
}

pub trait CallGlobal7: StgIntrinsic {
    #[allow(clippy::too_many_arguments)]
    fn global(
        &self,
        x0: crate::eval::stg::syntax::Ref,
        x1: crate::eval::stg::syntax::Ref,
        x2: crate::eval::stg::syntax::Ref,
        x3: crate::eval::stg::syntax::Ref,
        x4: crate::eval::stg::syntax::Ref,
        x5: crate::eval::stg::syntax::Ref,
        x6: crate::eval::stg::syntax::Ref,
    ) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x0, x1, x2, x3, x4, x5, x6])
    }
}
