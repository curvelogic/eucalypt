//! List intrinsics

use crate::common::sourcemap::SourceMap;

use super::{
    machine::StgIntrinsic,
    runtime::{call, StgWrapper},
    syntax::{
        dsl::{annotated_lambda, case, data, local, lref, str, value},
        tags, LambdaForm,
    },
};

/// A constant for CONS
pub struct Cons;

impl StgWrapper for Cons {
    fn name(&self) -> &str {
        "CONS"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            2, // [h t]
            data(tags::LIST_CONS, vec![lref(0), lref(1)]),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for Cons {
    fn execute(
        &self,
        _machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        panic!("CONS is STG only")
    }
}

/// A constant for NIL
pub struct Nil;

impl StgWrapper for Nil {
    fn name(&self) -> &str {
        "NIL"
    }

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
        value(data(tags::LIST_NIL, vec![]))
    }
}

impl StgIntrinsic for Nil {
    fn execute(
        &self,
        _machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        panic!("NIL is STG only")
    }
}

/// (Unsafe) list TAIL
pub struct Tail;

impl StgWrapper for Tail {
    fn name(&self) -> &str {
        "TAIL"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            1,
            case(
                local(0),
                vec![(tags::LIST_CONS, local(1))],
                call::global::panic(str("TAIL on empty list")),
            ),
            source_map.add_synthetic("TAIL"),
        )
    }
}

impl StgIntrinsic for Tail {
    fn execute(
        &self,
        _machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        panic!("TAIL is STG only")
    }
}

/// (Unsafe) list HEAD
pub struct Head;

impl StgWrapper for Head {
    fn name(&self) -> &str {
        "HEAD"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            1,
            case(
                local(0),
                vec![(tags::LIST_CONS, local(0))],
                call::global::panic(str("HEAD on empty list")),
            ),
            source_map.add_synthetic("HEAD"),
        )
    }
}

impl StgIntrinsic for Head {
    fn execute(
        &self,
        _machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        panic!("HEAD is STG only")
    }
}
