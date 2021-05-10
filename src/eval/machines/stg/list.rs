//! List intrinsics

use crate::common::sourcemap::SourceMap;

use super::{
    intrinsic::{CallGlobal1, CallGlobal2, Const, StgIntrinsic},
    panic::Panic,
    syntax::{
        dsl::{annotated_lambda, case, data, local, lref, str, value},
        tags, LambdaForm,
    },
};

/// A constant for CONS
pub struct Cons;

impl StgIntrinsic for Cons {
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

impl CallGlobal2 for Cons {}

/// A constant for NIL
pub struct Nil;

impl StgIntrinsic for Nil {
    fn name(&self) -> &str {
        "NIL"
    }

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
        value(data(tags::LIST_NIL, vec![]))
    }
}

impl Const for Nil {}

/// (Unsafe) list TAIL
pub struct Tail;

impl StgIntrinsic for Tail {
    fn name(&self) -> &str {
        "TAIL"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            1,
            case(
                local(0),
                vec![(tags::LIST_CONS, local(1))],
                Panic.global(str("TAIL on empty list")),
            ),
            source_map.add_synthetic("TAIL"),
        )
    }
}

impl CallGlobal1 for Tail {}

/// (Unsafe) list HEAD
pub struct Head;

impl StgIntrinsic for Head {
    fn name(&self) -> &str {
        "HEAD"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            1,
            case(
                local(0),
                vec![(tags::LIST_CONS, local(0))],
                Panic.global(str("HEAD on empty list")),
            ),
            source_map.add_synthetic("HEAD"),
        )
    }
}

impl CallGlobal1 for Head {}
