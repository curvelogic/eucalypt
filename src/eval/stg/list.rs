//! List intrinsics

use crate::common::sourcemap::Smid;

use super::{
    intrinsic::{CallGlobal1, CallGlobal2, Const, StgIntrinsic},
    panic::Panic,
    syntax::{
        dsl::{annotated_lambda, case, data, local, lref, str, value},
        LambdaForm,
    },
    tags::DataConstructor,
};

/// A constant for CONS
pub struct Cons;

impl StgIntrinsic for Cons {
    fn name(&self) -> &str {
        "CONS"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            2, // [h t]
            data(DataConstructor::ListCons.tag(), vec![lref(0), lref(1)]),
            annotation,
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        value(data(DataConstructor::ListNil.tag(), vec![]))
    }
}

impl Const for Nil {}

/// (Unsafe) list TAIL
pub struct Tail;

impl StgIntrinsic for Tail {
    fn name(&self) -> &str {
        "TAIL"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1,
            case(
                local(0),
                vec![(DataConstructor::ListCons.tag(), local(1))],
                Panic.global(str("TAIL on empty list")),
            ),
            annotation,
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

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1,
            case(
                local(0),
                vec![(DataConstructor::ListCons.tag(), local(0))],
                Panic.global(str("HEAD on empty list")),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for Head {}
