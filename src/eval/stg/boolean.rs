//! Boolean constants and functions

use crate::{
    common::sourcemap::Smid,
    eval::machine::intrinsic::{CallGlobal1, CallGlobal2, CallGlobal3, Const, StgIntrinsic},
};

use super::{
    syntax::{dsl, dsl::*, LambdaForm},
    tags::DataConstructor,
};

/// A constant for TRUE
pub struct True;

impl StgIntrinsic for True {
    fn name(&self) -> &str {
        "TRUE"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        value(t())
    }
}

impl Const for True {}

/// A constant for FALSE
pub struct False;

impl StgIntrinsic for False {
    fn name(&self) -> &str {
        "FALSE"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        value(f())
    }
}

impl Const for False {}

/// A constant for NOT
pub struct Not;

impl StgIntrinsic for Not {
    fn name(&self) -> &str {
        "NOT"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            1,
            switch(
                local(0),
                vec![
                    (DataConstructor::BoolFalse.tag(), t()),
                    (DataConstructor::BoolTrue.tag(), f()),
                ],
            ),
        )
    }
}

impl CallGlobal1 for Not {}

/// Boolean AND
pub struct And;

impl StgIntrinsic for And {
    fn name(&self) -> &str {
        "AND"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            2,
            switch(
                local(0),
                vec![
                    (
                        DataConstructor::BoolTrue.tag(),
                        switch(
                            local(1),
                            vec![
                                (DataConstructor::BoolTrue.tag(), t()),
                                (DataConstructor::BoolFalse.tag(), f()),
                            ],
                        ),
                    ),
                    (DataConstructor::BoolFalse.tag(), f()),
                ],
            ),
        )
    }
}

impl CallGlobal2 for And {}

/// Boolean OR
pub struct Or;

impl StgIntrinsic for Or {
    fn name(&self) -> &str {
        "OR"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            2,
            switch(
                local(0),
                vec![
                    (
                        DataConstructor::BoolFalse.tag(),
                        switch(
                            local(1),
                            vec![
                                (DataConstructor::BoolTrue.tag(), t()),
                                (DataConstructor::BoolFalse.tag(), f()),
                            ],
                        ),
                    ),
                    (DataConstructor::BoolTrue.tag(), t()),
                ],
            ),
        )
    }
}

impl CallGlobal2 for Or {}

/// Boolean IF
pub struct If;

impl StgIntrinsic for If {
    fn name(&self) -> &str {
        "IF"
    }

    /// The then/else branches (indices 1 and 2) are each entered at
    /// most once, so they never need an `Update` continuation.
    fn single_use_args(&self) -> &[usize] {
        &[1, 2]
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            3,
            dsl::switch_suppress(
                local(0),
                vec![
                    (DataConstructor::BoolTrue.tag(), local(1)),
                    (DataConstructor::BoolFalse.tag(), local(2)),
                ],
            ),
        )
    }
}

impl CallGlobal3 for If {}
