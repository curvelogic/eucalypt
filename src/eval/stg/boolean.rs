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

/// `CLAUSE(c, r)` — build a cond clause (condition, result) pair.
pub struct Clause;

impl StgIntrinsic for Clause {
    fn name(&self) -> &str {
        "CLAUSE"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            2,
            data(DataConstructor::Clause.tag(), vec![lref(0), lref(1)]),
        )
    }
}

impl CallGlobal2 for Clause {}

/// `COND(clauses)` — multi-way conditional over a list of `CLAUSE` pairs.
///
/// Walks the list evaluating each clause's condition in turn.  Returns the
/// result of the first clause whose condition is `true`, or — if no clause
/// matches — the last element of the list treated as a default value.
///
/// Local-variable layout (de Bruijn, innermost first):
/// ```text
/// lambda(1):  local(0) = clauses
///   case(clauses):
///     ListNil  → null
///     ListCons → local(0)=head, local(1)=tail
///       case(head):
///         Clause → local(0)=c, local(1)=r, local(2)=head*, local(3)=tail
///           case(c):
///             BoolTrue  → local(1)  [= r]
///             BoolFalse → COND(local(3))  [= recurse on tail]
///         fallback (non-Clause default) → local(0)  [= head]
///     fallback (non-list) → null
/// ```
pub struct Cond;

impl StgIntrinsic for Cond {
    fn name(&self) -> &str {
        "COND"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        let self_idx = self.index();
        lambda(
            1,
            // Outer case: match on the clauses list.
            case(
                local(0),
                vec![
                    (DataConstructor::ListNil.tag(), unit()),
                    (
                        DataConstructor::ListCons.tag(),
                        // After ListCons match:
                        //   local(0) = head, local(1) = tail
                        // Inner case: check if head is a Clause pair.
                        case(
                            local(0),
                            vec![(
                                DataConstructor::Clause.tag(),
                                // After Clause match:
                                //   local(0)=c, local(1)=r, local(2)=head*, local(3)=tail
                                // Evaluate the condition c.
                                switch(
                                    local(0),
                                    vec![
                                        (DataConstructor::BoolTrue.tag(), local(1)),
                                        (
                                            DataConstructor::BoolFalse.tag(),
                                            app(gref(self_idx), vec![lref(3)]),
                                        ),
                                    ],
                                ),
                            )],
                            // Fallback: head is not a Clause — treat as default.
                            // At this point local(0)=head, local(1)=tail (from ListCons).
                            local(0),
                        ),
                    ),
                ],
                unit(),
            ),
        )
    }
}

impl CallGlobal1 for Cond {}
