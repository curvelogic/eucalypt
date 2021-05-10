//! Boolean constants and functions

use crate::common::sourcemap::SourceMap;

use super::intrinsic::{CallGlobal1, CallGlobal2, CallGlobal3, Const, StgIntrinsic};
use super::syntax::{dsl::*, tags, LambdaForm};

/// A constant for TRUE
pub struct True;

impl StgIntrinsic for True {
    fn name(&self) -> &str {
        "TRUE"
    }

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
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

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
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

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            1,
            switch(
                local(0),
                vec![(tags::BOOL_FALSE, t()), (tags::BOOL_TRUE, f())],
            ),
            source_map.add_synthetic(self.name()),
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

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            2,
            switch(
                local(0),
                vec![
                    (
                        tags::BOOL_TRUE,
                        switch(
                            local(1),
                            vec![(tags::BOOL_TRUE, t()), (tags::BOOL_FALSE, f())],
                        ),
                    ),
                    (tags::BOOL_FALSE, f()),
                ],
            ),
            source_map.add_synthetic(self.name()),
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

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            2,
            switch(
                local(0),
                vec![
                    (
                        tags::BOOL_FALSE,
                        switch(
                            local(1),
                            vec![(tags::BOOL_TRUE, t()), (tags::BOOL_FALSE, f())],
                        ),
                    ),
                    (tags::BOOL_TRUE, t()),
                ],
            ),
            source_map.add_synthetic(self.name()),
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

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            3,
            switch(
                local(0),
                vec![(tags::BOOL_TRUE, local(1)), (tags::BOOL_FALSE, local(2))],
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl CallGlobal3 for If {}
