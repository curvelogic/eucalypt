//! Boolean constants and functions

use crate::common::sourcemap::SourceMap;

use super::syntax::{dsl::*, tags, LambdaForm};
use super::{machine::StgIntrinsic, runtime::StgWrapper};

/// A constant for TRUE
pub struct True;

impl StgWrapper for True {
    fn name(&self) -> &str {
        "TRUE"
    }

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
        value(t())
    }
}

impl StgIntrinsic for True {
    fn execute(
        &self,
        _machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        panic!("TRUE is STG only")
    }
}

/// A constant for FALSE
pub struct False;

impl StgWrapper for False {
    fn name(&self) -> &str {
        "FALSE"
    }

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
        value(f())
    }
}

impl StgIntrinsic for False {
    fn execute(
        &self,
        _machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        panic!("FALSE is STG only")
    }
}

/// A constant for NOT
pub struct Not;

impl StgWrapper for Not {
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

impl StgIntrinsic for Not {
    fn execute(
        &self,
        _machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        panic!("NOT is STG only")
    }
}

/// Boolean AND
pub struct And;

impl StgWrapper for And {
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

impl StgIntrinsic for And {
    fn execute(
        &self,
        _machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        panic!("AND is STG only")
    }
}

/// Boolean OR
pub struct Or;

impl StgWrapper for Or {
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

impl StgIntrinsic for Or {
    fn execute(
        &self,
        _machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        panic!("OR is STG only")
    }
}

/// Boolean IF
pub struct If;

impl StgWrapper for If {
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

impl StgIntrinsic for If {
    fn execute(
        &self,
        _machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        panic!("IF is STG only")
    }
}
