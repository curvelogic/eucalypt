//! Intrinsics for arithmetic functions

use serde_json::Number;

use crate::eval::error::ExecutionError;

use super::runtime::{machine_return_num, num_arg};
use super::syntax::Ref;
use super::{intrinsic::StgIntrinsic, machine::Machine, runtime::machine_return_bool};

/// ADD(l, r) - add l to r
pub struct Add;

impl StgIntrinsic for Add {
    fn name(&self) -> &str {
        "ADD"
    }

    fn execute(
        &self,
        machine: &mut Machine,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, &args[0])?;
        let y = num_arg(machine, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let total = l
                .checked_add(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, Number::from(total))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let total = l
                .checked_add(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, Number::from(total))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l + r) {
                machine_return_num(machine, ret)
            } else {
                Err(ExecutionError::NumericDomainError(x, y))
            }
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

/// SUB(l, r) - sub r from l
pub struct Sub;

impl StgIntrinsic for Sub {
    fn name(&self) -> &str {
        "SUB"
    }

    fn execute(
        &self,
        machine: &mut Machine,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, &args[0])?;
        let y = num_arg(machine, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let result = l
                .checked_sub(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let result = l
                .checked_sub(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l - r) {
                machine_return_num(machine, ret)
            } else {
                Err(ExecutionError::NumericDomainError(x, y))
            }
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

/// MUL(l, r) - mul r from l
pub struct Mul;

impl StgIntrinsic for Mul {
    fn name(&self) -> &str {
        "MUL"
    }

    fn execute(
        &self,
        machine: &mut Machine,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, &args[0])?;
        let y = num_arg(machine, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let product = l
                .checked_mul(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, Number::from(product))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let product = l
                .checked_mul(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, Number::from(product))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l * r) {
                machine_return_num(machine, ret)
            } else {
                Err(ExecutionError::NumericDomainError(x, y))
            }
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

/// DIV(l, r) - div l by r
pub struct Div;

impl StgIntrinsic for Div {
    fn name(&self) -> &str {
        "DIV"
    }

    fn execute(
        &self,
        machine: &mut Machine,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, &args[0])?;
        let y = num_arg(machine, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let result = l
                .checked_div(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let result = l
                .checked_div(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l / r) {
                machine_return_num(machine, ret)
            } else {
                Err(ExecutionError::NumericDomainError(x, y))
            }
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

/// GT(l, r) l > r
pub struct Gt;

impl StgIntrinsic for Gt {
    fn name(&self) -> &str {
        "GT"
    }

    fn execute(
        &self,
        machine: &mut Machine,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, &args[0])?;
        let y = num_arg(machine, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            machine_return_bool(machine, l > r)
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            machine_return_bool(machine, l > r)
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            machine_return_bool(machine, l > r)
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

/// GTE(l, r) l >= r
pub struct Gte;

impl StgIntrinsic for Gte {
    fn name(&self) -> &str {
        "GTE"
    }

    fn execute(
        &self,
        machine: &mut Machine,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, &args[0])?;
        let y = num_arg(machine, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            machine_return_bool(machine, l >= r)
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            machine_return_bool(machine, l >= r)
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            machine_return_bool(machine, l >= r)
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

/// LT(l, r) l > r
pub struct Lt;

impl StgIntrinsic for Lt {
    fn name(&self) -> &str {
        "LT"
    }

    fn execute(
        &self,
        machine: &mut Machine,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, &args[0])?;
        let y = num_arg(machine, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            machine_return_bool(machine, l < r)
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            machine_return_bool(machine, l < r)
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            machine_return_bool(machine, l < r)
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

/// LTE(l, r) l <= r
pub struct Lte;

impl StgIntrinsic for Lte {
    fn name(&self) -> &str {
        "LTE"
    }

    fn execute(
        &self,
        machine: &mut Machine,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, &args[0])?;
        let y = num_arg(machine, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            machine_return_bool(machine, l <= r)
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            machine_return_bool(machine, l <= r)
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            machine_return_bool(machine, l <= r)
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

#[cfg(test)]
pub mod tests {
    use gcmodule::Cc;
    use std::rc::Rc;

    use super::*;
    use crate::{
        common::sourcemap::SourceMap,
        eval::{
            emit::DebugEmitter,
            intrinsics,
            machines::stg::{
                env,
                machine::Machine,
                panic::Panic,
                runtime,
                syntax::{dsl::*, StgSyn},
            },
        },
    };

    lazy_static! {
        static ref RUNTIME: Box<dyn runtime::Runtime> = {
            let mut rt = runtime::StandardRuntime::default();
            rt.add(Box::new(Add));
            rt.add(Box::new(Panic));
            Box::new(rt)
        };
    }

    /// Construct a machine with the arithmetic intrinsics
    pub fn machine(syntax: Rc<StgSyn>) -> Machine<'static> {
        let env = env::EnvFrame::default();
        Machine::new(
            syntax,
            Cc::new(env),
            RUNTIME.globals(&mut SourceMap::default()),
            RUNTIME.intrinsics(),
            Box::new(DebugEmitter::default()),
            true,
        )
    }

    #[test]
    pub fn test_boxed_add() {
        let syntax = letrec_(
            vec![
                value(box_num(2)),
                value(box_num(3)),
                value(app(
                    gref(intrinsics::index("ADD").unwrap()),
                    vec![lref(0), lref(1)],
                )),
            ],
            unbox_num(local(2), local(0)),
        );
        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), atom(num(5)));
    }

    #[test]
    pub fn test_unboxed_add() {
        let syntax = letrec_(
            vec![],
            app_bif(intrinsics::index_u8("ADD"), vec![num(2), num(2)]),
        );
        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), atom(num(4)));
    }
}
