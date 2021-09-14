//! Intrinsics for arithmetic functions

use serde_json::Number;

use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    machine::intrinsic::{CallGlobal2, IntrinsicMachine, StgIntrinsic},
    memory::{mutator::MutatorHeapView, syntax::Ref},
};

use super::support::{machine_return_bool, machine_return_num, num_arg};

/// ADD(l, r) - add l to r
pub struct Add;

impl StgIntrinsic for Add {
    fn name(&self) -> &str {
        "ADD"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let total = l
                .checked_add(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, view, Number::from(total))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let total = l
                .checked_add(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, view, Number::from(total))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l + r) {
                machine_return_num(machine, view, ret)
            } else {
                Err(ExecutionError::NumericDomainError(x, y))
            }
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

impl CallGlobal2 for Add {}

/// SUB(l, r) - sub r from l
pub struct Sub;

impl StgIntrinsic for Sub {
    fn name(&self) -> &str {
        "SUB"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let result = l
                .checked_sub(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let result = l
                .checked_sub(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l - r) {
                machine_return_num(machine, view, ret)
            } else {
                Err(ExecutionError::NumericDomainError(x, y))
            }
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

impl CallGlobal2 for Sub {}

/// MUL(l, r) - mul r from l
pub struct Mul;

impl StgIntrinsic for Mul {
    fn name(&self) -> &str {
        "MUL"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let product = l
                .checked_mul(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, view, Number::from(product))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let product = l
                .checked_mul(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, view, Number::from(product))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l * r) {
                machine_return_num(machine, view, ret)
            } else {
                Err(ExecutionError::NumericDomainError(x, y))
            }
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

impl CallGlobal2 for Mul {}

/// DIV(l, r) - div l by r
pub struct Div;

impl StgIntrinsic for Div {
    fn name(&self) -> &str {
        "DIV"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let result = l
                .checked_div(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let result = l
                .checked_div(r)
                .map_or(Err(ExecutionError::NumericRangeError(x, y)), Ok)?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l / r) {
                machine_return_num(machine, view, ret)
            } else {
                Err(ExecutionError::NumericDomainError(x, y))
            }
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

impl CallGlobal2 for Div {}

/// GT(l, r) l > r
pub struct Gt;

impl StgIntrinsic for Gt {
    fn name(&self) -> &str {
        "GT"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            machine_return_bool(machine, view, l > r)
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            machine_return_bool(machine, view, l > r)
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            machine_return_bool(machine, view, l > r)
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

impl CallGlobal2 for Gt {}

/// GTE(l, r) l >= r
pub struct Gte;

impl StgIntrinsic for Gte {
    fn name(&self) -> &str {
        "GTE"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            machine_return_bool(machine, view, l >= r)
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            machine_return_bool(machine, view, l >= r)
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            machine_return_bool(machine, view, l >= r)
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

impl CallGlobal2 for Gte {}

/// LT(l, r) l > r
pub struct Lt;

impl StgIntrinsic for Lt {
    fn name(&self) -> &str {
        "LT"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            machine_return_bool(machine, view, l < r)
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            machine_return_bool(machine, view, l < r)
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            machine_return_bool(machine, view, l < r)
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

impl CallGlobal2 for Lt {}

/// LTE(l, r) l <= r
pub struct Lte;

impl StgIntrinsic for Lte {
    fn name(&self) -> &str {
        "LTE"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            machine_return_bool(machine, view, l <= r)
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            machine_return_bool(machine, view, l <= r)
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            machine_return_bool(machine, view, l <= r)
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

impl CallGlobal2 for Lte {}

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::eval::{
        intrinsics,
        memory::syntax::Native,
        stg::{panic::Panic, runtime::Runtime, syntax::dsl::*, testing},
    };

    pub fn runtime() -> Box<dyn Runtime> {
        testing::runtime(vec![Box::new(Add), Box::new(Panic)])
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
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.native_return(), Some(Native::Num(5.into())));
    }

    #[test]
    pub fn test_unboxed_add() {
        let syntax = letrec_(
            vec![],
            app_bif(intrinsics::index_u8("ADD"), vec![num(2), num(2)]),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.native_return(), Some(Native::Num(4.into())));
    }
}
