//! Intrinsics for arithmetic functions

use std::rc::Rc;

use serde_json::Number;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal1, CallGlobal2, IntrinsicMachine, StgIntrinsic},
        memory::{
            mutator::MutatorHeapView,
            syntax::{Native, Ref},
        },
    },
};

use super::{
    support::{machine_return_bool, machine_return_num, num_arg},
    syntax::{
        dsl::{annotated_lambda, app_bif, case, force, local, lref},
        LambdaForm, StgSyn,
    },
    tags::DataConstructor,
};

/// Check if a number is zero
fn is_zero(n: &Number) -> bool {
    n.as_i64() == Some(0) || n.as_u64() == Some(0) || n.as_f64() == Some(0.0)
}

/// ADD(l, r) - add l to r
pub struct Add;

impl StgIntrinsic for Add {
    fn name(&self) -> &str {
        "ADD"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let total = l
                .checked_add(r)
                .ok_or(ExecutionError::NumericRangeError(x, y))?;
            machine_return_num(machine, view, Number::from(total))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let total = l
                .checked_add(r)
                .ok_or(ExecutionError::NumericRangeError(x, y))?;
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

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let result = l
                .checked_sub(r)
                .ok_or(ExecutionError::NumericRangeError(x, y))?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let result = l
                .checked_sub(r)
                .ok_or(ExecutionError::NumericRangeError(x, y))?;
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

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let product = l
                .checked_mul(r)
                .ok_or(ExecutionError::NumericRangeError(x, y))?;
            machine_return_num(machine, view, Number::from(product))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let product = l
                .checked_mul(r)
                .ok_or(ExecutionError::NumericRangeError(x, y))?;
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

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if is_zero(&y) {
            return Err(ExecutionError::DivisionByZero);
        }

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let result = l
                .checked_div(r)
                .ok_or(ExecutionError::NumericRangeError(x, y))?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let result = l
                .checked_div(r)
                .ok_or(ExecutionError::NumericRangeError(x, y))?;
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

/// MOD(l, r) - mod r from l
pub struct Mod;

impl StgIntrinsic for Mod {
    fn name(&self) -> &str {
        "MOD"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if is_zero(&y) {
            return Err(ExecutionError::DivisionByZero);
        }

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let product = l
                .checked_rem(r)
                .ok_or(ExecutionError::NumericRangeError(x, y))?;
            machine_return_num(machine, view, Number::from(product))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let product = l
                .checked_rem(r)
                .ok_or(ExecutionError::NumericRangeError(x, y))?;
            machine_return_num(machine, view, Number::from(product))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l % r) {
                machine_return_num(machine, view, ret)
            } else {
                Err(ExecutionError::NumericDomainError(x, y))
            }
        } else {
            Err(ExecutionError::NumericDomainError(x, y))
        }
    }
}

impl CallGlobal2 for Mod {}

/// Unbox any boxed native type (number, string, symbol, or ZDT).
///
/// Each branch extracts the inner native value to local(0). The
/// fallback handles values that are already unboxed atoms.
fn unbox_any(scrutinee: Rc<StgSyn>, then: Rc<StgSyn>) -> Rc<StgSyn> {
    case(
        scrutinee,
        vec![
            (DataConstructor::BoxedNumber.tag(), then.clone()),
            (DataConstructor::BoxedString.tag(), then.clone()),
            (DataConstructor::BoxedSymbol.tag(), then.clone()),
            (DataConstructor::BoxedZdt.tag(), then.clone()),
        ],
        then,
    )
}

/// Build a wrapper for a polymorphic two-argument comparison intrinsic.
///
/// Unboxes and forces both arguments for any boxed native type, then
/// calls the BIF with the raw native values.
fn comparison_wrapper(index: usize, annotation: Smid) -> LambdaForm {
    // Environment evolution (matching the default wrapper pattern):
    //
    //   lambda args:                                          [x, y]
    //   unbox_any x:  case on local(0)
    //     branch matches BoxedXxx → inner ref at local(0):    [inner_x] [x, y]
    //   force inner_x:                              [raw_x]   [inner_x] [x, y]
    //   unbox_any y:  case on local(3) (= y from lambda args)
    //     branch matches BoxedXxx → inner ref:  [inner_y]     [raw_x] [inner_x] [x, y]
    //   force inner_y:                  [raw_y] [inner_y]     [raw_x] [inner_x] [x, y]
    //
    //   BIF args: raw_x = lref(2), raw_y = lref(0)
    let bif_call = app_bif(index as u8, vec![lref(2), lref(0)]);
    let force_y = force(local(0), bif_call);
    let unbox_y = unbox_any(local(3), force_y);
    let force_x = force(local(0), unbox_y);
    let unbox_x = unbox_any(local(0), force_x);
    annotated_lambda(2, unbox_x, annotation)
}

/// Ordering comparison between two numbers, trying i64, u64, then f64
/// representations. Returns the `std::cmp::Ordering` or an error if
/// the numbers cannot be compared.
fn num_ord(x: &Number, y: &Number) -> Result<std::cmp::Ordering, ExecutionError> {
    if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
        Ok(l.cmp(&r))
    } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
        Ok(l.cmp(&r))
    } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
        l.partial_cmp(&r)
            .ok_or_else(|| ExecutionError::NumericDomainError(x.clone(), y.clone()))
    } else {
        Err(ExecutionError::NumericDomainError(x.clone(), y.clone()))
    }
}

/// Polymorphic ordered comparison. Dispatches on native type, computes
/// the ordering, and applies the given predicate.
fn ordered_cmp(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    args: &[Ref],
    pred: fn(std::cmp::Ordering) -> bool,
    name: &str,
) -> Result<bool, ExecutionError> {
    let x = machine.nav(view).resolve_native(&args[0])?;
    let y = machine.nav(view).resolve_native(&args[1])?;
    match (x, y) {
        (Native::Num(ref nx), Native::Num(ref ny)) => Ok(pred(num_ord(nx, ny)?)),
        (Native::Str(sx), Native::Str(sy)) => {
            let ls = &*view.scoped(sx);
            let rs = &*view.scoped(sy);
            Ok(pred(ls.as_str().cmp(rs.as_str())))
        }
        (Native::Sym(sx), Native::Sym(sy)) => {
            let pool = machine.symbol_pool();
            let ls = pool.resolve(sx);
            let rs = pool.resolve(sy);
            Ok(pred(ls.cmp(rs)))
        }
        (Native::Zdt(ref dx), Native::Zdt(ref dy)) => Ok(pred(dx.cmp(dy))),
        _ => Err(ExecutionError::Panic(format!(
            "cannot compare values with {name}: operands must be the same type \
             (both numbers, strings, symbols, or datetimes)"
        ))),
    }
}

/// GT(l, r) l > r
pub struct Gt;

impl StgIntrinsic for Gt {
    fn name(&self) -> &str {
        "GT"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        comparison_wrapper(self.index(), annotation)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let result = ordered_cmp(machine, view, args, std::cmp::Ordering::is_gt, "GT")?;
        machine_return_bool(machine, view, result)
    }
}

impl CallGlobal2 for Gt {}

/// GTE(l, r) l >= r
pub struct Gte;

impl StgIntrinsic for Gte {
    fn name(&self) -> &str {
        "GTE"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        comparison_wrapper(self.index(), annotation)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let result = ordered_cmp(machine, view, args, std::cmp::Ordering::is_ge, "GTE")?;
        machine_return_bool(machine, view, result)
    }
}

impl CallGlobal2 for Gte {}

/// LT(l, r) l < r
pub struct Lt;

impl StgIntrinsic for Lt {
    fn name(&self) -> &str {
        "LT"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        comparison_wrapper(self.index(), annotation)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let result = ordered_cmp(machine, view, args, std::cmp::Ordering::is_lt, "LT")?;
        machine_return_bool(machine, view, result)
    }
}

impl CallGlobal2 for Lt {}

/// LTE(l, r) l <= r
pub struct Lte;

impl StgIntrinsic for Lte {
    fn name(&self) -> &str {
        "LTE"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        comparison_wrapper(self.index(), annotation)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let result = ordered_cmp(machine, view, args, std::cmp::Ordering::is_le, "LTE")?;
        machine_return_bool(machine, view, result)
    }
}

impl CallGlobal2 for Lte {}

/// CEIL(x)
pub struct Ceil;

impl StgIntrinsic for Ceil {
    fn name(&self) -> &str {
        "CEILING"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;

        if x.is_i64() || x.is_u64() {
            machine_return_num(machine, view, x)
        } else if let Some(val) = x.as_f64() {
            if let Some(ret) = Number::from_f64(val.ceil()) {
                machine_return_num(machine, view, ret)
            } else {
                Err(ExecutionError::NumericDomainError(x, Number::from(0)))
            }
        } else {
            unreachable!();
        }
    }
}

impl CallGlobal1 for Ceil {}

/// FLOOR(x)
pub struct Floor;

impl StgIntrinsic for Floor {
    fn name(&self) -> &str {
        "FLOOR"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;

        if x.is_i64() || x.is_u64() {
            machine_return_num(machine, view, x)
        } else if let Some(val) = x.as_f64() {
            if let Some(ret) = Number::from_f64(val.floor()) {
                machine_return_num(machine, view, ret)
            } else {
                Err(ExecutionError::NumericDomainError(x, Number::from(0)))
            }
        } else {
            unreachable!();
        }
    }
}

impl CallGlobal1 for Floor {}

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
