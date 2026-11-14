//! Intrinsics for arithmetic functions

use std::rc::Rc;

use serde_json::Number;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{
            CallGlobal1, CallGlobal2, CallGlobal3, IntrinsicMachine, StgIntrinsic,
        },
        memory::{
            mutator::MutatorHeapView,
            number::ExactNum,
            syntax::{Native, Ref},
        },
    },
};

use super::{
    array::array_binop,
    support::{machine_return_bool, machine_return_num, num_arg},
    syntax::{
        dsl::{app_bif, case, force, lambda, local, lref},
        LambdaForm, StgSyn,
    },
    tags::DataConstructor,
};

/// Check if a number is zero
fn is_zero(n: &Number) -> bool {
    n.as_i64() == Some(0) || n.as_u64() == Some(0) || n.as_f64() == Some(0.0)
}

/// Floor division for signed integers (rounds toward negative infinity).
///
/// Differs from Rust's truncating `/` for negative dividends:
/// `-7 floor_div 2 = -4` vs `-7 / 2 = -3`.
fn floor_div_i64(a: i64, b: i64) -> Option<i64> {
    let d = a.checked_div(b)?;
    let r = a.checked_rem(b)?;
    // If remainder is non-zero and its sign differs from the divisor,
    // truncation rounded the wrong way — correct by subtracting 1.
    if r != 0 && ((r ^ b) < 0) {
        Some(d - 1)
    } else {
        Some(d)
    }
}

/// Floor modulus for signed integers (result has same sign as divisor).
///
/// Differs from Rust's truncating `%` for negative dividends:
/// `-7 floor_mod 3 = 2` vs `-7 % 3 = -1`.
fn floor_mod_i64(a: i64, b: i64) -> Option<i64> {
    let r = a.checked_rem(b)?;
    if r != 0 && ((r ^ b) < 0) {
        Some(r + b)
    } else {
        Some(r)
    }
}

/// Convert a floored/truncated f64 to an integer Number when possible,
/// falling back to f64 representation for values outside i64 range.
fn num_from_floored(val: f64) -> Option<Number> {
    if val.is_finite() && val >= i64::MIN as f64 && val <= i64::MAX as f64 {
        let i = val as i64;
        if (i as f64) == val {
            return Some(Number::from(i));
        }
    }
    Number::from_f64(val)
}

/// ADD(l, r) - add l to r
pub struct Add;

impl StgIntrinsic for Add {
    fn name(&self) -> &str {
        "ADD"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        arithmetic_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        // Dispatch to array arithmetic if either operand is an NdArray
        let a_native = machine.resolve_native(view, &args[0]);
        let b_native = machine.resolve_native(view, &args[1]);
        if let (Ok(ref a), Ok(ref b)) = (&a_native, &b_native) {
            if matches!(a, Native::NdArray(_)) || matches!(b, Native::NdArray(_)) {
                return array_binop(machine, view, &args[0], &args[1], |a, b| a + b);
            }
        }

        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let total = l.checked_add(r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(total))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let total = l.checked_add(r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(total))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l + r) {
                machine_return_num(machine, view, ret)
            } else {
                Err(ExecutionError::NumericDomainError(
                    machine.annotation(),
                    x,
                    y,
                ))
            }
        } else {
            Err(ExecutionError::NumericDomainError(
                machine.annotation(),
                x,
                y,
            ))
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        arithmetic_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        // Dispatch to array arithmetic if either operand is an NdArray
        let a_native = machine.resolve_native(view, &args[0]);
        let b_native = machine.resolve_native(view, &args[1]);
        if let (Ok(ref a), Ok(ref b)) = (&a_native, &b_native) {
            if matches!(a, Native::NdArray(_)) || matches!(b, Native::NdArray(_)) {
                return array_binop(machine, view, &args[0], &args[1], |a, b| a - b);
            }
        }

        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let result = l.checked_sub(r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let result = l.checked_sub(r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l - r) {
                machine_return_num(machine, view, ret)
            } else {
                Err(ExecutionError::NumericDomainError(
                    machine.annotation(),
                    x,
                    y,
                ))
            }
        } else {
            Err(ExecutionError::NumericDomainError(
                machine.annotation(),
                x,
                y,
            ))
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        arithmetic_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        // Dispatch to array arithmetic if either operand is an NdArray
        let a_native = machine.resolve_native(view, &args[0]);
        let b_native = machine.resolve_native(view, &args[1]);
        if let (Ok(ref a), Ok(ref b)) = (&a_native, &b_native) {
            if matches!(a, Native::NdArray(_)) || matches!(b, Native::NdArray(_)) {
                return array_binop(machine, view, &args[0], &args[1], |a, b| a * b);
            }
        }

        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let product = l.checked_mul(r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(product))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let product = l.checked_mul(r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(product))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l * r) {
                machine_return_num(machine, view, ret)
            } else {
                Err(ExecutionError::NumericDomainError(
                    machine.annotation(),
                    x,
                    y,
                ))
            }
        } else {
            Err(ExecutionError::NumericDomainError(
                machine.annotation(),
                x,
                y,
            ))
        }
    }
}

impl CallGlobal2 for Mul {}

/// DIV(l, r) - floor division (rounds toward negative infinity, always integer)
pub struct Div;

impl StgIntrinsic for Div {
    fn name(&self) -> &str {
        "DIV"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        arithmetic_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        // Dispatch to array arithmetic if either operand is an NdArray
        let a_native = machine.resolve_native(view, &args[0]);
        let b_native = machine.resolve_native(view, &args[1]);
        if let (Ok(ref a), Ok(ref b)) = (&a_native, &b_native) {
            if matches!(a, Native::NdArray(_)) || matches!(b, Native::NdArray(_)) {
                return array_binop(machine, view, &args[0], &args[1], |a, b| a / b);
            }
        }

        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if is_zero(&y) {
            return Err(ExecutionError::DivisionByZero(
                machine.annotation(),
                "floor division (/)".to_string(),
            ));
        }

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let result = floor_div_i64(l, r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let result = l.checked_div(r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            let result = (l / r).floor();
            if let Some(n) = num_from_floored(result) {
                machine_return_num(machine, view, n)
            } else {
                Err(ExecutionError::NumericDomainError(
                    machine.annotation(),
                    x,
                    y,
                ))
            }
        } else {
            Err(ExecutionError::NumericDomainError(
                machine.annotation(),
                x,
                y,
            ))
        }
    }
}

impl CallGlobal2 for Div {}

/// MOD(l, r) - floor modulus (result has same sign as divisor)
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
            return Err(ExecutionError::DivisionByZero(
                machine.annotation(),
                "modulo (%)".to_string(),
            ));
        }

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let result = floor_mod_i64(l, r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let result = l.checked_rem(r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            let q = (l / r).floor();
            let result = l - r * q;
            if let Some(ret) = Number::from_f64(result) {
                machine_return_num(machine, view, ret)
            } else {
                Err(ExecutionError::NumericDomainError(
                    machine.annotation(),
                    x,
                    y,
                ))
            }
        } else {
            Err(ExecutionError::NumericDomainError(
                machine.annotation(),
                x,
                y,
            ))
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

/// Build a fast-path wrapper for a polymorphic two-argument BIF.
///
/// The outer `case` forces x to WHNF and dispatches on its tag:
///
/// **Boxed x** (BoxedNumber/String/Symbol/Zdt):
/// - inner_x at L(0); full unbox+force chain for both args.
/// - Environment path: `[inner_x][x,y]` → force → `[raw_x][inner_x][x,y]`
///   → unbox_any(L(3)=y) → force → `[raw_y]...[raw_x]...`
///   → BIF(lref(2)=raw_x, lref(0)=raw_y)
///
/// **Native x** (already unboxed, e.g. Num/Str/Sym/NdArray):
/// - x(native) at L(0), original [x,y] at L(1..); y is at L(2).
/// - Inner `case` forces y to WHNF:
///   - Boxed y: inner_y at L(0) → force → raw_y at L(0), x(native) at L(2)
///     → BIF(lref(2)=x_native, lref(0)=raw_y)
///   - Native y: y(native) at L(0), x(native) at L(1)
///     → BIF(lref(1)=x_native, lref(0)=y_native) — **no `force` frames**
///
/// For the native/native fast path this saves two env-frame allocations
/// compared with routing through `unbox_any` + `force` for each arg.
fn binary_wrapper(index: usize) -> LambdaForm {
    // ── Boxed-x path: full unbox + force for both args ──────────────
    // After boxed-x branch: [inner_x][x,y]  L(0)=inner_x, L(2)=x, L(3)=y
    // After force inner_x:  [raw_x][inner_x][x,y]  L(0)=raw_x, L(3)=y
    // After unbox_any(L(3)): any-y branch/fallback puts inner/native at L(0),
    //   raw_x slides to L(1) or higher.
    // After force y:  L(0)=raw_y, ..., L(2)=raw_x
    let bif_full = app_bif(index as u8, vec![lref(2), lref(0)]);
    let force_y_full = force(local(0), bif_full);
    let unbox_y_full = unbox_any(local(3), force_y_full);
    let force_x_full = force(local(0), unbox_y_full);

    // ── Native-x + boxed-y path ──────────────────────────────────────
    // After outer fallback:  [x(nat)][x,y]  L(0)=x(nat), L(2)=y
    // After boxed-y branch:  [inner_y][x(nat)][x,y]  L(0)=inner_y, L(1)=x(nat)
    // After force inner_y:   [raw_y][inner_y][x(nat)]...  L(0)=raw_y, L(2)=x(nat)
    let bif_nx_fy = app_bif(index as u8, vec![lref(2), lref(0)]);
    let force_inner_y = force(local(0), bif_nx_fy);

    // ── Native-x + native-y fast path ────────────────────────────────
    // After outer fallback + y fallback:
    //   [y(nat)][x(nat)][x,y]  L(0)=y(nat), L(1)=x(nat)
    let bif_nx_ny = app_bif(index as u8, vec![lref(1), lref(0)]);

    // Inner dispatch on y when x is already native (y is at L(2))
    let check_y = case(
        local(2),
        vec![
            (DataConstructor::BoxedNumber.tag(), force_inner_y.clone()),
            (DataConstructor::BoxedString.tag(), force_inner_y.clone()),
            (DataConstructor::BoxedSymbol.tag(), force_inner_y.clone()),
            (DataConstructor::BoxedZdt.tag(), force_inner_y),
        ],
        bif_nx_ny,
    );

    // Outer dispatch on x: boxed → full path; native → fast path
    let body = case(
        local(0),
        vec![
            (DataConstructor::BoxedNumber.tag(), force_x_full.clone()),
            (DataConstructor::BoxedString.tag(), force_x_full.clone()),
            (DataConstructor::BoxedSymbol.tag(), force_x_full.clone()),
            (DataConstructor::BoxedZdt.tag(), force_x_full),
        ],
        check_y,
    );

    lambda(2, body)
}

/// Build a wrapper for a polymorphic two-argument comparison intrinsic.
///
/// Delegates to `binary_wrapper` which applies a native/native fast path
/// to avoid redundant env-frame allocations when both args are already
/// unboxed natives.
fn comparison_wrapper(index: usize) -> LambdaForm {
    binary_wrapper(index)
}

/// Build a wrapper for a polymorphic two-argument arithmetic intrinsic.
///
/// Like `comparison_wrapper`, delegates to `binary_wrapper`. The `execute`
/// methods are responsible for returning the correct result type:
/// `machine_return_boxed_num` for scalar results, `machine_return_ndarray`
/// for array results.
fn arithmetic_wrapper(index: usize) -> LambdaForm {
    binary_wrapper(index)
}

/// Ordering comparison between two numbers by exact value, agreeing
/// with set, vec and dict key order ([`ExactNum`]). Returns an error if
/// either is NaN, which has no place in the numeric order.
fn num_ord(smid: Smid, x: &Number, y: &Number) -> Result<std::cmp::Ordering, ExecutionError> {
    let is_nan = |n: &Number| n.as_f64().is_some_and(f64::is_nan);
    if is_nan(x) || is_nan(y) {
        return Err(ExecutionError::NumericDomainError(
            smid,
            x.clone(),
            y.clone(),
        ));
    }
    Ok(ExactNum::from_number(x).cmp(&ExactNum::from_number(y)))
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
    let x = machine.resolve_native(view, &args[0])?;
    let y = machine.resolve_native(view, &args[1])?;
    match (x, y) {
        (Native::Num(ref nx), Native::Num(ref ny)) => {
            Ok(pred(num_ord(machine.annotation(), nx, ny)?))
        }
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
        _ => Err(ExecutionError::ComparisonTypeMismatch(
            machine.annotation(),
            name.to_string(),
        )),
    }
}

/// GT(l, r) l > r
pub struct Gt;

impl StgIntrinsic for Gt {
    fn name(&self) -> &str {
        "GT"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        comparison_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let result = ordered_cmp(machine, view, args, std::cmp::Ordering::is_gt, ">")?;
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        comparison_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let result = ordered_cmp(machine, view, args, std::cmp::Ordering::is_ge, ">=")?;
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        comparison_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let result = ordered_cmp(machine, view, args, std::cmp::Ordering::is_lt, "<")?;
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        comparison_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let result = ordered_cmp(machine, view, args, std::cmp::Ordering::is_le, "<=")?;
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
                Err(ExecutionError::NumericDomainError(
                    machine.annotation(),
                    x,
                    Number::from(0),
                ))
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
                Err(ExecutionError::NumericDomainError(
                    machine.annotation(),
                    x,
                    Number::from(0),
                ))
            }
        } else {
            unreachable!();
        }
    }
}

impl CallGlobal1 for Floor {}

/// POW(base, exp) - raise base to the power exp
///
/// For integer base and non-negative integer exponent, returns an
/// integer result, raising a numeric range (overflow) error when the
/// result is out of range, as `ADD` and `MUL` do. A float result only
/// when an operand is a float, or for a negative integer exponent
/// (whose result is not in general an integer); these use f64 powf.
pub struct Pow;

impl StgIntrinsic for Pow {
    fn name(&self) -> &str {
        "POW"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let base = num_arg(machine, view, &args[0])?;
        let exp = num_arg(machine, view, &args[1])?;

        // Integer base with non-negative integer exponent: integer
        // result or overflow error, never a silent float.
        if !base.is_f64() {
            if let Some(e) = exp.as_u64() {
                let result = match (base.as_i64(), base.as_u64()) {
                    (Some(b), _) => checked_int_pow(b, e).map(Number::from),
                    (None, Some(b)) => checked_uint_pow(b, e).map(Number::from),
                    (None, None) => None,
                };
                return match result {
                    Some(n) => machine_return_num(machine, view, n),
                    None => Err(ExecutionError::NumericRangeError(
                        machine.annotation(),
                        base,
                        exp,
                    )),
                };
            }
        }

        // Float operand or negative integer exponent: f64 arithmetic
        if let (Some(b), Some(e)) = (base.as_f64(), exp.as_f64()) {
            let result = b.powf(e);
            if let Some(ret) = Number::from_f64(result) {
                machine_return_num(machine, view, ret)
            } else if result.is_nan() && b < 0.0 && e.fract() != 0.0 {
                // Negative base raised to a fractional exponent produces a complex number.
                Err(ExecutionError::ComplexResult(
                    machine.annotation(),
                    base,
                    exp,
                ))
            } else {
                Err(ExecutionError::NumericDomainError(
                    machine.annotation(),
                    base,
                    exp,
                ))
            }
        } else {
            Err(ExecutionError::NumericDomainError(
                machine.annotation(),
                base,
                exp,
            ))
        }
    }
}

/// Integer exponentiation with overflow checking
fn checked_int_pow(base: i64, exp: u64) -> Option<i64> {
    match base {
        // Bases whose powers never grow, whatever the exponent
        0 | 1 => Some(if exp == 0 { 1 } else { base }),
        -1 => Some(if exp.is_multiple_of(2) { 1 } else { -1 }),
        _ => base.checked_pow(u32::try_from(exp).ok()?),
    }
}

/// Exponentiation of a base above `i64::MAX` with overflow checking
fn checked_uint_pow(base: u64, exp: u64) -> Option<u64> {
    base.checked_pow(u32::try_from(exp).ok()?)
}

impl CallGlobal2 for Pow {}

/// PDIV(l, r) - precise division (always returns exact float result)
pub struct PreciseDiv;

impl StgIntrinsic for PreciseDiv {
    fn name(&self) -> &str {
        "PDIV"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if is_zero(&y) {
            return Err(ExecutionError::DivisionByZero(
                machine.annotation(),
                "precise division (\u{00f7})".to_string(),
            ));
        }

        if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l / r) {
                machine_return_num(machine, view, ret)
            } else {
                Err(ExecutionError::NumericDomainError(
                    machine.annotation(),
                    x,
                    y,
                ))
            }
        } else {
            Err(ExecutionError::NumericDomainError(
                machine.annotation(),
                x,
                y,
            ))
        }
    }
}

impl CallGlobal2 for PreciseDiv {}

/// QUOT(l, r) - truncation division (rounds toward zero)
pub struct Quot;

impl StgIntrinsic for Quot {
    fn name(&self) -> &str {
        "QUOT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if is_zero(&y) {
            return Err(ExecutionError::DivisionByZero(
                machine.annotation(),
                "truncation division (quot)".to_string(),
            ));
        }

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let result = l.checked_div(r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let result = l.checked_div(r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            let result = (l / r).trunc();
            if let Some(n) = num_from_floored(result) {
                machine_return_num(machine, view, n)
            } else {
                Err(ExecutionError::NumericDomainError(
                    machine.annotation(),
                    x,
                    y,
                ))
            }
        } else {
            Err(ExecutionError::NumericDomainError(
                machine.annotation(),
                x,
                y,
            ))
        }
    }
}

impl CallGlobal2 for Quot {}

/// REM(l, r) - truncation remainder (result has same sign as dividend)
pub struct Rem;

impl StgIntrinsic for Rem {
    fn name(&self) -> &str {
        "REM"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;

        if is_zero(&y) {
            return Err(ExecutionError::DivisionByZero(
                machine.annotation(),
                "remainder (rem)".to_string(),
            ));
        }

        if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
            let result = l.checked_rem(r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
            let result = l.checked_rem(r).ok_or(ExecutionError::NumericRangeError(
                machine.annotation(),
                x,
                y,
            ))?;
            machine_return_num(machine, view, Number::from(result))
        } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
            if let Some(ret) = Number::from_f64(l % r) {
                machine_return_num(machine, view, ret)
            } else {
                Err(ExecutionError::NumericDomainError(
                    machine.annotation(),
                    x,
                    y,
                ))
            }
        } else {
            Err(ExecutionError::NumericDomainError(
                machine.annotation(),
                x,
                y,
            ))
        }
    }
}

impl CallGlobal2 for Rem {}

// ── Modular arithmetic (eu-oaz72.24) ─────────────────────────────────
//
// `mulmod`, `powmod` and `modinv` take i64 operands and moduli and compute
// in i128, so `a * b % m` and modular exponentiation are exact across the
// whole i64 range: both factors are reduced into [0, m) first, so their
// product is below 2^126. Results are the non-negative residue in [0, m).

/// The value of `n` as an i64 if it is a whole number in the i64 range,
/// whether stored as an integer or as a float such as `7.0`, as the bitwise
/// operations accept (see `require_i64`).
fn whole_i64(n: &Number) -> Option<i64> {
    if let Some(i) = n.as_i64() {
        Some(i)
    } else if let Some(u) = n.as_u64() {
        i64::try_from(u).ok()
    } else {
        // 2^63 is exactly representable and is the first float past i64::MAX.
        n.as_f64()
            .filter(|f| f.fract() == 0.0 && *f >= -(2f64.powi(63)) && *f < 2f64.powi(63))
            .map(|f| f as i64)
    }
}

/// Extract an i64 operand for a modular-arithmetic intrinsic, rejecting
/// fractional numbers and numbers outside the i64 range.
fn modular_operand(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
    function: &str,
    role: &str,
) -> Result<i128, ExecutionError> {
    let n = num_arg(machine, view, arg)?;
    if let Some(i) = whole_i64(&n) {
        return Ok(i128::from(i));
    }
    let fractional = n
        .as_f64()
        .is_some_and(|f| f.fract() != 0.0 || !f.is_finite());
    let problem = if fractional {
        "a whole number"
    } else {
        "in the 64-bit signed range"
    };
    Err(ExecutionError::ModularArithmetic(
        machine.annotation(),
        format!("{function} requires each argument to be {problem}, but the {role} is {n}"),
    ))
}

/// Extract a modulus, which must be a positive integer.
fn modular_modulus(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
    function: &str,
) -> Result<i128, ExecutionError> {
    let m = modular_operand(machine, view, arg, function, "modulus")?;
    if m <= 0 {
        return Err(ExecutionError::ModularArithmetic(
            machine.annotation(),
            format!(
                "{function} requires a positive modulus, but the modulus is {m}\n  \
                 help: the result of {function} lies in [0, m), so m must be at least 1"
            ),
        ));
    }
    Ok(m)
}

/// `a * b mod m` for `0 <= a, b < m <= i64::MAX`: exact in i128.
fn mulmod_i128(a: i128, b: i128, m: i128) -> i128 {
    (a * b) % m
}

/// `b ^ e mod m` by square-and-multiply, for `e >= 0` and `m >= 1`.
fn powmod_i128(b: i128, mut e: i128, m: i128) -> i128 {
    let mut base = b.rem_euclid(m);
    let mut acc = 1 % m;
    while e > 0 {
        if e & 1 == 1 {
            acc = mulmod_i128(acc, base, m);
        }
        base = mulmod_i128(base, base, m);
        e >>= 1;
    }
    acc
}

/// Inverse of `a` modulo `m >= 1` by the extended Euclidean algorithm:
/// `Ok(x)` with `0 <= x < m` and `a * x = 1 (mod m)`, or `Err(gcd)` when
/// `gcd(a, m) != 1` and no inverse exists.
fn modinv_i128(a: i128, m: i128) -> Result<i128, i128> {
    let (mut r0, mut r1) = (a.rem_euclid(m), m);
    let (mut s0, mut s1) = (1i128, 0i128);
    while r1 != 0 {
        let q = r0 / r1;
        (r0, r1) = (r1, r0 - q * r1);
        (s0, s1) = (s1, s0 - q * s1);
    }
    // r0 = gcd(a mod m, m); for m = 1 every value is invertible (to 0).
    if r0 == 1 || m == 1 {
        Ok(s0.rem_euclid(m))
    } else {
        Err(r0)
    }
}

/// Return an i128 residue in [0, m) with m <= i64::MAX as a number.
fn return_residue(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    r: i128,
) -> Result<(), ExecutionError> {
    // A residue lies in [0, m) and m <= i64::MAX, so this cannot fail.
    let r = i64::try_from(r).map_err(|_| {
        ExecutionError::Panic(
            machine.annotation(),
            format!("modular residue {r} out of the i64 range"),
        )
    })?;
    machine_return_num(machine, view, Number::from(r))
}

/// MULMOD(a, b, m) - `a * b mod m`, exact for all i64 operands
pub struct MulMod;

impl StgIntrinsic for MulMod {
    fn name(&self) -> &str {
        "MULMOD"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let a = modular_operand(machine, view, &args[0], "mulmod", "first factor")?;
        let b = modular_operand(machine, view, &args[1], "mulmod", "second factor")?;
        let m = modular_modulus(machine, view, &args[2], "mulmod")?;
        let r = mulmod_i128(a.rem_euclid(m), b.rem_euclid(m), m);
        return_residue(machine, view, r)
    }
}

impl CallGlobal3 for MulMod {}

/// POWMOD(b, e, m) - `b ^ e mod m` for `e >= 0`, exact for all i64 operands
pub struct PowMod;

impl StgIntrinsic for PowMod {
    fn name(&self) -> &str {
        "POWMOD"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let b = modular_operand(machine, view, &args[0], "powmod", "base")?;
        let e = modular_operand(machine, view, &args[1], "powmod", "exponent")?;
        let m = modular_modulus(machine, view, &args[2], "powmod")?;
        if e < 0 {
            return Err(ExecutionError::ModularArithmetic(
                machine.annotation(),
                format!(
                    "powmod requires a non-negative exponent, but the exponent is {e}\n  \
                     help: for b^-k mod m, use powmod(modinv(b, m), k, m)"
                ),
            ));
        }
        let r = powmod_i128(b, e, m);
        return_residue(machine, view, r)
    }
}

impl CallGlobal3 for PowMod {}

/// MODINV(a, m) - the inverse of `a` modulo `m`, an error if none exists
pub struct ModInv;

impl StgIntrinsic for ModInv {
    fn name(&self) -> &str {
        "MODINV"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let a = modular_operand(machine, view, &args[0], "modinv", "value")?;
        let m = modular_modulus(machine, view, &args[1], "modinv")?;
        match modinv_i128(a, m) {
            Ok(r) => return_residue(machine, view, r),
            Err(g) => Err(ExecutionError::ModularArithmetic(
                machine.annotation(),
                format!(
                    "{a} has no inverse modulo {m}, because gcd({a}, {m}) = {g}\n  \
                     help: modinv(a, m) exists only when a and m are coprime"
                ),
            )),
        }
    }
}

impl CallGlobal2 for ModInv {}

/// Extract an i64 from a Number, accepting integer-valued floats
/// (which arise from intrinsics like `sort-nums` that convert via f64).
fn require_i64(n: &Number, smid: crate::common::sourcemap::Smid) -> Result<i64, ExecutionError> {
    if let Some(i) = n.as_i64() {
        Ok(i)
    } else if let Some(u) = n.as_u64() {
        i64::try_from(u).map_err(|_| {
            ExecutionError::BitwiseIntegerRequired(
                smid,
                format!("{u} is out of the i64 range for bitwise operations"),
            )
        })
    } else if let Some(f) = n.as_f64() {
        if f.fract() == 0.0 && f >= i64::MIN as f64 && f <= i64::MAX as f64 {
            Ok(f as i64)
        } else {
            Err(ExecutionError::BitwiseIntegerRequired(
                smid,
                format!("got {f}, which is not a whole number"),
            ))
        }
    } else {
        Err(ExecutionError::BitwiseIntegerRequired(
            smid,
            "value cannot be represented as an integer".to_string(),
        ))
    }
}

/// BITAND(l, r) - bitwise AND
pub struct BitAnd;

impl StgIntrinsic for BitAnd {
    fn name(&self) -> &str {
        "BITAND"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;
        let smid = machine.annotation();
        let result = require_i64(&x, smid)? & require_i64(&y, smid)?;
        machine_return_num(machine, view, Number::from(result))
    }
}

impl CallGlobal2 for BitAnd {}

/// BITOR(l, r) - bitwise OR
pub struct BitOr;

impl StgIntrinsic for BitOr {
    fn name(&self) -> &str {
        "BITOR"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;
        let smid = machine.annotation();
        let result = require_i64(&x, smid)? | require_i64(&y, smid)?;
        machine_return_num(machine, view, Number::from(result))
    }
}

impl CallGlobal2 for BitOr {}

/// BITXOR(l, r) - bitwise XOR
pub struct BitXor;

impl StgIntrinsic for BitXor {
    fn name(&self) -> &str {
        "BITXOR"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;
        let smid = machine.annotation();
        let result = require_i64(&x, smid)? ^ require_i64(&y, smid)?;
        machine_return_num(machine, view, Number::from(result))
    }
}

impl CallGlobal2 for BitXor {}

/// BITNOT(n) - bitwise NOT (flip all 64 bits)
pub struct BitNot;

impl StgIntrinsic for BitNot {
    fn name(&self) -> &str {
        "BITNOT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let smid = machine.annotation();
        let result = !require_i64(&x, smid)?;
        machine_return_num(machine, view, Number::from(result))
    }
}

impl CallGlobal1 for BitNot {}

/// SHL(n, k) - left shift n by k bits
pub struct Shl;

impl StgIntrinsic for Shl {
    fn name(&self) -> &str {
        "SHL"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;
        let smid = machine.annotation();
        let n = require_i64(&x, smid)?;
        let k = require_i64(&y, smid)?;
        if !(0..64).contains(&k) {
            return Err(ExecutionError::BitshiftRangeError(smid, k));
        }
        let result = n << k;
        machine_return_num(machine, view, Number::from(result))
    }
}

impl CallGlobal2 for Shl {}

/// SHR(n, k) - arithmetic right shift n by k bits
pub struct Shr;

impl StgIntrinsic for Shr {
    fn name(&self) -> &str {
        "SHR"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let y = num_arg(machine, view, &args[1])?;
        let smid = machine.annotation();
        let n = require_i64(&x, smid)?;
        let k = require_i64(&y, smid)?;
        if !(0..64).contains(&k) {
            return Err(ExecutionError::BitshiftRangeError(smid, k));
        }
        let result = n >> k;
        machine_return_num(machine, view, Number::from(result))
    }
}

impl CallGlobal2 for Shr {}

/// POPCOUNT(n) - count set bits
pub struct PopCount;

impl StgIntrinsic for PopCount {
    fn name(&self) -> &str {
        "POPCOUNT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let n = require_i64(&x, machine.annotation())?;
        let result = n.count_ones() as i64;
        machine_return_num(machine, view, Number::from(result))
    }
}

impl CallGlobal1 for PopCount {}

/// CTZ(n) - count trailing zeros (position of lowest set bit; 64 if zero)
pub struct Ctz;

impl StgIntrinsic for Ctz {
    fn name(&self) -> &str {
        "CTZ"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let n = require_i64(&x, machine.annotation())?;
        let result = n.trailing_zeros() as i64;
        machine_return_num(machine, view, Number::from(result))
    }
}

impl CallGlobal1 for Ctz {}

/// CLZ(n) - count leading zeros
pub struct Clz;

impl StgIntrinsic for Clz {
    fn name(&self) -> &str {
        "CLZ"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let x = num_arg(machine, view, &args[0])?;
        let n = require_i64(&x, machine.annotation())?;
        let result = n.leading_zeros() as i64;
        machine_return_num(machine, view, Number::from(result))
    }
}

impl CallGlobal1 for Clz {}

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::eval::{
        intrinsics,
        memory::syntax::Native,
        stg::{panic::Panic, runtime::Runtime, syntax::dsl::*, testing},
    };

    pub fn runtime() -> Box<dyn Runtime> {
        testing::runtime(vec![Box::new(Add), Box::new(Pow), Box::new(Panic)])
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
        // ADD called directly (bypassing the wrapper) returns a native atom via
        // machine_return_num.  native_return() should give Some(Num(4)).
        let syntax = letrec_(
            vec![],
            app_bif(intrinsics::index_u8("ADD"), vec![num(2), num(2)]),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert!(m.terminated());
        // Result is a native atom — native_return() should yield Some(Num(4)).
        assert_eq!(m.native_return(), Some(Native::Num(4.into())));
    }

    #[test]
    pub fn test_unboxed_pow_int() {
        let syntax = letrec_(
            vec![],
            app_bif(intrinsics::index_u8("POW"), vec![num(2), num(10)]),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.native_return(), Some(Native::Num(1024.into())));
    }

    #[test]
    pub fn test_unboxed_pow_zero_exp() {
        let syntax = letrec_(
            vec![],
            app_bif(intrinsics::index_u8("POW"), vec![num(5), num(0)]),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.native_return(), Some(Native::Num(1.into())));
    }

    #[test]
    fn test_floor_div_i64() {
        assert_eq!(floor_div_i64(7, 2), Some(3));
        assert_eq!(floor_div_i64(-7, 2), Some(-4));
        assert_eq!(floor_div_i64(7, -2), Some(-4));
        assert_eq!(floor_div_i64(-7, -2), Some(3));
        assert_eq!(floor_div_i64(6, 3), Some(2));
        assert_eq!(floor_div_i64(-6, 3), Some(-2));
        assert_eq!(floor_div_i64(0, 5), Some(0));
    }

    #[test]
    fn test_floor_mod_i64() {
        assert_eq!(floor_mod_i64(7, 3), Some(1));
        assert_eq!(floor_mod_i64(-7, 3), Some(2));
        assert_eq!(floor_mod_i64(7, -3), Some(-2));
        assert_eq!(floor_mod_i64(-7, -3), Some(-1));
        assert_eq!(floor_mod_i64(6, 3), Some(0));
        assert_eq!(floor_mod_i64(0, 5), Some(0));
    }

    #[test]
    fn test_num_from_floored() {
        assert_eq!(num_from_floored(3.0), Some(Number::from(3_i64)));
        assert_eq!(num_from_floored(-4.0), Some(Number::from(-4_i64)));
        assert_eq!(num_from_floored(0.0), Some(Number::from(0_i64)));
    }
}
