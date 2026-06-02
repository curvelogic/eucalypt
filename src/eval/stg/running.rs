//! Running aggregate intrinsics: running-max, running-min, running-sum

use std::convert::TryInto;

use serde_json::Number;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal1, IntrinsicMachine, StgIntrinsic},
        memory::{mutator::MutatorHeapView, syntax::Ref},
    },
};

use super::{
    force::SeqNumList,
    support::{collect_num_list, machine_return_boxed_num, machine_return_num_list},
    syntax::{
        dsl::{app_bif, force, lambda, lref},
        LambdaForm,
    },
};

/// Shared wrapper for single-list intrinsics: force via SeqNumList
/// then call the intrinsic.
fn num_list_wrapper(intrinsic: &dyn StgIntrinsic) -> LambdaForm {
    let bif_index: u8 = intrinsic.index().try_into().unwrap();
    lambda(
        1, // [nums]
        force(
            SeqNumList.global(lref(0)),
            // [concrete_nums] [nums]
            app_bif(bif_index, vec![lref(0)]),
        ),
    )
}

/// `running-max(nums)` — running maximum over a number list.
/// `[3, 1, 4, 1]` → `[3, 3, 4, 4]`
pub struct RunningMax;

impl StgIntrinsic for RunningMax {
    fn name(&self) -> &str {
        "RUNNING_MAX"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        num_list_wrapper(self)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let nums = collect_num_list(machine, view, args[0].clone())?;
        let mut result = Vec::with_capacity(nums.len());
        let mut acc = f64::NEG_INFINITY;
        for &n in &nums {
            acc = acc.max(n);
            result.push(acc);
        }
        machine_return_num_list(machine, view, result)
    }
}

impl CallGlobal1 for RunningMax {}

/// `running-min(nums)` — running minimum over a number list.
/// `[3, 1, 4, 1]` → `[3, 1, 1, 1]`
pub struct RunningMin;

impl StgIntrinsic for RunningMin {
    fn name(&self) -> &str {
        "RUNNING_MIN"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        num_list_wrapper(self)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let nums = collect_num_list(machine, view, args[0].clone())?;
        let mut result = Vec::with_capacity(nums.len());
        let mut acc = f64::INFINITY;
        for &n in &nums {
            acc = acc.min(n);
            result.push(acc);
        }
        machine_return_num_list(machine, view, result)
    }
}

impl CallGlobal1 for RunningMin {}

/// `running-sum(nums)` — cumulative sum over a number list.
/// `[3, 1, 4, 1]` → `[3, 4, 8, 9]`
pub struct RunningSum;

impl StgIntrinsic for RunningSum {
    fn name(&self) -> &str {
        "RUNNING_SUM"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        num_list_wrapper(self)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let nums = collect_num_list(machine, view, args[0].clone())?;
        let mut result = Vec::with_capacity(nums.len());
        let mut acc = 0.0_f64;
        for &n in &nums {
            acc += n;
            result.push(acc);
        }
        machine_return_num_list(machine, view, result)
    }
}

impl CallGlobal1 for RunningSum {}

/// Convert an f64 result to a `Number`, preferring integer representation
/// for whole-number values to match the behaviour of arithmetic operators
/// (which try integer addition first).
fn f64_to_number(val: f64) -> Number {
    if val.is_finite() && val.fract() == 0.0 && val >= i64::MIN as f64 && val <= i64::MAX as f64 {
        Number::from(val as i64)
    } else {
        Number::from_f64(val).unwrap_or_else(|| Number::from(0))
    }
}

/// `SUM_LIST(nums)` — sum all elements of a number list.
///
/// Replaces `sum(l): foldl(+, 0, l)`, which builds an N-deep lazy accumulator
/// thunk chain that the GC must traverse each collection cycle.  This BIF uses
/// `SeqNumList` to unbox and force all elements, then folds in a single O(N)
/// Rust pass with no heap thunks.
pub struct SumList;

impl StgIntrinsic for SumList {
    fn name(&self) -> &str {
        "SUM_LIST"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        num_list_wrapper(self)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let nums = collect_num_list(machine, view, args[0].clone())?;
        let total: f64 = nums.iter().sum();
        machine_return_boxed_num(machine, view, f64_to_number(total))
    }
}

impl CallGlobal1 for SumList {}

/// `PRODUCT_LIST(nums)` — multiply all elements of a number list.
///
/// Replaces `product(l): foldl(*, 1, l)` which has the same lazy accumulator
/// thunk-chain problem as `sum`.  Returns 1 for empty lists.
pub struct ProductList;

impl StgIntrinsic for ProductList {
    fn name(&self) -> &str {
        "PRODUCT_LIST"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        num_list_wrapper(self)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let nums = collect_num_list(machine, view, args[0].clone())?;
        let total: f64 = nums.iter().product();
        machine_return_boxed_num(machine, view, f64_to_number(total))
    }
}

impl CallGlobal1 for ProductList {}
