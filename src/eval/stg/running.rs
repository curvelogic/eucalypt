//! Running aggregate intrinsics: running-max, running-min, running-sum
//! Native aggregate intrinsics: max-of, min-of

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
    support::{collect_num_list, machine_return_num, machine_return_num_list},
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

/// `MAX_OF(nums)` — maximum of a numeric list.
///
/// Replaces `max-of(l)` which calls `count(l)` at every recursion level,
/// making the original O(N²).  This intrinsic forces via `SeqNumList`,
/// collects to `Vec<f64>`, and folds with `f64::max` in a single O(N) pass.
///
/// Returns an error for empty lists (matching `max-of` semantics).
pub struct MaxOf;

impl StgIntrinsic for MaxOf {
    fn name(&self) -> &str {
        "MAX_OF"
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
        if nums.is_empty() {
            return Err(ExecutionError::Panic(
                machine.annotation(),
                "max of empty list".to_string(),
            ));
        }
        let max = nums.iter().copied().fold(f64::NEG_INFINITY, f64::max);
        let n = if max.fract() == 0.0 && max.abs() < 9.007_199_254_740_992e15 {
            Number::from(max as i64)
        } else {
            Number::from_f64(max).unwrap_or_else(|| Number::from(0))
        };
        machine_return_num(machine, view, n)
    }
}

impl CallGlobal1 for MaxOf {}

/// `MIN_OF(nums)` — minimum of a numeric list.
///
/// Replaces `min-of(l)` with the same O(N²)→O(N) improvement as `MAX_OF`.
///
/// Returns an error for empty lists (matching `min-of` semantics).
pub struct MinOf;

impl StgIntrinsic for MinOf {
    fn name(&self) -> &str {
        "MIN_OF"
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
        if nums.is_empty() {
            return Err(ExecutionError::Panic(
                machine.annotation(),
                "min of empty list".to_string(),
            ));
        }
        let min = nums.iter().copied().fold(f64::INFINITY, f64::min);
        let n = if min.fract() == 0.0 && min.abs() < 9.007_199_254_740_992e15 {
            Number::from(min as i64)
        } else {
            Number::from_f64(min).unwrap_or_else(|| Number::from(0))
        };
        machine_return_num(machine, view, n)
    }
}

impl CallGlobal1 for MinOf {}
