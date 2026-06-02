//! Running aggregate intrinsics: running-max, running-min, running-sum

use std::convert::TryInto;

use serde_json;

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

/// `SUM_NUM_LIST(nums)` — sum a list of numbers in a single Rust pass.
///
/// The wrapper forces all elements via `SeqNumList` so that `execute`
/// receives a fully-evaluated cons structure.  Replacing `sum: foldl(+, 0)`
/// with this native BIF eliminates the N-deep thunk chain that `foldl`
/// builds before any arithmetic occurs, giving O(N) ticks instead of the
/// O(N²) behaviour of the lazy accumulator pattern.
pub struct SumNumList;

impl StgIntrinsic for SumNumList {
    fn name(&self) -> &str {
        "SUM_NUM_LIST"
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
        // Use integer representation when the result is a whole number so
        // that YAML/JSON output matches what `foldl(+, 0, l)` produces.
        let n = if total.fract() == 0.0 && total.is_finite() {
            serde_json::Number::from(total as i64)
        } else {
            serde_json::Number::from_f64(total).ok_or_else(|| {
                ExecutionError::Panic(Smid::default(), "SUM_NUM_LIST: non-finite sum".to_string())
            })?
        };
        machine_return_boxed_num(machine, view, n)
    }
}

impl CallGlobal1 for SumNumList {}

/// `PRODUCT_NUM_LIST(nums)` — multiply a list of numbers in a single Rust pass.
///
/// Replaces `product: foldl(*, 1)` with the same native approach as
/// `SUM_NUM_LIST`.  Eliminates the N-deep lazy accumulator thunk chain.
pub struct ProductNumList;

impl StgIntrinsic for ProductNumList {
    fn name(&self) -> &str {
        "PRODUCT_NUM_LIST"
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
        let n = if total.fract() == 0.0 && total.is_finite() {
            serde_json::Number::from(total as i64)
        } else {
            serde_json::Number::from_f64(total).ok_or_else(|| {
                ExecutionError::Panic(
                    Smid::default(),
                    "PRODUCT_NUM_LIST: non-finite product".to_string(),
                )
            })?
        };
        machine_return_boxed_num(machine, view, n)
    }
}

impl CallGlobal1 for ProductNumList {}
