//! Running aggregate intrinsics: running-max, running-min, running-sum

use std::convert::TryInto;

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
    support::{collect_num_list, machine_return_num_list},
    syntax::{
        dsl::{annotated_lambda, app_bif, force, lref},
        LambdaForm,
    },
};

/// Shared wrapper for single-list intrinsics: force via SeqNumList
/// then call the intrinsic.
fn num_list_wrapper(intrinsic: &dyn StgIntrinsic, annotation: Smid) -> LambdaForm {
    let bif_index: u8 = intrinsic.index().try_into().unwrap();
    annotated_lambda(
        1, // [nums]
        force(
            SeqNumList.global(lref(0)),
            // [concrete_nums] [nums]
            app_bif(bif_index, vec![lref(0)]),
        ),
        annotation,
    )
}

/// `running-max(nums)` — running maximum over a number list.
/// `[3, 1, 4, 1]` → `[3, 3, 4, 4]`
pub struct RunningMax;

impl StgIntrinsic for RunningMax {
    fn name(&self) -> &str {
        "RUNNING_MAX"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        num_list_wrapper(self, annotation)
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

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        num_list_wrapper(self, annotation)
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

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        num_list_wrapper(self, annotation)
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
