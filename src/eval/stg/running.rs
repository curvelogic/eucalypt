//! Running aggregate intrinsics: running-max, running-min, running-sum,
//! scanl-add

use std::convert::TryInto;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal1, CallGlobal2, IntrinsicMachine, StgIntrinsic},
        memory::{mutator::MutatorHeapView, syntax::Ref},
    },
};

use super::{
    force::SeqNumList,
    support::{collect_num_list, machine_return_num_list, num_arg},
    syntax::{
        dsl::{app_bif, force, lambda, local, lref, unbox_num},
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

/// `SCANL_ADD(seed, nums)` — prefix sums of a number list from an initial seed.
///
/// Returns a num list of length `L + 1` where L is the input length:
/// `[seed, seed+n0, seed+n0+n1, …]`.  This is the num-list fast path for
/// `scanl(+, seed, nums)` — it avoids the per-element thunk chain of the
/// interpreter-level recursive `scanl` definition.
///
/// The prelude exposes this as `scanl-add(seed, nums)` (and via catenation as
/// `nums scanl-add(seed)`).  The general `scanl(op, i, l)` prelude function
/// remains for non-numeric or non-addition cases.
///
/// Example: `[3, 1, 4]` with seed=10 → `[10, 13, 14, 18]`
pub struct ScanlAdd;

impl StgIntrinsic for ScanlAdd {
    fn name(&self) -> &str {
        "SCANL_ADD"
    }

    /// Wrapper: force the num list (arg 1, `nums`) via `SeqNumList`, then
    /// unbox arg 0 (`seed`, a `BoxedNumber`), then call the BIF.
    ///
    /// STG argument layout inside the lambda:
    ///   local 0 = seed  (BoxedNumber)
    ///   local 1 = nums  (list)
    ///
    /// After `force(SeqNumList(local 1), ...)`:
    ///   local 0 = forced_list
    ///   local 1 = seed (BoxedNumber, was local 0)
    ///
    /// After `unbox_num(local(1), ...)`:
    ///   local 0 = seed_inner (native Num)
    ///   local 1 = forced_list
    ///
    /// BIF call: `(local 0 = seed_native, local 1 = forced_list)`
    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        let bif_index: u8 = self.index().try_into().unwrap();
        lambda(
            2, // [seed, nums] — seed is local 0, nums is local 1
            force(
                SeqNumList.global(lref(1)),
                // After force: local 0 = forced_list, local 1 = seed (BoxedNumber)
                unbox_num(
                    local(1),
                    // After unbox_num: local 0 = seed_inner (native Num), local 1 = forced_list
                    app_bif(bif_index, vec![lref(0), lref(1)]),
                ),
            ),
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let seed = {
            let num = num_arg(machine, view, &args[0])?;
            num.as_f64().unwrap_or(0.0)
        };
        let nums = collect_num_list(machine, view, args[1].clone())?;

        let mut result = Vec::with_capacity(nums.len() + 1);
        let mut acc = seed;
        result.push(acc);
        for &n in &nums {
            acc += n;
            result.push(acc);
        }

        machine_return_num_list(machine, view, result)
    }
}

impl CallGlobal2 for ScanlAdd {}
