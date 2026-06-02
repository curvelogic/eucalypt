//! Running aggregate intrinsics: running-max, running-min, running-sum,
//! sliding-sum

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

/// `SLIDING_SUM_NUM_LIST(n, nums)` — sliding window sums over a number list.
///
/// Replaces `window(n, 1) map(sum)` which creates N cons cells for the window
/// structure, calls `count` (an O(n) traversal) per window step to check the
/// window is full, and then calls `sum` (an `foldl`-based fold) per window.
/// For a list of length L with window size n, the eucalypt pipeline does
/// O(L×n) work; this BIF uses a rolling sum for O(L) time with no heap
/// allocation per element.
///
/// Returns a num list of length `max(0, L - n + 1)`.  Returns an empty list
/// when L < n or n = 0.
///
/// Example: `[1, 2, 3, 4, 5]` with n=3 → `[6.0, 9.0, 12.0]`
pub struct SlidingSumNumList;

impl StgIntrinsic for SlidingSumNumList {
    fn name(&self) -> &str {
        "SLIDING_SUM_NUM_LIST"
    }

    /// Custom wrapper: force the num list (arg 0, `nums`) via `SeqNumList`,
    /// then pass `(n, forced_nums)` to the BIF.  `n` (arg 1) is passed
    /// as-is; `num_arg` in `execute` handles unboxing.
    ///
    /// The STG calling convention for this BIF is `(nums, n)` — i.e.
    /// `nums` is local 0 and `n` is local 1 inside the lambda.  This
    /// matches the prelude definition `sliding-sum(nums, n)`, which allows
    /// the idiomatic catenation call `list sliding-sum(window-size)`.
    /// Custom wrapper: force the num list (arg 1, `nums`) via `SeqNumList`,
    /// then pass `(n, forced_nums)` to the BIF.  `n` (arg 0) is passed
    /// as-is; `num_arg` in `execute` handles unboxing.
    ///
    /// The STG calling convention for this BIF is `(n, nums)` — i.e.
    /// `n` is local 0 and `nums` is local 1 inside the lambda.  This
    /// matches the prelude definition `__SLIDING_SUM_NUM_LIST(n, nums)`,
    /// which is called from `sliding-sum(nums, n)` as a curried form
    /// to allow the idiomatic catenation call `list sliding-sum(window-size)`.
    /// Custom wrapper: force the num list (arg 1, `nums`) via `SeqNumList`,
    /// then unbox arg 0 (`n`, which arrives as a `BoxedNumber`), and call
    /// the BIF with the unboxed window size and the forced list.
    ///
    /// STG argument layout inside the lambda:
    ///   local 0 = n   (BoxedNumber)
    ///   local 1 = nums (list)
    ///
    /// After `force(SeqNumList(local1), ...)`:
    ///   local 0 = forced_list
    ///   local 1 = n (BoxedNumber, was local 0)
    ///   local 2 = nums (was local 1)
    ///
    /// After `unbox_num(local(1), ...)`:
    ///   local 0 = inner Num ref (native atom)
    ///   local 1 = forced_list (was local 0)
    ///   ... (higher indices: BoxedNumber, nums, etc.)
    ///
    /// BIF call: `(local 0 = n_native, local 1 = forced_list)`
    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        let bif_index: u8 = self.index().try_into().unwrap();
        lambda(
            2, // [n, nums]  — n is local 0, nums is local 1
            force(
                SeqNumList.global(lref(1)),
                // After force: local 0 = forced_list, local 1 = n (BoxedNumber), local 2 = nums
                unbox_num(
                    local(1),
                    // After unbox_num: local 0 = n_inner (native Num), local 1 = forced_list
                    app_bif(bif_index, vec![lref(0), lref(1)]),
                    // BIF receives (n = local 0, forced_list = local 1)
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
        let n = {
            let num = num_arg(machine, view, &args[0])?;
            num.as_u64().unwrap_or(0) as usize
        };
        let nums = collect_num_list(machine, view, args[1].clone())?;

        if n == 0 || nums.len() < n {
            return machine_return_num_list(machine, view, vec![]);
        }

        let mut result = Vec::with_capacity(nums.len() - n + 1);

        // Seed the first window sum.
        let mut win_sum: f64 = nums[..n].iter().sum();
        result.push(win_sum);

        // Roll the window forward one element at a time.
        for i in n..nums.len() {
            win_sum += nums[i] - nums[i - n];
            result.push(win_sum);
        }

        machine_return_num_list(machine, view, result)
    }
}

impl CallGlobal2 for SlidingSumNumList {}
