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
    force::{SeqList, SeqNumList},
    support::{collect_num_list, data_list_arg, machine_return_bool, machine_return_num_list},
    syntax::{
        dsl::{app_bif, force, lambda, lref},
        LambdaForm,
    },
    tags::DataConstructor,
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

/// Shared wrapper for boolean-list intrinsics: force via SeqList then call the BIF.
fn bool_list_wrapper(intrinsic: &dyn StgIntrinsic) -> LambdaForm {
    let bif_index: u8 = intrinsic.index().try_into().unwrap();
    lambda(
        1, // [bools]
        force(
            SeqList.global(lref(0)),
            // [concrete_bools] [bools]
            app_bif(bif_index, vec![lref(0)]),
        ),
    )
}

/// Extract a bool from a forced element closure.
///
/// After `SeqList`, boolean elements are `Cons { tag: BoolTrue/BoolFalse, .. }`.
fn read_bool(
    closure: &crate::eval::machine::env::SynClosure,
    view: &MutatorHeapView<'_>,
) -> Result<bool, ExecutionError> {
    let code = view.scoped(closure.code());
    match &*code {
        crate::eval::memory::syntax::HeapSyn::Cons { tag, .. } => match (*tag).try_into() {
            Ok(DataConstructor::BoolTrue) => Ok(true),
            Ok(DataConstructor::BoolFalse) => Ok(false),
            _ => Err(ExecutionError::Panic(
                crate::common::sourcemap::Smid::default(),
                format!("ALL/ANY_TRUE_LIST: expected boolean, got tag {}", tag),
            )),
        },
        _ => Err(ExecutionError::Panic(
            crate::common::sourcemap::Smid::default(),
            "ALL/ANY_TRUE_LIST: expected boolean value".to_string(),
        )),
    }
}

/// `ALL_TRUE_LIST(bools)` — true iff every element of a boolean list is true.
///
/// Replaces `all-true?: foldl(and, true)` which builds an N-deep lazy
/// accumulator thunk chain.  Uses `SeqList` to force the list upfront,
/// then traverses in a single O(N) Rust loop.
pub struct AllTrueList;

impl StgIntrinsic for AllTrueList {
    fn name(&self) -> &str {
        "ALL_TRUE_LIST"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        bool_list_wrapper(self)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let iter = data_list_arg(machine, view, args[0].clone())?;
        for item in iter {
            let closure = item?;
            if !read_bool(&closure, &view)? {
                return machine_return_bool(machine, view, false);
            }
        }
        machine_return_bool(machine, view, true)
    }
}

impl CallGlobal1 for AllTrueList {}
