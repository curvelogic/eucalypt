//! Intrinsics for opaque PRNG stream values backed by SplitMix64.
//!
//! These intrinsics expose `Native::Prng(u64)` — an opaque state word —
//! through six operations:
//!
//! - `STREAM_NEW`     — seed a new stream from an integer
//! - `STREAM_VALUE`   — read the current float value without advancing
//! - `STREAM_ADVANCE` — advance the state, returning the new stream
//! - `STREAM_FLOAT`   — convenience: returns `[float, advanced_stream]`
//! - `STREAM_INT`     — returns `[int_in_0_to_n, advanced_stream]`
//! - `STREAM_SPLIT`   — split one stream into two independent streams

use serde_json::Number;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal1, CallGlobal2, IntrinsicMachine, StgIntrinsic},
        memory::{
            alloc::ScopedAllocator,
            array::Array,
            mutator::MutatorHeapView,
            syntax::{LambdaForm, Native, Ref, StgBuilder},
        },
    },
};

use super::{
    prng::{seed_to_u64, splitmix64},
    support::{machine_return_num, num_arg},
    tags::DataConstructor,
};

/// Extract a `Native::Prng` state word from a ref, or return a
/// `TypeMismatch` error.
fn prng_arg(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<u64, ExecutionError> {
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::Prng(state) = native {
        Ok(state)
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            Box::new(crate::eval::types::IntrinsicType::Unknown),
            Box::new(crate::eval::types::IntrinsicType::Unknown),
            None,
        ))
    }
}

/// Return a `Native::Prng(state)` atom as the machine result.
fn machine_return_stream(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    state: u64,
) -> Result<(), ExecutionError> {
    use crate::eval::memory::syntax::HeapSyn;
    machine.set_closure(crate::eval::machine::env::SynClosure::new(
        view.alloc(HeapSyn::Atom {
            evaluand: Ref::V(Native::Prng(state)),
        })?
        .as_ptr(),
        machine.root_env(),
    ))
}

/// Build and return a two-element list `[first, second]` where both
/// elements are `Native::Prng` values.
fn machine_return_stream_pair(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    first: u64,
    second: u64,
) -> Result<(), ExecutionError> {
    use crate::eval::memory::syntax::HeapSyn;
    // Bindings (built in reverse for cons-list construction):
    // [0] nil
    // [1] Atom(Stream(second))
    // [2] ListCons(L(1), L(0))   — [second]
    // [3] Atom(Stream(first))
    // [4] ListCons(L(3), L(2))   — [first, second]
    let mut bindings = vec![LambdaForm::value(view.nil()?.as_ptr())];

    bindings.push(LambdaForm::value(
        view.alloc(HeapSyn::Atom {
            evaluand: Ref::V(Native::Prng(second)),
        })?
        .as_ptr(),
    ));
    let len = bindings.len();
    bindings.push(LambdaForm::value(
        view.data(
            DataConstructor::ListCons.tag(),
            Array::from_slice(&view, &[Ref::L(len - 1), Ref::L(len - 2)]),
        )?
        .as_ptr(),
    ));

    bindings.push(LambdaForm::value(
        view.alloc(HeapSyn::Atom {
            evaluand: Ref::V(Native::Prng(first)),
        })?
        .as_ptr(),
    ));
    let len = bindings.len();
    bindings.push(LambdaForm::value(
        view.data(
            DataConstructor::ListCons.tag(),
            Array::from_slice(&view, &[Ref::L(len - 1), Ref::L(len - 2)]),
        )?
        .as_ptr(),
    ));

    let list_index = bindings.len() - 1;
    let syn = view
        .letrec(
            Array::from_slice(&view, &bindings),
            view.atom(Ref::L(list_index))?,
        )?
        .as_ptr();
    machine.set_closure(crate::eval::machine::env::SynClosure::new(
        syn,
        machine.root_env(),
    ))
}

/// Build and return a two-element list `[float, stream]`.
///
/// The float is wrapped in `BoxedNumber` as eucalypt expects.
fn machine_return_float_stream(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    float_val: f64,
    stream_state: u64,
) -> Result<(), ExecutionError> {
    use crate::eval::memory::syntax::HeapSyn;

    let n = Number::from_f64(float_val).ok_or_else(|| {
        ExecutionError::Panic(Smid::default(), "STREAM produced invalid float".to_string())
    })?;

    // Bindings:
    // [0] nil
    // [1] Atom(Stream(stream_state))
    // [2] ListCons(L(1), L(0))           — [stream]
    // [3] BoxedNumber(Num(float_val))
    // [4] ListCons(L(3), L(2))           — [float, stream]
    let mut bindings = vec![LambdaForm::value(view.nil()?.as_ptr())];

    bindings.push(LambdaForm::value(
        view.alloc(HeapSyn::Atom {
            evaluand: Ref::V(Native::Prng(stream_state)),
        })?
        .as_ptr(),
    ));
    let len = bindings.len();
    bindings.push(LambdaForm::value(
        view.data(
            DataConstructor::ListCons.tag(),
            Array::from_slice(&view, &[Ref::L(len - 1), Ref::L(len - 2)]),
        )?
        .as_ptr(),
    ));

    bindings.push(LambdaForm::value(
        view.data(
            DataConstructor::BoxedNumber.tag(),
            Array::from_slice(&view, &[Ref::V(Native::Num(n))]),
        )?
        .as_ptr(),
    ));
    let len = bindings.len();
    bindings.push(LambdaForm::value(
        view.data(
            DataConstructor::ListCons.tag(),
            Array::from_slice(&view, &[Ref::L(len - 1), Ref::L(len - 2)]),
        )?
        .as_ptr(),
    ));

    let list_index = bindings.len() - 1;
    let syn = view
        .letrec(
            Array::from_slice(&view, &bindings),
            view.atom(Ref::L(list_index))?,
        )?
        .as_ptr();
    machine.set_closure(crate::eval::machine::env::SynClosure::new(
        syn,
        machine.root_env(),
    ))
}

/// Build and return a two-element list `[int, stream]`.
///
/// The integer is wrapped in `BoxedNumber` as eucalypt expects.
fn machine_return_int_stream(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    int_val: u64,
    stream_state: u64,
) -> Result<(), ExecutionError> {
    use crate::eval::memory::syntax::HeapSyn;

    // Bindings:
    // [0] nil
    // [1] Atom(Stream(stream_state))
    // [2] ListCons(L(1), L(0))            — [stream]
    // [3] BoxedNumber(Num(int_val as i64))
    // [4] ListCons(L(3), L(2))            — [int, stream]
    let mut bindings = vec![LambdaForm::value(view.nil()?.as_ptr())];

    bindings.push(LambdaForm::value(
        view.alloc(HeapSyn::Atom {
            evaluand: Ref::V(Native::Prng(stream_state)),
        })?
        .as_ptr(),
    ));
    let len = bindings.len();
    bindings.push(LambdaForm::value(
        view.data(
            DataConstructor::ListCons.tag(),
            Array::from_slice(&view, &[Ref::L(len - 1), Ref::L(len - 2)]),
        )?
        .as_ptr(),
    ));

    bindings.push(LambdaForm::value(
        view.data(
            DataConstructor::BoxedNumber.tag(),
            Array::from_slice(&view, &[Ref::V(Native::Num(Number::from(int_val as i64)))]),
        )?
        .as_ptr(),
    ));
    let len = bindings.len();
    bindings.push(LambdaForm::value(
        view.data(
            DataConstructor::ListCons.tag(),
            Array::from_slice(&view, &[Ref::L(len - 1), Ref::L(len - 2)]),
        )?
        .as_ptr(),
    ));

    let list_index = bindings.len() - 1;
    let syn = view
        .letrec(
            Array::from_slice(&view, &bindings),
            view.atom(Ref::L(list_index))?,
        )?
        .as_ptr();
    machine.set_closure(crate::eval::machine::env::SynClosure::new(
        syn,
        machine.root_env(),
    ))
}

// ─── STREAM_NEW ──────────────────────────────────────────────────────────────

/// `STREAM_NEW(seed)` — create a new stream from an integer seed.
///
/// Returns `Native::Prng(state)` where `state` is derived from the seed
/// via `seed_to_u64`.
pub struct StreamNew;

impl StgIntrinsic for StreamNew {
    fn name(&self) -> &str {
        "STREAM_NEW"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let seed_num = num_arg(machine, view, &args[0])?;
        let state = seed_to_u64(&seed_num);
        machine_return_stream(machine, view, state)
    }
}

impl CallGlobal1 for StreamNew {}

// ─── STREAM_VALUE ─────────────────────────────────────────────────────────────

/// `STREAM_VALUE(stream)` — read the current float value `[0, 1)` without
/// advancing the stream state.
pub struct StreamValue;

impl StgIntrinsic for StreamValue {
    fn name(&self) -> &str {
        "STREAM_VALUE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let state = prng_arg(machine, view, &args[0])?;
        let (_, z) = splitmix64(state);
        let float_val = (z >> 11) as f64 / ((1u64 << 53) as f64);
        let result = Number::from_f64(float_val).ok_or_else(|| {
            ExecutionError::Panic(
                Smid::default(),
                "STREAM_VALUE produced invalid float".to_string(),
            )
        })?;
        machine_return_num(machine, view, result)
    }
}

impl CallGlobal1 for StreamValue {}

// ─── STREAM_ADVANCE ──────────────────────────────────────────────────────────

/// `STREAM_ADVANCE(stream)` — advance the SplitMix64 state, returning a new
/// stream with the next state.
pub struct StreamAdvance;

impl StgIntrinsic for StreamAdvance {
    fn name(&self) -> &str {
        "STREAM_ADVANCE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let state = prng_arg(machine, view, &args[0])?;
        let (next_state, _) = splitmix64(state);
        machine_return_stream(machine, view, next_state)
    }
}

impl CallGlobal1 for StreamAdvance {}

// ─── STREAM_FLOAT ─────────────────────────────────────────────────────────────

/// `STREAM_FLOAT(stream)` — convenience intrinsic returning `[float, stream]`.
///
/// Advances the state and returns both the float value in `[0, 1)` and the
/// new stream in a two-element list.
pub struct StreamFloat;

impl StgIntrinsic for StreamFloat {
    fn name(&self) -> &str {
        "STREAM_FLOAT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let state = prng_arg(machine, view, &args[0])?;
        let (next_state, z) = splitmix64(state);
        let float_val = (z >> 11) as f64 / ((1u64 << 53) as f64);
        machine_return_float_stream(machine, view, float_val, next_state)
    }
}

impl CallGlobal1 for StreamFloat {}

// ─── STREAM_INT ───────────────────────────────────────────────────────────────

/// `STREAM_INT(n, stream)` — return `[random_int_in_0_to_n, advanced_stream]`.
///
/// The random integer is computed as `splitmix64_output % n`.
/// Returns the advanced stream alongside the integer.
pub struct StreamInt;

impl StgIntrinsic for StreamInt {
    fn name(&self) -> &str {
        "STREAM_INT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let n_num = num_arg(machine, view, &args[0])?;
        let n = n_num.as_u64().unwrap_or(1).max(1);
        let state = prng_arg(machine, view, &args[1])?;
        let (next_state, z) = splitmix64(state);
        let int_val = z % n;
        machine_return_int_stream(machine, view, int_val, next_state)
    }
}

impl CallGlobal2 for StreamInt {}

// ─── STREAM_SPLIT ─────────────────────────────────────────────────────────────

/// `STREAM_SPLIT(stream)` — split one stream into two independent streams.
///
/// Uses the SplitMix64 next state and mixed output as the two new seeds:
/// `state_a = next_state`, `state_b = z` (the mixed output word).
pub struct StreamSplit;

impl StgIntrinsic for StreamSplit {
    fn name(&self) -> &str {
        "STREAM_SPLIT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let state = prng_arg(machine, view, &args[0])?;
        let (next_state, z) = splitmix64(state);
        machine_return_stream_pair(machine, view, next_state, z)
    }
}

impl CallGlobal1 for StreamSplit {}
