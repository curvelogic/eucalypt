//! Vec intrinsics for the eucalypt VM.
//!
//! These intrinsics operate on `Native::Vec` values, providing
//! construction, indexed access, slicing, sampling, and conversion.

use crate::common::sourcemap::Smid;
use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    machine::intrinsic::{CallGlobal1, CallGlobal2, CallGlobal3, IntrinsicMachine, StgIntrinsic},
    memory::{
        mutator::MutatorHeapView,
        set::Primitive,
        syntax::{Native, Ref},
    },
    stg::tags::DataConstructor,
};

use super::{
    force::{SeqList, SeqNumList},
    support::{
        collect_num_list, data_list_arg, machine_return_num, machine_return_vec,
        native_to_set_primitive, resolve_native_unboxing, set_primitive_to_native, vec_arg,
    },
    syntax::{
        dsl::{app_bif, force, lambda, lref},
        LambdaForm,
    },
};

/// Convert the index argument `what` of vec function `func` to a `usize`.
///
/// A non-number, a non-integer (`0.5`) and a negative number are errors
/// naming the function and the value, never clamped or truncated. An
/// integral float such as `2.0` is accepted.
fn vec_index(func: &str, what: &str, native: &Native, smid: Smid) -> Result<usize, ExecutionError> {
    let Native::Num(num) = native else {
        return Err(ExecutionError::Panic(
            smid,
            format!("{func}: {what} must be a number"),
        ));
    };
    if let Some(n) = num.as_u64() {
        // Beyond `usize` (only on 32-bit targets) it is out of range anyway.
        return Ok(usize::try_from(n).unwrap_or(usize::MAX));
    }
    if num.as_i64().is_some() {
        return Err(ExecutionError::Panic(
            smid,
            format!("{func} {what} {num} out of range: vec indices start at 0"),
        ));
    }
    let f = num.as_f64().unwrap_or(f64::NAN);
    if f.fract() != 0.0 || !f.is_finite() {
        return Err(ExecutionError::Panic(
            smid,
            format!("{func} {what} {num} must be an integer"),
        ));
    }
    if f < 0.0 {
        return Err(ExecutionError::Panic(
            smid,
            format!("{func} {what} {num} out of range: vec indices start at 0"),
        ));
    }
    Ok(f as usize)
}

/// The index as the user wrote it, rather than as converted to `usize`
/// (which saturates a huge value such as `1e20`).
fn num_display(native: &Native) -> String {
    match native {
        Native::Num(num) => num.to_string(),
        _ => String::new(),
    }
}

/// VEC.OF — convert a list of primitives to a vec.
pub struct VecOf;

impl StgIntrinsic for VecOf {
    fn name(&self) -> &str {
        "VEC.OF"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        let bif_index: u8 = self.index().try_into().unwrap();
        // Deep-force the list so all elements are at WHNF before execute()
        lambda(
            1, // [list]
            force(
                SeqList.global(lref(0)),
                // [forced_list] [list]
                app_bif(bif_index, vec![lref(0)]),
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
        // The wrapper has deep-forced the spine, so walk it via the neutral
        // list ABI and read each element's native primitive with `value_native`.
        let smid = machine.annotation();
        let items = data_list_arg(machine, view, args[0].clone())?;
        let mut primitives = Vec::with_capacity(items.len());
        for item in &items {
            let native = machine.value_native(view, item).ok_or_else(|| {
                ExecutionError::Panic(smid, "non-primitive value in vec construction".to_string())
            })?;
            primitives.push(native_to_set_primitive(smid, &native)?);
        }

        machine_return_vec(machine, view, &primitives)
    }
}

impl CallGlobal1 for VecOf {}

/// VEC.LEN — return the number of elements in a vec.
pub struct VecLen;

impl StgIntrinsic for VecLen {
    fn name(&self) -> &str {
        "VEC.LEN"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let v = vec_arg(machine, view, &args[0])?;
        let n = serde_json::Number::from(v.len() as u64);
        machine_return_num(machine, view, n)
    }
}

impl CallGlobal1 for VecLen {}

/// VEC.NTH — return the element at index n (0-based).
///
/// Returns an error if the index is negative, not an integer, or out of
/// bounds.
pub struct VecNth;

impl StgIntrinsic for VecNth {
    fn name(&self) -> &str {
        "VEC.NTH"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // `Smid::default()` lets the error take its location from the call
        // site on the stack (`vec.nth(…)`); the intrinsic's annotation
        // would point at the vec argument instead.
        let smid = Smid::default();
        let native_n = resolve_native_unboxing(machine, view, &args[0])?;
        let n = vec_index("vec.nth", "index", &native_n, smid)?;
        let v = vec_arg(machine, view, &args[1])?;
        let prim = v.get(n).ok_or_else(|| {
            ExecutionError::Panic(
                smid,
                format!(
                    "vec.nth index {} out of range for vec of length {}",
                    num_display(&native_n),
                    v.len()
                ),
            )
        })?;
        let native = set_primitive_to_native(&prim);
        // Return a boxed value (BoxedNumber/BoxedString/BoxedSymbol) so that the
        // result is comparable via the EQ wrapper's Cons branches, matching the
        // format of list elements and the output of other intrinsics.
        let box_tag = match &native {
            Native::Num(_) => DataConstructor::BoxedNumber.tag(),
            Native::Str(_) => DataConstructor::BoxedString.tag(),
            Native::Sym(_) => DataConstructor::BoxedSymbol.tag(),
            _ => {
                return Err(ExecutionError::Panic(
                    smid,
                    "vec.nth: unexpected native type in vec element".to_string(),
                ))
            }
        };
        let boxed = machine.native_value(view, native)?;
        let value = machine.data_value(view, box_tag, &[boxed])?;
        machine.set_result(value)
    }
}

impl CallGlobal2 for VecNth {}

/// VEC.SLICE — return a sub-vec `[from, to)`.
///
/// Indices past the end are clamped to the vec length; a negative or
/// non-integer index is an error.
pub struct VecSlice;

impl StgIntrinsic for VecSlice {
    fn name(&self) -> &str {
        "VEC.SLICE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // As for VEC.NTH: locate the error at the call site.
        let smid = Smid::default();
        let native_from = resolve_native_unboxing(machine, view, &args[0])?;
        let native_to = resolve_native_unboxing(machine, view, &args[1])?;
        let from = vec_index("vec.slice", "from", &native_from, smid)?;
        let to = vec_index("vec.slice", "to", &native_to, smid)?;
        let v = vec_arg(machine, view, &args[2])?;
        let elements = v.slice(from, to).to_vec();
        machine_return_vec(machine, view, &elements)
    }
}

impl CallGlobal3 for VecSlice {}

/// VEC.SAMPLE — pick k random elements without replacement using
/// random floats from an external source (the random stream).
///
/// Args: [floats_list, vec] where floats_list is a list of k floats
/// in [0,1). Uses partial Fisher-Yates on an index array, converting
/// each float to an index in the remaining range.
pub struct VecSample;

impl StgIntrinsic for VecSample {
    fn name(&self) -> &str {
        "VEC.SAMPLE"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use super::syntax::dsl::local;
        let bif_index: u8 = self.index().try_into().unwrap();
        // lambda args: [floats, vec]
        // Force vec first (WHNF), then deep-force floats via SeqNumList
        lambda(
            2,
            force(
                local(1), // force vec
                // [forced_vec] [floats, vec]
                force(
                    SeqNumList.global(lref(1)),
                    // [forced_floats] [forced_vec] [floats, vec]
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
        // args: [forced_floats, forced_vec]
        let floats = collect_num_list(machine, view, args[0].clone())?;
        let v = vec_arg(machine, view, &args[1])?;
        let len = v.len();
        let count = floats.len().min(len);
        let mut indices: Vec<usize> = (0..len).collect();
        for (i, &f) in floats.iter().take(count).enumerate() {
            let remaining = len - i;
            let j = i + (((f * remaining as f64) as usize) % remaining);
            indices.swap(i, j);
        }
        let sampled: Vec<Primitive> = indices[..count]
            .iter()
            .filter_map(|&idx| v.get(idx))
            .collect();
        machine_return_vec(machine, view, &sampled)
    }
}

impl CallGlobal2 for VecSample {}

/// VEC.SHUFFLE — return a new vec with elements in random order using
/// random floats from an external source (the random stream).
pub struct VecShuffle;

impl StgIntrinsic for VecShuffle {
    fn name(&self) -> &str {
        "VEC.SHUFFLE"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use super::syntax::dsl::local;
        let bif_index: u8 = self.index().try_into().unwrap();
        lambda(
            2,
            force(
                local(1),
                force(
                    SeqNumList.global(lref(1)),
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
        let floats = collect_num_list(machine, view, args[0].clone())?;
        let v = vec_arg(machine, view, &args[1])?;
        let mut elements: Vec<Primitive> = v.elements().to_vec();
        let len = elements.len();
        // Full Fisher-Yates shuffle
        for (step, &f) in floats.iter().enumerate() {
            let i = len - 1 - step;
            if i == 0 {
                break;
            }
            let j = ((f * (i + 1) as f64) as usize) % (i + 1);
            elements.swap(i, j);
        }
        machine_return_vec(machine, view, &elements)
    }
}

impl CallGlobal2 for VecShuffle {}

/// VEC.TO_LIST — convert a vec to a cons-list.
///
/// Elements are wrapped in the appropriate box constructors
/// (BoxedNumber, BoxedString, BoxedSymbol) matching the list format.
pub struct VecToList;

impl StgIntrinsic for VecToList {
    fn name(&self) -> &str {
        "VEC.TO_LIST"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let elements: Vec<Primitive> = vec_arg(machine, view, &args[0])?.elements().to_vec();

        // Box each element and assemble the list via the neutral ABI.
        let mut items = Vec::with_capacity(elements.len());
        for prim in elements {
            let native = set_primitive_to_native(&prim);
            let box_tag = match &native {
                Native::Num(_) => DataConstructor::BoxedNumber.tag(),
                Native::Str(_) => DataConstructor::BoxedString.tag(),
                Native::Sym(_) => DataConstructor::BoxedSymbol.tag(),
                _ => {
                    return Err(ExecutionError::Panic(
                        Smid::default(),
                        "unexpected native type in vec".to_string(),
                    ))
                }
            };
            let boxed = machine.native_value(view, native)?;
            items.push(machine.data_value(view, box_tag, &[boxed])?);
        }

        machine.return_closure_list(view, items)
    }
}

impl CallGlobal1 for VecToList {}
