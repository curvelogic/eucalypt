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
        vec::HeapVec,
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
            primitives.push(native_to_set_primitive(smid, view, &native)?);
        }

        machine_return_vec(
            machine,
            view,
            HeapVec::from_primitives(primitives.into_iter()),
        )
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
/// Returns an error if the index is out of bounds.
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
        let native_n = resolve_native_unboxing(machine, view, &args[0])?;
        let n = match &native_n {
            Native::Num(num) => num
                .as_u64()
                .map(|v| v as usize)
                .or_else(|| num.as_f64().map(|v| v as usize))
                .unwrap_or(0),
            _ => {
                return Err(ExecutionError::Panic(
                    Smid::default(),
                    "vec.nth: index must be a number".to_string(),
                ))
            }
        };
        let v = vec_arg(machine, view, &args[1])?;
        let prim = v.get(n).ok_or_else(|| {
            ExecutionError::Panic(
                Smid::default(),
                format!("vec.nth: index {} out of bounds (len {})", n, v.len()),
            )
        })?;
        let native = set_primitive_to_native(machine, view, prim)?;
        // Return a boxed value (BoxedNumber/BoxedString/BoxedSymbol) so that the
        // result is comparable via the EQ wrapper's Cons branches, matching the
        // format of list elements and the output of other intrinsics.
        let box_tag = match &native {
            Native::Num(_) => DataConstructor::BoxedNumber.tag(),
            Native::Str(_) => DataConstructor::BoxedString.tag(),
            Native::Sym(_) => DataConstructor::BoxedSymbol.tag(),
            _ => {
                return Err(ExecutionError::Panic(
                    Smid::default(),
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
/// Indices are clamped to the vec length.
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
        let native_from = resolve_native_unboxing(machine, view, &args[0])?;
        let native_to = resolve_native_unboxing(machine, view, &args[1])?;
        let from = match &native_from {
            Native::Num(n) => n
                .as_u64()
                .map(|v| v as usize)
                .or_else(|| n.as_f64().map(|v| v as usize))
                .unwrap_or(0),
            _ => {
                return Err(ExecutionError::Panic(
                    Smid::default(),
                    "vec.slice: from must be a number".to_string(),
                ))
            }
        };
        let to = match &native_to {
            Native::Num(n) => n
                .as_u64()
                .map(|v| v as usize)
                .or_else(|| n.as_f64().map(|v| v as usize))
                .unwrap_or(0),
            _ => {
                return Err(ExecutionError::Panic(
                    Smid::default(),
                    "vec.slice: to must be a number".to_string(),
                ))
            }
        };
        let v = vec_arg(machine, view, &args[2])?;
        machine_return_vec(machine, view, v.slice(from, to))
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
            .cloned()
            .collect();
        machine_return_vec(machine, view, HeapVec::from_primitives(sampled.into_iter()))
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
        machine_return_vec(
            machine,
            view,
            HeapVec::from_primitives(elements.into_iter()),
        )
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
            let native = set_primitive_to_native(machine, view, &prim)?;
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
