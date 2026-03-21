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
        syntax::{HeapSyn, Native, Ref},
        vec::HeapVec,
    },
    stg::tags::DataConstructor,
};

use crate::eval::machine::env::SynClosure;

use super::{
    prng::{seed_to_u64, splitmix64},
    support::{
        machine_return_num, machine_return_vec, native_to_set_primitive, resolve_native_unboxing,
        set_primitive_to_native, vec_arg,
    },
};

use crate::eval::memory::{
    alloc::ScopedAllocator, array::Array, syntax::LambdaForm, syntax::StgBuilder,
};

/// Resolve a ref from within a cons closure context.
///
/// Compiled eucalypt lists use `Ref::G` for the nil tail and `Ref::L`
/// for other elements. `navigate_local` only handles `Ref::L`, so this
/// helper dispatches correctly for all ref types.
fn resolve_list_ref(
    closure: &SynClosure,
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    r: &Ref,
) -> Result<SynClosure, ExecutionError> {
    use crate::eval::memory::alloc::ScopedAllocator;
    match r {
        Ref::L(_) => Ok(closure.navigate_local(&view, r.clone())),
        Ref::G(i) => machine.nav(view).global(*i),
        Ref::V(n) => {
            let ptr = view
                .alloc(HeapSyn::Atom {
                    evaluand: Ref::V(n.clone()),
                })?
                .as_ptr();
            Ok(SynClosure::new(ptr, machine.root_env()))
        }
    }
}

/// Extract a primitive from a closure representing a list element.
///
/// Handles both raw atom closures (`Atom { Ref::V(native) }`) and
/// boxed constructor closures (`Cons { BoxedNumber | BoxedString | … }`).
fn extract_primitive(
    item_closure: &SynClosure,
    _machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
) -> Result<Primitive, ExecutionError> {
    let code = view.scoped(item_closure.code());
    match &*code {
        HeapSyn::Atom { evaluand } => {
            let native = item_closure.navigate_local_native(&view, evaluand.clone());
            native_to_set_primitive(view, &native)
        }
        HeapSyn::Cons { args: cargs, .. } => {
            // Handle boxed values (BoxedNumber, BoxedString, BoxedSymbol, BoxedZdt)
            let inner_ref = cargs.get(0).ok_or_else(|| {
                ExecutionError::Panic(Smid::default(), "empty boxed value in vec".to_string())
            })?;
            let native = item_closure.navigate_local_native(&view, inner_ref.clone());
            native_to_set_primitive(view, &native)
        }
        _ => Err(ExecutionError::Panic(
            Smid::default(),
            "non-primitive value in vec construction".to_string(),
        )),
    }
}

/// VEC.OF — convert a list of primitives to a vec.
pub struct VecOf;

impl StgIntrinsic for VecOf {
    fn name(&self) -> &str {
        "VEC.OF"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // Resolve the list closure using the machine navigator (handles L, G, V).
        let mut current = machine.nav(view).resolve(&args[0])?;
        let mut primitives = Vec::new();

        loop {
            let code = view.scoped(current.code());
            match &*code {
                HeapSyn::Cons {
                    tag,
                    args: cons_args,
                } => match (*tag).try_into() {
                    Ok(DataConstructor::ListCons) => {
                        let h_ref = cons_args
                            .get(0)
                            .ok_or_else(|| {
                                ExecutionError::Panic(
                                    Smid::default(),
                                    "malformed cons cell".to_string(),
                                )
                            })?
                            .clone();
                        let t_ref = cons_args
                            .get(1)
                            .ok_or_else(|| {
                                ExecutionError::Panic(
                                    Smid::default(),
                                    "malformed cons cell".to_string(),
                                )
                            })?
                            .clone();
                        let head = resolve_list_ref(&current, machine, view, &h_ref)?;
                        primitives.push(extract_primitive(&head, machine, view)?);
                        current = resolve_list_ref(&current, machine, view, &t_ref)?;
                    }
                    Ok(DataConstructor::ListNil) => break,
                    _ => {
                        return Err(ExecutionError::Panic(
                            Smid::default(),
                            "expected list in vec.of".to_string(),
                        ))
                    }
                },
                _ => {
                    return Err(ExecutionError::Panic(
                        Smid::default(),
                        "expected list data in vec.of".to_string(),
                    ))
                }
            }
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
            Native::Num(num) => num.as_u64().unwrap_or(0) as usize,
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
        machine.set_closure(crate::eval::machine::env::SynClosure::new(
            view.alloc(HeapSyn::Cons {
                tag: box_tag,
                args: Array::from_slice(&view, &[Ref::V(native)]),
            })?
            .as_ptr(),
            machine.root_env(),
        ))
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
            Native::Num(n) => n.as_u64().unwrap_or(0) as usize,
            _ => {
                return Err(ExecutionError::Panic(
                    Smid::default(),
                    "vec.slice: from must be a number".to_string(),
                ))
            }
        };
        let to = match &native_to {
            Native::Num(n) => n.as_u64().unwrap_or(0) as usize,
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

/// VEC.SAMPLE — pick `n` random elements without replacement.
///
/// Uses a partial Fisher-Yates shuffle on an index array seeded by
/// SplitMix64. Returns a new vec of the sampled elements.
pub struct VecSample;

impl StgIntrinsic for VecSample {
    fn name(&self) -> &str {
        "VEC.SAMPLE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args: [n, seed, vec]
        let native_n = resolve_native_unboxing(machine, view, &args[0])?;
        let native_seed = resolve_native_unboxing(machine, view, &args[1])?;
        let n = match &native_n {
            Native::Num(num) => num.as_u64().unwrap_or(0) as usize,
            _ => {
                return Err(ExecutionError::Panic(
                    Smid::default(),
                    "vec.sample: count must be a number".to_string(),
                ))
            }
        };
        let seed_num = match &native_seed {
            Native::Num(num) => num.clone(),
            _ => {
                return Err(ExecutionError::Panic(
                    Smid::default(),
                    "vec.sample: seed must be a number".to_string(),
                ))
            }
        };
        let v = vec_arg(machine, view, &args[2])?;
        let len = v.len();
        let count = n.min(len);
        // Partial Fisher-Yates on an index vector
        let mut indices: Vec<usize> = (0..len).collect();
        let mut state = seed_to_u64(&seed_num);
        for i in 0..count {
            let (next_state, z) = splitmix64(state);
            state = next_state;
            let j = i + (z as usize % (len - i));
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

impl CallGlobal3 for VecSample {}

/// VEC.SHUFFLE — return a new vec with elements in random order.
///
/// Uses a full Fisher-Yates shuffle seeded by SplitMix64.
pub struct VecShuffle;

impl StgIntrinsic for VecShuffle {
    fn name(&self) -> &str {
        "VEC.SHUFFLE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args: [seed, vec]
        let native_seed = resolve_native_unboxing(machine, view, &args[0])?;
        let seed_num = match &native_seed {
            Native::Num(num) => num.clone(),
            _ => {
                return Err(ExecutionError::Panic(
                    Smid::default(),
                    "vec.shuffle: seed must be a number".to_string(),
                ))
            }
        };
        let v = vec_arg(machine, view, &args[1])?;
        let mut elements: Vec<Primitive> = v.elements().to_vec();
        let len = elements.len();
        let mut state = seed_to_u64(&seed_num);
        // Full Fisher-Yates shuffle
        for i in (1..len).rev() {
            let (next_state, z) = splitmix64(state);
            state = next_state;
            let j = z as usize % (i + 1);
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

        // Build a list of boxed values in reverse (same pattern as SetToList)
        let mut bindings = vec![LambdaForm::value(
            view.alloc(HeapSyn::Cons {
                tag: DataConstructor::ListNil.tag(),
                args: Array::default(),
            })?
            .as_ptr(),
        )];

        for prim in elements.into_iter().rev() {
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
            bindings.push(LambdaForm::value(
                view.alloc(HeapSyn::Cons {
                    tag: box_tag,
                    args: Array::from_slice(&view, &[Ref::V(native)]),
                })?
                .as_ptr(),
            ));
            let len = bindings.len();
            bindings.push(LambdaForm::value(
                view.alloc(HeapSyn::Cons {
                    tag: DataConstructor::ListCons.tag(),
                    args: Array::from_slice(&view, &[Ref::L(len - 1), Ref::L(len - 2)]),
                })?
                .as_ptr(),
            ));
        }

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
}

impl CallGlobal1 for VecToList {}
