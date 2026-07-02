//! Support functions for writing intrinsics

use std::convert::TryInto;

use chrono::{DateTime, FixedOffset};
use indexmap::IndexMap;
use serde_json::Number;

use crate::eval::{
    error::ExecutionError,
    machine::{env::SynClosure, env_builder::EnvBuilder, intrinsic::*},
    memory::{
        alloc::{ScopedAllocator, ScopedPtr},
        array::Array,
        mutator::MutatorHeapView,
        ndarray::HeapNdArray,
        set::HeapSet,
        syntax::StgBuilder,
        vec::HeapVec,
    },
    stg::tags::DataConstructor,
};
use crate::{
    common::sourcemap::Smid,
    eval::{memory::syntax::*, types::IntrinsicType},
};

/// Format a native value for inclusion in error messages.
///
/// Returns a short human-readable description of the value, or `None` for
/// types that are not easily represented as a short string (arrays, vecs, etc.).
fn describe_native(
    native: &Native,
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
) -> Option<String> {
    match native {
        Native::Num(n) => Some(n.to_string()),
        Native::Str(s) => {
            let scoped = view.scoped(*s);
            let text = (*scoped).as_str();
            const MAX_LEN: usize = 40;
            if text.chars().count() > MAX_LEN {
                let truncated: String = text.chars().take(MAX_LEN).collect();
                Some(format!("\"{}…\"", truncated))
            } else {
                Some(format!("\"{text}\""))
            }
        }
        Native::Sym(id) => {
            let name = machine.symbol_pool().resolve(*id);
            Some(format!(":{name}"))
        }
        Native::Zdt(dt) => Some(dt.to_rfc3339()),
        _ => None,
    }
}

/// Map a resolved native value to its intrinsic type for error reporting
fn native_type(native: &Native) -> IntrinsicType {
    match native {
        Native::Num(_) => IntrinsicType::Number,
        Native::Str(_) => IntrinsicType::String,
        Native::Sym(_) => IntrinsicType::Symbol,
        Native::Zdt(_) => IntrinsicType::ZonedDateTime,
        Native::NdArray(_) => IntrinsicType::Array,
        Native::Vec(_) => IntrinsicType::Vec,
        Native::Set(_) => IntrinsicType::Set,
        Native::Index(_) | Native::Prng(_) | Native::Producer(_) => IntrinsicType::Unknown,
    }
}

/// Attempt to classify a ref as a data constructor tag, for error reporting.
///
/// Used when `resolve_native` fails to provide a `NoBranchForDataTag` error
/// with the right tag so that contextual notes (e.g. "blocks cannot be used
/// in arithmetic") are generated correctly.
fn cons_tag_of(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Option<u8> {
    let nav = machine.nav(view);
    let closure = nav.resolve(arg).ok()?;
    let code = view.scoped(closure.code());
    match &*code {
        HeapSyn::Cons { tag, .. } => Some(*tag),
        _ => None,
    }
}

/// Helper for intrinsics to access a numeric arg.
///
/// When the argument is a data constructor (e.g. a block or list) rather than
/// a native numeric atom, produces a `NoBranchForDataTag` error so that the
/// contextual error notes (e.g. "blocks cannot be used in arithmetic",
/// "to concatenate two lists, use 'append'") are generated correctly.
pub fn num_arg(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<Number, ExecutionError> {
    match machine.resolve_native(view, arg) {
        Ok(Native::Num(n)) => Ok(n),
        Ok(native) => Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            Box::new(IntrinsicType::Number),
            Box::new(native_type(&native)),
            describe_native(&native, machine, view),
        )),
        Err(_) => {
            // resolve_native failed — likely a Cons (block/list). Inspect the
            // tag so the error message includes useful context.
            let tag = cons_tag_of(machine, view, arg).unwrap_or(DataConstructor::Block.tag());
            Err(ExecutionError::NoBranchForDataTag(
                machine.annotation(),
                tag,
                vec![DataConstructor::BoxedNumber.tag()],
            ))
        }
    }
}

/// Helper for intrinsics to access a str arg
pub fn str_arg(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<String, ExecutionError> {
    let native = machine.resolve_native(view, arg)?;
    if let Native::Str(s) = native {
        Ok((*view.scoped(s)).as_str().to_string())
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            Box::new(IntrinsicType::String),
            Box::new(native_type(&native)),
            describe_native(&native, machine, view),
        ))
    }
}

/// A string borrowed directly from the heap with its block pinned.
///
/// The `PinGuard` keeps the underlying heap block from being evacuated
/// during GC, so the `&str` remains valid even across allocations.
/// Derefs to `&str` for ergonomic use.
pub struct PinnedString {
    data: *const str,
    _guard: crate::eval::memory::mutator::PinGuard,
}

impl std::ops::Deref for PinnedString {
    type Target = str;
    fn deref(&self) -> &str {
        // SAFETY: The PinGuard keeps the heap block alive and unmoved.
        // The data pointer was obtained from a valid HeapString on the
        // heap, and the block cannot be evacuated while pinned.
        unsafe { &*self.data }
    }
}

impl AsRef<str> for PinnedString {
    fn as_ref(&self) -> &str {
        self
    }
}

/// Helper for intrinsics to borrow a str arg directly from the heap.
///
/// Returns a `PinnedString` that derefs to `&str` without cloning.
/// The underlying heap block is pinned for the lifetime of the
/// returned value, preventing GC evacuation.
pub fn str_arg_ref(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<PinnedString, ExecutionError> {
    let native = machine.resolve_native(view, arg)?;
    if let Native::Str(s) = native {
        let guard = view.pin(s);
        // SAFETY: The scoped pointer dereferences the HeapString on the
        // heap. We extract a raw pointer to the str slice, which remains
        // valid because the PinGuard prevents the block from moving.
        let str_ptr: *const str = (*view.scoped(s)).as_str() as *const str;
        Ok(PinnedString {
            data: str_ptr,
            _guard: guard,
        })
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            Box::new(IntrinsicType::String),
            Box::new(native_type(&native)),
            describe_native(&native, machine, view),
        ))
    }
}

/// Helper for intrinsics to access a sym arg
pub fn sym_arg(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<String, ExecutionError> {
    let native = machine.resolve_native(view, arg)?;
    if let Native::Sym(id) = native {
        Ok(machine.symbol_pool().resolve(id).to_string())
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            Box::new(IntrinsicType::Symbol),
            Box::new(native_type(&native)),
            describe_native(&native, machine, view),
        ))
    }
}

/// Helper for intrinsics to access a zoned date time arg
pub fn zdt_arg(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<DateTime<FixedOffset>, ExecutionError> {
    let native = machine.resolve_native(view, arg)?;
    if let Native::Zdt(dt) = native {
        Ok(dt)
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            Box::new(IntrinsicType::ZonedDateTime),
            Box::new(native_type(&native)),
            describe_native(&native, machine, view),
        ))
    }
}

/// Collect a (forced) list argument's elements into a `Vec<AbiClosure>`.
///
/// Engine-neutral: walks the `ListCons`/`ListNil` spine via the neutral
/// `data_tag`/`data_field` ABI, so it serves both the HeapSyn and bytecode
/// engines. Eager rather than lazy, but the caller has already forced the
/// spine so the result is identical (and eucalypt lists are finite data).
pub fn data_list_arg(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: Ref,
) -> Result<Vec<AbiClosure>, ExecutionError> {
    let smid = machine.annotation();
    let mut out = Vec::new();
    let mut current = machine.resolve_closure(view, &arg)?;
    loop {
        match machine.data_tag(view, &current) {
            Some(tag) if tag == DataConstructor::ListCons.tag() => {
                let head = machine.data_field(view, &current, 0).ok_or_else(|| {
                    ExecutionError::Panic(smid, "malformed cons cell".to_string())
                })?;
                out.push(head);
                current = machine.data_field(view, &current, 1).ok_or_else(|| {
                    ExecutionError::Panic(smid, "malformed cons cell".to_string())
                })?;
            }
            Some(tag) if tag == DataConstructor::ListNil.tag() => break,
            _ => {
                return Err(ExecutionError::NotValue(
                    smid,
                    "expected list data".to_string(),
                ))
            }
        }
    }
    Ok(out)
}

/// Collect a (forced, native-string) list argument into a `Vec<String>`.
///
/// Engine-neutral: walks the `ListCons`/`ListNil` spine via the neutral
/// `data_tag`/`data_field`/`field_native` ABI (works on both the HeapSyn and
/// bytecode engines). Eager rather than lazy, but the spine is already forced
/// by the caller so the result is identical.
pub fn str_list_arg(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: Ref,
) -> Result<Vec<String>, ExecutionError> {
    let smid = machine.annotation();
    let mut out = Vec::new();
    let mut current = machine.resolve_closure(view, &arg)?;
    loop {
        match machine.data_tag(view, &current) {
            Some(tag) if tag == DataConstructor::ListCons.tag() => {
                let native = machine.field_native(view, &current, 0).ok_or_else(|| {
                    ExecutionError::NotValue(smid, "expected string list data".to_string())
                })?;
                match native {
                    Native::Str(s) => out.push((*view.scoped(s)).as_str().to_string()),
                    other => {
                        return Err(ExecutionError::TypeMismatch(
                            smid,
                            Box::new(IntrinsicType::String),
                            Box::new(native_type(&other)),
                            None,
                        ))
                    }
                }
                current = machine.data_field(view, &current, 1).ok_or_else(|| {
                    ExecutionError::Panic(smid, "malformed cons cell".to_string())
                })?;
            }
            Some(tag) if tag == DataConstructor::ListNil.tag() => break,
            _ => {
                return Err(ExecutionError::NotValue(
                    smid,
                    "expected string list data".to_string(),
                ))
            }
        }
    }
    Ok(out)
}

/// What to return when the return should be ignored
pub fn machine_return_unit(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
) -> Result<(), ExecutionError> {
    machine.return_unit(view)
}

/// Return number from intrinsic
pub fn machine_return_num(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    n: Number,
) -> Result<(), ExecutionError> {
    machine.return_native(view, Native::Num(n))
}

/// Return string from intrinsic
pub fn machine_return_str(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    s: String,
) -> Result<(), ExecutionError> {
    let native = match view.str_ref(s)? {
        Ref::V(n) => n,
        _ => unreachable!("str_ref always yields a V ref"),
    };
    machine.return_native(view, native)
}

/// Return symbol from intrinsic
pub fn machine_return_sym(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    s: String,
) -> Result<(), ExecutionError> {
    let native = match view.sym_ref(machine.symbol_pool_mut(), s)? {
        Ref::V(n) => n,
        _ => unreachable!("sym_ref always yields a V ref"),
    };
    machine.return_native(view, native)
}

/// Return zoned date time from intrinsic
pub fn machine_return_zdt(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    zdt: DateTime<FixedOffset>,
) -> Result<(), ExecutionError> {
    machine.return_native(view, Native::Zdt(zdt))
}

/// Return a boxed number from an intrinsic whose wrapper does not auto-box
/// the result.
///
/// The default wrapper for `num -> num -> num` intrinsics wraps the result
/// in a `BoxedNumber` data constructor automatically via a `let_` step. When
/// an intrinsic overrides `wrapper` with a custom form (e.g.
/// `arithmetic_wrapper`) that does not include that boxing step, the `execute`
/// method must call this function so that the pipeline receives a proper
/// `BoxedNumber` constructor.
pub fn machine_return_boxed_num(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    n: Number,
) -> Result<(), ExecutionError> {
    machine.return_boxed_num(view, n)
}

/// Resolve a native value from a Ref, looking through boxed constructors
/// if necessary. This handles both raw `Atom(Ref::V(native))` chains and
/// boxed values like `Cons { tag: BoxedNumber, args: [Ref::V(native)] }`.
pub fn resolve_native_unboxing(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<Native, ExecutionError> {
    // First try the standard resolve path
    let nav = machine.nav(view);
    if let Ok(native) = nav.resolve_native(arg) {
        return Ok(native);
    }

    // If that failed, the value may be a boxed constructor — resolve
    // the closure and check for a Cons with a box tag.
    let closure = nav.resolve(arg)?;
    let code = view.scoped(closure.code());
    match &*code {
        HeapSyn::Cons { tag, args } => {
            let dc: Result<DataConstructor, _> = (*tag).try_into();
            match dc {
                Ok(DataConstructor::BoxedNumber)
                | Ok(DataConstructor::BoxedString)
                | Ok(DataConstructor::BoxedSymbol)
                | Ok(DataConstructor::BoxedZdt)
                | Ok(DataConstructor::BoxedTypeData) => {
                    let inner_ref = args.get(0).ok_or_else(|| {
                        ExecutionError::Panic(Smid::default(), "empty boxed value".to_string())
                    })?;
                    let native = closure.navigate_local_native(&view, inner_ref);
                    Ok(native)
                }
                _ => Err(ExecutionError::NotValue(
                    machine.annotation(),
                    "non-boxed constructor".to_string(),
                )),
            }
        }
        _ => Err(ExecutionError::NotValue(
            machine.annotation(),
            "expected native value".to_string(),
        )),
    }
}

/// Convert a Native value to a set Primitive
pub fn native_to_set_primitive(
    smid: Smid,
    view: MutatorHeapView,
    native: &Native,
) -> Result<crate::eval::memory::set::Primitive, ExecutionError> {
    use crate::eval::memory::set::Primitive as SetPrim;
    match native {
        Native::Num(n) => Ok(SetPrim::from_number(n)),
        Native::Str(s) => Ok(SetPrim::Str(view.scoped(*s).as_str().to_string())),
        Native::Sym(id) => Ok(SetPrim::Sym(*id)),
        _ => Err(ExecutionError::TypeMismatch(
            smid,
            Box::new(crate::eval::types::IntrinsicType::Set),
            Box::new(native_type(native)),
            None,
        )),
    }
}

/// Convert a set Primitive back to a Native value
pub fn set_primitive_to_native(
    _machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    prim: &crate::eval::memory::set::Primitive,
) -> Result<Native, ExecutionError> {
    use crate::eval::memory::set::Primitive as SetPrim;
    match prim {
        SetPrim::Num(n) => {
            let num = serde_json::Number::from_f64(n.into_inner())
                .unwrap_or_else(|| serde_json::Number::from(0));
            Ok(Native::Num(num))
        }
        SetPrim::Str(s) => {
            let ptr = view.str(s.as_str())?.as_ptr();
            Ok(Native::Str(ptr))
        }
        SetPrim::Sym(id) => Ok(Native::Sym(*id)),
    }
}

/// Helper for intrinsics to access a set arg
pub fn set_arg<'guard>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'guard>,
    arg: &Ref,
) -> Result<ScopedPtr<'guard, HeapSet>, ExecutionError> {
    let native = resolve_native_unboxing(machine, view, arg)?;
    if let Native::Set(ptr) = native {
        Ok(view.scoped(ptr))
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            Box::new(crate::eval::types::IntrinsicType::Set),
            Box::new(native_type(&native)),
            describe_native(&native, machine, view),
        ))
    }
}

/// Return a set from intrinsic
pub fn machine_return_set(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    set: HeapSet,
) -> Result<(), ExecutionError> {
    let ptr = view.alloc(set)?.as_ptr();
    machine.return_native(view, Native::Set(ptr))
}

/// Helper for intrinsics to access an ndarray arg
pub fn ndarray_arg<'guard>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'guard>,
    arg: &Ref,
) -> Result<ScopedPtr<'guard, HeapNdArray>, ExecutionError> {
    let native = machine.resolve_native(view, arg)?;
    if let Native::NdArray(ptr) = native {
        Ok(view.scoped(ptr))
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            Box::new(IntrinsicType::Array),
            Box::new(native_type(&native)),
            None,
        ))
    }
}

/// Return an ndarray from intrinsic
pub fn machine_return_ndarray(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    arr: HeapNdArray,
) -> Result<(), ExecutionError> {
    let ptr = view.alloc(arr)?.as_ptr();
    machine.return_native(view, Native::NdArray(ptr))
}

/// Helper for intrinsics to access a vec arg.
pub fn vec_arg<'guard>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'guard>,
    arg: &Ref,
) -> Result<ScopedPtr<'guard, HeapVec>, ExecutionError> {
    let native = machine.resolve_native(view, arg)?;
    if let Native::Vec(ptr) = native {
        Ok(view.scoped(ptr))
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            Box::new(IntrinsicType::Vec),
            Box::new(native_type(&native)),
            None,
        ))
    }
}

/// Return a vec from an intrinsic.
pub fn machine_return_vec(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    vec: HeapVec,
) -> Result<(), ExecutionError> {
    let ptr = view.alloc(vec)?.as_ptr();
    machine.return_native(view, Native::Vec(ptr))
}

/// Return boolean from intrinsic
///
/// Reuses the pre-allocated TRUE/FALSE global closures rather than
/// allocating a fresh Cons node on each call, saving one heap
/// allocation per boolean-returning intrinsic invocation.
pub fn machine_return_bool(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    b: bool,
) -> Result<(), ExecutionError> {
    machine.return_bool(view, b)
}

/// Extract f64 values from a concrete (forced) number list.
///
/// Works with both raw `Atom(Ref::V(Native::Num(...)))` items and
/// boxed `Cons { tag: BoxedNumber, args: [...] }` items, as produced
/// by `SeqNumList`.
pub fn collect_num_list(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    list_ref: Ref,
) -> Result<Vec<f64>, ExecutionError> {
    let smid = machine.annotation();
    let items = data_list_arg(machine, view, list_ref)?;
    let mut numbers = Vec::new();
    for item in &items {
        match machine.value_native(view, item) {
            Some(Native::Num(n)) => numbers.push(n.as_f64().unwrap_or(0.0)),
            Some(_) => {
                return Err(ExecutionError::NotValue(
                    smid,
                    "non-numeric value in number list".to_string(),
                ))
            }
            None => {
                return Err(ExecutionError::NotValue(
                    smid,
                    "unexpected value in number list".to_string(),
                ))
            }
        }
    }
    Ok(numbers)
}

/// Build a heap cons-list of boxed numbers from a slice of f64, returning
/// the raw heap pointer to the head of the list.
///
/// Used internally to construct inner lists for `machine_return_num_list_of_lists`.
fn build_num_list_ptr(
    view: MutatorHeapView<'_>,
    nums: &[f64],
) -> Result<RefPtr<HeapSyn>, ExecutionError> {
    let mut bindings: Vec<LambdaForm> = vec![LambdaForm::value(view.nil()?.as_ptr())];
    for &item in nums.iter().rev() {
        let n = Number::from_f64(item).unwrap_or_else(|| Number::from(0));
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
    }
    let list_index = bindings.len() - 1;
    Ok(view
        .letrec(
            Array::from_slice(&view, &bindings),
            view.atom(Ref::L(list_index))?,
        )?
        .as_ptr())
}

/// Return a list of number-lists from an intrinsic.
///
/// Used for `ARRAY.INDICES` which returns a list of coordinate lists.
pub fn machine_return_num_list_of_lists(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    lists: Vec<Vec<f64>>,
) -> Result<(), ExecutionError> {
    // Build each inner list as a heap object and collect raw pointers.
    let inner_ptrs: Vec<RefPtr<HeapSyn>> = lists
        .iter()
        .map(|inner| build_num_list_ptr(view, inner))
        .collect::<Result<Vec<_>, _>>()?;

    // Build the outer cons list from these pointers.
    let mut bindings: Vec<LambdaForm> = vec![LambdaForm::value(view.nil()?.as_ptr())];
    for ptr in inner_ptrs.into_iter().rev() {
        bindings.push(LambdaForm::value(ptr));
        let len = bindings.len();
        bindings.push(LambdaForm::value(
            view.data(
                DataConstructor::ListCons.tag(),
                Array::from_slice(&view, &[Ref::L(len - 1), Ref::L(len - 2)]),
            )?
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
    machine.set_closure(SynClosure::new(syn, machine.root_env()))
}

/// Return a number list from intrinsic, following the same pattern
/// as `machine_return_str_list` but for boxed numbers.
pub fn machine_return_num_list(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    list: Vec<f64>,
) -> Result<(), ExecutionError> {
    let mut items = Vec::with_capacity(list.len());
    for item in list {
        let n = Number::from_f64(item).unwrap_or_else(|| Number::from(0));
        let native = machine.native_value(view, Native::Num(n))?;
        items.push(machine.data_value(view, DataConstructor::BoxedNumber.tag(), &[native])?);
    }
    machine.return_closure_list(view, items)
}

/// Return a string list from an iterator, streaming strings directly
/// to the heap without an intermediate `Vec<String>`.
///
/// Each string from the iterator is allocated on the heap immediately,
/// producing a small `Ref` pointer. The cons list is then built in
/// reverse from these refs, the same as `machine_return_str_list`.
pub fn machine_return_str_iter(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    iter: impl Iterator<Item = String>,
) -> Result<(), ExecutionError> {
    // Allocate each string on the heap as it arrives from the iterator,
    // collecting only the small Ref pointers (not full Strings).
    let refs: Vec<Ref> = iter
        .map(|s| view.str_ref(s))
        .collect::<Result<Vec<_>, _>>()?;

    // Build the cons list in reverse from the heap refs
    let mut bindings = vec![LambdaForm::value(view.nil()?.as_ptr())];
    for str_ref in refs.into_iter().rev() {
        bindings.push(LambdaForm::value(
            view.data(
                DataConstructor::BoxedString.tag(),
                Array::from_slice(&view, &[str_ref]),
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
    }
    let list_index = bindings.len() - 1;
    let syn = view
        .letrec(
            Array::from_slice(&view, &bindings),
            view.atom(Ref::L(list_index))?,
        )?
        .as_ptr();
    machine.set_closure(SynClosure::new(syn, machine.root_env()))
}

/// Return a string list from intrinsic
pub fn machine_return_str_list(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    list: Vec<String>,
) -> Result<(), ExecutionError> {
    let mut items = Vec::with_capacity(list.len());
    for item in list {
        // Match `view.str_ref`: intern/allocate the string as a native.
        let Ref::V(native) = view.str_ref(item)? else {
            unreachable!("str_ref yields a value ref")
        };
        let boxed = machine.native_value(view, native)?;
        items.push(machine.data_value(view, DataConstructor::BoxedString.tag(), &[boxed])?);
    }
    machine.return_closure_list(view, items)
}

/// Return a list of closures from intrinsic
pub fn machine_return_closure_list(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    list: Vec<SynClosure>,
) -> Result<(), ExecutionError> {
    // Delegate to the neutral list builder (HeapSyn items). Bytecode-native
    // callers should build `AbiClosure`s and call `return_closure_list`
    // directly.
    machine.return_closure_list(view, list.into_iter().map(AbiClosure::Heap).collect())
}

/// Return a list of closures from intrinsic
pub fn machine_return_block_pair_closure_list(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    block: IndexMap<String, SynClosure>,
) -> Result<(), ExecutionError> {
    // env of values
    let values: Vec<_> = block.values().cloned().collect();
    let value_frame = view.from_closures(
        values.iter().cloned(),
        values.len(),
        machine.env(view),
        Smid::default(),
    )?;
    let len = block.len();

    // env of pairs
    let mut pairs = vec![];
    for (i, k) in block.keys().enumerate() {
        pairs.push(SynClosure::new(
            view.data(
                DataConstructor::BlockPair.tag(),
                Array::from_slice(
                    &view,
                    &[view.sym_ref(machine.symbol_pool_mut(), k)?, Ref::L(i)],
                ),
            )?
            .as_ptr(),
            value_frame,
        ));
    }
    let pair_frame = view.from_closures(
        pairs.iter().cloned(),
        pairs.len(),
        value_frame,
        Smid::default(),
    )?;

    // env of links [lnull, l0, l1 ..] [p0 p1 p2...] [v0 v1 ..]
    let mut bindings = vec![LambdaForm::value(view.nil()?.as_ptr())];
    for i in (0..len + 1).rev() {
        bindings.push(LambdaForm::value(
            view.data(
                DataConstructor::ListCons.tag(),
                Array::from_slice(&view, &[Ref::L(len + i + 1), Ref::L(len - i)]),
            )?
            .as_ptr(),
        ));
    }

    let syn = view
        .letrec(Array::from_slice(&view, &bindings), view.atom(Ref::L(len))?)?
        .as_ptr();
    machine.set_closure(SynClosure::new(syn, pair_frame))
}

pub mod call {
    pub mod bif {
        use std::rc::Rc;

        use crate::eval::{
            intrinsics,
            stg::syntax::{dsl::app_bif, Ref, StgSyn},
        };

        /// A call to a STG intrinsic function
        fn call_bif(name: &str, args: &[Ref]) -> Rc<StgSyn> {
            app_bif(intrinsics::index_u8(name), args.to_vec())
        }

        pub fn eq(x: Ref, y: Ref) -> Rc<StgSyn> {
            call_bif("EQ", &[x, y])
        }

        pub fn emit0() -> Rc<StgSyn> {
            call_bif("EMIT0", &[])
        }

        pub fn emitt() -> Rc<StgSyn> {
            call_bif("EMITT", &[])
        }

        pub fn emitf() -> Rc<StgSyn> {
            call_bif("EMITF", &[])
        }

        pub fn emit_seq_start() -> Rc<StgSyn> {
            call_bif("EMIT[", &[])
        }

        pub fn emit_tag_seq_start(tag: Ref) -> Rc<StgSyn> {
            call_bif("EMITTAG[", &[tag])
        }

        pub fn emit_seq_end() -> Rc<StgSyn> {
            call_bif("EMIT]", &[])
        }

        pub fn emit_block_start() -> Rc<StgSyn> {
            call_bif("EMIT{", &[])
        }

        pub fn emit_tag_block_start(tag: Ref) -> Rc<StgSyn> {
            call_bif("EMITTAG{", &[tag])
        }

        pub fn emit_block_end() -> Rc<StgSyn> {
            call_bif("EMIT}", &[])
        }

        pub fn emit_doc_start() -> Rc<StgSyn> {
            call_bif("EMIT<", &[])
        }

        pub fn emit_doc_end() -> Rc<StgSyn> {
            call_bif("EMIT>", &[])
        }

        pub fn emit_native(x: Ref) -> Rc<StgSyn> {
            call_bif("EMITx", &[x])
        }

        pub fn emit_tag_native(tag: Ref, x: Ref) -> Rc<StgSyn> {
            call_bif("EMITTAGx", &[tag, x])
        }

        pub fn saturated(x: Ref) -> Rc<StgSyn> {
            call_bif("SATURATED", &[x])
        }

        pub fn join(list: Ref, sep: Ref) -> Rc<StgSyn> {
            call_bif("JOIN", &[list, sep])
        }

        pub fn str(x: Ref) -> Rc<StgSyn> {
            call_bif("STR", &[x])
        }

        pub fn fmt(x: Ref, y: Ref) -> Rc<StgSyn> {
            call_bif("FMT", &[x, y])
        }

        pub fn panic(x: Ref) -> Rc<StgSyn> {
            call_bif("PANIC", &[x])
        }

        pub fn merge(list: Ref, sep: Ref) -> Rc<StgSyn> {
            call_bif("MERGE", &[list, sep])
        }

        pub fn merge_with(list: Ref, sep: Ref, f: Ref) -> Rc<StgSyn> {
            call_bif("MERGEWITH", &[list, sep, f])
        }
    }
}
