//! Set intrinsics for the eucalypt VM.
//!
//! These intrinsics operate on `Native::Set` values, providing
//! construction, membership, mutation, and set algebra.

use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    machine::intrinsic::{CallGlobal0, CallGlobal1, CallGlobal2, IntrinsicMachine, StgIntrinsic},
    memory::{
        mutator::MutatorHeapView,
        set::HeapSet,
        syntax::{HeapSyn, Native, Ref},
    },
    stg::tags::DataConstructor,
};

use super::support::{
    data_list_arg, machine_return_bool, machine_return_num, machine_return_set,
    native_to_set_primitive, resolve_native_unboxing, set_arg, set_primitive_to_native,
};

/// SET.EMPTY — return an empty set
pub struct SetEmpty;

impl StgIntrinsic for SetEmpty {
    fn name(&self) -> &str {
        "SET.EMPTY"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        machine_return_set(machine, view, HeapSet::empty())
    }
}

impl CallGlobal0 for SetEmpty {}

/// SET.FROM_LIST — convert a list of primitives to a set
pub struct SetFromList;

impl StgIntrinsic for SetFromList {
    fn name(&self) -> &str {
        "SET.FROM_LIST"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let iter = data_list_arg(machine, view, args[0].clone())?;
        let mut primitives = Vec::new();
        for item_closure in iter {
            let code = view.scoped(item_closure.code());
            match &*code {
                HeapSyn::Atom { evaluand } => {
                    let native = item_closure.navigate_local_native(&view, evaluand.clone());
                    primitives.push(native_to_set_primitive(view, &native)?);
                }
                HeapSyn::Cons {
                    tag: _,
                    args: cargs,
                } => {
                    // Handle boxed values (BoxedNumber, BoxedString, BoxedSymbol)
                    let inner_ref = cargs.get(0).ok_or_else(|| {
                        ExecutionError::Panic("empty boxed value in set".to_string())
                    })?;
                    let native = item_closure.navigate_local_native(&view, inner_ref.clone());
                    primitives.push(native_to_set_primitive(view, &native)?);
                }
                _ => {
                    return Err(ExecutionError::Panic(
                        "non-primitive value in set construction".to_string(),
                    ))
                }
            }
        }
        machine_return_set(
            machine,
            view,
            HeapSet::from_primitives(primitives.into_iter()),
        )
    }
}

impl CallGlobal1 for SetFromList {}

/// SET.TO_LIST — convert a set to a sorted list of boxed values
pub struct SetToList;

impl StgIntrinsic for SetToList {
    fn name(&self) -> &str {
        "SET.TO_LIST"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        use crate::eval::memory::{
            alloc::ScopedAllocator, array::Array, syntax::LambdaForm, syntax::StgBuilder,
        };

        let set = set_arg(machine, view, &args[0])?;
        let sorted = set.sorted_elements();

        // Build a list of boxed values in reverse
        let mut bindings = vec![LambdaForm::value(
            view.alloc(HeapSyn::Cons {
                tag: DataConstructor::ListNil.tag(),
                args: Array::default(),
            })?
            .as_ptr(),
        )];

        for prim in sorted.into_iter().rev() {
            let native = set_primitive_to_native(machine, view, prim)?;
            // Determine the box tag
            let box_tag = match &native {
                Native::Num(_) => DataConstructor::BoxedNumber.tag(),
                Native::Str(_) => DataConstructor::BoxedString.tag(),
                Native::Sym(_) => DataConstructor::BoxedSymbol.tag(),
                _ => {
                    return Err(ExecutionError::Panic(
                        "unexpected native type in set".to_string(),
                    ))
                }
            };
            // Box the value
            bindings.push(LambdaForm::value(
                view.alloc(HeapSyn::Cons {
                    tag: box_tag,
                    args: Array::from_slice(&view, &[Ref::V(native)]),
                })?
                .as_ptr(),
            ));
            // Cons it onto the list
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

impl CallGlobal1 for SetToList {}

/// SET.ADD — add an element to a set
pub struct SetAdd;

impl StgIntrinsic for SetAdd {
    fn name(&self) -> &str {
        "SET.ADD"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let native = resolve_native_unboxing(machine, view, &args[0])?;
        let prim = native_to_set_primitive(view, &native)?;
        let set = set_arg(machine, view, &args[1])?;
        machine_return_set(machine, view, set.with_added(prim))
    }
}

impl CallGlobal2 for SetAdd {}

/// SET.REMOVE — remove an element from a set
pub struct SetRemove;

impl StgIntrinsic for SetRemove {
    fn name(&self) -> &str {
        "SET.REMOVE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let native = resolve_native_unboxing(machine, view, &args[0])?;
        let prim = native_to_set_primitive(view, &native)?;
        let set = set_arg(machine, view, &args[1])?;
        machine_return_set(machine, view, set.with_removed(&prim))
    }
}

impl CallGlobal2 for SetRemove {}

/// SET.CONTAINS — check if an element is in a set
pub struct SetContains;

impl StgIntrinsic for SetContains {
    fn name(&self) -> &str {
        "SET.CONTAINS"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let native = resolve_native_unboxing(machine, view, &args[0])?;
        let prim = native_to_set_primitive(view, &native)?;
        let set = set_arg(machine, view, &args[1])?;
        machine_return_bool(machine, view, set.contains(&prim))
    }
}

impl CallGlobal2 for SetContains {}

/// SET.SIZE — return the number of elements
pub struct SetSize;

impl StgIntrinsic for SetSize {
    fn name(&self) -> &str {
        "SET.SIZE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let set = set_arg(machine, view, &args[0])?;
        let n = serde_json::Number::from(set.len() as u64);
        machine_return_num(machine, view, n)
    }
}

impl CallGlobal1 for SetSize {}

/// SET.UNION — return the union of two sets
pub struct SetUnion;

impl StgIntrinsic for SetUnion {
    fn name(&self) -> &str {
        "SET.UNION"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // Pipeline: `a set.union(b)` → SET.UNION(b, a) → args[0]=b, args[1]=a
        let a = set_arg(machine, view, &args[1])?;
        let b = set_arg(machine, view, &args[0])?;
        machine_return_set(machine, view, a.union(&b))
    }
}

impl CallGlobal2 for SetUnion {}

/// SET.INTERSECT — return the intersection of two sets
pub struct SetIntersect;

impl StgIntrinsic for SetIntersect {
    fn name(&self) -> &str {
        "SET.INTERSECT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // Pipeline: `a set.intersect(b)` → SET.INTERSECT(b, a) → args[0]=b, args[1]=a
        let a = set_arg(machine, view, &args[1])?;
        let b = set_arg(machine, view, &args[0])?;
        machine_return_set(machine, view, a.intersect(&b))
    }
}

impl CallGlobal2 for SetIntersect {}

/// SET.DIFF — return the difference (a - b)
pub struct SetDiff;

impl StgIntrinsic for SetDiff {
    fn name(&self) -> &str {
        "SET.DIFF"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // Pipeline: `a set.diff(b)` → SET.DIFF(b, a) → args[0]=b, args[1]=a
        let a = set_arg(machine, view, &args[1])?;
        let b = set_arg(machine, view, &args[0])?;
        machine_return_set(machine, view, a.diff(&b))
    }
}

impl CallGlobal2 for SetDiff {}
