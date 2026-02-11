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
        set::HeapSet,
        syntax::StgBuilder,
    },
    stg::tags::DataConstructor,
};
use crate::{
    common::sourcemap::Smid,
    eval::{memory::syntax::*, types::IntrinsicType},
};

/// Map a resolved native value to its intrinsic type for error reporting
fn native_type(native: &Native) -> IntrinsicType {
    match native {
        Native::Num(_) => IntrinsicType::Number,
        Native::Str(_) => IntrinsicType::String,
        Native::Sym(_) => IntrinsicType::Symbol,
        Native::Zdt(_) => IntrinsicType::ZonedDateTime,
        Native::Index(_) | Native::Set(_) => IntrinsicType::Unknown,
    }
}

/// Helper for intrinsics to access a numeric arg
pub fn num_arg(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<Number, ExecutionError> {
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::Num(n) = native {
        Ok(n)
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            IntrinsicType::Number,
            native_type(&native),
        ))
    }
}

/// Helper for intrinsics to access a str arg
pub fn str_arg(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<String, ExecutionError> {
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::Str(s) = native {
        Ok((*view.scoped(s)).as_str().to_string())
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            IntrinsicType::String,
            native_type(&native),
        ))
    }
}

/// Helper for intrinsics to access a sym arg
pub fn sym_arg(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<String, ExecutionError> {
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::Sym(id) = native {
        Ok(machine.symbol_pool().resolve(id).to_string())
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            IntrinsicType::Symbol,
            native_type(&native),
        ))
    }
}

/// Helper for intrinsics to access a zoned date time arg
pub fn zdt_arg(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<DateTime<FixedOffset>, ExecutionError> {
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::Zdt(dt) = native {
        Ok(dt)
    } else {
        Err(ExecutionError::TypeMismatch(
            machine.annotation(),
            IntrinsicType::ZonedDateTime,
            native_type(&native),
        ))
    }
}

pub struct DataIterator<'scope> {
    closure: SynClosure,
    view: MutatorHeapView<'scope>,
}

impl Iterator for DataIterator<'_> {
    type Item = Result<SynClosure, ExecutionError>;

    fn next(&mut self) -> Option<Self::Item> {
        let code = self.view.scoped(self.closure.code());

        let (h_ref, t_ref) = match &*code {
            HeapSyn::Cons { tag, args } => match (*tag).try_into() {
                Ok(DataConstructor::ListCons) => (args.get(0), args.get(1)),
                Ok(DataConstructor::ListNil) => return None,
                _ => return Some(Err(ExecutionError::Panic("expected list data".to_string()))),
            },
            _ => return Some(Err(ExecutionError::Panic("expected list data".to_string()))),
        };

        let head = match h_ref {
            Some(h) => self.closure.navigate_local(&self.view, h),
            None => {
                return Some(Err(ExecutionError::Panic(
                    "malformed cons cell".to_string(),
                )))
            }
        };

        match t_ref {
            Some(t) => {
                self.closure = self.closure.navigate_local(&self.view, t);
            }
            None => {
                return Some(Err(ExecutionError::Panic(
                    "malformed cons cell".to_string(),
                )))
            }
        }

        Some(Ok(head))
    }
}

/// Helper for intrinsics to access a list argument
pub fn data_list_arg<'scope>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'scope>,
    arg: Ref,
) -> Result<DataIterator<'scope>, ExecutionError> {
    Ok(DataIterator {
        closure: machine.nav(view).resolve(&arg)?,
        view,
    })
}

/// An iterator for tracing through the list of string values as
/// established by SeqStrList
pub struct StrListIterator<'scope> {
    closure: SynClosure,
    view: MutatorHeapView<'scope>,
}

impl Iterator for StrListIterator<'_> {
    type Item = Result<String, ExecutionError>;

    fn next(&mut self) -> Option<Self::Item> {
        let code = self.view.scoped(self.closure.code());

        let (h_ref, t_ref) = match &*code {
            HeapSyn::Cons { tag, args } => match (*tag).try_into() {
                Ok(DataConstructor::ListCons) => (args.get(0), args.get(1)),
                Ok(DataConstructor::ListNil) => return None,
                _ => {
                    return Some(Err(ExecutionError::Panic(
                        "expected string list data".to_string(),
                    )))
                }
            },
            _ => {
                return Some(Err(ExecutionError::Panic(
                    "expected string list data".to_string(),
                )))
            }
        };

        let native = match h_ref {
            Some(h) => self.closure.navigate_local_native(&self.view, h),
            None => {
                return Some(Err(ExecutionError::Panic(
                    "malformed cons cell".to_string(),
                )))
            }
        };

        match t_ref {
            Some(t) => {
                self.closure = self.closure.navigate_local(&self.view, t);
            }
            None => {
                return Some(Err(ExecutionError::Panic(
                    "malformed cons cell".to_string(),
                )))
            }
        }

        if let Native::Str(s) = native {
            Some(Ok((*self.view.scoped(s)).as_str().to_string()))
        } else {
            Some(Err(ExecutionError::TypeMismatch(
                Smid::default(),
                IntrinsicType::String,
                native_type(&native),
            )))
        }
    }
}

/// Helper for intrinsics to access a string list arg
pub fn str_list_arg<'guard>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'guard>,
    arg: Ref,
) -> Result<StrListIterator<'guard>, ExecutionError> {
    Ok(StrListIterator {
        closure: machine.nav(view).resolve(&arg)?,
        view,
    })
}

/// What to return when the return should be ignored
pub fn machine_return_unit(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
) -> Result<(), ExecutionError> {
    machine.set_closure(SynClosure::new(view.unit()?.as_ptr(), machine.root_env()))
}

/// Return number from intrinsic
pub fn machine_return_num(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    n: Number,
) -> Result<(), ExecutionError> {
    machine.set_closure(SynClosure::new(
        view.alloc(HeapSyn::Atom {
            evaluand: Ref::V(Native::Num(n)),
        })?
        .as_ptr(),
        machine.root_env(),
    ))
}

/// Return string from intrinsic
pub fn machine_return_str(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    s: String,
) -> Result<(), ExecutionError> {
    machine.set_closure(SynClosure::new(
        view.alloc(HeapSyn::Atom {
            evaluand: view.str_ref(s)?,
        })?
        .as_ptr(),
        machine.root_env(),
    ))
}

/// Return symbol from intrinsic
pub fn machine_return_sym(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    s: String,
) -> Result<(), ExecutionError> {
    let evaluand = view.sym_ref(machine.symbol_pool_mut(), s)?;
    machine.set_closure(SynClosure::new(
        view.alloc(HeapSyn::Atom { evaluand })?.as_ptr(),
        machine.root_env(),
    ))
}

/// Return zoned date time from intrinsic
pub fn machine_return_zdt(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    zdt: DateTime<FixedOffset>,
) -> Result<(), ExecutionError> {
    machine.set_closure(SynClosure::new(
        view.alloc(HeapSyn::Atom {
            evaluand: Ref::V(Native::Zdt(zdt)),
        })?
        .as_ptr(),
        machine.root_env(),
    ))
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
                | Ok(DataConstructor::BoxedZdt) => {
                    let inner_ref = args
                        .get(0)
                        .ok_or_else(|| ExecutionError::Panic("empty boxed value".to_string()))?;
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
    view: MutatorHeapView,
    native: &Native,
) -> Result<crate::eval::memory::set::Primitive, ExecutionError> {
    use crate::eval::memory::set::Primitive as SetPrim;
    match native {
        Native::Num(n) => Ok(SetPrim::from_number(n)),
        Native::Str(s) => Ok(SetPrim::Str(view.scoped(*s).as_str().to_string())),
        Native::Sym(id) => Ok(SetPrim::Sym(*id)),
        _ => Err(ExecutionError::Panic(
            "only numbers, strings, and symbols can be set elements".to_string(),
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
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::Set(ptr) = native {
        Ok(view.scoped(ptr))
    } else {
        Err(ExecutionError::Panic("expected set argument".to_string()))
    }
}

/// Return a set from intrinsic
pub fn machine_return_set(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    set: HeapSet,
) -> Result<(), ExecutionError> {
    let ptr = view.alloc(set)?.as_ptr();
    machine.set_closure(SynClosure::new(
        view.alloc(HeapSyn::Atom {
            evaluand: Ref::V(Native::Set(ptr)),
        })?
        .as_ptr(),
        machine.root_env(),
    ))
}

/// Return boolean from intrinsic
pub fn machine_return_bool(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    b: bool,
) -> Result<(), ExecutionError> {
    machine.set_closure(SynClosure::new(
        if b {
            view.t()?.as_ptr()
        } else {
            view.f()?.as_ptr()
        },
        machine.root_env(),
    ))
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
    let mut bindings = vec![LambdaForm::value(view.nil()?.as_ptr())];
    for item in list.into_iter().rev() {
        bindings.push(LambdaForm::value(
            view.data(
                DataConstructor::BoxedString.tag(),
                Array::from_slice(&view, &[view.str_ref(item)?]),
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

/// Return a list of closures from intrinsic
pub fn machine_return_closure_list(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    list: Vec<SynClosure>,
) -> Result<(), ExecutionError> {
    // env of items
    let item_frame = view.from_closures(
        list.iter().cloned(),
        list.len(),
        machine.env(view),
        Smid::default(),
    )?;
    let len = list.len();

    // env of links [lnull, l0, l1 ..] [i0 i1 i2]
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
    machine.set_closure(SynClosure::new(syn, item_frame))
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
