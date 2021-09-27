//! Support functions for writing intrinsics

use std::convert::TryInto;

use chrono::{DateTime, FixedOffset};
use indexmap::IndexMap;
use serde_json::Number;

use crate::eval::{
    error::ExecutionError,
    machine::{env::Closure, env_builder::EnvBuilder, intrinsic::*},
    memory::{alloc::ScopedAllocator, array::Array, mutator::MutatorHeapView, syntax::StgBuilder},
    stg::tags::DataConstructor,
};
use crate::{common::sourcemap::Smid, eval::memory::syntax::*};

/// Helper for intrinsics to access a numeric arg
pub fn num_arg<'guard>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'guard>,
    arg: &Ref,
) -> Result<Number, ExecutionError> {
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::Num(n) = native {
        Ok(n)
    } else {
        Err(ExecutionError::NotEvaluatedNumber(Smid::default()))
    }
}

/// Helper for intrinsics to access a str arg
/// TODO: use &str
pub fn str_arg<'guard>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'guard>,
    arg: &Ref,
) -> Result<String, ExecutionError> {
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::Str(s) = native {
        Ok((*view.scoped(s)).as_str().to_string())
    } else {
        Err(ExecutionError::NotEvaluatedString(Smid::default()))
    }
}

/// Helper for intrinsics to access a sym arg
/// TODO: use &str
pub fn sym_arg<'guard>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'guard>,
    arg: &Ref,
) -> Result<String, ExecutionError> {
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::Sym(s) = native {
        Ok((*view.scoped(s)).as_str().to_string())
    } else {
        Err(ExecutionError::NotEvaluatedString(Smid::default()))
    }
}

/// Helper for intrinsics to access a zoned date time arg
pub fn zdt_arg<'guard>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'guard>,
    arg: &Ref,
) -> Result<DateTime<FixedOffset>, ExecutionError> {
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::Zdt(dt) = native {
        Ok(dt)
    } else {
        Err(ExecutionError::NotEvaluatedZdt(Smid::default()))
    }
}

pub struct DataIterator<'scope> {
    closure: Closure,
    view: MutatorHeapView<'scope>,
}

impl<'scope> Iterator for DataIterator<'scope> {
    type Item = Closure;

    fn next(&mut self) -> Option<Self::Item> {
        let code = self.view.scoped(self.closure.code());

        let (h_ref, t_ref) = match &*code {
            HeapSyn::Cons { tag, args } => match (*tag).try_into() {
                Ok(DataConstructor::ListCons) => (args.get(0), args.get(1)),
                Ok(DataConstructor::ListNil) => return None,
                _ => panic!("Non-list data after force"),
            },
            _ => panic!("Non-list after force"),
        };

        let head = match h_ref {
            Some(h) => self.closure.navigate_local(&self.view, h).clone(),
            None => {
                panic!("Bad cons cell (h)")
            } // error
        };

        if let Some(t) = t_ref {
            self.closure = self.closure.navigate_local(&self.view, t).clone();
        } else {
            panic!("Bad cons cell (t)")
        }

        Some(head)
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
    closure: Closure,
    view: MutatorHeapView<'scope>,
}

impl<'scope> Iterator for StrListIterator<'scope> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let code = self.view.scoped(self.closure.code());

        let (h_ref, t_ref) = match &*code {
            HeapSyn::Cons { tag, args } => match (*tag).try_into() {
                Ok(DataConstructor::ListCons) => (args.get(0), args.get(1)),
                Ok(DataConstructor::ListNil) => return None,
                _ => panic!("Non-list data after seqStrList"),
            },
            _ => panic!("Non-list after seqStrList"),
        };

        let native = match h_ref {
            Some(h) => self.closure.navigate_local_native(&self.view, h).clone(),
            None => panic!("bad cons cell (h)"),
        };

        if let Some(t) = t_ref {
            self.closure = self.closure.navigate_local(&self.view, t).clone();
        } else {
            panic!("bad cons cell (t)");
        }

        if let Native::Str(s) = native {
            Some((*self.view.scoped(s)).as_str().to_string())
        } else {
            panic!("Non-string item after seqStrList")
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
    machine.set_closure(Closure::new(view.unit()?.as_ptr(), machine.root_env()))
}

/// Return number from intrinsic
pub fn machine_return_num(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    n: Number,
) -> Result<(), ExecutionError> {
    machine.set_closure(Closure::new(
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
    machine.set_closure(Closure::new(
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
    machine.set_closure(Closure::new(
        view.alloc(HeapSyn::Atom {
            evaluand: view.sym_ref(s)?,
        })?
        .as_ptr(),
        machine.root_env(),
    ))
}

/// Return zoned date time from intrinsic
pub fn machine_return_zdt(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    zdt: DateTime<FixedOffset>,
) -> Result<(), ExecutionError> {
    machine.set_closure(Closure::new(
        view.alloc(HeapSyn::Atom {
            evaluand: Ref::V(Native::Zdt(zdt)),
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
    machine.set_closure(Closure::new(
        if b {
            view.t()?.as_ptr()
        } else {
            view.f()?.as_ptr()
        },
        machine.root_env(),
    ))
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
    machine.set_closure(Closure::new(syn, machine.root_env()))
}

/// Return a list of closures from intrinsic
pub fn machine_return_closure_list(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    list: Vec<Closure>,
) -> Result<(), ExecutionError> {
    // env of items
    let item_frame = view.from_closures(
        list.iter().cloned(),
        list.len(),
        machine.env(view),
        Smid::default(),
    );
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
    machine.set_closure(Closure::new(syn, item_frame))
}

/// Return a list of closures from intrinsic
pub fn machine_return_block_pair_closure_list(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    block: IndexMap<String, Closure>,
) -> Result<(), ExecutionError> {
    // env of values
    let values: Vec<_> = block.values().cloned().collect();
    let value_frame = view.from_closures(
        values.iter().cloned(),
        values.len(),
        machine.env(view),
        Smid::default(),
    );
    let len = block.len();

    // env of pairs
    let mut pairs = vec![];
    for (i, k) in block.keys().enumerate() {
        pairs.push(Closure::new(
            view.data(
                DataConstructor::BlockPair.tag(),
                Array::from_slice(&view, &[view.sym_ref(k)?, Ref::L(i)]),
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
    );

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
    machine.set_closure(Closure::new(syn, pair_frame))
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

        pub fn emit_seq_end() -> Rc<StgSyn> {
            call_bif("EMIT]", &[])
        }

        pub fn emit_block_start() -> Rc<StgSyn> {
            call_bif("EMIT{", &[])
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
