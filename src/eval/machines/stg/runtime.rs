//! The runtime provides intrinsic implementations and corresponding
//! global wrapper STG forms to allow the machine to do real work.
use crate::common::{
    prettify::{prettify, ToPretty},
    sourcemap::SourceMap,
};
use std::{convert::TryInto, rc::Rc};

use chrono::{DateTime, FixedOffset};
use indexmap::IndexMap;
use pretty::{DocAllocator, DocBuilder};
use serde_json::Number;

use super::{
    env::{Closure, EnvFrame},
    intrinsic::StgIntrinsic,
    machine::Machine,
    syntax::{
        dsl::{self, data, letrec_, local, lref, nil, sym, value, vref},
        LambdaForm, Native, Ref, Reference, StgSyn,
    },
    tags::DataConstructor,
};
use crate::{
    common::sourcemap::Smid,
    eval::{error::ExecutionError, intrinsics},
};

/// The STG-machine can be run with different sets of intrinsics
/// available (mainly for testing). A runtime packages together the
/// intrinsics and their STG-syntax wrappers.
pub trait Runtime: Sync {
    /// Initialise a runtime, registering source map info for any
    /// annotations
    fn prepare(&mut self, source_map: &mut SourceMap);

    /// Provide an environment of globals wrappers
    fn globals(&self) -> Rc<EnvFrame>;

    /// Provide the (immutable) intrinsic implementations
    fn intrinsics(&self) -> Vec<&dyn StgIntrinsic>;
}

pub enum NativeVariant {
    Boxed,
    Unboxed,
}

pub struct StandardRuntime {
    /// Intrinsic implementations
    impls: Vec<Box<dyn StgIntrinsic>>,
    /// Annotation SMIDs to apply to globals
    annotations: Option<Vec<Smid>>,
}

impl Default for StandardRuntime {
    fn default() -> Self {
        let impls = intrinsics::catalogue()
            .iter()
            .map(|i| -> Box<dyn StgIntrinsic> { Box::new(Unimplemented::new(i.name())) })
            .collect();
        StandardRuntime {
            impls,
            annotations: None,
        }
    }
}

impl StandardRuntime {
    pub fn add(&mut self, imp: Box<dyn StgIntrinsic>) {
        let index = imp.index();
        self.impls[index] = imp;
    }
}

impl ToPretty for StandardRuntime {
    fn pretty<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        let docs = self.lambdas().into_iter().enumerate().map(|(i, lam)| {
            let name = intrinsics::intrinsic(i).name();

            allocator
                .text(format!("(⊗{}) ", i))
                .append(name)
                .append(":")
                .append(allocator.space())
                .append(allocator.line())
                .append(allocator.text(prettify(&lam)))
                .append(allocator.line())
        });

        allocator.intersperse(docs, allocator.line())
    }
}

impl StandardRuntime {
    /// Generate the wrappers to populate the global environment
    fn lambdas(&self) -> Vec<LambdaForm> {
        let smids = self.annotations.as_ref().expect("runtime not initialised");

        self.impls
            .iter()
            .zip(smids)
            .map(|(g, ann)| g.wrapper(*ann))
            .collect()
    }
}

impl Runtime for StandardRuntime {
    /// Generate the STG wrappers for all the intrinsics
    fn prepare(&mut self, source_map: &mut SourceMap) {
        self.annotations = Some(
            self.impls
                .iter()
                .map(|bif| source_map.add_synthetic(bif.name()))
                .collect(),
        )
    }

    /// Provide all global STG wrappers for the machine
    ///
    /// Must not be called until globals have been generated
    fn globals(&self) -> Rc<EnvFrame> {
        EnvFrame::from_let(
            self.lambdas().as_slice(),
            &Rc::new(EnvFrame::default()),
            Smid::default(),
        )
    }

    /// Provide reference to intrinsic implementations for the machine
    fn intrinsics(&self) -> Vec<&dyn StgIntrinsic> {
        self.impls.iter().map(|b| b.as_ref()).collect()
    }
}

#[derive(Clone)]
pub struct Unimplemented {
    name: &'static str,
}

impl Unimplemented {
    fn new(name: &'static str) -> Self {
        Unimplemented { name }
    }
}

impl StgIntrinsic for Unimplemented {
    fn name(&self) -> &str {
        &self.name
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        dsl::value(call::bif::panic(dsl::str(&format!(
            "unimplemented intrinsic wrapper {}",
            self.name()
        ))))
    }

    fn execute(
        &self,
        _machine: &mut Machine,
        _args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        panic!("executing unimplemented intrinsic {}", self.name())
    }
}

/// Helper for intrinsics to access a numeric arg
pub fn num_arg(machine: &mut Machine, arg: &Ref) -> Result<Number, ExecutionError> {
    let native = machine.resolve_native(arg)?;
    if let Native::Num(n) = native {
        Ok(n)
    } else {
        Err(ExecutionError::NotEvaluatedNumber(Smid::default()))
    }
}

/// Helper for intrinsics to access a str arg
pub fn str_arg(machine: &mut Machine, arg: &Ref) -> Result<String, ExecutionError> {
    let native = machine.resolve_native(arg)?;
    if let Native::Str(s) = native {
        Ok(s)
    } else {
        Err(ExecutionError::NotEvaluatedString(Smid::default()))
    }
}

/// Helper for intrinsics to access a sym arg
pub fn sym_arg(machine: &mut Machine, arg: &Ref) -> Result<String, ExecutionError> {
    let native = machine.resolve_native(arg)?;
    if let Native::Sym(s) = native {
        Ok(s)
    } else {
        Err(ExecutionError::NotEvaluatedString(Smid::default()))
    }
}

/// Helper for intrinsics to access a zoned date time arg
pub fn zdt_arg(machine: &mut Machine, arg: &Ref) -> Result<DateTime<FixedOffset>, ExecutionError> {
    let native = machine.resolve_native(arg)?;
    if let Native::Zdt(dt) = native {
        Ok(dt)
    } else {
        Err(ExecutionError::NotEvaluatedZdt(Smid::default()))
    }
}

pub struct DataIterator {
    closure: Closure,
}

impl Iterator for DataIterator {
    type Item = Closure;

    fn next(&mut self) -> Option<Self::Item> {
        let (h, t) = match &**self.closure.code() {
            StgSyn::Cons { tag, args } => match (*tag).try_into() {
                Ok(DataConstructor::ListCons) => (args[0].clone(), args[1].clone()),
                Ok(DataConstructor::ListNil) => return None,
                _ => panic!("Non-list data after force"),
            },
            _ => panic!("Non-list after force"),
        };

        let head = self.closure.navigate_local(h);
        self.closure = self.closure.navigate_local(t);

        Some(head)
    }
}

/// Helper for intrinsics to access a list argument
pub fn data_list_arg(machine: &mut Machine, arg: Ref) -> Result<DataIterator, ExecutionError> {
    Ok(DataIterator {
        closure: machine.closure().navigate_local(arg),
    })
}

/// An iterator for tracing through the list of string values as
/// established by SeqStrList
pub struct StrListIterator {
    closure: Closure,
}

impl Iterator for StrListIterator {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let (h, t) = match &**self.closure.code() {
            StgSyn::Cons { tag, args } => match (*tag).try_into() {
                Ok(DataConstructor::ListCons) => (args[0].clone(), args[1].clone()),
                Ok(DataConstructor::ListNil) => return None,
                _ => panic!("Non-list data after seqStrList"),
            },
            _ => panic!("Non-list after seqStrList"),
        };

        let native = self.closure.navigate_local_native(h);
        self.closure = self.closure.navigate_local(t);
        if let Native::Str(ref s) = native {
            Some(s.clone())
        } else {
            panic!("Non-string item after seqStrList")
        }
    }
}

/// Helper for intrinsics to access a string list arg
pub fn str_list_arg(machine: &mut Machine, arg: Ref) -> Result<StrListIterator, ExecutionError> {
    Ok(StrListIterator {
        closure: machine.closure().navigate_local(arg),
    })
}

/// What to return when the return should be ignored
pub fn machine_return_unit(machine: &mut Machine) -> Result<(), ExecutionError> {
    machine.set_closure(Closure::new(dsl::unit(), Rc::new(EnvFrame::default())))
}

/// Return number from intrinsic
pub fn machine_return_num(machine: &mut Machine, n: Number) -> Result<(), ExecutionError> {
    machine.set_closure(Closure::new(
        Rc::new(StgSyn::Atom {
            evaluand: Reference::V(Native::Num(n)),
        }),
        Rc::new(EnvFrame::default()),
    ))
}

/// Return string from intrinsic
pub fn machine_return_str(machine: &mut Machine, s: String) -> Result<(), ExecutionError> {
    machine.set_closure(Closure::new(
        Rc::new(StgSyn::Atom {
            evaluand: Reference::V(Native::Str(s)),
        }),
        Rc::new(EnvFrame::default()),
    ))
}

/// Return symbol from intrinsic
pub fn machine_return_sym(machine: &mut Machine, s: String) -> Result<(), ExecutionError> {
    machine.set_closure(Closure::new(
        Rc::new(StgSyn::Atom {
            evaluand: Reference::V(Native::Sym(s)),
        }),
        Rc::new(EnvFrame::default()),
    ))
}

/// Return zoned date time from intrinsic
pub fn machine_return_zdt(
    machine: &mut Machine,
    zdt: DateTime<FixedOffset>,
) -> Result<(), ExecutionError> {
    machine.set_closure(Closure::new(
        Rc::new(StgSyn::Atom {
            evaluand: Reference::V(Native::Zdt(zdt)),
        }),
        Rc::new(EnvFrame::default()),
    ))
}

/// Return boolean from intrinsic
pub fn machine_return_bool(machine: &mut Machine, b: bool) -> Result<(), ExecutionError> {
    machine.set_closure(Closure::new(
        if b { dsl::t() } else { dsl::f() },
        Rc::new(EnvFrame::default()),
    ))
}

/// Return a string list from intrinsic
pub fn machine_return_str_list(
    machine: &mut Machine,
    list: Vec<String>,
) -> Result<(), ExecutionError> {
    let mut bindings = vec![value(nil())];
    for item in list.into_iter().rev() {
        bindings.push(value(data(
            DataConstructor::BoxedString.tag(),
            vec![vref(Native::Str(item))],
        )));
        let len = bindings.len();
        bindings.push(value(data(
            DataConstructor::ListCons.tag(),
            vec![lref(len - 1), lref(len - 2)],
        )));
    }
    let list_index = bindings.len() - 1;
    let syn = letrec_(bindings, local(list_index));
    machine.set_closure(Closure::new(syn, Rc::new(EnvFrame::empty())))
}

/// Return a list of closures from intrinsic
pub fn machine_return_closure_list(
    machine: &mut Machine,
    list: Vec<Closure>,
) -> Result<(), ExecutionError> {
    // env of items
    let item_frame = EnvFrame::from_closures(&list, machine.env(), Smid::default());
    let len = list.len();

    // env of links [lnull, l0, l1 ..] [i0 i1 i2]
    let mut bindings = vec![value(nil())];
    for i in (0..len + 1).rev() {
        bindings.push(value(data(
            DataConstructor::ListCons.tag(),
            vec![lref(len + i + 1), lref(len - i)],
        )));
    }

    let syn = letrec_(bindings, local(len));
    machine.set_closure(Closure::new(syn, item_frame))
}

/// Return a list of closures from intrinsic
pub fn machine_return_block_pair_closure_list(
    machine: &mut Machine,
    block: IndexMap<String, Closure>,
) -> Result<(), ExecutionError> {
    // env of values
    let values: Vec<_> = block.values().cloned().collect();
    let value_frame = EnvFrame::from_closures(&values, machine.env(), Smid::default());
    let len = block.len();

    // env of pairs
    let mut pairs = vec![];
    for (i, k) in block.keys().enumerate() {
        pairs.push(Closure::new(
            data(DataConstructor::BlockPair.tag(), vec![sym(k), lref(i)]),
            value_frame.clone(),
        ));
    }
    let pair_frame = EnvFrame::from_closures(&pairs, &value_frame, Smid::default());

    // env of links [lnull, l0, l1 ..] [p0 p1 p2...] [v0 v1 ..]
    let mut bindings = vec![value(nil())];
    for i in (0..len + 1).rev() {
        bindings.push(value(data(
            DataConstructor::ListCons.tag(),
            vec![lref(len + i + 1), lref(len - i)],
        )));
    }

    let syn = letrec_(bindings, local(len));
    machine.set_closure(Closure::new(syn, pair_frame))
}

pub mod call {
    pub mod bif {
        use std::rc::Rc;

        use crate::eval::{
            intrinsics,
            machines::stg::syntax::{dsl::app_bif, Ref, StgSyn},
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
