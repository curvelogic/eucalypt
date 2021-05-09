//! The runtime provides intrinsic implementations and corresponding
//! global wrapper STG forms to allow the machine to do real work.
use crate::common::{
    prettify::{prettify, ToPretty},
    sourcemap::SourceMap,
};
use std::rc::Rc;

use chrono::{DateTime, FixedOffset};
use gcmodule::Cc;
use indexmap::IndexMap;
use pretty::{DocAllocator, DocBuilder};
use serde_json::Number;

use super::{
    env::{Closure, EnvFrame},
    intrinsic::StgIntrinsic,
    machine::Machine,
    syntax::{
        dsl::{self, data, letrec_, local, lref, nil, sym, value, vref},
        tags, LambdaForm, Native, Ref, Reference, StgSyn,
    },
};
use crate::{
    common::sourcemap::Smid,
    eval::{error::ExecutionError, intrinsics},
};

/// The STG-machine can be run with different sets of intrinsics
/// available (mainly for testing). A runtime packages together the
/// intrinsics and their STG-syntax wrappers.
pub trait Runtime: Sync {
    /// Provide the globals wrappers
    ///
    /// NB. these contain RefCells and can be evaluated and mutated in
    /// place
    fn globals(&self, source_map: &mut SourceMap) -> Cc<EnvFrame>;

    /// Provide the (immutable) intrinsic implementations
    fn intrinsics(&self) -> Vec<&dyn StgIntrinsic>;
}

pub enum NativeVariant {
    Boxed,
    Unboxed,
}

pub struct StandardRuntime {
    impls: Vec<Box<dyn StgIntrinsic>>,
}

impl Default for StandardRuntime {
    fn default() -> Self {
        let impls = intrinsics::catalogue()
            .iter()
            .map(|i| -> Box<dyn StgIntrinsic> { Box::new(Unimplemented::new(i.name())) })
            .collect();
        StandardRuntime { impls }
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
        let mut source_map = SourceMap::default();
        let lambdas: Vec<_> = self
            .impls
            .iter()
            .map(|g| g.wrapper(&mut source_map))
            .collect();

        let docs = lambdas.iter().enumerate().map(|(i, lam)| {
            let name = intrinsics::intrinsic(i).name();

            allocator
                .text(format!("(⊗{}) ", i))
                .append(name)
                .append(":")
                .append(allocator.space())
                .append(allocator.line())
                .append(allocator.text(prettify(lam)))
                .append(allocator.line())
        });

        allocator.intersperse(docs, allocator.line())
    }
}

impl Runtime for StandardRuntime {
    /// Provide all global STG wrappers for the machine
    fn globals(&self, source_map: &mut SourceMap) -> Cc<EnvFrame> {
        let lambda_forms: Vec<LambdaForm> =
            self.impls.iter().map(|g| g.wrapper(source_map)).collect();
        EnvFrame::from_let(
            lambda_forms.as_slice(),
            &Cc::new(EnvFrame::default()),
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

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
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
            StgSyn::Cons { tag, args } => match *tag {
                tags::LIST_CONS => (args[0].clone(), args[1].clone()),
                tags::LIST_NIL => return None,
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
            StgSyn::Cons { tag, args } => match *tag {
                tags::LIST_CONS => (args[0].clone(), args[1].clone()),
                tags::LIST_NIL => return None,
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
    machine.set_closure(Closure::new(dsl::unit(), Cc::new(EnvFrame::default())))
}

/// Return number from intrinsic
pub fn machine_return_num(machine: &mut Machine, n: Number) -> Result<(), ExecutionError> {
    machine.set_closure(Closure::new(
        Rc::new(StgSyn::Atom {
            evaluand: Reference::V(Native::Num(n)),
        }),
        Cc::new(EnvFrame::default()),
    ))
}

/// Return string from intrinsic
pub fn machine_return_str(machine: &mut Machine, s: String) -> Result<(), ExecutionError> {
    machine.set_closure(Closure::new(
        Rc::new(StgSyn::Atom {
            evaluand: Reference::V(Native::Str(s)),
        }),
        Cc::new(EnvFrame::default()),
    ))
}

/// Return symbol from intrinsic
pub fn machine_return_sym(machine: &mut Machine, s: String) -> Result<(), ExecutionError> {
    machine.set_closure(Closure::new(
        Rc::new(StgSyn::Atom {
            evaluand: Reference::V(Native::Sym(s)),
        }),
        Cc::new(EnvFrame::default()),
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
        Cc::new(EnvFrame::default()),
    ))
}

/// Return boolean from intrinsic
pub fn machine_return_bool(machine: &mut Machine, b: bool) -> Result<(), ExecutionError> {
    machine.set_closure(Closure::new(
        if b { dsl::t() } else { dsl::f() },
        Cc::new(EnvFrame::default()),
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
            tags::BOXED_STRING,
            vec![vref(Native::Str(item))],
        )));
        let len = bindings.len();
        bindings.push(value(data(
            tags::LIST_CONS,
            vec![lref(len - 1), lref(len - 2)],
        )));
    }
    let list_index = bindings.len() - 1;
    let syn = letrec_(bindings, local(list_index));
    machine.set_closure(Closure::new(syn, Cc::new(EnvFrame::empty())))
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
            tags::LIST_CONS,
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
            data(tags::BLOCK_PAIR, vec![sym(k), lref(i)]),
            value_frame.clone(),
        ));
    }
    let pair_frame = EnvFrame::from_closures(&pairs, &value_frame, Smid::default());

    // env of links [lnull, l0, l1 ..] [p0 p1 p2...] [v0 v1 ..]
    let mut bindings = vec![value(nil())];
    for i in (0..len + 1).rev() {
        bindings.push(value(data(
            tags::LIST_CONS,
            vec![lref(len + i + 1), lref(len - i)],
        )));
    }

    let syn = letrec_(bindings, local(len));
    machine.set_closure(Closure::new(syn, pair_frame))
}

pub mod call {
    pub mod global {
        use std::rc::Rc;

        use crate::eval::{
            intrinsics,
            machines::stg::syntax::{
                dsl::{app, gref},
                Ref, StgSyn,
            },
        };

        /// A call to a global STG wrapper function
        fn call_global(name: &str, args: &[Ref]) -> Rc<StgSyn> {
            app(
                gref(
                    intrinsics::index(name)
                        .unwrap_or_else(|| panic!("No such intrinsic: {}", name)),
                ),
                args.to_vec(),
            )
        }
        pub fn eq(x: Ref, y: Ref) -> Rc<StgSyn> {
            call_global("EQ", &[x, y])
        }
        pub fn not(x: Ref) -> Rc<StgSyn> {
            call_global("NOT", &[x])
        }
        pub fn matches_key(x: Ref, y: Ref) -> Rc<StgSyn> {
            call_global("MATCHES_KEY", &[x, y])
        }
        pub fn extract_value(x: Ref) -> Rc<StgSyn> {
            call_global("EXTRACT_VALUE", &[x])
        }
        pub fn extract_key(x: Ref) -> Rc<StgSyn> {
            call_global("EXTRACT_KEY", &[x])
        }
        pub fn pack_pair(x: Ref) -> Rc<StgSyn> {
            call_global("PACK_PAIR", &[x])
        }
        pub fn block_pair(x: Ref) -> Rc<StgSyn> {
            call_global("BLOCK_PAIR", &[x])
        }
        pub fn lookup_or(x: Ref, y: Ref, z: Ref) -> Rc<StgSyn> {
            call_global("LOOKUPOR", &[x, y, z])
        }
        pub fn lookup_or_unboxed(x: Ref, y: Ref, z: Ref) -> Rc<StgSyn> {
            call_global("LOOKUPOR#", &[x, y, z])
        }
        pub fn and(x: Ref, y: Ref) -> Rc<StgSyn> {
            call_global("AND", &[x, y])
        }
        pub fn emitx(x: Ref) -> Rc<StgSyn> {
            call_global("EMITx", &[x])
        }
        pub fn render(x: Ref) -> Rc<StgSyn> {
            call_global("RENDER", &[x])
        }
        pub fn render_doc(x: Ref) -> Rc<StgSyn> {
            call_global("RENDER_DOC", &[x])
        }
        pub fn render_items(x: Ref) -> Rc<StgSyn> {
            call_global("RENDER_ITEMS", &[x])
        }
        pub fn render_block_items(x: Ref) -> Rc<StgSyn> {
            call_global("RENDER_BLOCK_ITEMS", &[x])
        }
        pub fn render_kv(x: Ref) -> Rc<StgSyn> {
            call_global("RENDER_KV", &[x])
        }
        pub fn suppresses(x: Ref) -> Rc<StgSyn> {
            call_global("SUPPRESSES", &[x])
        }
        pub fn panic(x: Ref) -> Rc<StgSyn> {
            call_global("PANIC", &[x])
        }
        pub fn kv(x: Ref) -> Rc<StgSyn> {
            call_global("KV", &[x])
        }
        pub fn dekv(x: Ref) -> Rc<StgSyn> {
            call_global("DEKV", &[x])
        }
        pub fn saturated(x: Ref) -> Rc<StgSyn> {
            call_global("SATURATED", &[x])
        }
        pub fn seq_str_list(x: Ref) -> Rc<StgSyn> {
            call_global("seqStrList", &[x])
        }
        pub fn merge_with(list: Ref, sep: Ref, f: Ref) -> Rc<StgSyn> {
            call_global("MERGEWITH", &[list, sep, f])
        }
        pub fn zdt_from_epoch(unix: Ref) -> Rc<StgSyn> {
            call_global("ZDT.FROM_EPOCH", &[unix])
        }
        pub fn zdt_fields(dt: Ref) -> Rc<StgSyn> {
            call_global("ZDT.FIELDS", &[dt])
        }
    }

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
