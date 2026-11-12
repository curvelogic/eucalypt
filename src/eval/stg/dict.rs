//! Dict intrinsics for the eucalypt VM (eu-oaz72.21).
//!
//! A dict is a `Native::Dict` holding the root of a persistent
//! weight-balanced tree of GC heap nodes (`memory::dict`). Keys are
//! deep-forced by the wrappers (`seqKey`, `seqDictPairs`) before `execute`
//! runs, so no intrinsic here forces anything or re-enters the machine.
//! Values are stored lazily, in memoising cells (`IntrinsicMachine::
//! memo_cell`), so a value is computed at most once however many dicts share
//! it, and self-referential memo tables work.
//!
//! See `docs/superpowers/plans/2026-11-12-dict-design.md`.

use std::convert::TryInto;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{
            AbiClosure, CallGlobal1, CallGlobal2, CallGlobal3, IntrinsicMachine, StgIntrinsic,
        },
        memory::{
            dict::{Dict, DictKey, DictKeyList, Entry, Tree},
            mutator::MutatorHeapView,
            syntax::{Native, Ref},
        },
        stg::tags::DataConstructor,
    },
};

use super::{
    force::{SeqDictPairs, SeqKey},
    support::{dict_arg, machine_return_bool, machine_return_num},
    syntax::{
        dsl::{app_bif, force, lambda, local, lref},
        LambdaForm,
    },
};

/// The deepest list-key nesting accepted: a list key is depth 1, a list
/// inside it depth 2, and so on. Key comparison and conversion recurse on
/// nesting, so the cap keeps that recursion bounded.
pub const MAX_KEY_DEPTH: usize = 32;

/// Describe a WHNF value that is neither a valid key nor a valid pair.
fn describe(machine: &dyn IntrinsicMachine, view: MutatorHeapView<'_>, v: &AbiClosure) -> String {
    match machine.data_tag(view, v) {
        Some(t) if t == DataConstructor::Block.tag() => "a block".to_string(),
        Some(t)
            if t == DataConstructor::BoolTrue.tag() || t == DataConstructor::BoolFalse.tag() =>
        {
            "a boolean".to_string()
        }
        Some(t) if t == DataConstructor::Unit.tag() => "null".to_string(),
        Some(t) if t == DataConstructor::ListNil.tag() => "an empty list".to_string(),
        Some(t) if t == DataConstructor::ListCons.tag() => {
            // Count the cells already evaluated; a tail not yet evaluated
            // ends the count, which is then a lower bound.
            let mut n = 0usize;
            let mut cur = v.clone();
            while machine.data_tag(view, &cur) == Some(DataConstructor::ListCons.tag()) {
                n += 1;
                match machine.data_field(view, &cur, 1) {
                    Some(t) => cur = t,
                    None => break,
                }
            }
            let exact = machine.data_tag(view, &cur) == Some(DataConstructor::ListNil.tag());
            let noun = if n == 1 { "element" } else { "elements" };
            if exact {
                format!("a list of {n} {noun}")
            } else {
                format!("a list of at least {n} {noun}")
            }
        }
        Some(t) if t == DataConstructor::BoxedNumber.tag() => "a number".to_string(),
        Some(t) if t == DataConstructor::BoxedString.tag() => "a string".to_string(),
        Some(t) if t == DataConstructor::BoxedSymbol.tag() => "a symbol".to_string(),
        Some(t) if t == DataConstructor::BoxedZdt.tag() => "a datetime".to_string(),
        Some(_) => "a structured value".to_string(),
        None => match v {
            AbiClosure::Byte(crate::eval::bytecode::BcValue::Native(n)) => {
                format!("a {}", n.type_description())
            }
            _ if v.arity() > 0 => "a function".to_string(),
            _ => "an unevaluated value".to_string(),
        },
    }
}

/// Convert a deep-forced key value to a `DictKey`, allocating heap key
/// lists for list keys. Never forces.
fn key_of(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    v: &AbiClosure,
    depth: usize,
) -> Result<DictKey, ExecutionError> {
    let smid = machine.annotation();
    let tag = machine.data_tag(view, v);
    let is_list =
        tag == Some(DataConstructor::ListCons.tag()) || tag == Some(DataConstructor::ListNil.tag());
    if is_list {
        if depth >= MAX_KEY_DEPTH {
            return Err(ExecutionError::BadDictKey(
                smid,
                format!("list keys may be nested at most {MAX_KEY_DEPTH} deep"),
            ));
        }
        let mut elements = Vec::new();
        let mut cur = v.clone();
        while machine.data_tag(view, &cur) == Some(DataConstructor::ListCons.tag()) {
            let head = machine.data_field(view, &cur, 0).ok_or_else(|| {
                ExecutionError::Panic(smid, "malformed cons cell in dict key".to_string())
            })?;
            elements.push(key_of(machine, view, &head, depth + 1)?);
            cur = machine.data_field(view, &cur, 1).ok_or_else(|| {
                ExecutionError::Panic(smid, "malformed cons cell in dict key".to_string())
            })?;
        }
        return Ok(DictKey::List(DictKeyList::alloc(&view, &elements)?));
    }
    let scalar = tag.is_none()
        || tag == Some(DataConstructor::BoxedNumber.tag())
        || tag == Some(DataConstructor::BoxedString.tag())
        || tag == Some(DataConstructor::BoxedSymbol.tag());
    if scalar {
        match machine.value_native(view, v) {
            Some(Native::Num(n)) => return Ok(DictKey::from_number(&n)),
            Some(Native::Str(p)) => return Ok(DictKey::Str(p)),
            Some(Native::Sym(id)) => return Ok(DictKey::Sym(id)),
            _ => {}
        }
    }
    Err(ExecutionError::BadDictKey(
        smid,
        format!("{} cannot be a key", describe(machine, view, v)),
    ))
}

/// Resolve a (deep-forced) key argument.
fn key_arg(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<DictKey, ExecutionError> {
    let v = machine.resolve_closure(view, arg)?;
    key_of(machine, view, &v, 0)
}

/// Render a key for an error message: numbers as they print, strings
/// quoted, symbols as `:name`, lists bracketed.
fn render_key(machine: &dyn IntrinsicMachine, view: MutatorHeapView<'_>, key: &DictKey) -> String {
    match key {
        DictKey::Int(_) | DictKey::UInt(_) | DictKey::Float(_) => key
            .to_number()
            .map_or_else(|| "NaN".to_string(), |n| n.to_string()),
        DictKey::Str(_) => format!("{:?}", key.str_in(&view).unwrap_or_default()),
        DictKey::Sym(id) => format!(":{}", machine.symbol_pool().resolve(*id)),
        DictKey::List(_) => {
            let parts: Vec<String> = key
                .list_in(&view)
                .unwrap_or_default()
                .iter()
                .map(|k| render_key(machine, view, k))
                .collect();
            format!("[{}]", parts.join(", "))
        }
    }
}

/// Convert a stored key back to a eucalypt value: boxed scalars, and
/// cons-lists for list keys.
fn key_value(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    key: &DictKey,
) -> Result<AbiClosure, ExecutionError> {
    let (tag, native) = match key {
        DictKey::Int(_) | DictKey::UInt(_) | DictKey::Float(_) => {
            let n = key.to_number().ok_or_else(|| {
                ExecutionError::Panic(machine.annotation(), "NaN dict key".to_string())
            })?;
            (DataConstructor::BoxedNumber.tag(), Native::Num(n))
        }
        DictKey::Str(p) => (DataConstructor::BoxedString.tag(), Native::Str(*p)),
        DictKey::Sym(id) => (DataConstructor::BoxedSymbol.tag(), Native::Sym(*id)),
        DictKey::List(_) => {
            let elements = key.list_in(&view).unwrap_or_default();
            let mut acc = machine.data_value(view, DataConstructor::ListNil.tag(), &[])?;
            for k in elements.iter().rev() {
                let head = key_value(machine, view, k)?;
                acc = machine.data_value(view, DataConstructor::ListCons.tag(), &[head, acc])?;
            }
            return Ok(acc);
        }
    };
    let boxed = machine.native_value(view, native)?;
    machine.data_value(view, tag, &[boxed])
}

/// Return a dict.
fn return_dict(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    tree: Tree,
) -> Result<(), ExecutionError> {
    machine.return_native(view, Native::Dict(tree))
}

/// A wrapper that deep-forces the key (argument 0) with `seqKey`, forces
/// the dict (the last argument) to WHNF, and passes every other argument
/// through unforced.
fn key_dict_wrapper(index: usize, arity: usize) -> LambdaForm {
    let bif: u8 = index.try_into().unwrap();
    // [k ... d] → force(seqKey(k)) → [fk] [k ... d]
    //           → force(d)         → [fd] [fk] [k ... d]
    // In the final env: fd = 0, fk = 1, k = 2, argument i = 2 + i.
    let mut args = vec![lref(1)];
    for i in 1..arity - 1 {
        args.push(lref(2 + i));
    }
    args.push(lref(0));
    lambda(
        arity as u8,
        force(
            SeqKey.global(lref(0)),
            // After the first force, d (argument arity - 1) is at 1 + (arity - 1).
            force(local(arity), app_bif(bif, args)),
        ),
    )
}

/// DICT.OF(pairs) — build a dict from `[key, value]` pairs. Equal keys:
/// the first key and the last value are kept.
pub struct DictOf;

impl StgIntrinsic for DictOf {
    fn name(&self) -> &str {
        "DICT.OF"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        let bif: u8 = self.index().try_into().unwrap();
        lambda(
            1, // [pairs]
            force(
                SeqDictPairs.global(lref(0)),
                // [prepared] [pairs]
                app_bif(bif, vec![lref(0)]),
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
        let smid = machine.annotation();
        let tree = {
            let m: &dyn IntrinsicMachine = machine;
            let mut entries: Vec<Entry> = Vec::new();
            let mut cur = m.resolve_closure(view, &args[0])?;
            while m.data_tag(view, &cur) == Some(DataConstructor::ListCons.tag()) {
                let item = m.data_field(view, &cur, 0).ok_or_else(|| {
                    ExecutionError::Panic(smid, "malformed cons cell".to_string())
                })?;
                if m.data_tag(view, &item) != Some(DataConstructor::BlockPair.tag()) {
                    return Err(ExecutionError::BadDictPair(smid, describe(m, view, &item)));
                }
                let (Some(k), Some(v)) =
                    (m.data_field(view, &item, 0), m.data_field(view, &item, 1))
                else {
                    return Err(ExecutionError::Panic(
                        smid,
                        "malformed dict pair".to_string(),
                    ));
                };
                let key = key_of(m, view, &k, 0)?;
                let AbiClosure::Byte(value) = m.memo_cell(view, v)?;
                entries.push((key, value));
                cur = m.data_field(view, &cur, 1).ok_or_else(|| {
                    ExecutionError::Panic(smid, "malformed cons cell".to_string())
                })?;
            }
            let d = Dict::new(view, m.symbol_pool());
            let sorted = d.sort_last_wins(entries);
            d.build_sorted(&sorted)?
        };
        return_dict(machine, view, tree)
    }
}

impl CallGlobal1 for DictOf {}

/// DICT.TO_LIST(d) — the entries as `[key, value]` pairs in key order.
pub struct DictToList;

impl StgIntrinsic for DictToList {
    fn name(&self) -> &str {
        "DICT.TO_LIST"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let items = {
            let m: &dyn IntrinsicMachine = machine;
            let tree = dict_arg(m, view, &args[0])?;
            let entries = Dict::new(view, m.symbol_pool()).entries(tree);
            let nil = m.data_value(view, DataConstructor::ListNil.tag(), &[])?;
            let mut items = Vec::with_capacity(entries.len());
            for (k, v) in entries {
                let key = key_value(m, view, &k)?;
                let tail = m.data_value(
                    view,
                    DataConstructor::ListCons.tag(),
                    &[AbiClosure::Byte(v), nil.clone()],
                )?;
                items.push(m.data_value(view, DataConstructor::ListCons.tag(), &[key, tail])?);
            }
            items
        };
        machine.return_closure_list(view, items)
    }
}

impl CallGlobal1 for DictToList {}

/// DICT.GET(k, d) — the value at `k`; an error naming the key if absent.
pub struct DictGet;

impl StgIntrinsic for DictGet {
    fn name(&self) -> &str {
        "DICT.GET"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        key_dict_wrapper(self.index(), 2)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let found = {
            let m: &dyn IntrinsicMachine = machine;
            let key = key_arg(m, view, &args[0])?;
            let tree = dict_arg(m, view, &args[1])?;
            let d = Dict::new(view, m.symbol_pool());
            match d.lookup(tree, &key) {
                Some(p) => d.node(p).value().clone(),
                None => {
                    return Err(ExecutionError::DictKeyNotFound(
                        m.annotation(),
                        render_key(m, view, &key),
                    ))
                }
            }
        };
        machine.set_result(AbiClosure::Byte(found))
    }
}

impl CallGlobal2 for DictGet {}

/// DICT.GET_OR(k, default, d) — the value at `k`, or `default`.
pub struct DictGetOr;

impl StgIntrinsic for DictGetOr {
    fn name(&self) -> &str {
        "DICT.GET_OR"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        key_dict_wrapper(self.index(), 3)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let result = {
            let m: &dyn IntrinsicMachine = machine;
            let key = key_arg(m, view, &args[0])?;
            let tree = dict_arg(m, view, &args[2])?;
            let d = Dict::new(view, m.symbol_pool());
            match d.lookup(tree, &key) {
                Some(p) => AbiClosure::Byte(d.node(p).value().clone()),
                None => m.resolve_closure(view, &args[1])?,
            }
        };
        machine.set_result(result)
    }
}

impl CallGlobal3 for DictGetOr {}

/// DICT.ASSOC(k, v, d) — `d` with `k` mapped to the (lazy) value `v`. An
/// equal key already present is kept; only its value is replaced.
pub struct DictAssoc;

impl StgIntrinsic for DictAssoc {
    fn name(&self) -> &str {
        "DICT.ASSOC"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        key_dict_wrapper(self.index(), 3)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let tree = {
            let m: &dyn IntrinsicMachine = machine;
            let key = key_arg(m, view, &args[0])?;
            let tree = dict_arg(m, view, &args[2])?;
            let value = m.resolve_closure(view, &args[1])?;
            let AbiClosure::Byte(value) = m.memo_cell(view, value)?;
            Dict::new(view, m.symbol_pool()).insert(tree, key, value)?
        };
        return_dict(machine, view, tree)
    }
}

impl CallGlobal3 for DictAssoc {}

/// DICT.DISSOC(k, d) — `d` without `k` (`d` itself if `k` is absent).
pub struct DictDissoc;

impl StgIntrinsic for DictDissoc {
    fn name(&self) -> &str {
        "DICT.DISSOC"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        key_dict_wrapper(self.index(), 2)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let tree = {
            let m: &dyn IntrinsicMachine = machine;
            let key = key_arg(m, view, &args[0])?;
            let tree = dict_arg(m, view, &args[1])?;
            Dict::new(view, m.symbol_pool()).delete(tree, &key)?
        };
        return_dict(machine, view, tree)
    }
}

impl CallGlobal2 for DictDissoc {}

/// DICT.CONTAINS(k, d) — whether `d` holds `k`.
pub struct DictContains;

impl StgIntrinsic for DictContains {
    fn name(&self) -> &str {
        "DICT.CONTAINS"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        key_dict_wrapper(self.index(), 2)
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let found = {
            let m: &dyn IntrinsicMachine = machine;
            let key = key_arg(m, view, &args[0])?;
            let tree = dict_arg(m, view, &args[1])?;
            Dict::new(view, m.symbol_pool())
                .lookup(tree, &key)
                .is_some()
        };
        machine_return_bool(machine, view, found)
    }
}

impl CallGlobal2 for DictContains {}

/// DICT.SIZE(d) — the number of entries.
pub struct DictSize;

impl StgIntrinsic for DictSize {
    fn name(&self) -> &str {
        "DICT.SIZE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let size = {
            let m: &dyn IntrinsicMachine = machine;
            let tree = dict_arg(m, view, &args[0])?;
            Dict::new(view, m.symbol_pool()).size(tree)
        };
        machine_return_num(machine, view, serde_json::Number::from(size))
    }
}

impl CallGlobal1 for DictSize {}
