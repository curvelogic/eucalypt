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

use std::{collections::HashMap, convert::TryInto};

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{
            AbiClosure, CallGlobal1, CallGlobal2, CallGlobal3, CallGlobal4, IntrinsicMachine,
            StgIntrinsic,
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
        dsl::{app, app_bif, data, force, lambda, let_, local, lref, no_index, thunk},
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

/// The dict `tree` as a value (for `pop-*`'s `rest`).
fn dict_value(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    tree: Tree,
) -> Result<AbiClosure, ExecutionError> {
    machine.native_value(view, Native::Dict(tree))
}

/// A wrapper that forces the dict (the only argument) and calls the
/// intrinsic.
fn dict_only_wrapper(index: usize) -> LambdaForm {
    let bif: u8 = index.try_into().unwrap();
    lambda(
        1, // [d]
        force(local(0), app_bif(bif, vec![lref(0)])),
    )
}

/// DICT.KEYS(d) — the keys in key order.
pub struct DictKeys;

impl StgIntrinsic for DictKeys {
    fn name(&self) -> &str {
        "DICT.KEYS"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        dict_only_wrapper(self.index())
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
            let d = Dict::new(view, m.symbol_pool());
            let mut keys = Vec::with_capacity(d.size(tree) as usize);
            d.for_each(tree, |n| keys.push(n.key()));
            keys.iter()
                .map(|k| key_value(m, view, k))
                .collect::<Result<Vec<_>, _>>()?
        };
        machine.return_closure_list(view, items)
    }
}

impl CallGlobal1 for DictKeys {}

/// DICT.VALUES(d) — the (lazy) values in key order.
pub struct DictValues;

impl StgIntrinsic for DictValues {
    fn name(&self) -> &str {
        "DICT.VALUES"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        dict_only_wrapper(self.index())
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
            let d = Dict::new(view, m.symbol_pool());
            let mut values = Vec::with_capacity(d.size(tree) as usize);
            d.for_each(tree, |n| values.push(AbiClosure::Byte(n.value().clone())));
            values
        };
        machine.return_closure_list(view, items)
    }
}

impl CallGlobal1 for DictValues {}

/// Which end of the dict an extremum intrinsic reads, and whether it also
/// returns the rest of the dict.
#[derive(Clone, Copy)]
enum End {
    Min,
    Max,
}

/// The shared body of `DICT.MIN`, `DICT.MAX`, `DICT.POP_MIN` and
/// `DICT.POP_MAX`: `[k, v]`, or `[k, v, rest]` when `pop`.
fn extremum(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    args: &[Ref],
    end: End,
    pop: bool,
    what: &str,
) -> Result<(), ExecutionError> {
    let items = {
        let m: &dyn IntrinsicMachine = machine;
        let tree = dict_arg(m, view, &args[0])?;
        let d = Dict::new(view, m.symbol_pool());
        let empty = || ExecutionError::EmptyDict(m.annotation(), what.to_string());
        let (node, rest) = if pop {
            let popped = match end {
                End::Min => d.delete_min(tree)?,
                End::Max => d.delete_max(tree)?,
            };
            let (node, rest) = popped.ok_or_else(empty)?;
            (node, Some(rest))
        } else {
            let node = match end {
                End::Min => d.min(tree),
                End::Max => d.max(tree),
            };
            (node.ok_or_else(empty)?, None)
        };
        let n = d.node(node);
        let mut items = vec![
            key_value(m, view, &n.key())?,
            AbiClosure::Byte(n.value().clone()),
        ];
        if let Some(rest) = rest {
            items.push(dict_value(m, view, rest)?);
        }
        items
    };
    machine.return_closure_list(view, items)
}

/// DICT.MIN(d) — `[k, v]` for the least key; an error if `d` is empty.
pub struct DictMin;

impl StgIntrinsic for DictMin {
    fn name(&self) -> &str {
        "DICT.MIN"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        dict_only_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        extremum(machine, view, args, End::Min, false, "dict.min")
    }
}

impl CallGlobal1 for DictMin {}

/// DICT.MAX(d) — `[k, v]` for the greatest key; an error if `d` is empty.
pub struct DictMax;

impl StgIntrinsic for DictMax {
    fn name(&self) -> &str {
        "DICT.MAX"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        dict_only_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        extremum(machine, view, args, End::Max, false, "dict.max")
    }
}

impl CallGlobal1 for DictMax {}

/// DICT.POP_MIN(d) — `[k, v, rest]` for the least key; an error if `d` is
/// empty.
pub struct DictPopMin;

impl StgIntrinsic for DictPopMin {
    fn name(&self) -> &str {
        "DICT.POP_MIN"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        dict_only_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        extremum(machine, view, args, End::Min, true, "dict.pop-min")
    }
}

impl CallGlobal1 for DictPopMin {}

/// DICT.POP_MAX(d) — `[k, v, rest]` for the greatest key; an error if `d`
/// is empty.
pub struct DictPopMax;

impl StgIntrinsic for DictPopMax {
    fn name(&self) -> &str {
        "DICT.POP_MAX"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        dict_only_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        extremum(machine, view, args, End::Max, true, "dict.pop-max")
    }
}

impl CallGlobal1 for DictPopMax {}

/// DICT.RANGE(lo, hi, d) — the sub-dict of entries with `lo ≤ k < hi`
/// (empty when `hi ≤ lo`).
pub struct DictRange;

impl StgIntrinsic for DictRange {
    fn name(&self) -> &str {
        "DICT.RANGE"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        let bif: u8 = self.index().try_into().unwrap();
        lambda(
            3, // [lo hi d]
            force(
                SeqKey.global(lref(0)),
                // [flo] [lo hi d]
                force(
                    SeqKey.global(lref(2)),
                    // [fhi] [flo] [lo hi d]
                    force(
                        local(4),
                        // [fd] [fhi] [flo] [lo hi d]
                        app_bif(bif, vec![lref(2), lref(1), lref(0)]),
                    ),
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
        let tree = {
            let m: &dyn IntrinsicMachine = machine;
            let lo = key_arg(m, view, &args[0])?;
            let hi = key_arg(m, view, &args[1])?;
            let tree = dict_arg(m, view, &args[2])?;
            Dict::new(view, m.symbol_pool()).range(tree, &lo, &hi)?
        };
        return_dict(machine, view, tree)
    }
}

impl CallGlobal3 for DictRange {}

/// DICT.UPDATE(k, f, default, d) — `d` with `k` mapped to the lazy value
/// `f(old)`, where `old` is the value at `k`, or `default` if `d` does not
/// hold `k`. The application is built over a fresh frame `[f, old]`, so the
/// stored value does not keep the caller's frame (and the previous dict)
/// alive, and it lives in a memoising cell.
pub struct DictUpdate;

impl StgIntrinsic for DictUpdate {
    fn name(&self) -> &str {
        "DICT.UPDATE"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        key_dict_wrapper(self.index(), 4)
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
            let tree = dict_arg(m, view, &args[3])?;
            let d = Dict::new(view, m.symbol_pool());
            let old = match d.lookup(tree, &key) {
                Some(p) => AbiClosure::Byte(d.node(p).value().clone()),
                None => m.memo_cell(view, m.resolve_closure(view, &args[2])?)?,
            };
            let f = m.memo_cell(view, m.resolve_closure(view, &args[1])?)?;
            let applied = m.apply1_thunk(view, f, old)?;
            let AbiClosure::Byte(value) = m.memo_cell(view, applied)?;
            d.insert(tree, key, value)?
        };
        return_dict(machine, view, tree)
    }
}

impl CallGlobal4 for DictUpdate {}

/// DICT.UPDATE_STRICT(k, f, default, d) — as `DICT.UPDATE`, but the new
/// value `f(old)` is evaluated (to WHNF) before it is stored. The wrapper
/// forces it; `execute` then stores the evaluated value as `DICT.ASSOC`
/// does, with arguments `[forced key, value, forced dict]`.
pub struct DictUpdateStrict;

impl StgIntrinsic for DictUpdateStrict {
    fn name(&self) -> &str {
        "DICT.UPDATE_STRICT"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        let bif: u8 = self.index().try_into().unwrap();
        let get_or: u8 = DictGetOr.index().try_into().unwrap();
        lambda(
            4, // [k f default d]
            force(
                SeqKey.global(lref(0)),
                // [fk] [k f default d]
                force(
                    local(4),
                    // [fd] [fk] [k f default d]
                    //  0    1    2 3 4       5
                    let_(
                        vec![thunk(app_bif(get_or, vec![lref(1), lref(4), lref(0)]))],
                        // [old] [fd] [fk] [k f default d]
                        //  0     1    2    3 4 5       6
                        force(
                            app(lref(4), vec![lref(0)]),
                            // [new] [old] [fd] [fk] [k f default d]
                            //  0     1     2    3    4 5 6       7
                            app_bif(bif, vec![lref(3), lref(0), lref(2)]),
                        ),
                    ),
                ),
            ),
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        DictAssoc.execute(machine, view, emitter, args)
    }
}

impl CallGlobal4 for DictUpdateStrict {}

/// DICT.MERGE_WITH(f, a, b) — the union of `a` and `b`; where both hold a
/// key, `a`'s key is kept and the value is the lazy `f(a_value, b_value)`,
/// built over a fresh frame and stored in a memoising cell.
pub struct DictMergeWith;

impl StgIntrinsic for DictMergeWith {
    fn name(&self) -> &str {
        "DICT.MERGE_WITH"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        let bif: u8 = self.index().try_into().unwrap();
        lambda(
            3, // [f a b]
            force(
                local(1),
                // [fa] [f a b]
                force(
                    local(3),
                    // [fb] [fa] [f a b]
                    app_bif(bif, vec![lref(2), lref(1), lref(0)]),
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
        let tree = {
            let m: &dyn IntrinsicMachine = machine;
            let a = dict_arg(m, view, &args[1])?;
            let b = dict_arg(m, view, &args[2])?;
            let f = m.memo_cell(view, m.resolve_closure(view, &args[0])?)?;
            Dict::new(view, m.symbol_pool()).union_with(a, b, &mut |va, vb| {
                let applied = m.apply2_thunk(
                    view,
                    f.clone(),
                    AbiClosure::Byte(va.clone()),
                    AbiClosure::Byte(vb.clone()),
                )?;
                let AbiClosure::Byte(value) = m.memo_cell(view, applied)?;
                Ok(value)
            })?
        };
        return_dict(machine, view, tree)
    }
}

impl CallGlobal3 for DictMergeWith {}

/// DICT.TO_BLOCK(d) — a block of the entries in key order: a symbol key
/// names its entry, a string key the symbol with its text, a number key the
/// symbol with its rendered text; a list key, or two keys naming the same
/// entry, is an error. Returns the block's list of pairs; the wrapper wraps
/// it as a block.
pub struct DictToBlock;

impl StgIntrinsic for DictToBlock {
    fn name(&self) -> &str {
        "DICT.TO_BLOCK"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        let bif: u8 = self.index().try_into().unwrap();
        lambda(
            1, // [d]
            force(
                local(0),
                // [fd] [d]
                force(
                    app_bif(bif, vec![lref(0)]),
                    // [pairs] [fd] [d]
                    data(DataConstructor::Block.tag(), vec![lref(0), no_index()]),
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
        // Block key names first (interning needs the machine mutably), then
        // the pairs.
        let (names, values) = {
            let m: &dyn IntrinsicMachine = machine;
            let tree = dict_arg(m, view, &args[0])?;
            let d = Dict::new(view, m.symbol_pool());
            let mut names = Vec::with_capacity(d.size(tree) as usize);
            let mut values = Vec::with_capacity(d.size(tree) as usize);
            let mut keys = Vec::with_capacity(d.size(tree) as usize);
            let mut error = None;
            d.for_each(tree, |n| {
                let key = n.key();
                let name = match key {
                    DictKey::Int(_) | DictKey::UInt(_) | DictKey::Float(_) => {
                        Some(render_key(m, view, &key))
                    }
                    DictKey::Str(_) => key.str_in(&view).map(str::to_string),
                    DictKey::Sym(id) => Some(m.symbol_pool().resolve(id).to_string()),
                    DictKey::List(_) => None,
                };
                match name {
                    Some(name) => {
                        names.push(name);
                        keys.push(key);
                        values.push(n.value().clone());
                    }
                    None => {
                        if error.is_none() {
                            error = Some(ExecutionError::DictKeyNotBlockKey(
                                m.annotation(),
                                render_key(m, view, &key),
                            ));
                        }
                    }
                }
            });
            if let Some(e) = error {
                return Err(e);
            }
            // Two keys naming the same entry (`"x"` and `:x`, `1` and
            // `"1"`) are an error, as for rendering (design note §8,
            // OPEN 5): a block holds each key once.
            let mut seen: HashMap<&str, DictKey> = HashMap::with_capacity(names.len());
            for (name, key) in names.iter().zip(&keys) {
                if let Some(first) = seen.insert(name.as_str(), *key) {
                    return Err(ExecutionError::DictBlockKeyCollision(
                        m.annotation(),
                        render_key(m, view, &first),
                        render_key(m, view, key),
                        name.clone(),
                    ));
                }
            }
            (names, values)
        };
        let ids: Vec<_> = names
            .iter()
            .map(|n| machine.symbol_pool_mut().intern(n))
            .collect();
        let items = {
            let m: &dyn IntrinsicMachine = machine;
            let mut items = Vec::with_capacity(ids.len());
            for (id, value) in ids.into_iter().zip(values) {
                let key = m.native_value(view, Native::Sym(id))?;
                items.push(m.data_value(
                    view,
                    DataConstructor::BlockPair.tag(),
                    &[key, AbiClosure::Byte(value)],
                )?);
            }
            items
        };
        machine.return_closure_list(view, items)
    }
}

impl CallGlobal1 for DictToBlock {}
