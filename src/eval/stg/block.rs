//! Block intrinsics

use std::{mem::swap, rc::Rc};

use indexmap::IndexMap;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        intrinsics,
        machine::{
            env::SynClosure,
            env_builder::EnvBuilder,
            intrinsic::{CallGlobal1, CallGlobal2, CallGlobal3, IntrinsicMachine, StgIntrinsic},
            vm::HeapNavigator,
        },
        memory::{
            array::Array,
            mutator::MutatorHeapView,
            syntax::{HeapSyn, Native, Ref, StgBuilder},
        },
    },
};

use super::{
    eq::Eq,
    panic::Panic,
    runtime::NativeVariant,
    support::{
        call, data_list_arg, machine_return_block_pair_closure_list, machine_return_bool,
        machine_return_closure_list,
    },
    syntax::{
        dsl::{self},
        LambdaForm, StgSyn,
    },
    tags::DataConstructor,
};

/// BLOCK
///
/// BLOCK takes a list of key-value pairs (of some form) and coerces /
/// wraps them up a a block that can be understood by LOOKUP /
/// LOOKUPOR.
pub struct Block;

impl StgIntrinsic for Block {
    fn name(&self) -> &str {
        "BLOCK"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;

        let kv_items = lambda(
            2, // [items self]
            switch(
                local(0),
                vec![
                    (
                        DataConstructor::ListCons.tag(), // [h t] [items self]
                        force(
                            Kv.global(lref(0)),
                            // [kv] [h t] [items self]
                            force(
                                app(lref(4), vec![lref(2), lref(4)]),
                                // [tt] [kv] [h t] [itesm self]
                                data(DataConstructor::ListCons.tag(), vec![lref(1), lref(0)]),
                            ),
                        ),
                    ),
                    (DataConstructor::ListNil.tag(), local(0)),
                ],
            ),
        );

        annotated_lambda(
            1,
            letrec_(
                vec![
                    // [kvitems mapped] [list]
                    kv_items,
                    value(app(lref(0), vec![lref(2), lref(0)])),
                ],
                data(DataConstructor::Block.tag(), vec![lref(1), no_index()]),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for Block {}

/// KV
///
/// KV coerces its argument into a valid block key-value either as
/// BLOCK_PAIR or wrapping a list in a BLOCK_KV_LIST
pub struct Kv;

impl StgIntrinsic for Kv {
    fn name(&self) -> &str {
        "KV"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;

        annotated_lambda(
            1,
            case(
                local(0),
                vec![
                    (
                        DataConstructor::BlockPair.tag(), // [k v] [pair]
                        local(2),
                    ),
                    (
                        DataConstructor::ListCons.tag(), // [h t] [list]
                        data(DataConstructor::BlockKvList.tag(), vec![lref(2)]),
                    ),
                    (
                        DataConstructor::BlockKvList.tag(), // [l] [kvl]
                        local(1),
                    ),
                ],
                call::bif::panic(str("invalid key-value element in block")),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for Kv {}

/// DEKV
///
/// Takes a block KV element and converts it to a [k, v] list
pub struct Dekv;

impl StgIntrinsic for Dekv {
    fn name(&self) -> &str {
        "DEKV"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;

        annotated_lambda(
            1, // [pair]
            case(
                local(0),
                vec![
                    (
                        DataConstructor::BlockPair.tag(), // [k v] [pair]
                        letrec_(
                            vec![
                                // [nil boxk [v]] [k v] [pair]
                                value(nil()),
                                thunk(data(DataConstructor::BoxedSymbol.tag(), vec![lref(3)])),
                                value(data(
                                    DataConstructor::ListCons.tag(),
                                    vec![lref(4), lref(0)],
                                )),
                            ],
                            data(DataConstructor::ListCons.tag(), vec![lref(1), lref(2)]),
                        ),
                    ),
                    (
                        DataConstructor::BlockKvList.tag(), // [cons] [pair]
                        local(0),
                    ),
                ],
                call::bif::panic(str("invalid key-value element in block")),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for Dekv {}

/// ELEMENTS(block)
///
/// Return block as list of [k, v] lists
pub struct Elements;

impl StgIntrinsic for Elements {
    fn name(&self) -> &str {
        "ELEMENTS"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;

        let map_list = lambda(
            2, // [list self]
            case(
                local(0),
                vec![
                    (
                        DataConstructor::ListCons.tag(), // [h t] [list self]
                        letrec_(
                            // [dekv-h rest] [h t] [list self]
                            vec![
                                thunk(Dekv.global(lref(2))),
                                thunk(app(lref(5), vec![lref(3), lref(5)])),
                            ],
                            data(DataConstructor::ListCons.tag(), vec![lref(0), lref(1)]),
                        ),
                    ),
                    (DataConstructor::ListNil.tag(), local(0)),
                ],
                call::bif::panic(str("bad element in block")),
            ),
        );

        annotated_lambda(
            1, // [block]
            letrec_(
                vec![map_list], // [map_list] [block]
                case(
                    local(1),
                    vec![(
                        DataConstructor::Block.tag(), // [list index]  [map_list] [block]
                        app(lref(2), vec![lref(0), lref(2)]),
                    )],
                    call::bif::panic(str("elements called on non-block")),
                ),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for Elements {}

/// MATCHES_KEY(pair, unboxed_sym)
///
/// Return true iff the key-value pair matches the provided key symbol
pub struct MatchesKey;

impl StgIntrinsic for MatchesKey {
    fn name(&self) -> &str {
        "MATCHES_KEY"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;
        annotated_lambda(
            2, // [pair unboxsym]
            case(
                local(0),
                vec![
                    (
                        DataConstructor::BlockPair.tag(),
                        Eq.global(lref(0), lref(3)), // [k v] [pair unboxsym]
                    ),
                    (
                        DataConstructor::BlockKvList.tag(), // [l] [pair sym]
                        case(
                            local(0),
                            vec![(
                                DataConstructor::ListCons.tag(), // [h t] [l] [pair sym]
                                unbox_sym(
                                    local(0), // [unbox_h] [h t] [l] [pair sym]
                                    Eq.global(lref(0), lref(5)),
                                ),
                            )],
                            f(),
                        ),
                    ),
                ],
                call::bif::panic(str("bad key-value pair in MATCHES_KEY")),
            ),
            annotation,
        )
    }
}

impl CallGlobal2 for MatchesKey {}

/// EXTRACT_VALUE
///
/// If the argument is a block key value form, return the value.
pub struct ExtractValue;

impl StgIntrinsic for ExtractValue {
    fn name(&self) -> &str {
        "EXTRACT_VALUE"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;
        annotated_lambda(
            1, // [pair]
            case(
                local(0),
                vec![
                    (
                        DataConstructor::BlockPair.tag(), // [k v] [pair]
                        local(1),
                    ),
                    (
                        DataConstructor::BlockKvList.tag(), // [l] [pair]
                        switch(
                            local(0),
                            vec![(
                                DataConstructor::ListCons.tag(), // [h t] [l] [pair]
                                switch(
                                    local(1),
                                    vec![(
                                        DataConstructor::ListCons.tag(), // [h t] [h t] [l] [pair]
                                        local(0),
                                    )],
                                ),
                            )],
                        ),
                    ),
                ],
                call::bif::panic(str("bad key-value pair in EXTRACT_VALUE")),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for ExtractValue {}

/// EXTRACT_KEY(kv)
///
/// If the argument is a block key value form, return the unboxed key symbol.
pub struct ExtractKey;

impl StgIntrinsic for ExtractKey {
    fn name(&self) -> &str {
        "EXTRACT_KEY"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;
        annotated_lambda(
            1, // [pair]
            case(
                local(0),
                vec![
                    (
                        DataConstructor::BlockPair.tag(), // [k v] [pair]
                        local(0),
                    ),
                    (
                        DataConstructor::BlockKvList.tag(), // [l] [pair]
                        switch(
                            local(0),
                            vec![(
                                DataConstructor::ListCons.tag(), // [h t] [l] [pair]
                                unbox_sym(local(0), local(0)),
                            )],
                        ),
                    ),
                ],
                call::bif::panic(str("bad key-value pair in EXTRACT_KEY")),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for ExtractKey {}

/// PACK_PAIR(kv)
///
/// Packs a kv pair into an outer BLOCK_PAIR(k, kv) for processing by
/// merge (so that the kv can be exposed again in the new block)
pub struct PackPair;

impl StgIntrinsic for PackPair {
    fn name(&self) -> &str {
        "PACK_PAIR"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;
        annotated_lambda(
            1, // [kv]
            force(
                ExtractKey.global(lref(0)), // [sym] [kv]
                data(DataConstructor::BlockPair.tag(), vec![lref(0), lref(1)]),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for PackPair {}

/// BLOCK_PAIR(kv)
///
/// Force a KV into a block pair representation
pub struct BlockPair;

impl StgIntrinsic for BlockPair {
    fn name(&self) -> &str {
        "BLOCK_PAIR"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;
        annotated_lambda(
            1, // [kv]
            switch(
                local(0),
                vec![
                    (
                        DataConstructor::BlockPair.tag(),
                        // [k v] [kv]
                        local(2),
                    ),
                    (
                        DataConstructor::BlockKvList.tag(), // [lcons] [kv]
                        switch(
                            local(0),
                            vec![(
                                DataConstructor::ListCons.tag(), // [k t] [lcons] [kv]
                                switch(
                                    local(1),
                                    vec![(
                                        DataConstructor::ListCons.tag(), // [v .] [k t] [lcons] [kv]
                                        data(
                                            DataConstructor::BlockPair.tag(),
                                            vec![lref(2), lref(0)],
                                        ),
                                    )],
                                ),
                            )],
                        ),
                    ),
                ],
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for BlockPair {}

/// Threshold for building a block index. Blocks with at least this
/// many elements will have an index built on first lookup.
const BLOCK_INDEX_THRESHOLD: usize = 16;

/// LOOKUPOR(key, default, obj) is lookup with default
///
/// Uses a hybrid approach: first attempts an O(1) index lookup via
/// a BIF, then falls back to a linear STG find loop if no index is
/// available. Lazy indexing builds the index on first lookup for
/// blocks with >= BLOCK_INDEX_THRESHOLD elements.
pub struct LookupOr(pub NativeVariant);

impl StgIntrinsic for LookupOr {
    fn name(&self) -> &str {
        match self.0 {
            NativeVariant::Boxed => "LOOKUPOR",
            NativeVariant::Unboxed => "LOOKUPOR#",
        }
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;

        let bif_index: u8 = intrinsics::index(self.name())
            .expect("LOOKUPOR must be registered")
            .try_into()
            .unwrap();

        let find = lambda(
            4, // [list k d find]
            case(
                local(0),
                vec![
                    (
                        DataConstructor::ListCons.tag(), // [h t] [list k d find]
                        switch(
                            MatchesKey.global(lref(0), lref(3)),
                            vec![
                                (
                                    DataConstructor::BoolTrue.tag(),
                                    // [h t] [list k d find]
                                    ExtractValue.global(lref(0)),
                                ),
                                (
                                    DataConstructor::BoolFalse.tag(),
                                    app(lref(5), vec![lref(1), lref(3), lref(4), lref(5)]),
                                ),
                            ],
                        ),
                    ),
                    (
                        DataConstructor::ListNil.tag(), // [list k d] [find]
                        local(2),
                    ),
                ],
                call::bif::panic(str("bad block content")),
            ),
        );

        annotated_lambda(
            3, // [k d block]
            switch(
                local(2),
                vec![(
                    DataConstructor::Block.tag(),
                    // [blocklist blockindex] [k d block]
                    letrec_(
                        vec![find], // [find] [blocklist blockindex] [k d block]
                        match self.0 {
                            NativeVariant::Unboxed => {
                                // Try index lookup via BIF first
                                // env: [find] [blocklist blockindex] [k d block]
                                // BIF args: sym=L(3)=k, blocklist=L(1), blockindex=L(2), block=L(5)
                                case(
                                    app_bif(bif_index, vec![lref(3), lref(1), lref(2), lref(5)]),
                                    vec![
                                        (
                                            DataConstructor::ListCons.tag(),
                                            // [value _] [find] [blocklist blockindex] [k d block]
                                            local(0),
                                        ),
                                        (
                                            DataConstructor::ListNil.tag(),
                                            // [] [find] [blocklist blockindex] [k d block]
                                            app(lref(0), vec![lref(1), lref(3), lref(4), lref(0)]),
                                        ),
                                    ],
                                    // fallback (native return — shouldn't happen)
                                    // [native] [find] [blocklist blockindex] [k d block]
                                    app(lref(1), vec![lref(2), lref(4), lref(5), lref(1)]),
                                )
                            }
                            NativeVariant::Boxed => {
                                unbox_sym(
                                    local(3),
                                    // [sym] [find] [blocklist blockindex] [k d block]
                                    // BIF args: sym=L(0), blocklist=L(2), blockindex=L(3), block=L(6)
                                    case(
                                        app_bif(
                                            bif_index,
                                            vec![lref(0), lref(2), lref(3), lref(6)],
                                        ),
                                        vec![
                                            (
                                                DataConstructor::ListCons.tag(),
                                                // [value _] [sym] [find] [blocklist blockindex] [k d block]
                                                local(0),
                                            ),
                                            (
                                                DataConstructor::ListNil.tag(),
                                                // [] [sym] [find] [blocklist blockindex] [k d block]
                                                app(
                                                    lref(1),
                                                    vec![lref(2), lref(0), lref(5), lref(1)],
                                                ),
                                            ),
                                        ],
                                        // fallback (native return — shouldn't happen)
                                        // [native] [sym] [find] [blocklist blockindex] [k d block]
                                        app(lref(2), vec![lref(3), lref(1), lref(6), lref(2)]),
                                    ),
                                )
                            }
                        },
                    ),
                )],
            ),
            annotation,
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args: [sym_key, blocklist, blockindex, block]
        // Check for an existing index — use ok() for sym key since it
        // may be an unforced thunk (BIF can't force thunks)
        if let Ok(Native::Index(ref map)) = machine.nav(view).resolve_native(&args[2]) {
            if let Ok(Native::Sym(sym_id)) = machine.nav(view).resolve_native(&args[0]) {
                if let Some(&position) = map.get(&sym_id) {
                    if let Some(value) = walk_list_to_position(machine, view, &args[1], position) {
                        let value_env =
                            view.from_closure(value, machine.root_env(), Smid::default())?;
                        let nil_ref = Ref::V(Native::Num(serde_json::Number::from(0)));
                        let cons = view.data(
                            DataConstructor::ListCons.tag(),
                            Array::from_slice(&view, &[Ref::L(0), nil_ref]),
                        )?;
                        return machine.set_closure(SynClosure::new(cons.as_ptr(), value_env));
                    }
                }
            }
            // Index exists but key not found or not resolved — return ListNil
            let nil = view.nil()?;
            return machine.set_closure(SynClosure::new(nil.as_ptr(), machine.root_env()));
        }

        // No index — check if we should build one
        let count = count_list(machine, view, &args[1]);

        if count >= BLOCK_INDEX_THRESHOLD {
            // Build index
            let map = build_index(machine, view, &args[1]);

            // Try lookup — but the key may be an unforced thunk, so
            // use ok() rather than ? to gracefully fall back to find loop
            if let Ok(Native::Sym(sym_id)) = machine.nav(view).resolve_native(&args[0]) {
                if let Some(&position) = map.get(&sym_id) {
                    if let Some(value) = walk_list_to_position(machine, view, &args[1], position) {
                        // Store the index in the block for future lookups
                        store_index_in_block(view, machine, &args[3], map);

                        let value_env =
                            view.from_closure(value, machine.root_env(), Smid::default())?;
                        let nil_ref = Ref::V(Native::Num(serde_json::Number::from(0)));
                        let cons = view.data(
                            DataConstructor::ListCons.tag(),
                            Array::from_slice(&view, &[Ref::L(0), nil_ref]),
                        )?;
                        return machine.set_closure(SynClosure::new(cons.as_ptr(), value_env));
                    }
                }
                // Key not found — store index for future lookups
                store_index_in_block(view, machine, &args[3], map);
            }
            // Key wasn't a native sym (unforced thunk) — delegate to find loop
        }

        // No index available or below threshold — return ListNil to signal "use find loop"
        let nil = view.nil()?;
        machine.set_closure(SynClosure::new(nil.as_ptr(), machine.root_env()))
    }
}

/// An iterator over a cons-list that resolves all ref types (L, V, G).
///
/// Unlike DataIterator, this handles lists whose cons cells may
/// contain non-local refs (e.g. global refs to K[] / KEmptyList).
/// Uses the HeapNavigator for global resolution, avoiding heap
/// allocation.
struct BlockListIterator<'a, 'scope> {
    closure: SynClosure,
    nav: &'a HeapNavigator<'scope>,
    done: bool,
}

impl Iterator for BlockListIterator<'_, '_> {
    type Item = SynClosure;

    fn next(&mut self) -> Option<Self::Item> {
        use std::convert::TryInto;

        if self.done {
            return None;
        }

        let code = self.nav.view.scoped(self.closure.code());
        match &*code {
            HeapSyn::Cons { tag, args } => match (*tag).try_into() {
                Ok(DataConstructor::ListCons) => {
                    let h = args.get(0)?;
                    let t = args.get(1)?;

                    let head = self.nav.resolve_in_closure(&self.closure, h)?;

                    match self.nav.resolve_in_closure(&self.closure, t) {
                        Some(tail) => self.closure = tail,
                        None => self.done = true,
                    }

                    Some(head)
                }
                Ok(DataConstructor::ListNil) => None,
                _ => None,
            },
            _ => None,
        }
    }
}

/// Walk a cons-list to the given position and extract the value from the pair.
fn walk_list_to_position(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    blocklist_ref: &Ref,
    position: usize,
) -> Option<SynClosure> {
    let nav = machine.nav(view);
    let closure = nav.resolve(blocklist_ref).ok()?;
    let pair = (BlockListIterator {
        closure,
        nav: &nav,
        done: false,
    })
    .nth(position)?;
    extract_value_from_pair(machine, view, &pair)
}

/// Extract the value from a block pair closure.
///
/// Only handles BlockPair form (which is what the index maps).
fn extract_value_from_pair(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    pair: &SynClosure,
) -> Option<SynClosure> {
    use crate::eval::memory::syntax::HeapSyn;

    let code = view.scoped(pair.code());
    match &*code {
        HeapSyn::Cons { tag, args } if *tag == DataConstructor::BlockPair.tag() => {
            let v = args.get(1)?;
            machine.nav(view).resolve_in_closure(pair, v)
        }
        _ => None,
    }
}

/// Count elements in a cons-list
fn count_list(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    blocklist_ref: &Ref,
) -> usize {
    let nav = machine.nav(view);
    match nav.resolve(blocklist_ref) {
        Ok(closure) => (BlockListIterator {
            closure,
            nav: &nav,
            done: false,
        })
        .count(),
        Err(_) => 0,
    }
}

/// Build a block index (HashMap<SymbolId, usize>) from a cons-list of pairs
fn build_index(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    blocklist_ref: &Ref,
) -> std::collections::HashMap<crate::eval::memory::symbol::SymbolId, usize> {
    let mut map = std::collections::HashMap::new();

    let nav = machine.nav(view);
    let closure = match nav.resolve(blocklist_ref) {
        Ok(c) => c,
        Err(_) => return map,
    };

    let iter = BlockListIterator {
        closure,
        nav: &nav,
        done: false,
    };

    for (position, pair) in iter.enumerate() {
        if let Some(sym_id) = pair_key_symbol_id(view, &pair) {
            map.insert(sym_id, position);
        }
    }
    map
}

/// Extract the symbol ID from a block pair's key
fn pair_key_symbol_id(
    view: MutatorHeapView<'_>,
    pair: &SynClosure,
) -> Option<crate::eval::memory::symbol::SymbolId> {
    use crate::eval::memory::syntax::HeapSyn;

    let code = view.scoped(pair.code());
    match &*code {
        HeapSyn::Cons { tag, args } if *tag == DataConstructor::BlockPair.tag() => {
            let k = args.get(0)?;
            let native = pair.navigate_local_native(&view, k);
            if let Native::Sym(id) = native {
                Some(id)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Store a freshly-built index back into the block's data structure.
///
/// Resolves the block ref to its Cons node and mutates args[1] from
/// Num(0) to Index(map). Safe because the block has already been
/// forced (we're inside its case branch) and the mutation only
/// replaces a sentinel value with an equivalent-shaped Ref::V.
fn store_index_in_block(
    view: MutatorHeapView<'_>,
    machine: &dyn IntrinsicMachine,
    block_ref: &Ref,
    map: std::collections::HashMap<crate::eval::memory::symbol::SymbolId, usize>,
) {
    if let Ok(closure) = machine.nav(view).resolve(block_ref) {
        let code_ptr = closure.code();
        // SAFETY: The block has been forced (we destructured it in a case
        // branch), so code_ptr points to a valid HeapSyn::Cons. We mutate
        // only args[1] which is the index slot — replacing the Num(0)
        // sentinel with an Index value. No other code reads this slot
        // concurrently during BIF execution.
        let heap_syn = unsafe { &mut *code_ptr.as_ptr() };
        if let HeapSyn::Cons { tag, ref mut args } = heap_syn {
            if *tag == DataConstructor::Block.tag() {
                let _ = args.set(1, Ref::V(Native::Index(std::rc::Rc::new(map))));
            }
        }
    }
}

impl CallGlobal3 for LookupOr {}

/// LOOKUP(k, block)
pub struct Lookup;

impl StgIntrinsic for Lookup {
    fn name(&self) -> &str {
        "LOOKUP"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;

        annotated_lambda(
            2, // [k block]
            unbox_sym(
                local(0),
                // [sym] [k block]
                let_(
                    vec![value(LookupFail.global(lref(1), lref(3)))],
                    // [fail] [sym] [k block]
                    LookupOr(NativeVariant::Unboxed).global(lref(1), lref(0), lref(3)),
                ),
            ),
            annotation,
        )
    }
}

impl CallGlobal2 for Lookup {}

/// MERGE(l, r)
///
/// Merge two blocks preserving order where possible and with values
/// from r overriding those in l
pub struct Merge;

/// Items are passed to the MERGE intrinsic as block_pairs of k and
/// the kv closure and to the MERGEWITH intrinsic as block_pairs of k
/// and v. The same function can deconstruct either.
fn deconstruct(
    view: MutatorHeapView,
    pool: &crate::eval::memory::symbol::SymbolPool,
    pair_closure: &SynClosure,
) -> Result<(String, SynClosure), ExecutionError> {
    use crate::eval::memory::syntax;

    let code = view.scoped(pair_closure.code());
    match &*code {
        syntax::HeapSyn::Cons { tag, args } if *tag == DataConstructor::BlockPair.tag() => {
            let k = args.get(0).unwrap();
            let kv = args.get(1).unwrap();

            let sym = if let syntax::Native::Sym(id) = pair_closure.navigate_local_native(&view, k)
            {
                pool.resolve(id).to_string()
            } else {
                panic!("bad block_pair passed to merge intrinsic: non-symbolic key")
            };

            let kv_closure = pair_closure.navigate_local(&view, kv);

            Ok((sym, kv_closure))
        }
        _ => {
            panic!("bad block_pair passed to merge intrinsic: non-data type")
        }
    }
}

impl StgIntrinsic for Merge {
    fn name(&self) -> &str {
        "MERGE"
    }

    /// Expose the two lists to the intrinsic
    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;

        let pack_items = lambda(
            2, // [list self]
            switch(
                local(0),
                vec![
                    (
                        DataConstructor::ListCons.tag(), // [h t] [list self]
                        force(
                            PackPair.global(lref(0)),
                            // [pp-h] [h t] [list self]
                            force(
                                app(lref(4), vec![lref(2), lref(4)]),
                                // [p-t] [pp-h] [h t] [list self]
                                data(DataConstructor::ListCons.tag(), vec![lref(1), lref(0)]),
                            ),
                        ),
                    ),
                    (DataConstructor::ListNil.tag(), local(0)),
                ],
            ),
        );

        annotated_lambda(
            2, // [l r]
            switch(
                local(0),
                vec![(
                    DataConstructor::Block.tag(), // [lcons lindex] [l r]
                    switch(
                        local(3),
                        vec![(
                            DataConstructor::Block.tag(), // [rcons rindex] [lcons lindex] [l r]
                            let_(
                                vec![pack_items],
                                // [pack] [rcons rindex] [lcons lindex]
                                force(
                                    app(lref(0), vec![lref(3), lref(0)]),
                                    // [p-l] [pack] [rcons rindex] [lcons lindex]
                                    force(
                                        app(lref(1), vec![lref(2), lref(1)]),
                                        // [p-r] [p-l] [pack] [rcons rindex] [lcons lindex]
                                        force(
                                            call::bif::merge(lref(1), lref(0)),
                                            data(
                                                DataConstructor::Block.tag(),
                                                vec![lref(0), no_index()],
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        )],
                    ),
                )],
            ),
            annotation,
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let l = data_list_arg(machine, view, args[0].clone())?;
        let r = data_list_arg(machine, view, args[1].clone())?;

        let mut merge: IndexMap<String, SynClosure> = IndexMap::new();

        for item in l {
            let item = item?;
            let (k, kv) = deconstruct(view, machine.symbol_pool(), &item)?;
            merge.insert(k, kv);
        }

        for item in r {
            let item = item?;
            let (k, kv) = deconstruct(view, machine.symbol_pool(), &item)?;
            merge.insert(k, kv);
        }

        machine_return_closure_list(machine, view, merge.into_iter().map(|(_, v)| v).collect())
    }
}

impl CallGlobal2 for Merge {}

/// MERGEWITH(l, r, fn)
///
/// Merge two blocks preserving order where possible and with values
/// from r combined with those in l via fn(l, r)
pub struct MergeWith;

impl StgIntrinsic for MergeWith {
    fn name(&self) -> &str {
        "MERGEWITH"
    }

    /// Expose the two lists to the intrinsic
    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;

        let pair_items = lambda(
            2, // [list self]
            switch(
                local(0),
                vec![
                    (
                        DataConstructor::ListCons.tag(), // [h t] [list self]
                        force(
                            BlockPair.global(lref(0)),
                            // [bp-h] [h t] [list self]
                            force(
                                app(lref(4), vec![lref(2), lref(4)]),
                                // [p-t] [bp-h] [h t] [list self]
                                data(DataConstructor::ListCons.tag(), vec![lref(1), lref(0)]),
                            ),
                        ),
                    ),
                    (DataConstructor::ListNil.tag(), local(0)),
                ],
            ),
        );

        annotated_lambda(
            3, // [l r f]
            switch(
                local(0),
                vec![(
                    DataConstructor::Block.tag(), // [lcons lindex] [l r f]
                    switch(
                        local(3),
                        vec![(
                            DataConstructor::Block.tag(), // [rcons rindex] [lcons lindex] [l r f]
                            let_(
                                vec![pair_items],
                                // [pack] [rcons rindex] [lcons lindex] [l r f]
                                force(
                                    app(lref(0), vec![lref(3), lref(0)]),
                                    // [p-l] [pack] [rcons rindex] [lcons lindex] [l r f]
                                    force(
                                        app(lref(1), vec![lref(2), lref(1)]),
                                        // [p-r] [p-l] [pack] [rcons rindex] [lcons lindex] [l r f]
                                        force(
                                            call::bif::merge_with(lref(1), lref(0), lref(9)),
                                            data(
                                                DataConstructor::Block.tag(),
                                                vec![lref(0), no_index()],
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        )],
                    ),
                )],
            ),
            annotation,
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let l = data_list_arg(machine, view, args[0].clone())?;
        let r = data_list_arg(machine, view, args[1].clone())?;
        let f = args[2].clone();

        let mut merge: IndexMap<String, SynClosure> = IndexMap::new();

        for item in l {
            let item = item?;
            let (key, value) = deconstruct(view, machine.symbol_pool(), &item)?;
            merge.insert(key, value);
        }

        for item in r {
            let item = item?;
            let (key, nv) = deconstruct(view, machine.symbol_pool(), &item)?;
            if let Some(ov) = merge.get_mut(&key) {
                let args = [ov.clone(), nv];
                let mut combined = SynClosure::new(
                    view.app(f.bump(2), Array::from_slice(&view, &[Ref::L(0), Ref::L(1)]))?
                        .as_ptr(),
                    view.from_closures(
                        args.iter().cloned(),
                        2,
                        machine.env(view),
                        Smid::default(),
                    )?,
                );
                swap(ov, &mut combined);
            } else {
                merge.insert(key, nv);
            }
        }

        machine_return_block_pair_closure_list(machine, view, merge)
    }
}

impl CallGlobal3 for MergeWith {}

/// DEEPMERGE(l, r, fn)
///
/// Merge two blocks, recursing into any subblocks. If either `l` or
/// `r` is not a block, return `r` — the right-hand side takes
/// precedence at every level, consistent with the `<<` operator
/// semantics ("`r` over `l`").
pub struct DeepMerge;

impl StgIntrinsic for DeepMerge {
    fn name(&self) -> &str {
        "DEEPMERGE"
    }

    /// Deep merge operation
    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;

        annotated_lambda(
            2,
            case(
                local(0),
                vec![(
                    DataConstructor::Block.tag(),
                    // [lcons lindex] [l r]
                    case(
                        local(3),
                        vec![(
                            DataConstructor::Block.tag(),
                            // [rcons rindex] [lcons lindex] [l r]
                            MergeWith.global(lref(4), lref(5), gref(self.index())),
                        )],
                        // [r] [lcons lindex] [l r]
                        local(0),
                    ),
                )],
                // [l] [l r]
                local(2),
            ),
            annotation,
        )
    }
}

impl CallGlobal3 for DeepMerge {}

/// ISBLOCK(value)
///
/// Return true if the value is a block, false otherwise
pub struct IsBlock;

impl StgIntrinsic for IsBlock {
    fn name(&self) -> &str {
        "ISBLOCK"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        use crate::eval::memory::syntax;
        let closure = machine.nav(view).resolve(&args[0])?;
        let code = view.scoped(closure.code());
        let is_block = matches!(
            &*code,
            syntax::HeapSyn::Cons { tag, .. } if *tag == DataConstructor::Block.tag()
        );
        machine_return_bool(machine, view, is_block)
    }
}

impl CallGlobal1 for IsBlock {}

/// Levenshtein edit distance between two strings
fn edit_distance(a: &str, b: &str) -> usize {
    let a_len = a.len();
    let b_len = b.len();
    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }
    let mut prev: Vec<usize> = (0..=b_len).collect();
    let mut curr = vec![0; b_len + 1];
    for (i, ca) in a.chars().enumerate() {
        curr[0] = i + 1;
        for (j, cb) in b.chars().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            curr[j + 1] = (prev[j] + cost).min(prev[j + 1] + 1).min(curr[j] + 1);
        }
        swap(&mut prev, &mut curr);
    }
    prev[b_len]
}

/// Collect all key names from a block as strings
fn collect_block_keys(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    block_closure: &SynClosure,
) -> Vec<String> {
    let nav = machine.nav(view);
    let code = view.scoped(block_closure.code());
    let blocklist_ref = match &*code {
        HeapSyn::Cons { tag, args } if *tag == DataConstructor::Block.tag() => args.get(0),
        _ => None,
    };
    let list_closure = blocklist_ref.and_then(|r| nav.resolve_in_closure(block_closure, r));
    let Some(list_closure) = list_closure else {
        return Vec::new();
    };
    let iter = BlockListIterator {
        closure: list_closure,
        nav: &nav,
        done: false,
    };
    let pool = machine.symbol_pool();
    iter.filter_map(|pair| {
        let sym_id = pair_key_symbol_id(view, &pair)?;
        Some(pool.resolve(sym_id).to_string())
    })
    .collect()
}

/// Find similar keys using edit distance
fn suggest_similar(key: &str, available: &[String], max_suggestions: usize) -> Vec<String> {
    let key_len = key.len();
    let threshold = (key_len * 2 / 5).max(2);
    let mut candidates: Vec<(usize, &str)> = available
        .iter()
        .filter_map(|k| {
            let dist = edit_distance(key, k);
            if dist > 0 && dist <= threshold {
                Some((dist, k.as_str()))
            } else {
                None
            }
        })
        .collect();
    candidates.sort_by_key(|(dist, _)| *dist);
    candidates
        .into_iter()
        .take(max_suggestions)
        .map(|(_, k)| k.to_string())
        .collect()
}

/// Format a key-not-found error message with optional suggestions
fn format_key_not_found(key: &str, suggestions: &[String]) -> String {
    if suggestions.is_empty() {
        format!("key '{key}' not found in block")
    } else if suggestions.len() == 1 {
        format!(
            "key '{key}' not found in block (did you mean '{}'?)",
            suggestions[0]
        )
    } else {
        let quoted: Vec<String> = suggestions.iter().map(|n| format!("'{n}'")).collect();
        format!(
            "key '{key}' not found in block (did you mean one of: {}?)",
            quoted.join(", ")
        )
    }
}

/// LOOKUPFAIL(key, block)
///
/// Called when a key lookup fails. Collects available keys from the
/// block, computes edit distances, and produces a helpful error
/// message with "did you mean?" suggestions.
pub struct LookupFail;

impl StgIntrinsic for LookupFail {
    fn name(&self) -> &str {
        "LOOKUPFAIL"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let nav = machine.nav(view);
        let key_name = match nav.resolve_native(&args[0]) {
            Ok(Native::Sym(sym_id)) => machine.symbol_pool().resolve(sym_id).to_string(),
            _ => "?".to_string(),
        };
        let block_closure = nav.resolve(&args[1])?;
        let available_keys = collect_block_keys(machine, view, &block_closure);
        let suggestions = suggest_similar(&key_name, &available_keys, 3);
        let message = format_key_not_found(&key_name, &suggestions);
        Err(ExecutionError::Panic(message))
    }
}

impl CallGlobal2 for LookupFail {}

/// Compile a thunk that calls LOOKUPFAIL with the key and block reference
///
/// This is used from the compile-time path when a static key lookup
/// cannot find a matching key. At runtime, LookupFail will collect the
/// available keys from the block and produce a helpful error message
/// with "did you mean?" suggestions.
pub fn lookup_fail_thunk(key: &str, obj_ref: super::syntax::Ref) -> Rc<StgSyn> {
    use dsl::*;
    let_(
        vec![value(box_sym(key))],
        LookupFail.global(lref(0), obj_ref.bump(1)),
    )
}

/// Compile a panic for a missing key
pub fn panic_key_not_found(key: &str) -> Rc<StgSyn> {
    use dsl::*;

    let_(
        vec![value(box_str(format!("key '{}' not found in block", key)))],
        Panic.global(lref(0)),
    )
}

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::eval::stg::{
        constant::KEmptyList, eq::Eq, panic::Panic, runtime::Runtime, syntax::dsl::*, testing,
    };

    pub fn runtime() -> Box<dyn Runtime> {
        testing::runtime(vec![
            Box::new(Block),
            Box::new(Kv),
            Box::new(MatchesKey),
            Box::new(ExtractValue),
            Box::new(LookupOr(NativeVariant::Boxed)),
            Box::new(LookupOr(NativeVariant::Unboxed)),
            Box::new(Panic),
            Box::new(Eq),
            Box::new(KEmptyList),
        ])
    }

    #[test]
    pub fn test_simple_kv_match() {
        let syntax = letrec_(
            vec![
                value(box_str("value")),
                value(data(
                    DataConstructor::BlockPair.tag(),
                    vec![sym("key"), lref(0)],
                )),
                value(Kv.global(lref(1))),
            ],
            MatchesKey.global(lref(2), sym("key")),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.bool_return(), Some(true));
    }

    #[test]
    pub fn test_simple_kv_mismatch() {
        let syntax = letrec_(
            vec![
                value(box_str("value")),
                value(data(
                    DataConstructor::BlockPair.tag(),
                    vec![sym("key"), lref(0)],
                )),
                value(Kv.global(lref(1))),
            ],
            MatchesKey.global(lref(2), sym("different")),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.bool_return(), Some(false));
    }

    #[test]
    pub fn test_simple_kvlist_match() {
        let syntax = letrec_(
            vec![
                value(box_sym("key")),
                value(box_str("value")),
                value(data(
                    DataConstructor::ListCons.tag(),
                    vec![lref(1), KEmptyList.gref()],
                )),
                value(data(
                    DataConstructor::ListCons.tag(),
                    vec![lref(0), lref(2)],
                )),
                value(Kv.global(lref(3))),
            ],
            MatchesKey.global(lref(4), sym("key")),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.bool_return(), Some(true));
    }

    /// Helper to build a block with `n` keys (k0..kN) and string values (v0..vN).
    /// Returns (letrec_bindings, block_ref_index) — the block is at lref(block_ref_index).
    fn build_n_key_block(n: usize) -> (Vec<LambdaForm>, usize) {
        let mut bindings: Vec<LambdaForm> = Vec::new();

        // For each key, create: value, pair, kv
        // Then chain into a cons-list and wrap in Block
        let mut kv_indices = Vec::new();
        for i in 0..n {
            let val_idx = bindings.len();
            bindings.push(value(box_str(format!("v{i}"))));
            bindings.push(value(data(
                DataConstructor::BlockPair.tag(),
                vec![sym(format!("k{i}")), lref(val_idx)],
            )));
            let kv_idx = bindings.len();
            bindings.push(value(Kv.global(lref(val_idx + 1))));
            kv_indices.push(kv_idx);
        }

        // Build cons list from back to front
        let mut list_idx = {
            // Start with nil (KEmptyList global ref)
            let nil_idx = bindings.len();
            bindings.push(value(data(
                DataConstructor::ListCons.tag(),
                vec![lref(kv_indices[n - 1]), KEmptyList.gref()],
            )));
            nil_idx
        };

        for i in (0..n - 1).rev() {
            let new_idx = bindings.len();
            bindings.push(value(data(
                DataConstructor::ListCons.tag(),
                vec![lref(kv_indices[i]), lref(list_idx)],
            )));
            list_idx = new_idx;
        }

        // Wrap in Block
        let block_idx = bindings.len();
        bindings.push(value(data(
            DataConstructor::Block.tag(),
            vec![lref(list_idx), no_index()],
        )));

        (bindings, block_idx)
    }

    #[test]
    pub fn test_block_lookup_below_threshold() {
        // 15 keys — below BLOCK_INDEX_THRESHOLD (16)
        let (mut bindings, block_idx) = build_n_key_block(15);

        // Lookup k0 (first key)
        let key_idx = bindings.len();
        bindings.push(value(box_sym("k0")));
        let default_idx = bindings.len();
        bindings.push(value(box_str("fail")));
        let lookup_idx = bindings.len();
        bindings.push(value(LookupOr(NativeVariant::Boxed).global(
            lref(key_idx),
            lref(default_idx),
            lref(block_idx),
        )));

        let syntax = letrec_(
            bindings,
            case(
                local(lookup_idx),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(5000)).unwrap();
        assert_eq!(m.string_return(), Some("v0".to_string()));
    }

    #[test]
    pub fn test_block_lookup_at_threshold() {
        // 16 keys — at BLOCK_INDEX_THRESHOLD, index should be built
        let (mut bindings, block_idx) = build_n_key_block(16);

        // Lookup k15 (last key)
        let key_idx = bindings.len();
        bindings.push(value(box_sym("k15")));
        let default_idx = bindings.len();
        bindings.push(value(box_str("fail")));
        let lookup_idx = bindings.len();
        bindings.push(value(LookupOr(NativeVariant::Boxed).global(
            lref(key_idx),
            lref(default_idx),
            lref(block_idx),
        )));

        let syntax = letrec_(
            bindings,
            case(
                local(lookup_idx),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(5000)).unwrap();
        assert_eq!(m.string_return(), Some("v15".to_string()));
    }

    #[test]
    pub fn test_block_lookup_above_threshold() {
        // 17 keys — above BLOCK_INDEX_THRESHOLD
        let (mut bindings, block_idx) = build_n_key_block(17);

        // Lookup k8 (middle key)
        let key_idx = bindings.len();
        bindings.push(value(box_sym("k8")));
        let default_idx = bindings.len();
        bindings.push(value(box_str("fail")));
        let lookup_idx = bindings.len();
        bindings.push(value(LookupOr(NativeVariant::Boxed).global(
            lref(key_idx),
            lref(default_idx),
            lref(block_idx),
        )));

        let syntax = letrec_(
            bindings,
            case(
                local(lookup_idx),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(5000)).unwrap();
        assert_eq!(m.string_return(), Some("v8".to_string()));
    }

    #[test]
    pub fn test_block_lookup_missing_key_below_threshold() {
        // 15 keys — missing key should return default
        let (mut bindings, block_idx) = build_n_key_block(15);

        let key_idx = bindings.len();
        bindings.push(value(box_sym("missing")));
        let default_idx = bindings.len();
        bindings.push(value(box_str("default-value")));
        let lookup_idx = bindings.len();
        bindings.push(value(LookupOr(NativeVariant::Boxed).global(
            lref(key_idx),
            lref(default_idx),
            lref(block_idx),
        )));

        let syntax = letrec_(
            bindings,
            case(
                local(lookup_idx),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(5000)).unwrap();
        assert_eq!(m.string_return(), Some("default-value".to_string()));
    }

    #[test]
    pub fn test_block_lookup_missing_key_above_threshold() {
        // 17 keys — missing key should return default (exercises index path)
        let (mut bindings, block_idx) = build_n_key_block(17);

        let key_idx = bindings.len();
        bindings.push(value(box_sym("missing")));
        let default_idx = bindings.len();
        bindings.push(value(box_str("default-value")));
        let lookup_idx = bindings.len();
        bindings.push(value(LookupOr(NativeVariant::Boxed).global(
            lref(key_idx),
            lref(default_idx),
            lref(block_idx),
        )));

        let syntax = letrec_(
            bindings,
            case(
                local(lookup_idx),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(5000)).unwrap();
        assert_eq!(m.string_return(), Some("default-value".to_string()));
    }

    #[test]
    pub fn test_simple_block_lookup() {
        let syntax = letrec_(
            vec![
                value(box_str("v1")),
                value(data(
                    DataConstructor::BlockPair.tag(),
                    vec![sym("k1"), lref(0)],
                )),
                value(Kv.global(lref(1))),
                value(box_str("v2")),
                value(data(
                    DataConstructor::BlockPair.tag(),
                    vec![sym("k2"), lref(0)],
                )),
                value(Kv.global(lref(4))),
                value(data(
                    DataConstructor::ListCons.tag(),
                    vec![lref(5), KEmptyList.gref()],
                )),
                value(data(
                    DataConstructor::ListCons.tag(),
                    vec![lref(2), lref(6)],
                )),
                value(data(
                    DataConstructor::Block.tag(),
                    vec![lref(7), no_index()],
                )),
                value(box_sym("k1")),
                value(box_str("fail")),
                value(LookupOr(NativeVariant::Boxed).global(lref(9), lref(10), lref(8))),
            ],
            case(
                local(11),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.string_return(), Some("v1".to_string()));
    }
}
