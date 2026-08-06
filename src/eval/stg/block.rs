//! Block intrinsics

use std::rc::Rc;

use indexmap::IndexMap;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        intrinsics,
        machine::intrinsic::{
            AbiClosure, CallGlobal1, CallGlobal2, CallGlobal3, IntrinsicMachine, StgIntrinsic,
        },
        memory::{
            mutator::MutatorHeapView,
            syntax::{Native, Ref},
        },
    },
};

use super::{
    eq::Eq,
    panic::Panic,
    runtime::NativeVariant,
    support::{call, data_list_arg, machine_return_block_pair_closure_list, machine_return_bool},
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
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

        lambda(
            1,
            letrec_(
                vec![
                    // [kvitems mapped] [list]
                    kv_items,
                    value(app(lref(0), vec![lref(2), lref(0)])),
                ],
                data(DataConstructor::Block.tag(), vec![lref(1), no_index()]),
            ),
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use dsl::*;

        lambda(
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use dsl::*;

        lambda(
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
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

        lambda(
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use dsl::*;
        lambda(
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use dsl::*;
        lambda(
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use dsl::*;
        lambda(
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
                                // The key (h) may be a BoxedSymbol constructor
                                // (from compiled literal symbols like :x) or a
                                // raw Atom { V(Sym(id)) } (from dynamically
                                // created symbols like `str.of sym`).  Use
                                // `case` to force h and handle both: the
                                // BoxedSymbol branch unboxes, the fallback
                                // passes through a raw sym atom as-is.
                                case(
                                    local(0),
                                    vec![(
                                        DataConstructor::BoxedSymbol.tag(),
                                        // [inner] [h t] [l] [pair]
                                        local(0),
                                    )],
                                    // default: h is a raw sym atom, return it
                                    local(0),
                                ),
                            )],
                        ),
                    ),
                ],
                call::bif::panic(str("bad key-value pair in EXTRACT_KEY")),
            ),
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use dsl::*;
        lambda(
            1, // [kv]
            force(
                ExtractKey.global(lref(0)), // [sym] [kv]
                data(DataConstructor::BlockPair.tag(), vec![lref(0), lref(1)]),
            ),
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use dsl::*;
        lambda(
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
                                        // Force the key to WHNF via
                                        // ExtractKey applied to the original
                                        // KV element, which handles both
                                        // BoxedSymbol and raw atom keys.
                                        // This ensures dynamically-created
                                        // keys (e.g. `str.of sym`) are fully
                                        // evaluated before building the
                                        // BlockPair for the merge intrinsic.
                                        force(
                                            ExtractKey.global(lref(5)),
                                            // [sym] [v .] [k t] [lcons] [kv]
                                            data(
                                                DataConstructor::BlockPair.tag(),
                                                vec![lref(0), lref(1)],
                                            ),
                                        ),
                                    )],
                                ),
                            )],
                        ),
                    ),
                ],
            ),
        )
    }
}

impl CallGlobal1 for BlockPair {}

/// LOOKUPOR(key, default, obj) is lookup with default
///
/// Delegates to a linear STG find loop. Used to also attempt an O(1)
/// index-lookup BIF first, with lazy indexing building the index on first
/// lookup for blocks at or above a size threshold — a mutable optimisation
/// only the (now-deleted) HeapSyn engine supported, since bytecode blocks
/// are template closures with no in-place mutation (eu-oufc).
pub struct LookupOr(pub NativeVariant);

impl StgIntrinsic for LookupOr {
    fn name(&self) -> &str {
        match self.0 {
            NativeVariant::Boxed => "LOOKUPOR",
            NativeVariant::Unboxed => "LOOKUPOR#",
        }
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
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

        // Use plain lambda (no annotation) so that the call-site annotation
        // set by the Ann node wrapping LookupOr at compile time is preserved
        // in self.annotation when the inner switch fires. This allows
        // NoBranchForDataTag (raised when obj is not a block) to carry the
        // user's source location rather than the synthetic LOOKUPOR# label.

        lambda(
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
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args: [sym_key, blocklist, blockindex, block]
        //
        // This BIF used to also implement a mutable block-index optimisation
        // (a `SymbolId -> position` cache mutated into the block's index slot
        // in place) for the HeapSyn engine. The bytecode engine never
        // supported it — blocks are template closures, so it always took
        // this branch and fell back to the STG-level find loop — and HeapSyn
        // was deleted by the Phase 4 collapse (eu-oufc), so the optimisation
        // path (and the `nav`/`root_env`/`set_closure` calls it needed) went
        // with it. Always signal "use the find loop" now.
        machine.return_closure_list(view, vec![])
    }
}

impl CallGlobal3 for LookupOr {}

/// SAFE_LOOKUP(obj, k) — safe key lookup with null propagation.
///
/// Returns the value at key `k` if `obj` is a block containing `k`,
/// otherwise returns `null` (Unit). Null-propagating: if `obj` is
/// `null` or any non-block value, returns `null` rather than erroring.
///
/// Delegates to the STG-level find loop, same as `LookupOr` (see its
/// comment) — the mutable block-index optimisation it used to also try was
/// HeapSyn-only and was deleted with it (eu-oufc).
pub struct SafeLookup(pub NativeVariant);

impl StgIntrinsic for SafeLookup {
    fn name(&self) -> &str {
        "SAFE_LOOKUP"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use dsl::*;

        let bif_index: u8 = intrinsics::index(self.name())
            .expect("SAFE_LOOKUP must be registered")
            .try_into()
            .unwrap();

        // find: [list k find] — linear search with no default arg; returns
        // Unit on miss.
        let find = lambda(
            3, // [list k find]
            case(
                local(0),
                vec![
                    (
                        DataConstructor::ListCons.tag(), // [h t] [list k find]
                        switch(
                            MatchesKey.global(lref(0), lref(3)),
                            vec![
                                (
                                    DataConstructor::BoolTrue.tag(),
                                    // [h t] [list k find]
                                    ExtractValue.global(lref(0)),
                                ),
                                (
                                    DataConstructor::BoolFalse.tag(),
                                    app(lref(4), vec![lref(1), lref(3), lref(4)]),
                                ),
                            ],
                        ),
                    ),
                    (
                        DataConstructor::ListNil.tag(), // key not found — return null
                        unit(),
                    ),
                ],
                call::bif::panic(str("bad block content")),
            ),
        );

        // Arguments are passed as (obj, k) from the prelude: __SAFE_LOOKUP(a, k)
        // so local(0) = obj, local(1) = k
        lambda(
            2, // [obj k]
            case(
                local(0), // scrutinise obj
                vec![(
                    DataConstructor::Block.tag(),
                    // Matched Block! env: [blocklist blockindex] [obj k]
                    // L(0)=blocklist, L(1)=blockindex, L(2)=obj, L(3)=k
                    letrec_(
                        vec![find], // [find] [blocklist blockindex] [obj k]
                        // L(0)=find, L(1)=blocklist, L(2)=blockindex, L(3)=obj, L(4)=k
                        match self.0 {
                            NativeVariant::Unboxed => {
                                // k is already an unboxed sym (L(4))
                                // BIF args: sym=L(4), blocklist=L(1), blockindex=L(2), block=L(3)
                                case(
                                    app_bif(bif_index, vec![lref(4), lref(1), lref(2), lref(3)]),
                                    vec![
                                        (
                                            DataConstructor::ListCons.tag(),
                                            // [value _] [find] [blocklist blockindex] [obj k]
                                            local(0),
                                        ),
                                        (
                                            DataConstructor::ListNil.tag(),
                                            // [] [find] [blocklist blockindex] [obj k]
                                            // call find(blocklist, k, find)
                                            app(lref(0), vec![lref(1), lref(4), lref(0)]),
                                        ),
                                    ],
                                    // fallback (native return — shouldn't happen)
                                    // [native] [find] [blocklist blockindex] [obj k]
                                    app(lref(1), vec![lref(2), lref(5), lref(1)]),
                                )
                            }
                            NativeVariant::Boxed => {
                                // k is a boxed sym at L(4); unbox it first
                                unbox_sym(
                                    local(4),
                                    // [sym] [find] [blocklist blockindex] [obj k]
                                    // L(0)=sym, L(1)=find, L(2)=blocklist, L(3)=blockindex, L(4)=obj, L(5)=k
                                    // BIF args: sym=L(0), blocklist=L(2), blockindex=L(3), block=L(4)
                                    case(
                                        app_bif(
                                            bif_index,
                                            vec![lref(0), lref(2), lref(3), lref(4)],
                                        ),
                                        vec![
                                            (
                                                DataConstructor::ListCons.tag(),
                                                // [value _] [sym] [find] [blocklist blockindex] [obj k]
                                                local(0),
                                            ),
                                            (
                                                DataConstructor::ListNil.tag(),
                                                // [] [sym] [find] [blocklist blockindex] [obj k]
                                                // call find(blocklist, sym, find)
                                                app(lref(1), vec![lref(2), lref(0), lref(1)]),
                                            ),
                                        ],
                                        // fallback (native return — shouldn't happen)
                                        // [native] [sym] [find] [blocklist blockindex] [obj k]
                                        app(lref(2), vec![lref(3), lref(1), lref(2)]),
                                    ),
                                )
                            }
                        },
                    ),
                )],
                unit(), // non-block (including null): return null
            ),
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args: [sym_key, blocklist, blockindex, block]
        // Same BIF logic as LookupOr (see its comment): the mutable
        // block-index optimisation was HeapSyn-only and was deleted with it
        // (eu-oufc). Always signal "use the find loop". The wrapper handles
        // the null-propagation for non-block values.
        machine.return_closure_list(view, vec![])
    }
}

impl CallGlobal2 for SafeLookup {}

/// LOOKUP(k, block)
pub struct Lookup;

impl StgIntrinsic for Lookup {
    fn name(&self) -> &str {
        "LOOKUP"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use dsl::*;

        lambda(
            2, // [k block]
            unbox_sym(
                local(0),
                // [sym] [k block]
                let_(
                    vec![thunk(LookupFail.global(lref(0), lref(2)))],
                    // [fail] [sym] [k block]
                    LookupOr(NativeVariant::Unboxed).global(lref(1), lref(0), lref(3)),
                ),
            ),
        )
    }
}

impl CallGlobal2 for Lookup {}

/// LOOKUP_FAIL(key_sym, block)
///
/// Generate a helpful "key not found" error with "did you mean?"
/// suggestions based on edit distance from the block's actual keys.
pub struct LookupFail;

impl StgIntrinsic for LookupFail {
    fn name(&self) -> &str {
        "LOOKUP_FAIL"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use dsl::*;

        // Use plain lambda (no annotation) so the call-site annotation
        // set by the Ann node in lookup_fail() is not overwritten when
        // the wrapper is entered. This allows LookupFailure errors to
        // carry the user's source location.

        lambda(
            2, // [sym block]
            force(
                local(1), // force block
                // [forced-block] [sym block]
                app_bif(
                    intrinsics::index(self.name())
                        .expect("LOOKUP_FAIL must be registered")
                        .try_into()
                        .unwrap(),
                    vec![lref(1), lref(0)],
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
        // Resolve the key symbol to a string. The sym arg may arrive as
        // either a direct Ref::V(Native::Sym) or as a closure wrapping
        // a native value. Uses the neutral `resolve_native` ABI so it
        // serves both the HeapSyn and bytecode engines.
        let key_name = match &args[0] {
            Ref::V(Native::Sym(id)) => machine.symbol_pool().resolve(*id).to_string(),
            other => match machine.resolve_native(view, other) {
                Ok(Native::Sym(id)) => machine.symbol_pool().resolve(id).to_string(),
                _ => "<unknown>".to_string(),
            },
        };

        // Collect keys from the block. The block arg has been forced by
        // the wrapper, so it should be a Block cons cell accessible via
        // resolve.
        let available_keys = collect_block_keys(machine, view, &args[1]);

        // Compute suggestions via edit distance
        let max_distance = (key_name.len() / 2).clamp(2, 4);
        let suggestions =
            crate::eval::error::suggest_similar(&key_name, &available_keys, 3, max_distance);

        Err(ExecutionError::LookupFailure(
            machine.annotation(),
            Box::new((key_name, suggestions, available_keys)),
        ))
    }
}

impl CallGlobal2 for LookupFail {}

/// Collect the key names of a (forced) block value, engine-neutrally.
///
/// Walks the block's `ListCons`/`ListNil` spine of `BlockPair`s via the
/// neutral `data_tag`/`data_field`/`value_native` ABI (forcing each spine
/// cell), so it serves both the HeapSyn and bytecode engines. This is
/// best-effort — it feeds the "did you mean?" hint on a lookup failure, so
/// any structural surprise simply yields the keys gathered so far.
fn collect_block_keys(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    block_ref: &Ref,
) -> Vec<String> {
    let mut keys = Vec::new();

    // Walk the kv-pair spine one cell at a time, re-deriving each cell from
    // the rooted `block_ref` (a GC-stable `Ref` into the machine env) rather
    // than carrying a spine handle across the `force` that reads each pair's
    // key. A forced sub-evaluation can evacuate the heap, so any `AbiClosure`
    // held across it would dangle (eu-f3ss); re-resolving from the root after
    // every force keeps every handle fresh. This mirrors `get_list_element_at`
    // in debug.rs. It is the error path (a lookup has already failed), so the
    // extra spine walks are cheap.
    for index in 0.. {
        match nth_block_pair_cell(machine, view, block_ref, index) {
            Some(cell) => {
                if let Some(key) = pair_key_name(machine, view, &cell) {
                    keys.push(key);
                }
            }
            None => break,
        }
    }

    keys
}

/// Re-derive the `index`-th `ListCons` cell of a block's kv-pair spine from the
/// rooted `block_ref`, forcing the spine as far as needed. Returns `None` once
/// the spine ends (nil), on any error, or on any non-list shape.
///
/// Deriving from the GC-stable `Ref` on each call — rather than caching a spine
/// handle across the per-pair `force` in `collect_block_keys` — keeps the walk
/// safe against heap evacuation during those forces (eu-f3ss). The walk itself
/// holds no handle across an allocation: each `force` consumes the handle it is
/// given, and the intervening `data_field` reads do not allocate.
fn nth_block_pair_cell(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    block_ref: &Ref,
    index: usize,
) -> Option<AbiClosure> {
    let block = machine.resolve_closure(view, block_ref).ok()?;
    let block = machine.force(block).ok()?;
    if machine.data_tag(view, &block) != Some(DataConstructor::Block.tag()) {
        return None;
    }
    let mut current = machine.data_field(view, &block, 0)?;
    for _ in 0..index {
        let cell = machine.force(current).ok()?;
        if machine.data_tag(view, &cell) != Some(DataConstructor::ListCons.tag()) {
            return None;
        }
        current = machine.data_field(view, &cell, 1)?;
    }
    let cell = machine.force(current).ok()?;
    if machine.data_tag(view, &cell) != Some(DataConstructor::ListCons.tag()) {
        return None;
    }
    Some(cell)
}

/// Best-effort read of a kv-list cell's head `BlockPair` key as a string,
/// via the neutral ABI. Returns `None` on any non-`BlockPair` / non-symbol
/// shape.
fn pair_key_name(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    cell: &AbiClosure,
) -> Option<String> {
    let head = machine.data_field(view, cell, 0)?;
    let head = machine.force(head).ok()?;
    if machine.data_tag(view, &head) != Some(DataConstructor::BlockPair.tag()) {
        return None;
    }
    let key = machine.data_field(view, &head, 0)?;
    match machine.value_native(view, &key)? {
        Native::Sym(id) => Some(machine.symbol_pool().resolve(id).to_string()),
        _ => None,
    }
}

/// MERGE(l, r)
///
/// Merge two blocks preserving order where possible and with values
/// from r overriding those in l
pub struct Merge;

/// Items are passed to the MERGE intrinsic as block_pairs of k and
/// the kv closure and to the MERGEWITH intrinsic as block_pairs of k
/// and v. The same function can deconstruct either.
///
/// Engine-neutral: reads the pair's key (field 0) and value (field 1) via the
/// `data_tag`/`data_field`/`value_native` ABI, so it serves both engines.
fn deconstruct(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    pair: &AbiClosure,
) -> Result<(String, AbiClosure), ExecutionError> {
    if machine.data_tag(view, pair) != Some(DataConstructor::BlockPair.tag()) {
        return Err(ExecutionError::Panic(
            Smid::default(),
            "bad block_pair passed to merge intrinsic: non-data type".to_string(),
        ));
    }

    // Key (field 0): a raw or boxed symbol, surfaced as a `Native::Sym` by
    // `value_native`.
    let key = machine.data_field(view, pair, 0).ok_or_else(|| {
        ExecutionError::Panic(Smid::default(), "block pair missing key".to_string())
    })?;
    let sym = match machine.value_native(view, &key) {
        Some(Native::Sym(id)) => machine.symbol_pool().resolve(id).to_string(),
        _ => {
            return Err(ExecutionError::Panic(
                Smid::default(),
                "bad block_pair passed to merge intrinsic: non-symbolic key".to_string(),
            ))
        }
    };

    // Value (field 1): the kv closure (MERGE) or bare value (MERGEWITH).
    let value = machine.data_field(view, pair, 1).ok_or_else(|| {
        ExecutionError::Panic(
            Smid::default(),
            "failed to resolve block pair value in merge".to_string(),
        )
    })?;

    Ok((sym, value))
}

impl StgIntrinsic for Merge {
    fn name(&self) -> &str {
        "MERGE"
    }

    /// Expose the two lists to the intrinsic, preserving metadata.
    ///
    /// Uses `demeta` to capture metadata from both operands before
    /// pattern-matching blocks, then re-attaches the correct metadata
    /// to the merged result. For shallow merge, RHS metadata wins when
    /// both operands carry metadata.
    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
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

        // merge_core: lambda(2, [l_blk, r_blk]) — merges two bare blocks
        // (no metadata), returning a bare Block. Indices within are identical
        // to the original Merge wrapper since it has the same arity and
        // structure.
        let merge_core = lambda(
            2, // [l_blk, r_blk]
            switch(
                local(0),
                vec![(
                    DataConstructor::Block.tag(), // [lcons lindex] [l_blk r_blk]
                    switch(
                        local(3),
                        vec![(
                            DataConstructor::Block.tag(), // [rcons rindex] [lcons lindex] [l_blk r_blk]
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
        );

        // Use plain lambda so the call-site annotation set by the Ann node
        // emitted by the compiler at application sites is not overwritten
        // when the intrinsic wrapper is entered.
        lambda(
            2, // [l, r]
            let_(
                vec![merge_core],
                // [merge_core, l, r]
                demeta(
                    local(1), // examine l
                    // l has meta → [l_meta, l_body, merge_core, l, r]
                    demeta(
                        local(4), // examine r
                        // both have meta → [r_meta, r_body, l_meta, l_body, merge_core, l, r]
                        // shallow merge: RHS metadata wins
                        force(
                            app(lref(4), vec![lref(3), lref(1)]), // merge_core(l_body, r_body)
                            // [merged, r_meta, r_body, l_meta, l_body, merge_core, l, r]
                            with_meta(lref(1), lref(0)), // r_meta wins
                        ),
                        // only l has meta → [r_whnf, l_meta, l_body, merge_core, l, r]
                        force(
                            app(lref(3), vec![lref(2), lref(0)]), // merge_core(l_body, r_whnf)
                            // [merged, r_whnf, l_meta, l_body, merge_core, l, r]
                            with_meta(lref(2), lref(0)), // l_meta
                        ),
                    ),
                    // l has no meta → [l_whnf, merge_core, l, r]
                    demeta(
                        local(3), // examine r
                        // only r has meta → [r_meta, r_body, l_whnf, merge_core, l, r]
                        force(
                            app(lref(3), vec![lref(2), lref(1)]), // merge_core(l_whnf, r_body)
                            // [merged, r_meta, r_body, l_whnf, merge_core, l, r]
                            with_meta(lref(1), lref(0)), // r_meta
                        ),
                        // neither has meta → [r_whnf, l_whnf, merge_core, l, r]
                        app(lref(2), vec![lref(1), lref(0)]), // merge_core(l_whnf, r_whnf)
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
        let l = data_list_arg(machine, view, args[0].clone())?;
        let r = data_list_arg(machine, view, args[1].clone())?;

        // Engine-neutral: dedup pairs by key (RHS wins) and rebuild the
        // kv-list via `return_closure_list` — no runtime code synthesis.
        let mut merge: IndexMap<String, AbiClosure> = IndexMap::new();
        for item in l.iter().chain(r.iter()) {
            let (k, kv) = deconstruct(machine, view, item)?;
            merge.insert(k, kv);
        }

        machine.return_closure_list(view, merge.into_iter().map(|(_, v)| v).collect())
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
    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
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

        // Use plain lambda so the call-site annotation set by the Ann node
        // emitted by the compiler at application sites is not overwritten
        // when the intrinsic wrapper is entered.

        lambda(
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
        let f = machine.resolve_closure(view, &args[2])?;

        // The value-combining step builds a lazy `f(ov, nv)` application thunk
        // via the neutral `apply2_thunk` primitive (a fixed-shape `App(L0,[L1,
        // L2])`), so this runs byte-identically on both engines.
        let mut merge: IndexMap<String, AbiClosure> = IndexMap::new();

        for item in &l {
            let (key, value) = deconstruct(machine, view, item)?;
            merge.insert(key, value);
        }

        for item in &r {
            let (key, nv) = deconstruct(machine, view, item)?;
            if let Some(ov) = merge.get_mut(&key) {
                let combined = machine.apply2_thunk(view, f.clone(), ov.clone(), nv)?;
                *ov = combined;
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

    /// Deep merge operation, preserving metadata from both operands.
    ///
    /// Uses `demeta` to capture metadata from both operands before
    /// pattern-matching blocks. RHS metadata wins when both carry metadata.
    /// Sub-block values are still recursively deep-merged (via MergeWith).
    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use dsl::*;

        // merge_deep_core: lambda(2, [l_blk, r_blk]) — deep-merges two bare
        // blocks (no metadata). Replicates the original case/MergeWith logic.
        let merge_deep_core = lambda(
            2, // [l_blk, r_blk]
            case(
                local(0), // l_blk
                vec![(
                    DataConstructor::Block.tag(), // [lcons lindex] [l_blk r_blk]
                    case(
                        local(3), // r_blk in [lcons lindex l_blk r_blk]
                        vec![(
                            DataConstructor::Block.tag(), // [rcons rindex lcons lindex l_blk r_blk]
                            MergeWith.global(lref(4), lref(5), gref(self.index())),
                        )],
                        // r not block: return r_whnf
                        local(0),
                    ),
                )],
                // l not block: return r_blk
                local(2),
            ),
        );

        // Use plain lambda so the call-site annotation set by the Ann node
        // emitted by the compiler at application sites is not overwritten
        // when the intrinsic wrapper is entered.

        lambda(
            2, // [l, r]
            let_(
                vec![merge_deep_core],
                // [merge_deep_core, l, r]
                demeta(
                    local(1), // examine l
                    // l has meta → [l_meta, l_body, merge_deep_core, l, r]
                    demeta(
                        local(4), // examine r
                        // both have meta → [r_meta, r_body, l_meta, l_body, merge_deep_core, l, r]
                        // RHS metadata wins
                        force(
                            app(lref(4), vec![lref(3), lref(1)]), // merge_deep_core(l_body, r_body)
                            // [merged, r_meta, r_body, l_meta, l_body, merge_deep_core, l, r]
                            with_meta(lref(1), lref(0)), // r_meta wins
                        ),
                        // only l has meta → [r_whnf, l_meta, l_body, merge_deep_core, l, r]
                        force(
                            app(lref(3), vec![lref(2), lref(0)]), // merge_deep_core(l_body, r_whnf)
                            // [merged, r_whnf, l_meta, l_body, merge_deep_core, l, r]
                            with_meta(lref(2), lref(0)), // l_meta
                        ),
                    ),
                    // l has no meta → [l_whnf, merge_deep_core, l, r]
                    demeta(
                        local(3), // examine r
                        // only r has meta → [r_meta, r_body, l_whnf, merge_deep_core, l, r]
                        force(
                            app(lref(3), vec![lref(2), lref(1)]), // merge_deep_core(l_whnf, r_body)
                            // [merged, r_meta, r_body, l_whnf, merge_deep_core, l, r]
                            with_meta(lref(1), lref(0)), // r_meta
                        ),
                        // neither has meta → [r_whnf, l_whnf, merge_deep_core, l, r]
                        app(lref(2), vec![lref(1), lref(0)]), // merge_deep_core(l_whnf, r_whnf)
                    ),
                ),
            ),
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
        let closure = machine.resolve_closure(view, &args[0])?;
        let is_block = machine.data_tag(view, &closure) == Some(DataConstructor::Block.tag());
        machine_return_bool(machine, view, is_block)
    }
}

impl CallGlobal1 for IsBlock {}

/// Compile a lookup failure for a statically known missing key.
///
/// Uses the LookupFail intrinsic so that the runtime can collect
/// block keys and offer "did you mean?" suggestions via edit distance.
///
/// When `annotation` is valid, wraps the call with an Ann node so
/// that `LookupFailure` errors carry the user's source location.
pub fn lookup_fail(key: &str, obj: super::syntax::Ref, annotation: Smid) -> Rc<StgSyn> {
    use dsl::*;

    let stg = LookupFail.global(sym(key), obj);
    if annotation.is_valid() {
        ann(annotation, stg)
    } else {
        stg
    }
}

/// Compile a panic for a missing key (legacy fallback, kept for tests)
pub fn panic_key_not_found(key: &str) -> Rc<StgSyn> {
    use dsl::*;

    let_(
        vec![value(box_str(format!("Key not found: {key}")))],
        Panic.global(lref(0)),
    )
}

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::eval::stg::{
        boolean::{False, True},
        constant::KEmptyList,
        eq::Eq,
        panic::Panic,
        runtime::Runtime,
        syntax::dsl::*,
        testing,
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
            Box::new(True),
            Box::new(False),
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
