//! Block intrinsics

use std::{mem::swap, rc::Rc};

use indexmap::IndexMap;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::{
            env::Closure,
            env_builder::EnvBuilder,
            intrinsic::{CallGlobal1, CallGlobal2, CallGlobal3, IntrinsicMachine, StgIntrinsic},
        },
        memory::{
            alloc::ScopedAllocator,
            array::Array,
            mutator::MutatorHeapView,
            syntax::{Ref, RefPtr, StgBuilder},
        },
    },
};

use super::{
    eq::Eq,
    panic::Panic,
    runtime::NativeVariant,
    support::{
        call, data_list_arg, machine_return_block_pair_closure_list, machine_return_closure_list,
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
                data(DataConstructor::Block.tag(), vec![lref(1)]),
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
                        DataConstructor::Block.tag(), // [list]  [map_list] [block]
                        app(lref(1), vec![lref(0), lref(1)]),
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

/// LOOKUPOR(key, default, obj) is lookup with default
///
/// NB. The compiler creates calls to LOOKUPOR with and unboxed
/// symbol. Is this right? No.
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
                    // [blocklist] [k d block]
                    letrec_(
                        vec![find], // [find] [blocklist] [k d block]
                        match self.0 {
                            NativeVariant::Unboxed => {
                                app(lref(0), vec![lref(1), lref(2), lref(3), lref(0)])
                            }
                            NativeVariant::Boxed => {
                                unbox_sym(
                                    local(2),
                                    // [sym] [find] [blocklist] [k d block]
                                    app(lref(1), vec![lref(2), lref(0), lref(4), lref(1)]),
                                )
                            }
                        },
                    ),
                )],
            ),
            annotation,
        )
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
                    vec![value(call::bif::panic(str("key not found")))],
                    // [panic] [sym] [k block]
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
    pair_closure: &Closure,
) -> Result<(String, RefPtr<Closure>), ExecutionError> {
    use crate::eval::memory::syntax;

    let code = view.scoped(pair_closure.code());
    match &*code {
        syntax::HeapSyn::Cons { tag, args } if *tag == DataConstructor::BlockPair.tag() => {
            let k = args.get(0).unwrap();
            let kv = args.get(1).unwrap();

            let sym = if let syntax::Native::Sym(s) = pair_closure.navigate_local_native(&view, k) {
                view.scoped(s).as_str().to_string()
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
                    DataConstructor::Block.tag(), // [lcons] [l r]
                    switch(
                        local(2),
                        vec![(
                            DataConstructor::Block.tag(), // [rcons] [lcons] [l r]
                            let_(
                                vec![pack_items],
                                // [pack] [rcons] [lcons]
                                force(
                                    app(lref(0), vec![lref(2), lref(0)]),
                                    // [p-l] [pack] [rcons] [lcons]
                                    force(
                                        app(lref(1), vec![lref(2), lref(1)]),
                                        // [p-r] [p-l] [pack] [rcons] [lcons]
                                        force(
                                            call::bif::merge(lref(1), lref(0)),
                                            data(DataConstructor::Block.tag(), vec![lref(0)]),
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

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let l = data_list_arg(machine, view, args[0].clone())?;
        let r = data_list_arg(machine, view, args[1].clone())?;

        let mut merge: IndexMap<String, RefPtr<Closure>> = IndexMap::new();

        for item in l {
            let item = &*(view.scoped(item));
            let (k, kv) = deconstruct(view, item)?;
            merge.insert(k, kv);
        }

        for item in r {
            let item = &*(view.scoped(item));
            let (k, kv) = deconstruct(view, item)?;
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
                    DataConstructor::Block.tag(), // [lcons] [l r f]
                    switch(
                        local(2),
                        vec![(
                            DataConstructor::Block.tag(), // [rcons] [lcons] [l r f]
                            let_(
                                vec![pair_items],
                                // [pack] [rcons] [lcons] [l r f]
                                force(
                                    app(lref(0), vec![lref(2), lref(0)]),
                                    // [p-l] [pack] [rcons] [lcons] [l r f]
                                    force(
                                        app(lref(1), vec![lref(2), lref(1)]),
                                        // [p-r] [p-l] [pack] [rcons] [lcons] [l r f]
                                        force(
                                            call::bif::merge_with(lref(1), lref(0), lref(7)),
                                            data(DataConstructor::Block.tag(), vec![lref(0)]),
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

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let l = data_list_arg(machine, view, args[0].clone())?;
        let r = data_list_arg(machine, view, args[1].clone())?;
        let f = args[2].clone();

        let mut merge: IndexMap<String, RefPtr<Closure>> = IndexMap::new();

        for item in l {
            let item = &*(view.scoped(item));
            let (key, value) = deconstruct(view, item)?;
            merge.insert(key, value);
        }

        for item in r {
            let item = &*(view.scoped(item));
            let (key, nv) = deconstruct(view, item)?;
            if let Some(ov) = merge.get_mut(&key) {
                let mut combined = view
                    .alloc(Closure::new(
                        view.app(f.bump(2), Array::from_slice(&view, &[Ref::L(0), Ref::L(1)]))?
                            .as_ptr(),
                        view.from_closures(
                            [*ov, nv].iter().cloned(),
                            2,
                            machine.env(view),
                            Smid::default(),
                        ),
                    ))?
                    .as_ptr();
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
/// Merge two blocks, recursing into any subblocks, if either l or r
/// are not blocks then return r. (TODO: Really?)
pub struct DeepMerge;

impl StgIntrinsic for DeepMerge {
    fn name(&self) -> &str {
        "DEEPMERGE"
    }

    ///
    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        use dsl::*;

        annotated_lambda(
            2,
            case(
                local(0),
                vec![(
                    DataConstructor::Block.tag(),
                    // [lcons] [l r]
                    case(
                        local(2),
                        vec![(
                            DataConstructor::Block.tag(),
                            // [rcons] [lcons] [l r]
                            MergeWith.global(lref(2), lref(3), gref(self.index())),
                        )],
                        // [r] [lcons] [l r]
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

/// Compile a panic for a missing key
pub fn panic_key_not_found(key: &str) -> Rc<StgSyn> {
    use dsl::*;

    let_(
        vec![value(box_str(format!("Key not found: {}", key)))],
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
                value(data(DataConstructor::Block.tag(), vec![lref(7)])),
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
