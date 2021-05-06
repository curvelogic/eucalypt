//! Block intrinsics

use std::{mem::swap, rc::Rc};

use indexmap::IndexMap;

use crate::{
    common::sourcemap::{Smid, SourceMap},
    eval::error::ExecutionError,
};

use super::{
    env::{Closure, EnvFrame},
    machine::{Machine, StgIntrinsic},
    runtime::{call, data_list_arg, machine_return_closure_list, NativeVariant, StgWrapper},
    syntax::{
        dsl::{app, lref},
        Native, Ref, StgSyn,
    },
};

use super::syntax::{dsl, tags, LambdaForm};

/// BLOCK
///
/// BLOCK takes a list of key-value pairs (of some form) and coerces /
/// wraps them up a a block that can be understood by LOOKUP /
/// LOOKUPOR.
pub struct Block;

impl StgWrapper for Block {
    fn name(&self) -> &str {
        "BLOCK"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;

        let kv_items = lambda(
            2, // [items self]
            switch(
                local(0),
                vec![
                    (
                        tags::LIST_CONS, // [h t] [items self]
                        force(
                            call::global::kv(lref(0)),
                            // [kv] [h t] [items self]
                            force(
                                app(lref(4), vec![lref(2), lref(4)]),
                                // [tt] [kv] [h t] [itesm self]
                                data(tags::LIST_CONS, vec![lref(1), lref(0)]),
                            ),
                        ),
                    ),
                    (tags::LIST_NIL, local(0)),
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
                data(tags::BLOCK, vec![lref(1)]),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for Block {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("BLOCK is STG only")
    }
}

/// KV
///
/// KV coerces its argument into a valid block key-value either as
/// BLOCK_PAIR or wrapping a list in a BLOCK_KV_LIST
pub struct Kv;

impl StgWrapper for Kv {
    fn name(&self) -> &str {
        "KV"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;

        annotated_lambda(
            1,
            case(
                local(0),
                vec![
                    (
                        tags::BLOCK_PAIR, // [k v] [pair]
                        local(2),
                    ),
                    (
                        tags::LIST_CONS, // [h t] [list]
                        data(tags::BLOCK_KV_LIST, vec![lref(2)]),
                    ),
                    (
                        tags::BLOCK_KV_LIST, // [l] [kvl]
                        local(1),
                    ),
                ],
                call::bif::panic(str("invalid key-value element in block")),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for Kv {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("KV is STG only")
    }
}

/// DEKV
///
/// Takes a block KV element and converts it to a [k, v] list
pub struct Dekv;

impl StgWrapper for Dekv {
    fn name(&self) -> &str {
        "DEKV"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;

        annotated_lambda(
            1, // [pair]
            case(
                local(0),
                vec![
                    (
                        tags::BLOCK_PAIR, // [k v] [pair]
                        letrec_(
                            vec![
                                // [nil boxk [v]] [k v] [pair]
                                value(nil()),
                                thunk(data(tags::BOXED_SYMBOL, vec![lref(3)])),
                                value(data(tags::LIST_CONS, vec![lref(4), lref(0)])),
                            ],
                            data(tags::LIST_CONS, vec![lref(1), lref(2)]),
                        ),
                    ),
                    (
                        tags::BLOCK_KV_LIST, // [cons] [pair]
                        local(0),
                    ),
                ],
                call::bif::panic(str("invalid key-value element in block")),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for Dekv {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("DEKV is STG only")
    }
}

/// ELEMENTS(block)
///
/// Return block as list of [k, v] lists
pub struct Elements;

impl StgWrapper for Elements {
    fn name(&self) -> &str {
        "ELEMENTS"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;

        let map_list = lambda(
            2, // [list self]
            case(
                local(0),
                vec![
                    (
                        tags::LIST_CONS, // [h t] [list self]
                        letrec_(
                            // [dekv-h rest] [h t] [list self]
                            vec![
                                thunk(call::global::dekv(lref(2))),
                                thunk(app(lref(5), vec![lref(3), lref(5)])),
                            ],
                            data(tags::LIST_CONS, vec![lref(0), lref(1)]),
                        ),
                    ),
                    (tags::LIST_NIL, local(0)),
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
                        tags::BLOCK, // [list]  [map_list] [block]
                        app(lref(1), vec![lref(0), lref(1)]),
                    )],
                    call::bif::panic(str("elements called on non-block")),
                ),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for Elements {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("ELEMENTS is STG only")
    }
}

/// MATCHES_KEY(pair, unboxed_sym)
///
/// Return true iff the key-value pair matches the provided key symbol
pub struct MatchesKey;

impl StgWrapper for MatchesKey {
    fn name(&self) -> &str {
        "MATCHES_KEY"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;
        annotated_lambda(
            2, // [pair unboxsym]
            case(
                local(0),
                vec![
                    (
                        tags::BLOCK_PAIR,
                        call::global::eq(lref(0), lref(3)), // [k v] [pair unboxsym]
                    ),
                    (
                        tags::BLOCK_KV_LIST, // [l] [pair sym]
                        case(
                            local(0),
                            vec![(
                                tags::LIST_CONS, // [h t] [l] [pair sym]
                                unbox_sym(
                                    local(0), // [unbox_h] [h t] [l] [pair sym]
                                    call::global::eq(lref(0), lref(5)),
                                ),
                            )],
                            f(),
                        ),
                    ),
                ],
                call::bif::panic(str("bad key-value pair in MATCHES_KEY")),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for MatchesKey {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("MATCHES_KEY is STG only")
    }
}

/// EXTRACT_VALUE
///
/// If the argument is a block key value form, return the value.
pub struct ExtractValue;

impl StgWrapper for ExtractValue {
    fn name(&self) -> &str {
        "EXTRACT_VALUE"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;
        annotated_lambda(
            1, // [pair]
            case(
                local(0),
                vec![
                    (
                        tags::BLOCK_PAIR, // [k v] [pair]
                        local(1),
                    ),
                    (
                        tags::BLOCK_KV_LIST, // [l] [pair]
                        switch(
                            local(0),
                            vec![(
                                tags::LIST_CONS, // [h t] [l] [pair]
                                switch(
                                    local(1),
                                    vec![(
                                        tags::LIST_CONS, // [h t] [h t] [l] [pair]
                                        local(0),
                                    )],
                                ),
                            )],
                        ),
                    ),
                ],
                call::bif::panic(str("bad key-value pair in EXTRACT_VALUE")),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for ExtractValue {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("EXTRACT_VALUE is STG only")
    }
}

/// EXTRACT_KEY(kv)
///
/// If the argument is a block key value form, return the unboxed key symbol.
pub struct ExtractKey;

impl StgWrapper for ExtractKey {
    fn name(&self) -> &str {
        "EXTRACT_KEY"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;
        annotated_lambda(
            1, // [pair]
            case(
                local(0),
                vec![
                    (
                        tags::BLOCK_PAIR, // [k v] [pair]
                        local(0),
                    ),
                    (
                        tags::BLOCK_KV_LIST, // [l] [pair]
                        switch(
                            local(0),
                            vec![(
                                tags::LIST_CONS, // [h t] [l] [pair]
                                unbox_sym(local(0), local(0)),
                            )],
                        ),
                    ),
                ],
                call::bif::panic(str("bad key-value pair in EXTRACT_KEY")),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for ExtractKey {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("EXTRACT_KEY is STG only")
    }
}

/// PACK_PAIR(kv)
///
/// Packs a kv pair into an outer BLOCK_PAIR(k, kv) for processing by
/// merge (so that the kv can be exposed again in the new block)
pub struct PackPair;

impl StgWrapper for PackPair {
    fn name(&self) -> &str {
        "PACK_PAIR"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;
        annotated_lambda(
            1, // [kv]
            force(
                call::global::extract_key(lref(0)), // [sym] [kv]
                data(tags::BLOCK_PAIR, vec![lref(0), lref(1)]),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for PackPair {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("PACK_PAIR is STG only")
    }
}

/// BLOCK_PAIR(kv)
///
/// Force a KV into a block pair representation
pub struct BlockPair;

impl StgWrapper for BlockPair {
    fn name(&self) -> &str {
        "BLOCK_PAIR"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;
        annotated_lambda(
            1, // [kv]
            switch(
                local(0),
                vec![
                    (
                        tags::BLOCK_PAIR,
                        // [k v] [kv]
                        local(2),
                    ),
                    (
                        tags::BLOCK_KV_LIST, // [lcons] [kv]
                        switch(
                            local(0),
                            vec![(
                                tags::LIST_CONS, // [k t] [lcons] [kv]
                                switch(
                                    local(1),
                                    vec![(
                                        tags::LIST_CONS, // [v .] [k t] [lcons] [kv]
                                        data(tags::BLOCK_PAIR, vec![lref(2), lref(0)]),
                                    )],
                                ),
                            )],
                        ),
                    ),
                ],
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for BlockPair {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("BLOCK_PAIR is STG only")
    }
}

/// LOOKUPOR(key, default, obj) is lookup with default
///
/// NB. The compiler creates calls to LOOKUPOR with and unboxed
/// symbol. Is this right? No.
pub struct LookupOr(pub NativeVariant);

impl StgWrapper for LookupOr {
    fn name(&self) -> &str {
        match self.0 {
            NativeVariant::Boxed => "LOOKUPOR",
            NativeVariant::Unboxed => "LOOKUPOR#",
        }
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;

        let find = annotated_lambda(
            4, // [list k d find]
            case(
                local(0),
                vec![
                    (
                        tags::LIST_CONS, // [h t] [list k d find]
                        switch(
                            call::global::matches_key(lref(0), lref(3)),
                            vec![
                                (
                                    tags::BOOL_TRUE,
                                    // [h t] [list k d find]
                                    call::global::extract_value(lref(0)),
                                ),
                                (
                                    tags::BOOL_FALSE,
                                    app(lref(5), vec![lref(1), lref(3), lref(4), lref(5)]),
                                ),
                            ],
                        ),
                    ),
                    (
                        tags::LIST_NIL, // [list k d] [find]
                        local(2),
                    ),
                ],
                call::bif::panic(str("bad block content")),
            ),
            source_map.add_synthetic("LookupOr.find"),
        );

        annotated_lambda(
            3, // [k d block]
            switch(
                local(2),
                vec![(
                    tags::BLOCK,
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
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for LookupOr {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("LOOKUPOR is STG only")
    }
}

/// LOOKUP(k, block)
pub struct Lookup;

impl StgWrapper for Lookup {
    fn name(&self) -> &str {
        "LOOKUP"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;

        annotated_lambda(
            2, // [k block]
            unbox_sym(
                local(0),
                // [sym] [k block]
                let_(
                    vec![value(call::bif::panic(str("key not found")))],
                    // [panic] [sym] [k block]
                    call::global::lookup_or_unboxed(lref(1), lref(0), lref(3)),
                ),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for Lookup {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("LOOKUP is STG only")
    }
}

/// MERGE(l, r)
///
/// Merge two blocks preserving order where possible and with values
/// from r overriding those in l
pub struct Merge;

impl StgWrapper for Merge {
    fn name(&self) -> &str {
        "MERGE"
    }

    /// Expose the two lists to the intrinsic
    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;

        let pack_items = lambda(
            2, // [list self]
            switch(
                local(0),
                vec![
                    (
                        tags::LIST_CONS, // [h t] [list self]
                        force(
                            call::global::pack_pair(lref(0)),
                            // [pp-h] [h t] [list self]
                            force(
                                app(lref(4), vec![lref(2), lref(4)]),
                                // [p-t] [pp-h] [h t] [list self]
                                data(tags::LIST_CONS, vec![lref(1), lref(0)]),
                            ),
                        ),
                    ),
                    (tags::LIST_NIL, local(0)),
                ],
            ),
        );

        annotated_lambda(
            2, // [l r]
            switch(
                local(0),
                vec![(
                    tags::BLOCK, // [lcons] [l r]
                    switch(
                        local(2),
                        vec![(
                            tags::BLOCK, // [rcons] [lcons] [l r]
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
                                            data(tags::BLOCK, vec![lref(0)]),
                                        ),
                                    ),
                                ),
                            ),
                        )],
                    ),
                )],
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

/// Items are passed to the MERGE intrinsic as block_pairs of k and
/// the kv closure and to the MERGEWITH intrinsic as block_pairs of k
/// and v. The same function can deconstruct either.
fn deconstruct(pair_closure: Closure) -> Result<(String, Closure), ExecutionError> {
    match &**pair_closure.code() {
        StgSyn::Cons { tag, args } if *tag == tags::BLOCK_PAIR => {
            let k = args[0].clone();
            let kv = args[1].clone();

            let sym = if let Native::Sym(s) = pair_closure.navigate_local_native(k) {
                s
            } else {
                panic!("bad block_pair passed to merge intrinsic: non-symbolic key")
            };

            let kv_closure = pair_closure.navigate_local(kv);

            Ok((sym, kv_closure))
        }
        _ => {
            panic!("bad block_pair passed to merge intrinsic: non-data type")
        }
    }
}

impl StgIntrinsic for Merge {
    fn execute(&self, machine: &mut Machine, args: &[Ref]) -> Result<(), ExecutionError> {
        let l = data_list_arg(machine, args[0].clone())?;
        let r = data_list_arg(machine, args[1].clone())?;

        let mut merge: IndexMap<String, Closure> = IndexMap::new();

        for item in l {
            let (k, kv) = deconstruct(item)?;
            merge.insert(k, kv);
        }

        for item in r {
            let (k, kv) = deconstruct(item)?;
            merge.insert(k, kv);
        }

        machine_return_closure_list(machine, merge.into_iter().map(|(_, v)| v).collect())
    }
}

/// MERGEWITH(l, r, fn)
///
/// Merge two blocks preserving order where possible and with values
/// from r combined with those in l via fn(l, r)
pub struct MergeWith;

impl StgWrapper for MergeWith {
    fn name(&self) -> &str {
        "MERGEWITH"
    }

    /// Expose the two lists to the intrinsic
    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;

        let pair_items = lambda(
            2, // [list self]
            switch(
                local(0),
                vec![
                    (
                        tags::LIST_CONS, // [h t] [list self]
                        force(
                            call::global::block_pair(lref(0)),
                            // [bp-h] [h t] [list self]
                            force(
                                app(lref(4), vec![lref(2), lref(4)]),
                                // [p-t] [bp-h] [h t] [list self]
                                data(tags::LIST_CONS, vec![lref(1), lref(0)]),
                            ),
                        ),
                    ),
                    (tags::LIST_NIL, local(0)),
                ],
            ),
        );

        annotated_lambda(
            3, // [l r f]
            switch(
                local(0),
                vec![(
                    tags::BLOCK, // [lcons] [l r f]
                    switch(
                        local(2),
                        vec![(
                            tags::BLOCK, // [rcons] [lcons] [l r f]
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
                                            data(tags::BLOCK, vec![lref(0)]),
                                        ),
                                    ),
                                ),
                            ),
                        )],
                    ),
                )],
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for MergeWith {
    fn execute(&self, machine: &mut Machine, args: &[Ref]) -> Result<(), ExecutionError> {
        let l = data_list_arg(machine, args[0].clone())?;
        let r = data_list_arg(machine, args[1].clone())?;
        let f = args[2].clone();

        let mut merge: IndexMap<String, Closure> = IndexMap::new();

        for item in l {
            let (key, value) = deconstruct(item)?;
            merge.insert(key, value);
        }

        for item in r {
            let (key, nv) = deconstruct(item)?;
            if let Some(ov) = merge.get_mut(&key) {
                let mut combined = Closure::new(
                    app(f.bump(2), vec![lref(0), lref(1)]),
                    EnvFrame::from_closures(
                        &[ov.clone(), nv.clone()],
                        machine.env(),
                        Smid::default(),
                    ),
                );
                swap(ov, &mut combined);
            } else {
                merge.insert(key, nv);
            }
        }

        super::runtime::machine_return_block_pair_closure_list(machine, merge)
    }
}

/// DEEPMERGE(l, r, fn)
///
/// Merge two blocks, recursing into any subblocks, if either l or r
/// are not blocks then return r.
pub struct DeepMerge;

impl StgWrapper for DeepMerge {
    fn name(&self) -> &str {
        "DEEPMERGE"
    }

    ///
    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        use dsl::*;

        annotated_lambda(
            2,
            case(
                local(0),
                vec![(
                    tags::BLOCK,
                    // [lcons] [l r]
                    case(
                        local(2),
                        vec![(
                            tags::BLOCK,
                            // [rcons] [lcons] [l r]
                            call::global::merge_with(lref(2), lref(3), gref(self.index())),
                        )],
                        // [r] [lcons] [l r]
                        local(0),
                    ),
                )],
                // [l] [l r]
                local(2),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for DeepMerge {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("DEEPMERGE is STG only")
    }
}

/// Compile a panic for a missing key
pub fn panic_key_not_found(key: &str) -> Rc<StgSyn> {
    use dsl::*;

    let_(
        vec![value(box_str(format!("Key not found: {}", key)))],
        call::global::panic(lref(0)),
    )
}

#[cfg(test)]
pub mod tests {

    use gcmodule::Cc;
    use std::rc::Rc;

    use super::*;
    use crate::eval::{
        emit::DebugEmitter,
        machines::stg::{
            env,
            eq::Eq,
            machine::Machine,
            panic::Panic,
            runtime,
            syntax::{dsl::*, StgSyn},
        },
    };

    lazy_static! {
        static ref RUNTIME: Box<dyn runtime::Runtime> = {
            let mut rt = runtime::StandardRuntime::default();
            rt.add(Box::new(Block));
            rt.add(Box::new(Kv));
            rt.add(Box::new(MatchesKey));
            rt.add(Box::new(ExtractValue));
            rt.add(Box::new(LookupOr(NativeVariant::Boxed)));
            rt.add(Box::new(LookupOr(NativeVariant::Unboxed)));
            rt.add(Box::new(Panic));
            rt.add(Box::new(Eq));
            Box::new(rt)
        };
    }

    /// Construct a machine with the arithmetic intrinsics
    pub fn machine(syntax: Rc<StgSyn>) -> Machine<'static> {
        let env = env::EnvFrame::default();
        Machine::new(
            syntax,
            Cc::new(env),
            RUNTIME.globals(&mut SourceMap::default()),
            RUNTIME.intrinsics(),
            Box::new(DebugEmitter::default()),
            true,
        )
    }

    #[test]
    pub fn test_simple_kv_match() {
        let syntax = letrec_(
            vec![
                value(box_str("value")),
                value(data(tags::BLOCK_PAIR, vec![sym("key"), lref(0)])),
                value(call::global::kv(lref(1))),
            ],
            call::global::matches_key(lref(2), sym("key")),
        );

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), t());
    }

    #[test]
    pub fn test_simple_kv_mismatch() {
        let syntax = letrec_(
            vec![
                value(box_str("value")),
                value(data(tags::BLOCK_PAIR, vec![sym("key"), lref(0)])),
                value(call::global::kv(lref(1))),
            ],
            call::global::matches_key(lref(2), sym("different")),
        );

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), f());
    }

    #[test]
    pub fn test_simple_kvlist_match() {
        let syntax = letrec_(
            vec![
                value(box_sym("key")),
                value(box_str("value")),
                value(data(tags::LIST_NIL, vec![])),
                value(data(tags::LIST_CONS, vec![lref(1), lref(2)])),
                value(data(tags::LIST_CONS, vec![lref(0), lref(3)])),
                value(call::global::kv(lref(4))),
            ],
            call::global::matches_key(lref(5), sym("key")),
        );

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), t());
    }

    #[test]
    pub fn test_simple_block_lookup() {
        let syntax = letrec_(
            vec![
                value(box_str("v1")),
                value(data(tags::BLOCK_PAIR, vec![sym("k1"), lref(0)])),
                value(call::global::kv(lref(1))),
                value(box_str("v2")),
                value(data(tags::BLOCK_PAIR, vec![sym("k2"), lref(0)])),
                value(call::global::kv(lref(4))),
                value(data(tags::LIST_NIL, vec![])),
                value(data(tags::LIST_CONS, vec![lref(5), lref(6)])),
                value(data(tags::LIST_CONS, vec![lref(2), lref(7)])),
                value(data(tags::BLOCK, vec![lref(8)])),
                value(box_sym("k1")),
                value(box_str("fail")),
            ],
            call::global::lookup_or(lref(10), lref(11), lref(9)),
        );

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), box_str("v1"));
    }
}
