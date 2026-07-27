//! Metadata intrinsics

use crate::{
    common::sourcemap::Smid,
    eval::machine::intrinsic::{CallGlobal1, CallGlobal2, Const, StgIntrinsic},
};

use super::{
    block::Merge,
    constant::KEmptyBlock,
    syntax::{
        dsl::{case, demeta, lambda, let_, local, lref, value, with_meta},
        LambdaForm,
    },
    tags::DataConstructor,
};

/// META(obj) - return metadata of object or empty block
///
/// A value may carry several stacked metadata layers, so META recurses
/// into the body and combines the result with the outermost layer.
/// Layers combine according to a single rule:
///
/// - **both layers are blocks** — merge them, with the outer (later
///   applied) layer winning on key conflicts;
/// - **either layer is not a block** — non-block metadata is *opaque*.
///   It cannot be merged, so the outer layer stands as the answer on
///   its own.
///
/// The opaque case is what makes `{ :sym … }` shorthand blocks work:
/// the shorthand attaches a bare symbol, and merging a symbol into a
/// block raised `expected block, found symbol` before the rule above
/// was introduced (eu-3aa6s).  Symbol-shaped metadata now passes
/// straight through, exactly as `RAWMETA` reports it.
///
/// Note the asymmetry with `RAWMETA`: `RAWMETA` never recurses, so it
/// always reports the outermost layer verbatim.  `META` reports the
/// outermost layer *deep-merged with everything beneath it*, which is
/// only a different answer when every layer involved is a block.
pub struct Meta;

impl StgIntrinsic for Meta {
    fn name(&self) -> &str {
        "META"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            1,
            demeta(
                local(0),
                // careful: body itself may have more metadata so merge...
                let_(
                    // [meta body] [...]
                    vec![value(Meta.global(lref(1)))],
                    // [inner-meta meta body] [...]
                    case(
                        // scrutinise the outer metadata layer
                        local(1),
                        vec![(
                            DataConstructor::Block.tag(),
                            // outer layer is a block
                            // [kvs idx] [inner-meta meta body] [...]
                            case(
                                // scrutinise the accumulated inner metadata
                                local(2),
                                vec![(
                                    DataConstructor::Block.tag(),
                                    // both blocks: merge, outer layer wins
                                    // [kvs' idx'] [kvs idx] [inner-meta meta body] [...]
                                    Merge.global(lref(4), lref(5)),
                                )],
                                // inner metadata is opaque (non-block): the
                                // outer block layer stands alone
                                // [inner-meta'] [kvs idx] [inner-meta meta body] [...]
                                local(4),
                            ),
                        )],
                        // outer layer is opaque (non-block): return it as-is
                        // [meta'] [inner-meta meta body] [...]
                        local(0),
                    ),
                ),
                KEmptyBlock.global(),
            ),
        )
    }
}

impl CallGlobal1 for Meta {}

/// RAWMETA(obj) - return immediate metadata of object without recursing
/// into inner layers. Unlike META which merges all nested metadata,
/// this returns only the outermost metadata block. Useful for
/// inspecting metadata without triggering infinite recursion when
/// metadata values themselves carry metadata.
pub struct RawMeta;

impl StgIntrinsic for RawMeta {
    fn name(&self) -> &str {
        "RAWMETA"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            1,
            demeta(
                local(0),
                // Return just the immediate metadata without recursing
                // [meta body] — return meta (local(0))
                local(0),
                KEmptyBlock.global(),
            ),
        )
    }
}

impl CallGlobal1 for RawMeta {}

/// WITHMETA(meta, obj) - add meta to obj
pub struct WithMeta;

impl StgIntrinsic for WithMeta {
    fn name(&self) -> &str {
        "WITHMETA"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(2, with_meta(lref(0), lref(1)))
    }
}

impl CallGlobal2 for WithMeta {}
