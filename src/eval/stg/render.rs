//! The top-level render intrinsics

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{
            CallGlobal1, CallGlobal2, CallGlobal3, IntrinsicMachine, StgIntrinsic,
        },
        memory::{mutator::MutatorHeapView, syntax::Ref},
        stg::{runtime::NativeVariant, support::call},
    },
};

use super::{
    block::LookupOr,
    boolean::{And, Not},
    emit::EmitNative,
    eq::Eq,
    panic::Panic,
    support::machine_return_bool,
    syntax::{dsl::*, LambdaForm},
    tags::DataConstructor,
};

/// The main entrypoint wrapper which renders the program's value
/// using the emit intrinsics
pub struct Render;

impl StgIntrinsic for Render {
    fn name(&self) -> &str {
        "RENDER"
    }

    /// Evaluate inspect and recur
    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        let suppressed = lambda(
            1,
            demeta(
                local(0),
                // [meta body] [arg]
                Suppresses.global(lref(0)),
                // no meta
                f(),
            ),
        );

        let tagfn = lambda(
            1,
            demeta(
                local(0),
                // [meta body] [arg]
                Tag.global(lref(0)),
                // no meta
                unit(),
            ),
        );

        let render = lambda(
            1,
            letrec_(
                vec![tagfn, value(app(lref(0), vec![lref(2)]))], // [tagfn tag] [x]
                case(
                    local(2),
                    vec![
                        (DataConstructor::Unit.tag(), call::bif::emit0()),
                        (DataConstructor::BoolTrue.tag(), call::bif::emitt()),
                        (DataConstructor::BoolFalse.tag(), call::bif::emitf()),
                        (
                            DataConstructor::BoxedNumber.tag(), // [num] [tagfn tag] [boxnum]
                            case(
                                local(2),
                                vec![(
                                    DataConstructor::BoxedString.tag(), // [tag] [num] [tagfn tag] [boxnum]
                                    force(call::bif::emit_tag_native(lref(0), lref(1)), unit()),
                                )],
                                force(EmitNative.global(lref(1)), unit()), // [()] [num] [tagfn tag] [boxnum]
                            ),
                        ),
                        (
                            DataConstructor::BoxedSymbol.tag(), // [sym] [tagfn tag] [boxsym]
                            case(
                                local(2),
                                vec![(
                                    DataConstructor::BoxedString.tag(), // [tag] [sym] [tagfn tag] [boxsym]
                                    force(call::bif::emit_tag_native(lref(0), lref(1)), unit()),
                                )],
                                force(EmitNative.global(lref(1)), unit()), // [()] [sym] [tagfn tag] [boxsym]
                            ),
                        ),
                        (
                            DataConstructor::BoxedString.tag(), // [str] [tagfn tag] [boxstr]
                            case(
                                local(2),
                                vec![(
                                    DataConstructor::BoxedString.tag(), // [tagstr] [str] [tagfn tag] [boxstr]
                                    force(call::bif::emit_tag_native(lref(0), lref(1)), unit()),
                                )],
                                force(EmitNative.global(lref(1)), unit()), // [()] [str] [boxstr] [tagfn tag]
                            ),
                        ),
                        (
                            DataConstructor::BoxedZdt.tag(), // [zdt] [tagfn tag] [boxzdt]
                            case(
                                local(2),
                                vec![(
                                    DataConstructor::BoxedString.tag(), // [tag] [zdt] [tagfn tag] [boxzdt]
                                    force(call::bif::emit_tag_native(lref(0), lref(1)), unit()),
                                )],
                                force(EmitNative.global(lref(1)), unit()), // [()] [zdt] [tagfn tag] [boxzdt]
                            ),
                        ),
                        (
                            DataConstructor::ListCons.tag(), // [h t] [tagfn tag],
                            force(
                                // [h t] [tagfn tag]
                                case(
                                    local(3),
                                    vec![(
                                        DataConstructor::BoxedString.tag(), // [tagstr] [h t] [tagfn tag]
                                        force(call::bif::emit_tag_seq_start(lref(0)), unit()),
                                    )],
                                    call::bif::emit_seq_start(),
                                ), // [()] [h t] [tagfn tag] [cons],
                                force(RenderItems.global(lref(5)), call::bif::emit_seq_end()),
                            ),
                        ),
                        (
                            DataConstructor::ListNil.tag(), // [tagfn tag]
                            force(
                                case(
                                    local(1),
                                    vec![(
                                        DataConstructor::BoxedString.tag(), // // [tagstr] [] [lcons] [tagfn tag]
                                        force(call::bif::emit_tag_seq_start(lref(0)), unit()),
                                    )],
                                    call::bif::emit_seq_start(),
                                ),
                                call::bif::emit_seq_end(),
                            ),
                        ),
                        (
                            DataConstructor::Block.tag(), // [items] [tagfn tag]
                            force(
                                case(
                                    local(2),
                                    vec![(
                                        DataConstructor::BoxedString.tag(), // [tagstr] [cons] [tagfn tag] [block]
                                        force(call::bif::emit_tag_block_start(lref(0)), unit()),
                                    )],
                                    call::bif::emit_block_start(), // [()] [cons] [tagfn tag] [block]
                                ),
                                force(
                                    RenderBlockItems.global(lref(1)),
                                    call::bif::emit_block_end(),
                                ),
                            ),
                        ),
                    ],
                    call::bif::panic(str("unrenderable")),
                ),
            ),
        );

        annotated_lambda(
            1,
            let_(
                vec![suppressed, render], // [s r] [arg]
                switch(
                    app(lref(0), vec![lref(2)]),
                    vec![
                        (
                            DataConstructor::BoolFalse.tag(), // [] [s r] [arg]
                            app(lref(1), vec![lref(2)]),
                        ),
                        (DataConstructor::BoolTrue.tag(), unit()),
                    ],
                ),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for Render {}

/// Render as a document
pub struct RenderDoc;

impl StgIntrinsic for RenderDoc {
    fn name(&self) -> &str {
        "RENDER_DOC"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1, // [renderee]
            force(
                call::bif::emit_doc_start(), // [()] [renderee]
                force(Render.global(lref(1)), call::bif::emit_doc_end()),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for RenderDoc {}

/// Sequentially render items from a list and return unit
pub struct RenderItems;

impl StgIntrinsic for RenderItems {
    fn name(&self) -> &str {
        "RENDER_ITEMS"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1, // [cons]
            case(
                local(0),
                vec![
                    (
                        DataConstructor::ListCons.tag(), // [h t] [cons]
                        force(
                            Render.global(lref(0)),
                            RenderItems.global(lref(2)), // [()] [h t] [cons]
                        ),
                    ),
                    (
                        DataConstructor::ListNil.tag(), // [cons]
                        unit(),
                    ),
                ],
                call::bif::panic(str("improper list")),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for RenderItems {}

/// Sequentially render items from a list and return unit
pub struct RenderBlockItems;

impl StgIntrinsic for RenderBlockItems {
    fn name(&self) -> &str {
        "RENDER_BLOCK_ITEMS"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1, // [cons]
            case(
                local(0),
                vec![
                    (
                        DataConstructor::ListCons.tag(), // [h t] [cons]
                        force(
                            RenderKv.global(lref(0)),
                            RenderBlockItems.global(lref(2)), // [()] [h t] [cons]
                        ),
                    ),
                    (
                        DataConstructor::ListNil.tag(), // [cons]
                        unit(),
                    ),
                ],
                Panic.global(str("improper list")),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for RenderBlockItems {}

/// Determine if a reference refers to a lambda (and therefore cannot
/// be rendered) or a value / thunk.
///
/// HACK: Like the real STG, we do not support returning functions
/// into case continuations but unike a properly typed language we
/// cannot compile so as to ensure that functions are never cased
/// rather than applied. In fact render tries to case everything.
/// Therefore we need to allow it to test a value first to suppress
/// render of lambdas.
pub struct Saturated;

impl StgIntrinsic for Saturated {
    fn name(&self) -> &str {
        "SATURATED"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let closure = machine.nav(view).resolve(&args[0])?;
        machine_return_bool(machine, view, closure.arity() == 0)
    }
}

impl CallGlobal1 for Saturated {}

/// Determine whether the specified metadata suppresses render
pub struct Suppresses;

impl StgIntrinsic for Suppresses {
    fn name(&self) -> &str {
        "SUPPRESSES"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1,
            let_(
                vec![value(box_sym("normal"))],
                case(
                    local(1), // [:normal] [arg]
                    vec![(
                        DataConstructor::Block.tag(),
                        // [xs] [:normal] [arg]
                        unbox_sym(
                            LookupOr(NativeVariant::Unboxed).global(
                                sym("export"),
                                lref(1),
                                lref(2),
                            ),
                            // [boxsym] [xs] [:normal] [arg]
                            Eq.global(lref(0), sym("suppress")),
                        ),
                    )],
                    f(),
                ),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for Suppresses {}

/// Determine whether the specified metadata suppresses render
pub struct Tag;

impl StgIntrinsic for Tag {
    fn name(&self) -> &str {
        "TAG"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1,
            let_(
                vec![value(box_sym(""))],
                case(
                    local(1), // [:""] [arg]
                    vec![(
                        DataConstructor::Block.tag(),
                        // [xs] [:""] [arg]
                        LookupOr(NativeVariant::Unboxed).global(sym("tag"), lref(1), lref(2)),
                    )],
                    // [arg] [:""] [arg]
                    local(1),
                ),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for Tag {}

/// Render a key and a value from the provided "pair", so long as
/// output is not suppressed and the value is renderable (i.e. not a
/// lambda).
pub struct RenderKv;

impl StgIntrinsic for RenderKv {
    fn name(&self) -> &str {
        "RENDER_KV"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        let value_renderable = lambda(
            1,
            letrec_(
                // [saturated suppresses unsuppressed] [v]
                vec![
                    value(Saturated.global(lref(3))),
                    value(demeta(local(3), Suppresses.global(lref(0)), f())),
                    value(Not.global(lref(1))),
                ],
                And.global(lref(0), lref(2)),
            ),
        );

        annotated_lambda(
            1, // [kv]
            case(
                local(0),
                vec![
                    (
                        DataConstructor::BlockPair.tag(), // [k v] [kv]
                        switch(
                            let_(
                                vec![value_renderable],
                                // [v_r] [k v] [kv]
                                app(lref(0), vec![lref(2)]),
                            ),
                            vec![
                                (
                                    DataConstructor::BoolTrue.tag(),
                                    force(
                                        call::bif::emit_native(lref(0)), // [()] [k v] [kv]
                                        Render.global(lref(2)),
                                    ),
                                ),
                                (DataConstructor::BoolFalse.tag(), unit()),
                            ],
                        ),
                    ),
                    (
                        DataConstructor::BlockKvList.tag(), // [cons] [kv]
                        RenderItems.global(lref(0)),
                    ),
                ],
                unit(),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for RenderKv {}

#[cfg(test)]
pub mod tests {

    use super::*;

    use crate::eval::{
        emit::{Event, RenderMetadata},
        primitive::Primitive,
        stg::{
            block::{self, Kv},
            boolean, emit, eq, panic,
            runtime::{self, Runtime},
            testing,
        },
    };

    pub fn runtime() -> Box<dyn Runtime> {
        testing::runtime(vec![
            Box::new(emit::Emit0),
            Box::new(emit::EmitT),
            Box::new(emit::EmitF),
            Box::new(emit::EmitNative),
            Box::new(emit::EmitTagNative),
            Box::new(emit::EmitSeqStart),
            Box::new(emit::EmitTagSeqStart),
            Box::new(emit::EmitSeqEnd),
            Box::new(emit::EmitBlockStart),
            Box::new(emit::EmitTagBlockStart),
            Box::new(emit::EmitBlockEnd),
            Box::new(Render),
            Box::new(RenderItems),
            Box::new(RenderBlockItems),
            Box::new(RenderKv),
            Box::new(Saturated),
            Box::new(Suppresses),
            Box::new(block::Kv),
            Box::new(block::LookupOr(runtime::NativeVariant::Unboxed)),
            Box::new(block::MatchesKey),
            Box::new(block::ExtractValue),
            Box::new(eq::Eq),
            Box::new(panic::Panic),
            Box::new(boolean::And),
            Box::new(boolean::Not),
        ])
    }

    #[test]
    pub fn test_render_zdt() {
        let syntax = letrec_(vec![value(box_num(42))], Render.global(lref(0)));

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert!(m.unit_return());
    }

    #[test]
    pub fn test_render_str() {
        let syntax = letrec_(vec![value(box_str("foo"))], Render.global(lref(0)));

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert!(m.unit_return());
    }

    #[test]
    pub fn test_render_sym() {
        let syntax = letrec_(vec![value(box_sym("bar"))], Render.global(lref(0)));

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert!(m.unit_return());
    }

    #[test]
    pub fn test_suppresses_true() {
        let syntax = letrec_(
            vec![
                value(box_sym("suppress")),
                value(data(
                    DataConstructor::BlockPair.tag(),
                    vec![sym("export"), lref(0)],
                )),
                value(Kv.global(lref(1))),
                value(data(DataConstructor::ListNil.tag(), vec![])),
                value(data(
                    DataConstructor::ListCons.tag(),
                    vec![lref(2), lref(3)],
                )),
                value(data(DataConstructor::Block.tag(), vec![lref(4)])),
            ],
            Suppresses.global(lref(5)),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.bool_return(), Some(true));
    }

    #[test]
    pub fn test_suppresses_false() {
        let syntax = letrec_(
            vec![
                value(box_sym("suppress")),
                value(data(
                    DataConstructor::BlockPair.tag(),
                    vec![sym("normal"), lref(0)],
                )),
                value(Kv.global(lref(1))),
                value(data(DataConstructor::ListNil.tag(), vec![])),
                value(data(
                    DataConstructor::ListCons.tag(),
                    vec![lref(2), lref(3)],
                )),
                value(data(DataConstructor::Block.tag(), vec![lref(4)])),
            ],
            Suppresses.global(lref(5)),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.bool_return(), Some(false));
    }

    #[test]
    pub fn test_suppresses_false_by_default() {
        let syntax = letrec_(
            vec![
                value(data(DataConstructor::ListNil.tag(), vec![])),
                value(data(DataConstructor::Block.tag(), vec![lref(0)])),
            ],
            Suppresses.global(lref(1)),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.bool_return(), Some(false));
    }

    #[test]
    pub fn test_render_list() {
        let syntax = letrec_(
            vec![
                value(box_sym("foo")),
                value(box_sym("bar")),
                value(data(DataConstructor::ListNil.tag(), vec![])),
                value(data(
                    DataConstructor::ListCons.tag(),
                    vec![lref(1), lref(2)],
                )),
                value(data(
                    DataConstructor::ListCons.tag(),
                    vec![lref(0), lref(3)],
                )),
            ],
            Render.global(lref(4)),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert_eq!(
            m.captures(),
            &[
                Event::OutputSequenceStart(RenderMetadata::empty()),
                Event::OutputScalar(RenderMetadata::empty(), Primitive::Sym("foo".to_string())),
                Event::OutputScalar(RenderMetadata::empty(), Primitive::Sym("bar".to_string())),
                Event::OutputSequenceEnd
            ]
        );
        assert!(m.unit_return());
    }

    #[test]
    pub fn test_render_block() {
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
                value(data(DataConstructor::ListNil.tag(), vec![])),
                value(data(
                    DataConstructor::ListCons.tag(),
                    vec![lref(5), lref(6)],
                )),
                value(data(
                    DataConstructor::ListCons.tag(),
                    vec![lref(2), lref(7)],
                )),
                value(data(DataConstructor::Block.tag(), vec![lref(8)])),
            ],
            Render.global(lref(9)),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(300)).unwrap();
        assert!(m.unit_return());
    }

    #[test]
    pub fn test_render_empty_block() {
        let syntax = letrec_(
            vec![
                value(box_str("v1")),
                value(data(
                    DataConstructor::BlockPair.tag(),
                    vec![sym("k1"), lref(0)],
                )),
                value(Kv.global(lref(1))),
                value(data(DataConstructor::ListNil.tag(), vec![])),
                value(data(
                    DataConstructor::ListCons.tag(),
                    vec![lref(2), lref(3)],
                )),
                value(data(DataConstructor::Block.tag(), vec![lref(4)])),
            ],
            Render.global(lref(5)),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert!(m.unit_return());
    }

    #[test]
    pub fn test_collect() {
        let syntax = let_(
            vec![value(unit())],
            switch(
                letrec_(vec![value(unit())], local(0)),
                vec![(DataConstructor::Unit.tag(), local(0))],
            ),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(300)).unwrap();
    }
}
