//! The top-level render intrinsics

use crate::{common::sourcemap::SourceMap, eval::error::ExecutionError};

use super::{
    machine::{Machine, StgIntrinsic},
    runtime::{call, machine_return_bool, StgWrapper},
    syntax::{dsl::*, tags, LambdaForm, Ref},
};

/// The main entrypoint wrapper which renders the program's value
/// using the emit intrinsics
pub struct Render;

impl StgWrapper for Render {
    fn name(&self) -> &str {
        "RENDER"
    }

    /// Evaluate inspect and recur
    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        let suppressed = lambda(
            1,
            demeta(
                local(0),
                // [meta body] [arg]
                call::global::suppresses(lref(0)),
                // no meta
                f(),
            ),
        );

        let render = lambda(
            1,
            case(
                local(0),
                vec![
                    (tags::UNIT, call::bif::emit0()),
                    (tags::BOOL_TRUE, call::bif::emitt()),
                    (tags::BOOL_FALSE, call::bif::emitf()),
                    (
                        tags::BOXED_NUMBER, // [num] [boxnum]
                        force(call::global::emitx(lref(0)), unit()),
                    ),
                    (
                        tags::BOXED_SYMBOL, // [sym] [boxsym]
                        call::global::emitx(lref(0)),
                    ),
                    (
                        tags::BOXED_STRING, // [str] [boxstr]
                        call::global::emitx(lref(0)),
                    ),
                    (
                        tags::BOXED_ZDT, // [zdt] [boxzdt]
                        call::global::emitx(lref(0)),
                    ),
                    (
                        tags::LIST_CONS,
                        force(
                            call::bif::emit_seq_start(), // [()] [h t] [arg]
                            force(
                                call::global::render_items(lref(3)),
                                call::bif::emit_seq_end(),
                            ),
                        ),
                    ),
                    (
                        tags::LIST_NIL,
                        force(
                            call::bif::emit_seq_start(), // [()] [h t] [arg]
                            call::bif::emit_seq_end(),
                        ),
                    ),
                    (
                        tags::BLOCK,
                        force(
                            call::bif::emit_block_start(), // [()] [cons] [arg]
                            force(
                                call::global::render_block_items(lref(1)),
                                call::bif::emit_block_end(),
                            ),
                        ),
                    ),
                ],
                call::bif::panic(str("unrenderable")),
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
                            tags::BOOL_FALSE, // [] [s r] [arg]
                            app(lref(1), vec![lref(2)]),
                        ),
                        (tags::BOOL_TRUE, unit()),
                    ],
                ),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for Render {
    fn execute(&self, _machine: &mut Machine, _argss: &[Ref]) -> Result<(), ExecutionError> {
        panic!("RENDER is STG only")
    }
}

/// Render as a document
pub struct RenderDoc;

impl StgWrapper for RenderDoc {
    fn name(&self) -> &str {
        "RENDER_DOC"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            1, // [renderee]
            force(
                call::bif::emit_doc_start(), // [()] [renderee]
                force(call::global::render(lref(1)), call::bif::emit_doc_end()),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for RenderDoc {
    fn execute(&self, _machine: &mut Machine, _argss: &[Ref]) -> Result<(), ExecutionError> {
        panic!("RENDER_DOC is STG only")
    }
}

/// Sequentially render items from a list and return unit
pub struct RenderItems;

impl StgWrapper for RenderItems {
    fn name(&self) -> &str {
        "RENDER_ITEMS"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            1, // [cons]
            case(
                local(0),
                vec![
                    (
                        tags::LIST_CONS, // [h t] [cons]
                        force(
                            call::global::render(lref(0)),
                            call::global::render_items(lref(2)), // [()] [h t] [cons]
                        ),
                    ),
                    (
                        tags::LIST_NIL, // [cons]
                        unit(),
                    ),
                ],
                call::bif::panic(str("improper list")),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for RenderItems {
    fn execute(&self, _machine: &mut Machine, _argss: &[Ref]) -> Result<(), ExecutionError> {
        panic!("RENDER_ITEMS is STG only")
    }
}

/// Sequentially render items from a list and return unit
pub struct RenderBlockItems;

impl StgWrapper for RenderBlockItems {
    fn name(&self) -> &str {
        "RENDER_BLOCK_ITEMS"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            1, // [cons]
            case(
                local(0),
                vec![
                    (
                        tags::LIST_CONS, // [h t] [cons]
                        force(
                            call::global::render_kv(lref(0)),
                            call::global::render_block_items(lref(2)), // [()] [h t] [cons]
                        ),
                    ),
                    (
                        tags::LIST_NIL, // [cons]
                        unit(),
                    ),
                ],
                call::global::panic(str("improper list")),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for RenderBlockItems {
    fn execute(&self, _machine: &mut Machine, _argss: &[Ref]) -> Result<(), ExecutionError> {
        panic!("RENDER_BLOCK_ITEMS is STG only")
    }
}

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

impl StgWrapper for Saturated {
    fn name(&self) -> &str {
        "SATURATED"
    }
}

impl StgIntrinsic for Saturated {
    fn execute(&self, machine: &mut Machine, args: &[Ref]) -> Result<(), ExecutionError> {
        let closure = machine.resolve(&args[0])?;
        machine_return_bool(machine, closure.remaining_arity() == 0)
    }
}

/// Determine whether the specified metadata suppresses render
pub struct Suppresses;

impl StgWrapper for Suppresses {
    fn name(&self) -> &str {
        "SUPPRESSES"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            1,
            let_(
                vec![value(box_sym("normal"))],
                case(
                    local(1), // [:normal] [arg]
                    vec![(
                        tags::BLOCK,
                        // [xs] [:normal] [arg]
                        unbox_sym(
                            call::global::lookup_or_unboxed(sym("export"), lref(1), lref(2)),
                            // [boxsym] [xs] [:normal] [arg]
                            call::global::eq(lref(0), sym("suppress")),
                        ),
                    )],
                    f(),
                ),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for Suppresses {
    fn execute(&self, _machine: &mut Machine, _argss: &[Ref]) -> Result<(), ExecutionError> {
        panic!("SUPPRESSES is STG only")
    }
}

/// Render a key and a value from the provided "pair", so long as
/// output is not suppressed and the value is renderable (i.e. not a
/// lambda).
pub struct RenderKv;

impl StgWrapper for RenderKv {
    fn name(&self) -> &str {
        "RENDER_KV"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        let value_renderable = lambda(
            1,
            letrec_(
                // [saturated suppresses unsuppressed] [v]
                vec![
                    value(call::global::saturated(lref(3))),
                    value(demeta(local(3), call::global::suppresses(lref(0)), f())),
                    value(call::global::not(lref(1))),
                ],
                call::global::and(lref(0), lref(2)),
            ),
        );

        annotated_lambda(
            1, // [kv]
            case(
                local(0),
                vec![
                    (
                        tags::BLOCK_PAIR, // [k v] [kv]
                        switch(
                            let_(
                                vec![value_renderable],
                                // [v_r] [k v] [kv]
                                app(lref(0), vec![lref(2)]),
                            ),
                            vec![
                                (
                                    tags::BOOL_TRUE,
                                    force(
                                        call::bif::emit_native(lref(0)), // [()] [k v] [kv]
                                        call::global::render(lref(2)),
                                    ),
                                ),
                                (tags::BOOL_FALSE, unit()),
                            ],
                        ),
                    ),
                    (
                        tags::BLOCK_KV_LIST, // [cons] [kv]
                        call::global::render_items(lref(0)),
                    ),
                ],
                unit(),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for RenderKv {
    fn execute(&self, _machine: &mut Machine, _argss: &[Ref]) -> Result<(), ExecutionError> {
        panic!("RENDER_KV is STG only")
    }
}

#[cfg(test)]
pub mod tests {

    use gcmodule::Cc;
    use std::rc::Rc;

    use super::*;

    use crate::eval::{
        emit::{CapturingEmitter, Event, RenderMetadata},
        machines::stg::{
            block, boolean, emit, env, eq, machine::Machine, panic, runtime, syntax::StgSyn,
        },
        primitive::Primitive,
    };

    lazy_static! {
        static ref RUNTIME: Box<dyn runtime::Runtime> = {
            let mut rt = runtime::StandardRuntime::default();
            rt.add(Box::new(emit::Emit0));
            rt.add(Box::new(emit::EmitT));
            rt.add(Box::new(emit::EmitF));
            rt.add(Box::new(emit::EmitNative));
            rt.add(Box::new(emit::EmitSeqStart));
            rt.add(Box::new(emit::EmitSeqEnd));
            rt.add(Box::new(emit::EmitBlockStart));
            rt.add(Box::new(emit::EmitBlockEnd));
            rt.add(Box::new(Render));
            rt.add(Box::new(RenderItems));
            rt.add(Box::new(RenderBlockItems));
            rt.add(Box::new(RenderKv));
            rt.add(Box::new(Saturated));
            rt.add(Box::new(Suppresses));
            rt.add(Box::new(block::Kv));
            rt.add(Box::new(block::LookupOr(runtime::NativeVariant::Unboxed)));
            rt.add(Box::new(block::MatchesKey));
            rt.add(Box::new(block::ExtractValue));
            rt.add(Box::new(eq::Eq));
            rt.add(Box::new(panic::Panic));
            rt.add(Box::new(boolean::And));
            rt.add(Box::new(boolean::Not));
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
            Box::new(CapturingEmitter::default()),
            true,
        )
    }

    #[test]
    pub fn test_render_num() {
        let syntax = letrec_(vec![value(box_num(42))], call::global::render(lref(0)));

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), unit());
    }

    #[test]
    pub fn test_render_str() {
        let syntax = letrec_(vec![value(box_str("foo"))], call::global::render(lref(0)));

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), unit());
    }

    #[test]
    pub fn test_render_sym() {
        let syntax = letrec_(vec![value(box_sym("bar"))], call::global::render(lref(0)));

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), unit());
    }

    #[test]
    pub fn test_suppresses_true() {
        let syntax = letrec_(
            vec![
                value(box_sym("suppress")),
                value(data(tags::BLOCK_PAIR, vec![sym("export"), lref(0)])),
                value(call::global::kv(lref(1))),
                value(data(tags::LIST_NIL, vec![])),
                value(data(tags::LIST_CONS, vec![lref(2), lref(3)])),
                value(data(tags::BLOCK, vec![lref(4)])),
            ],
            call::global::suppresses(lref(5)),
        );

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), t());
    }

    #[test]
    pub fn test_suppresses_false() {
        let syntax = letrec_(
            vec![
                value(box_sym("suppress")),
                value(data(tags::BLOCK_PAIR, vec![sym("normal"), lref(0)])),
                value(call::global::kv(lref(1))),
                value(data(tags::LIST_NIL, vec![])),
                value(data(tags::LIST_CONS, vec![lref(2), lref(3)])),
                value(data(tags::BLOCK, vec![lref(4)])),
            ],
            call::global::suppresses(lref(5)),
        );

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), f());
    }

    #[test]
    pub fn test_suppresses_false_by_default() {
        let syntax = letrec_(
            vec![
                value(data(tags::LIST_NIL, vec![])),
                value(data(tags::BLOCK, vec![lref(0)])),
            ],
            call::global::suppresses(lref(1)),
        );

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), f());
    }

    #[test]
    pub fn test_render_list() {
        let syntax = letrec_(
            vec![
                value(box_sym("foo")),
                value(box_sym("bar")),
                value(data(tags::LIST_NIL, vec![])),
                value(data(tags::LIST_CONS, vec![lref(1), lref(2)])),
                value(data(tags::LIST_CONS, vec![lref(0), lref(3)])),
            ],
            call::global::render(lref(4)),
        );

        let mut m = machine(syntax);
        m.safe_run(200).unwrap();
        assert_eq!(
            m.emitter().captures(),
            &[
                Event::OutputSequenceStart(RenderMetadata::empty()),
                Event::OutputScalar(RenderMetadata::empty(), Primitive::Sym("foo".to_string())),
                Event::OutputScalar(RenderMetadata::empty(), Primitive::Sym("bar".to_string())),
                Event::OutputSequenceEnd
            ]
        );
        assert_eq!(*m.closure().code(), unit());
    }

    #[test]
    pub fn test_render_block() {
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
            ],
            call::global::render(lref(9)),
        );
        let mut m = machine(syntax);
        m.safe_run(300).unwrap();
        assert_eq!(*m.closure().code(), unit());
    }

    #[test]
    pub fn test_render_empty_block() {
        let syntax = letrec_(
            vec![
                value(box_str("v1")),
                value(data(tags::BLOCK_PAIR, vec![sym("k1"), lref(0)])),
                value(call::global::kv(lref(1))),
                value(data(tags::LIST_NIL, vec![])),
                value(data(tags::LIST_CONS, vec![lref(2), lref(3)])),
                value(data(tags::BLOCK, vec![lref(4)])),
            ],
            call::global::render(lref(5)),
        );

        let mut m = machine(syntax);
        m.safe_run(200).unwrap();
        assert_eq!(*m.closure().code(), unit());
    }

    #[test]
    pub fn test_collect() {
        let syntax = let_(
            vec![value(unit())],
            switch(
                letrec_(vec![value(unit())], local(0)),
                vec![(tags::UNIT, local(0))],
            ),
        );
        let mut m = machine(syntax);
        m.safe_run(300).unwrap();
    }
}