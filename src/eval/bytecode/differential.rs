//! Differential testing: run a synthetic `StgSyn` program through both the
//! HeapSyn engine and the bytecode engine and assert they agree (spec §9,
//! REFINEMENT B — harness-first on synthetic programs, before the prelude).
//!
//! This validates the bytecode machine against the reference engine at the
//! value level (exit code) using the same runtime globals for both. The full
//! `.eu`-corpus, byte-identical-output harness is wired once the `EU_BYTECODE`
//! flag path and the emit/capture lifecycle land.

#![cfg(test)]

use std::rc::Rc;

use crate::common::sourcemap::SourceMap;
use crate::eval::bytecode::{encode, BytecodeMachine};
use crate::eval::emit::NullEmitter;
use crate::eval::machine::intrinsic::StgIntrinsic;
use crate::eval::machine::standard_machine;
use crate::eval::stg::runtime::{Runtime, StandardRuntime};
use crate::eval::stg::syntax::StgSyn;
use crate::eval::stg::StgSettings;

/// Run `syntax` through both engines with a runtime built from `bifs`, and
/// assert the exit codes agree. Returns the (shared) exit code.
fn assert_engines_agree(syntax: Rc<StgSyn>, bifs: Vec<Box<dyn StgIntrinsic>>) -> Option<u8> {
    let mut rt = StandardRuntime::default();
    for b in bifs {
        rt.add(b);
    }
    rt.prepare(&mut SourceMap::default());
    let settings = StgSettings::default();

    // Reference: the HeapSyn tree-walk machine.
    let heap_exit = {
        let mut m = standard_machine(&settings, syntax.clone(), Box::new(NullEmitter), &rt)
            .expect("build HeapSyn machine");
        m.run(Some(1_000_000)).expect("HeapSyn run")
    };

    // Under test: the bytecode machine, sharing the same runtime globals.
    let bc_exit = {
        let globals = rt.globals();
        let (prog, root, gforms) = encode(&syntax, &globals);
        let mut m = BytecodeMachine::new(
            prog,
            root,
            &gforms,
            rt.intrinsics(),
            Box::new(NullEmitter),
            0,
            false,
        )
        .expect("build bytecode machine");
        m.run(Some(1_000_000)).expect("bytecode run")
    };

    assert_eq!(
        heap_exit, bc_exit,
        "HeapSyn and bytecode engines disagree on exit code"
    );
    heap_exit
}

/// Run `syntax` through both engines with a YAML emitter capturing to a
/// buffer, and assert the rendered output agrees. Uses the full standard
/// runtime (all emit/render intrinsics). Returns the shared output string.
fn assert_engines_render_agree(syntax: Rc<StgSyn>) -> String {
    use crate::eval::stg::make_standard_runtime;
    let mut source_map = SourceMap::default();
    let mut rt = make_standard_runtime(&mut source_map);
    rt.prepare(&mut source_map);
    let settings = StgSettings::default();

    // Reference: HeapSyn engine rendering to a YAML buffer.
    let mut heap_buf: Vec<u8> = Vec::new();
    {
        let mut emitter =
            crate::export::create_emitter("yaml", &mut heap_buf).expect("yaml emitter");
        emitter.stream_start();
        let mut m = standard_machine(&settings, syntax.clone(), emitter, rt.as_ref())
            .expect("build HeapSyn machine");
        m.run(None).expect("HeapSyn run");
        m.take_emitter().stream_end();
    }
    let heap_out = String::from_utf8(heap_buf).expect("HeapSyn output utf-8");

    // Under test: bytecode engine, same runtime globals + intrinsics.
    let mut bc_buf: Vec<u8> = Vec::new();
    {
        let mut emitter = crate::export::create_emitter("yaml", &mut bc_buf).expect("yaml emitter");
        emitter.stream_start();
        let (prog, root, gforms) = encode(&syntax, &rt.globals());
        let mut m = BytecodeMachine::new(prog, root, &gforms, rt.intrinsics(), emitter, 0, false)
            .expect("build bytecode machine");
        m.run(None).expect("bytecode run");
        m.take_emitter().stream_end();
    }
    let bc_out = String::from_utf8(bc_buf).expect("bytecode output utf-8");

    assert_eq!(
        heap_out, bc_out,
        "HeapSyn and bytecode engines disagree on rendered output"
    );
    heap_out
}

mod tests {
    use super::*;
    use crate::eval::stg::arith::{Add, Lt};
    use crate::eval::stg::boolean::{False, True};
    use crate::eval::stg::force::ForceWhnf;
    use crate::eval::stg::list::IsList;
    use crate::eval::stg::syntax::dsl;
    use crate::eval::stg::tags::DataConstructor;

    /// `case __ISLIST(value) { BoolTrue -> 1 ; BoolFalse -> 0 }`.
    fn islist_case(value: crate::eval::stg::syntax::LambdaForm) -> Rc<StgSyn> {
        let islist = crate::eval::intrinsics::index_u8("ISLIST");
        dsl::let_(
            vec![value],
            dsl::case(
                dsl::app_bif(islist, vec![dsl::lref(0)]),
                vec![
                    (DataConstructor::BoolTrue.tag(), dsl::atom(dsl::num(1))),
                    (DataConstructor::BoolFalse.tag(), dsl::atom(dsl::num(0))),
                ],
                dsl::atom(dsl::num(9)),
            ),
        )
    }

    #[test]
    fn agree_on_arithmetic() {
        // __ADD(2, 3) -> 5
        let syn = dsl::app_bif(
            crate::eval::intrinsics::index_u8("ADD"),
            vec![dsl::num(2), dsl::num(3)],
        );
        assert_eq!(assert_engines_agree(syn, vec![Box::new(Add)]), Some(5));
    }

    #[test]
    fn agree_on_let() {
        // let x = 5 in x -> 5
        let syn = dsl::let_(vec![dsl::value(dsl::atom(dsl::num(5)))], dsl::local(0));
        assert_eq!(assert_engines_agree(syn, vec![]), Some(5));
    }

    #[test]
    fn agree_on_case_over_nil() {
        // case nil { nil -> 7 } else 0 -> 7
        let nil_tag = crate::eval::stg::tags::DataConstructor::ListNil.tag();
        let syn = dsl::case(
            dsl::nil(),
            vec![(nil_tag, dsl::atom(dsl::num(7)))],
            dsl::atom(dsl::num(0)),
        );
        assert_eq!(assert_engines_agree(syn, vec![]), Some(7));
    }

    #[test]
    fn agree_on_application() {
        // let id = \x. x in id(5) -> 5
        let syn = dsl::let_(
            vec![dsl::lambda(1, dsl::local(0))],
            dsl::app(dsl::lref(0), vec![dsl::num(5)]),
        );
        assert_eq!(assert_engines_agree(syn, vec![]), Some(5));
    }

    #[test]
    fn agree_on_partial_application() {
        // letrec k = \x y. x
        //        p = k(9)
        // in p(4) -> 9   (PAP build + saturate)
        let syn = dsl::letrec_(
            vec![
                dsl::lambda(2, dsl::local(0)),
                dsl::thunk(dsl::app(dsl::lref(0), vec![dsl::num(9)])),
            ],
            dsl::app(dsl::lref(1), vec![dsl::num(4)]),
        );
        assert_eq!(assert_engines_agree(syn, vec![]), Some(9));
    }

    #[test]
    fn agree_on_comparison_bool() {
        // case __LT(2, 3) { BoolTrue -> 42 ; BoolFalse -> 7 } else 0 -> 42.
        // Exercises return_bool (a constructor-template return) + case
        // dispatch on the resulting boolean data value.
        let lt = crate::eval::intrinsics::index_u8("LT");
        let syn = dsl::case(
            dsl::app_bif(lt, vec![dsl::num(2), dsl::num(3)]),
            vec![
                (DataConstructor::BoolTrue.tag(), dsl::atom(dsl::num(42))),
                (DataConstructor::BoolFalse.tag(), dsl::atom(dsl::num(7))),
            ],
            dsl::atom(dsl::num(0)),
        );
        assert_eq!(
            assert_engines_agree(syn, vec![Box::new(Lt), Box::new(True), Box::new(False)],),
            Some(42)
        );
    }

    #[test]
    fn agree_on_demeta_meta_value() {
        // demeta(with_meta(99, 42), handler=[meta,body]->body, or_else=[v]->v)
        // A metadata-annotated value takes the handler branch -> body (42).
        let syn = dsl::demeta(
            dsl::with_meta(dsl::num(99), dsl::num(42)),
            dsl::local(1),
            dsl::local(0),
        );
        assert_eq!(assert_engines_agree(syn, vec![]), Some(42));
    }

    #[test]
    fn agree_on_demeta_plain_value() {
        // demeta(7, handler=[meta,body]->body, or_else=[v]->v) -> 7.
        // A plain (non-Meta) value takes the or_else branch with the value.
        let syn = dsl::demeta(dsl::atom(dsl::num(7)), dsl::local(1), dsl::local(0));
        assert_eq!(assert_engines_agree(syn, vec![]), Some(7));
    }

    #[test]
    fn agree_on_lookup_lit_hit() {
        // A block { k: 42 }, then a literal lookup of :k -> 42 (fast path:
        // the object is already a WHNF block).
        use crate::common::sourcemap::Smid;
        use crate::eval::stg::syntax::StgSyn;
        use std::rc::Rc;
        let syn = dsl::letrec_(
            vec![
                dsl::value(dsl::atom(dsl::num(42))),               // 0: v
                dsl::value(dsl::atom(dsl::num(7))),                // 1: w (default)
                dsl::value(dsl::pair("k", dsl::lref(0))),          // 2: BlockPair(:k, v)
                dsl::value(dsl::nil()),                            // 3: nil
                dsl::value(dsl::cons(dsl::lref(2), dsl::lref(3))), // 4: [pair]
                dsl::value(dsl::block(dsl::lref(4))),              // 5: Block([pair], 0)
            ],
            Rc::new(StgSyn::LookupLit {
                smid: Smid::default(),
                key: dsl::sym("k"),
                obj: dsl::lref(5),
                default: dsl::lref(1),
            }),
        );
        assert_eq!(assert_engines_agree(syn, vec![]), Some(42));
    }

    #[test]
    fn agree_on_lookup_lit_miss() {
        // The same block, looking up an absent key -> the default (7).
        use crate::common::sourcemap::Smid;
        use crate::eval::stg::syntax::StgSyn;
        use std::rc::Rc;
        let syn = dsl::letrec_(
            vec![
                dsl::value(dsl::atom(dsl::num(42))),
                dsl::value(dsl::atom(dsl::num(7))),
                dsl::value(dsl::pair("k", dsl::lref(0))),
                dsl::value(dsl::nil()),
                dsl::value(dsl::cons(dsl::lref(2), dsl::lref(3))),
                dsl::value(dsl::block(dsl::lref(4))),
            ],
            Rc::new(StgSyn::LookupLit {
                smid: Smid::default(),
                key: dsl::sym("absent"),
                obj: dsl::lref(5),
                default: dsl::lref(1),
            }),
        );
        assert_eq!(assert_engines_agree(syn, vec![]), Some(7));
    }

    #[test]
    fn agree_on_lookup_lit_slow_path() {
        // The object is an unevaluated thunk `id(block)`, not a WHNF block, so
        // LookupLit forces it (LookupLitForce continuation) before the lookup.
        use crate::common::sourcemap::Smid;
        use crate::eval::stg::syntax::StgSyn;
        use std::rc::Rc;
        let syn = dsl::letrec_(
            vec![
                dsl::value(dsl::atom(dsl::num(42))),                    // 0: v
                dsl::value(dsl::atom(dsl::num(7))),                     // 1: w
                dsl::value(dsl::pair("k", dsl::lref(0))),               // 2: pair
                dsl::value(dsl::nil()),                                 // 3: nil
                dsl::value(dsl::cons(dsl::lref(2), dsl::lref(3))),      // 4: list
                dsl::value(dsl::block(dsl::lref(4))),                   // 5: block (WHNF)
                dsl::lambda(1, dsl::local(0)),                          // 6: id
                dsl::thunk(dsl::app(dsl::lref(6), vec![dsl::lref(5)])), // 7: id(block)
            ],
            Rc::new(StgSyn::LookupLit {
                smid: Smid::default(),
                key: dsl::sym("k"),
                obj: dsl::lref(7),
                default: dsl::lref(1),
            }),
        );
        assert_eq!(assert_engines_agree(syn, vec![]), Some(42));
    }

    #[test]
    fn agree_on_rendered_boxed_number() {
        // RENDER_DOC(boxed 42) — the top-level render path (RenderDoc ->
        // Render -> EMITx) emitting a YAML document. Both engines must produce
        // byte-identical output.
        let render_doc = crate::eval::intrinsics::index("RENDER_DOC").expect("RENDER_DOC");
        let syn = dsl::let_(
            vec![dsl::value(dsl::data(
                DataConstructor::BoxedNumber.tag(),
                vec![dsl::num(42)],
            ))],
            dsl::app(dsl::gref(render_doc), vec![dsl::lref(0)]),
        );
        let out = assert_engines_render_agree(syn);
        assert!(out.contains("42"), "expected 42 in output, got {out:?}");
    }

    #[test]
    fn agree_on_rendered_block() {
        // RENDER_DOC({ k: 42 }) — exercises the block-rendering walker
        // (RenderKv / block start+end) plus scalar emission. Byte-identical
        // YAML output required.
        let render_doc = crate::eval::intrinsics::index("RENDER_DOC").expect("RENDER_DOC");
        let syn = dsl::letrec_(
            vec![
                dsl::value(dsl::data(
                    DataConstructor::BoxedNumber.tag(),
                    vec![dsl::num(42)],
                )), // 0: boxed 42
                dsl::value(dsl::pair("k", dsl::lref(0))), // 1: BlockPair(:k, v)
                dsl::value(dsl::nil()),                   // 2: nil
                dsl::value(dsl::cons(dsl::lref(1), dsl::lref(2))), // 3: [pair]
                dsl::value(dsl::block(dsl::lref(3))),     // 4: block
            ],
            dsl::app(dsl::gref(render_doc), vec![dsl::lref(4)]),
        );
        let out = assert_engines_render_agree(syn);
        assert!(out.contains("k") && out.contains("42"), "got {out:?}");
    }

    #[test]
    fn agree_on_dbg_repr() {
        // RENDER_DOC(DBG_REPR(boxed 42)) — the debug-repr intrinsic, migrated
        // to the neutral ABI (resolve_closure/data_tag/data_field/value_native),
        // must render byte-identically on both engines.
        let dbg_repr = crate::eval::intrinsics::index("DBG_REPR").expect("DBG_REPR");
        let render_doc = crate::eval::intrinsics::index("RENDER_DOC").expect("RENDER_DOC");
        let syn = dsl::letrec_(
            vec![
                dsl::value(dsl::data(
                    DataConstructor::BoxedNumber.tag(),
                    vec![dsl::num(42)],
                )), // 0: boxed 42
                dsl::value(dsl::app(dsl::gref(dbg_repr), vec![dsl::lref(0)])), // 1: DBG_REPR(0)
            ],
            dsl::app(dsl::gref(render_doc), vec![dsl::lref(1)]),
        );
        let out = assert_engines_render_agree(syn);
        assert!(out.contains("42"), "expected 42 in output, got {out:?}");
    }

    #[test]
    fn agree_on_islist_true() {
        // __ISLIST(nil) -> true -> 1. Exercises the migrated list predicate
        // (resolve_closure + data_tag) on the bytecode path.
        let syn = islist_case(dsl::value(dsl::nil()));
        assert_eq!(
            assert_engines_agree(syn, vec![Box::new(IsList), Box::new(True), Box::new(False)]),
            Some(1)
        );
    }

    #[test]
    fn agree_on_islist_false() {
        // __ISLIST(boxed 42) -> false -> 0.
        let syn = islist_case(dsl::value(dsl::data(
            DataConstructor::BoxedNumber.tag(),
            vec![dsl::num(42)],
        )));
        assert_eq!(
            assert_engines_agree(syn, vec![Box::new(IsList), Box::new(True), Box::new(False)]),
            Some(0)
        );
    }

    #[test]
    fn agree_on_list_nth() {
        // case __LIST.NTH(1, [boxed 10, boxed 20]) { BoxedNumber(x) -> x } -> 20.
        // Exercises the migrated data_list_arg (neutral Vec<AbiClosure>) + the
        // ListNth intrinsic on the bytecode path.
        use crate::eval::stg::list::ListNth;
        let boxed = |n: i64| {
            dsl::value(dsl::data(
                DataConstructor::BoxedNumber.tag(),
                vec![dsl::num(n)],
            ))
        };
        let nth = crate::eval::intrinsics::index_u8("LIST.NTH");
        let syn = dsl::letrec_(
            vec![
                boxed(10),                                         // 0
                boxed(20),                                         // 1
                dsl::value(dsl::nil()),                            // 2
                dsl::value(dsl::cons(dsl::lref(1), dsl::lref(2))), // 3: [20]
                dsl::value(dsl::cons(dsl::lref(0), dsl::lref(3))), // 4: [10, 20]
            ],
            dsl::case(
                // Bif arg order is (list, n).
                dsl::app_bif(nth, vec![dsl::lref(4), dsl::num(1)]),
                vec![(DataConstructor::BoxedNumber.tag(), dsl::local(0))],
                dsl::atom(dsl::num(0)),
            ),
        );
        assert_eq!(assert_engines_agree(syn, vec![Box::new(ListNth)]), Some(20));
    }

    #[test]
    fn agree_on_rendered_split_list() {
        // RENDER_DOC(__SPLIT("hi", "")) -> renders ["hi"]. Exercises the
        // migrated list builder (machine_return_str_list -> return_closure_list
        // + data_value + native_value over templates) end-to-end on bytecode.
        let split = crate::eval::intrinsics::index_u8("SPLIT");
        let render_doc = crate::eval::intrinsics::index("RENDER_DOC").expect("RENDER_DOC");
        let syn = dsl::let_(
            vec![dsl::value(dsl::app_bif(
                split,
                vec![dsl::str("hi"), dsl::str("")],
            ))],
            dsl::app(dsl::gref(render_doc), vec![dsl::lref(0)]),
        );
        let out = assert_engines_render_agree(syn);
        assert!(out.contains("hi"), "expected 'hi' in output, got {out:?}");
    }

    #[test]
    fn agree_on_force_whnf() {
        // let t = __ADD(1, 2) in __FORCE_WHNF(t) -> 3
        let add = crate::eval::intrinsics::index_u8("ADD");
        let force = crate::eval::intrinsics::index_u8("__FORCE_WHNF");
        let syn = dsl::let_(
            vec![dsl::thunk(dsl::app_bif(
                add,
                vec![dsl::num(1), dsl::num(2)],
            ))],
            dsl::app_bif(force, vec![dsl::lref(0)]),
        );
        assert_eq!(
            assert_engines_agree(syn, vec![Box::new(Add), Box::new(ForceWhnf)]),
            Some(3)
        );
    }
}
