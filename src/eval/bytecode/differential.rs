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

mod tests {
    use super::*;
    use crate::eval::stg::arith::Add;
    use crate::eval::stg::force::ForceWhnf;
    use crate::eval::stg::syntax::dsl;

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
