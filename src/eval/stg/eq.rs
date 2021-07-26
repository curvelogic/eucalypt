//! EQ intrinsic

use std::rc::Rc;

use serde_json::Number;

use crate::{common::sourcemap::Smid, eval::error::ExecutionError};

use super::{
    block::{ExtractKey, ExtractValue},
    boolean::And,
    intrinsic::{CallGlobal1, CallGlobal2},
    syntax::{Ref, StgSyn},
    tags::DataConstructor,
};
use super::{
    intrinsic::StgIntrinsic,
    machine::Machine,
    runtime::{call, machine_return_bool},
    syntax::{dsl::*, LambdaForm, Native},
    tags::Tag,
};

/// Equality recurses through data structures lazily and calls the
/// intrinsic only for natives.
pub struct Eq;

fn nullary_branch(tag: Tag) -> (Tag, Rc<StgSyn>) {
    (tag, case(local(1), vec![(tag, t())], f()))
}

fn unary_branch(tag: Tag) -> (Tag, Rc<StgSyn>) {
    (
        tag, // [x-content] [x y]
        case(
            local(2),
            vec![(
                tag, // [y-content] [x-content] [x y]
                Eq.global(lref(1), lref(0)),
            )],
            f(),
        ),
    )
}

fn binary_branch(tag: Tag) -> (Tag, Rc<StgSyn>) {
    (
        tag, // [xh xt] [x y]
        case(
            local(3),
            vec![(
                tag, // [yh yt] [xh xt] [x y]
                let_(
                    vec![
                        value(Eq.global(lref(2), lref(0))),
                        value(Eq.global(lref(3), lref(1))),
                    ],
                    And.global(lref(0), lref(1)),
                ),
            )],
            f(),
        ),
    )
}

fn num_eq(x: &Number, y: &Number) -> bool {
    if let (Some(l), Some(r)) = (x.as_i64(), y.as_i64()) {
        l == r
    } else if let (Some(l), Some(r)) = (x.as_u64(), y.as_u64()) {
        l == r
    } else if let (Some(l), Some(r)) = (x.as_f64(), y.as_f64()) {
        l == r
    } else {
        false
    }
}

impl StgIntrinsic for Eq {
    fn name(&self) -> &str {
        "EQ"
    }

    /// Switch on data type and recur or fallback to intrinsic for
    /// natives.
    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            2,
            case(
                local(0), // [x y]
                vec![
                    nullary_branch(DataConstructor::Unit.tag()),
                    nullary_branch(DataConstructor::BoolTrue.tag()),
                    nullary_branch(DataConstructor::BoolFalse.tag()),
                    nullary_branch(DataConstructor::ListNil.tag()),
                    // unary
                    unary_branch(DataConstructor::Block.tag()),
                    unary_branch(DataConstructor::BoxedNumber.tag()),
                    unary_branch(DataConstructor::BoxedString.tag()),
                    unary_branch(DataConstructor::BoxedSymbol.tag()),
                    // binary
                    binary_branch(DataConstructor::ListCons.tag()),
                    // block pair can be eq to block kv_list
                    (
                        DataConstructor::BlockPair.tag(),
                        // [xk xv] [x y]
                        switch(
                            local(3),
                            vec![
                                (
                                    DataConstructor::BlockPair.tag(),
                                    // [yk yv] [xk xv] [x y]
                                    let_(
                                        vec![
                                            value(Eq.global(lref(2), lref(0))),
                                            value(Eq.global(lref(3), lref(1))),
                                        ],
                                        And.global(lref(0), lref(1)),
                                    ),
                                ),
                                (
                                    DataConstructor::BlockKvList.tag(),
                                    // [. . . .] [_ycons] [xk xv] [x y]
                                    letrec_(
                                        vec![
                                            value(ExtractKey.global(lref(8))),
                                            value(ExtractValue.global(lref(8))),
                                            value(Eq.global(lref(0), lref(5))),
                                            value(Eq.global(lref(1), lref(6))),
                                        ],
                                        And.global(lref(2), lref(3)),
                                    ),
                                ),
                            ],
                        ),
                    ),
                    (
                        DataConstructor::BlockKvList.tag(),
                        // [xlcons] [x y]
                        switch(
                            local(2),
                            vec![
                                (
                                    DataConstructor::BlockPair.tag(),
                                    // [. . . .] [yk yv] [_xcons] [x y]
                                    letrec_(
                                        vec![
                                            value(ExtractKey.global(lref(7))),
                                            value(ExtractValue.global(lref(7))),
                                            value(Eq.global(lref(0), lref(4))),
                                            value(Eq.global(lref(1), lref(5))),
                                        ],
                                        And.global(lref(2), lref(3)),
                                    ),
                                ),
                                (
                                    DataConstructor::BlockKvList.tag(),
                                    // [ycons] [xcons] [x y]
                                    Eq.global(lref(1), lref(0)),
                                ),
                            ],
                        ),
                    ),
                ],
                // native and unknown tags
                force(
                    local(2),                        // [x-eval] [x y]
                    call::bif::eq(lref(0), lref(1)), // [y-eval] [x-eval] [x y]
                ),
            ),
            annotation,
        )
    }

    fn execute(&self, machine: &mut Machine, args: &[Ref]) -> Result<(), ExecutionError> {
        let x = machine.resolve_native(&args[0])?;
        let y = machine.resolve_native(&args[1])?;
        let eq = match (x, y) {
            (Native::Num(ref nx), Native::Num(ref ny)) => num_eq(nx, ny),
            (l, r) => l == r,
        };
        machine_return_bool(machine, eq)
    }
}

impl CallGlobal2 for Eq {}

#[cfg(test)]
pub mod tests {

    use std::rc::Rc;

    use serde_json::Number;

    use super::*;
    use crate::{
        common::sourcemap::SourceMap,
        eval::{
            emit::DebugEmitter,
            stg::{
                boolean::And,
                env,
                eq::Eq,
                machine::Machine,
                panic::Panic,
                runtime::{self, Runtime},
                syntax::StgSyn,
            },
        },
    };

    lazy_static! {
        static ref RUNTIME: Box<dyn runtime::Runtime> = {
            let mut rt = runtime::StandardRuntime::default();
            rt.add(Box::new(Panic));
            rt.add(Box::new(Eq));
            rt.add(Box::new(And));
            rt.prepare(&mut SourceMap::default());
            Box::new(rt)
        };
    }

    /// Construct a machine with the arithmetic intrinsics
    pub fn machine(syntax: Rc<StgSyn>) -> Machine<'static> {
        let env = env::EnvFrame::default();
        Machine::new(
            syntax,
            Rc::new(env),
            RUNTIME.globals(),
            RUNTIME.intrinsics(),
            Box::new(DebugEmitter::default()),
            true,
        )
    }

    #[test]
    pub fn test_box_syms() {
        let syntax = letrec_(
            vec![value(box_sym("foo")), value(box_sym("foo"))],
            Eq.global(lref(0), lref(1)),
        );

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), t());
    }

    #[test]
    pub fn test_box_strs() {
        let syntax = letrec_(
            vec![value(box_str("foo")), value(box_str("foo"))],
            Eq.global(lref(0), lref(1)),
        );

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), t());
    }

    #[test]
    pub fn test_box_nums() {
        let syntax = letrec_(
            vec![
                value(box_num(Number::from_f64(3.14159265).unwrap())),
                value(box_num(Number::from_f64(3.14159265).unwrap())),
            ],
            Eq.global(lref(0), lref(1)),
        );

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), t());
    }

    #[test]
    pub fn test_block_pair() {
        let syntax = letrec_(
            vec![
                value(box_str("value")),
                value(data(
                    DataConstructor::BlockPair.tag(),
                    vec![sym("key"), lref(0)],
                )),
                value(box_str("value")),
                value(data(
                    DataConstructor::BlockPair.tag(),
                    vec![sym("key"), lref(2)],
                )),
            ],
            Eq.global(lref(1), lref(3)),
        );

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), t());
    }
}
