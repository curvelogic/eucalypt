//! EQ intrinsic

use std::rc::Rc;

use serde_json::Number;

use crate::{common::sourcemap::SourceMap, eval::error::ExecutionError};

use super::syntax::{tags, Ref, StgSyn};
use super::{
    intrinsic::StgIntrinsic,
    machine::Machine,
    runtime::{call, machine_return_bool},
    syntax::{dsl::*, LambdaForm, Native, Tag},
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
                call::global::eq(lref(1), lref(0)),
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
                        value(call::global::eq(lref(2), lref(0))),
                        value(call::global::eq(lref(3), lref(1))),
                    ],
                    call::global::and(lref(0), lref(1)),
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
    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            2,
            case(
                local(0), // [x y]
                vec![
                    nullary_branch(tags::UNIT),
                    nullary_branch(tags::BOOL_TRUE),
                    nullary_branch(tags::BOOL_FALSE),
                    nullary_branch(tags::LIST_NIL),
                    // unary
                    unary_branch(tags::BLOCK),
                    unary_branch(tags::BOXED_NUMBER),
                    unary_branch(tags::BOXED_STRING),
                    unary_branch(tags::BOXED_SYMBOL),
                    // binary
                    binary_branch(tags::LIST_CONS),
                    // block pair can be eq to block kv_list
                    (
                        tags::BLOCK_PAIR,
                        // [xk xv] [x y]
                        switch(
                            local(3),
                            vec![
                                (
                                    tags::BLOCK_PAIR,
                                    // [yk yv] [xk xv] [x y]
                                    let_(
                                        vec![
                                            value(call::global::eq(lref(2), lref(0))),
                                            value(call::global::eq(lref(3), lref(1))),
                                        ],
                                        call::global::and(lref(0), lref(1)),
                                    ),
                                ),
                                (
                                    tags::BLOCK_KV_LIST,
                                    // [. . . .] [_ycons] [xk xv] [x y]
                                    letrec_(
                                        vec![
                                            value(call::global::extract_key(lref(8))),
                                            value(call::global::extract_value(lref(8))),
                                            value(call::global::eq(lref(0), lref(5))),
                                            value(call::global::eq(lref(1), lref(6))),
                                        ],
                                        call::global::and(lref(2), lref(3)),
                                    ),
                                ),
                            ],
                        ),
                    ),
                    (
                        tags::BLOCK_KV_LIST,
                        // [xlcons] [x y]
                        switch(
                            local(2),
                            vec![
                                (
                                    tags::BLOCK_PAIR,
                                    // [. . . .] [yk yv] [_xcons] [x y]
                                    letrec_(
                                        vec![
                                            value(call::global::extract_key(lref(7))),
                                            value(call::global::extract_value(lref(7))),
                                            value(call::global::eq(lref(0), lref(4))),
                                            value(call::global::eq(lref(1), lref(5))),
                                        ],
                                        call::global::and(lref(2), lref(3)),
                                    ),
                                ),
                                (
                                    tags::BLOCK_KV_LIST,
                                    // [ycons] [xcons] [x y]
                                    call::global::eq(lref(1), lref(0)),
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
            source_map.add_synthetic(self.name()),
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

#[cfg(test)]
pub mod tests {

    use gcmodule::Cc;
    use std::rc::Rc;

    use serde_json::Number;

    use super::*;
    use crate::eval::{
        emit::DebugEmitter,
        machines::stg::{
            boolean::And, env, eq::Eq, machine::Machine, panic::Panic, runtime, syntax::StgSyn,
        },
    };

    lazy_static! {
        static ref RUNTIME: Box<dyn runtime::Runtime> = {
            let mut rt = runtime::StandardRuntime::default();
            rt.add(Box::new(Panic));
            rt.add(Box::new(Eq));
            rt.add(Box::new(And));
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
    pub fn test_box_syms() {
        let syntax = letrec_(
            vec![value(box_sym("foo")), value(box_sym("foo"))],
            call::global::eq(lref(0), lref(1)),
        );

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), t());
    }

    #[test]
    pub fn test_box_strs() {
        let syntax = letrec_(
            vec![value(box_str("foo")), value(box_str("foo"))],
            call::global::eq(lref(0), lref(1)),
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
            call::global::eq(lref(0), lref(1)),
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
                value(data(tags::BLOCK_PAIR, vec![sym("key"), lref(0)])),
                value(box_str("value")),
                value(data(tags::BLOCK_PAIR, vec![sym("key"), lref(2)])),
            ],
            call::global::eq(lref(1), lref(3)),
        );

        let mut m = machine(syntax);
        m.safe_run(100).unwrap();
        assert_eq!(*m.closure().code(), t());
    }
}
