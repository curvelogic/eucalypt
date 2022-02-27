//! EQ intrinsic

use std::rc::Rc;

use serde_json::Number;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal1, CallGlobal2, IntrinsicMachine, StgIntrinsic},
        memory::{
            mutator::MutatorHeapView,
            string::HeapString,
            syntax::{Native, Ref, RefPtr},
        },
    },
};

use super::{
    block::{ExtractKey, ExtractValue},
    boolean::And,
    support::machine_return_bool,
    syntax::StgSyn,
    tags::DataConstructor,
};
use super::{
    syntax::{dsl::*, LambdaForm},
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

/// Compare two numbers for equality
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

/// Compare two heap strings for equality
fn str_eq(view: MutatorHeapView, x: RefPtr<HeapString>, y: RefPtr<HeapString>) -> bool {
    let sx = &*view.scoped(x);
    let sy = &*view.scoped(y);
    sx.as_str() == sy.as_str()
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
                    local(2),                                            // [x-eval] [x y]
                    app_bif(self.index() as u8, vec![lref(0), lref(1)]), // [y-eval] [x-eval] [x y]
                ),
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
        let x = machine.nav(view).resolve_native(&args[0])?;
        let y = machine.nav(view).resolve_native(&args[1])?;
        let eq = match (x, y) {
            (Native::Num(ref nx), Native::Num(ref ny)) => num_eq(nx, ny),
            (Native::Str(sx), Native::Str(sy)) => str_eq(view, sx, sy),
            (Native::Sym(sx), Native::Sym(sy)) => str_eq(view, sx, sy), // TODO: interning
            (l, r) => l == r,
        };
        machine_return_bool(machine, view, eq)
    }
}

impl CallGlobal2 for Eq {}

#[cfg(test)]
pub mod tests {

    use serde_json::Number;

    use super::*;
    use crate::eval::stg::{boolean::And, eq::Eq, panic::Panic, runtime::Runtime, testing};

    pub fn runtime() -> Box<dyn Runtime> {
        testing::runtime(vec![Box::new(Eq), Box::new(And), Box::new(Panic)])
    }

    #[test]
    pub fn test_box_syms() {
        let syntax = letrec_(
            vec![value(box_sym("foo")), value(box_sym("foo"))],
            Eq.global(lref(0), lref(1)),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.bool_return(), Some(true));
    }

    #[test]
    pub fn test_box_strs() {
        let syntax = letrec_(
            vec![value(box_str("foo")), value(box_str("foo"))],
            Eq.global(lref(0), lref(1)),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.bool_return(), Some(true));
    }

    #[test]
    pub fn test_box_nums() {
        let syntax = letrec_(
            vec![
                value(box_num(Number::from_f64(std::f64::consts::PI).unwrap())),
                value(box_num(Number::from_f64(std::f64::consts::PI).unwrap())),
            ],
            Eq.global(lref(0), lref(1)),
        );

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.bool_return(), Some(true));
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

        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        assert_eq!(m.bool_return(), Some(true));
    }
}
