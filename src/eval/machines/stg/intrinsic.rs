//! The trait for intrinsic functions / constants

use std::convert::TryInto;

use crate::{
    common::sourcemap::SourceMap,
    eval::{error::ExecutionError, intrinsics, types::IntrinsicType},
};

use super::{
    machine::Machine,
    syntax::{
        dsl::{
            annotated_lambda, app_bif, data, force, let_, local, lref, unbox_num, unbox_str,
            unbox_sym, unbox_zdt, value,
        },
        tags, LambdaForm, Ref,
    },
};

/// All intrinsics have an STG syntax wrapper
pub trait StgIntrinsic: Sync {
    /// The name of the intrinsic
    fn name(&self) -> &str;

    /// The STG wrapper for calling the intrinsic
    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        wrap(self.index(), self.info(), source_map)
    }

    /// Index of the intrinsic
    fn index(&self) -> usize {
        intrinsics::index(self.name()).unwrap()
    }

    /// Type and arity information for the intrinsic
    fn info(&self) -> &intrinsics::Intrinsic {
        intrinsics::intrinsic(self.index())
    }

    /// An intrinsic has mutable access to the machine
    ///
    /// A call to an intrinsic may assume that its strict arguments are
    /// already evaluated (by the corresponding global wrapper) but must
    /// take care of updating the machine's closure and stack as
    /// appropriate to constitute a return.
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("{} is STG-only", self.name());
    }
}

/// Basic intrinsic wrapper that evals and unboxes strict arguments
///
/// Type checks? Unbox?
pub fn wrap(index: usize, info: &intrinsics::Intrinsic, source_map: &mut SourceMap) -> LambdaForm {
    let arity = info.arity();

    // nullaries can go direct to the intrinsic
    if arity == 0 {
        return value(app_bif(index.try_into().unwrap(), vec![]));
    }

    let return_type = info.ty().ret(arity - 1);

    // Precalculate the offsets we'll need to access the evaluated
    // arguments.
    //
    // Unbox / force uses two envs, force just one.
    // We eval left to right, the later arguments will be accessible
    // at shallower depths.
    let mut offset = 0;
    let mut offsets = vec![0];
    for i in info.strict_args() {
        match info.ty().arg(*i) {
            Some(IntrinsicType::Number)
            | Some(IntrinsicType::String)
            | Some(IntrinsicType::Symbol)
            | Some(IntrinsicType::ZonedDateTime) => {
                offset += 2;
            }
            _ => {
                offset += 1;
            }
        }
        offsets.push(offset);
    }

    // if lambda args are [x y z], by evaling x first, then y, then z,
    // we end up with the unboxed x deeper in the environment than y
    // and z, e.g.:
    //
    // [eval-z] [eval-unbox-y] [unbox-y] [eval-unbox-x] [unbox-x] [x y z]
    //
    // so strict arg indexes are reversed, non-strict args reach deep
    // into the lambda bound var environment
    let mut args: Vec<usize> = vec![0; arity];
    let strict_depth = offsets[offsets.len() - 1];
    let mut counter = 1;
    for (i, arg) in args.iter_mut().enumerate().take(arity) {
        if info.strict_args().contains(&i) {
            *arg = strict_depth - offsets[counter];
            counter += 1;
        } else {
            *arg = strict_depth + i;
        }
    }

    let mut syntax = app_bif(
        index.try_into().unwrap(),
        args.iter().map(|i| lref(*i)).collect(),
    );

    // using let_ for boxing leaves indexes undisturbed
    match return_type {
        Some(IntrinsicType::Number) => {
            syntax = let_(vec![value(syntax)], data(tags::BOXED_NUMBER, vec![lref(0)]));
        }
        Some(IntrinsicType::String) => {
            syntax = let_(vec![value(syntax)], data(tags::BOXED_STRING, vec![lref(0)]));
        }
        Some(IntrinsicType::Symbol) => {
            syntax = let_(vec![value(syntax)], data(tags::BOXED_SYMBOL, vec![lref(0)]));
        }
        Some(IntrinsicType::ZonedDateTime) => {
            syntax = let_(vec![value(syntax)], data(tags::BOXED_ZDT, vec![lref(0)]));
        }
        _ => {}
    }

    // At present our boxes are non-strict data structures which can
    // hold thunks (e.g suspended bif calls) - therefore unboxes in
    // itself is not enough to satisfy a strict intrinisc, we must
    // unbox then force. So our environments proliferate rather...

    // each wrapping case / unbox adds one environment layer. there
    // are strict_len of them so the innermost has a reference that
    // traverses all of them plus the index into the lambda args
    //
    // by the time we're in the heart of the innermost CASE, the envs
    // look like (assume z does not need unboxing)
    // [eval-z] [eval-unbox-y] [unbox-y] [eval-unbox-x] [unbox-x] [x y z]
    //                                   ^                        ^
    //                                   |                        |
    //                                   |             (first unbox sees x y z as 0 1 2)
    //           (2nd unbox sees eval-unbox-x as 0, x y z as 2 3 4)
    //
    // In this scenario, offsets contains [0, 2, 4, 5]
    // etc.
    let mut offset_iter = offsets.iter().rev();
    let _ = offset_iter.next(); // discard total offset
    for i in info.strict_args().iter().rev() {
        let arg_offset = offset_iter.next().unwrap();
        match info.ty().arg(*i) {
            Some(IntrinsicType::Number) => {
                syntax = unbox_num(local(arg_offset + *i), force(local(0), syntax));
            }
            Some(IntrinsicType::String) => {
                syntax = unbox_str(local(arg_offset + *i), force(local(0), syntax));
            }
            Some(IntrinsicType::Symbol) => {
                syntax = unbox_sym(local(arg_offset + *i), force(local(0), syntax));
            }
            Some(IntrinsicType::ZonedDateTime) => {
                syntax = unbox_zdt(local(arg_offset + *i), force(local(0), syntax));
            }
            _ => {
                syntax = force(local(arg_offset + *i), syntax);
            }
        }
    }

    annotated_lambda(
        arity.try_into().unwrap(),
        syntax,
        source_map.add_synthetic(info.name()),
    )
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{
        common::sourcemap::Smid,
        eval::{intrinsics, types},
    };

    #[test]
    pub fn test_wrapper() {
        let intrinsic = intrinsics::Intrinsic::new(
            "TEST",
            types::function(vec![types::num(), types::num(), types::num()]).unwrap(),
            vec![0, 1],
        );
        let wrapper = wrap(99, &intrinsic, &mut SourceMap::default());
        let syntax = annotated_lambda(
            2,
            unbox_num(
                local(0),
                force(
                    local(0),
                    unbox_num(
                        local(3),
                        force(
                            local(0),
                            let_(
                                vec![value(app_bif(99, vec![lref(2), lref(0)]))],
                                data(tags::BOXED_NUMBER, vec![lref(0)]),
                            ),
                        ),
                    ),
                ),
            ),
            Smid::fake(0),
        );

        assert_eq!(wrapper, syntax);
    }
}
