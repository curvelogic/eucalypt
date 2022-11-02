//! Continuations store in STG stack

use std::fmt;

use crate::eval::{
    memory::{
        alloc::StgObject,
        array::Array,
        collect::{CollectorHeapView, GcScannable, ScanPtr},
        syntax::{HeapSyn, RefPtr},
    },
    stg::tags::Tag,
};

use super::env::{EnvFrame, SynClosure};

/// Continuations used on the stack to record how to handle returns
///
/// Three of the variants are standard STG machine continuations:
/// - `Branch` records CASE branch tables while the scrutinee is
///   evaluated
/// - `Update` defers a thunk update for when the expression is
///   WHNF
/// - `ApplyTo` holds args while the expression in call position is
///   evaluated
///
/// The last variant, `DeMeta` is a specialised case statement to
/// deconstruct a metadata / body pair (which is a data structured
/// encoded directly in the syntax rather than a Cons form).
#[derive(Clone)]
pub enum Continuation {
    /// Expect a data constructor and match to a branch (or native to fallback)
    Branch {
        /// Branches for data constructor destructuring
        branches: Array<(Tag, RefPtr<HeapSyn>)>,
        /// Fallback for unmatched data or native
        fallback: Option<RefPtr<HeapSyn>>,
        /// Environment of case statement
        environment: RefPtr<EnvFrame>,
    },
    /// Update thunk in environment at index i
    Update {
        environment: RefPtr<EnvFrame>,
        index: usize,
    },
    /// Once callable is evaluated, apply to args
    ApplyTo { args: Array<SynClosure> },
    /// Catch metadata and pass it (with body) to handler
    DeMeta {
        /// handler receives metdata and body as bound args
        handler: RefPtr<HeapSyn>,
        /// or_else receives the body as a bound arg
        or_else: RefPtr<HeapSyn>,
        /// Environment of handlers
        environment: RefPtr<EnvFrame>,
    },
}

impl StgObject for Continuation {}

pub fn match_tag(tag: Tag, branches: &[(Tag, RefPtr<HeapSyn>)]) -> Option<RefPtr<HeapSyn>> {
    for (t, body) in branches {
        if *t == tag {
            return Some(*body);
        }
    }
    None
}

impl fmt::Display for Continuation {
    /// Summarise a continuation
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Continuation::Branch {
                branches, fallback, ..
            } => {
                let mut tags: Vec<String> = branches.iter().map(|b| format!("{}", b.0)).collect();
                if fallback.is_some() {
                    tags.push("…".to_string());
                }
                let desc = &tags.join(",");
                write!(f, "⑂<{}>", desc)
            }
            Continuation::Update { index, .. } => {
                write!(f, "☇[ρ,{}]", index)
            }
            Continuation::ApplyTo { args } => {
                write!(f, "•(×{})", args.len())
            }
            Continuation::DeMeta { .. } => {
                write!(f, "ƒ(`,•)")
            }
        }
    }
}

impl GcScannable for Continuation {
    fn scan<'a, 'b>(
        &'a self,
        scope: &'a dyn crate::eval::memory::collect::CollectorScope,
        marker: &'b mut CollectorHeapView<'a>,
    ) -> Vec<ScanPtr<'a>> {
        let mut grey = vec![];
        dbg!("cont");
        match self {
            Continuation::Branch {
                branches,
                fallback,
                environment,
            } => {
                if let Some(data) = branches.allocated_data() {
                    marker.mark(data);
                    for (_tag, branch) in branches.iter() {
                        marker.mark(*branch);
                        grey.push(ScanPtr::from_non_null(scope, *branch));
                    }
                }

                if let Some(fb) = fallback {
                    marker.mark(*fb);
                    grey.push(ScanPtr::from_non_null(scope, *fb));
                }
                marker.mark(*environment);
                grey.push(ScanPtr::from_non_null(scope, *environment));
            }
            Continuation::Update {
                environment,
                index: _,
            } => {
                marker.mark(*environment);
                grey.push(ScanPtr::from_non_null(scope, *environment));
            }
            Continuation::ApplyTo { args } => {
                if let Some(data) = args.allocated_data() {
                    marker.mark(data);
                    for arg in args.iter() {
                        grey.push(ScanPtr::new(scope, arg));
                    }
                }
            }
            Continuation::DeMeta {
                handler,
                or_else,
                environment,
            } => {
                marker.mark(*handler);
                grey.push(ScanPtr::from_non_null(scope, *handler));

                marker.mark(*or_else);
                grey.push(ScanPtr::from_non_null(scope, *or_else));

                marker.mark(*environment);
                grey.push(ScanPtr::from_non_null(scope, *environment));
            }
        }

        grey
    }
}
