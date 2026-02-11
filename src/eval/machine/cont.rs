//! Continuations store in STG stack

use std::fmt;

use crate::{
    common::sourcemap::Smid,
    eval::{
        memory::{
            alloc::StgObject,
            array::Array,
            collect::{CollectorHeapView, GcScannable, ScanPtr},
            syntax::{HeapSyn, RefPtr},
        },
        stg::tags::Tag,
    },
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
        /// Lowest tag in the branch table
        min_tag: Tag,
        /// Indexed branch table: entry at `[tag - min_tag]` holds the
        /// handler for that tag, or `None` if no branch exists
        branch_table: Array<Option<RefPtr<HeapSyn>>>,
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
    ApplyTo {
        args: Array<SynClosure>,
        annotation: Smid,
    },
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

/// O(1) tag dispatch: look up the branch for `tag` by direct indexing
pub fn match_tag(
    tag: Tag,
    min_tag: Tag,
    branch_table: &[Option<RefPtr<HeapSyn>>],
) -> Option<RefPtr<HeapSyn>> {
    if tag < min_tag {
        return None;
    }
    let index = (tag - min_tag) as usize;
    branch_table.get(index).copied().flatten()
}

impl fmt::Display for Continuation {
    /// Summarise a continuation
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Continuation::Branch {
                min_tag,
                branch_table,
                fallback,
                ..
            } => {
                let mut tags: Vec<String> = branch_table
                    .iter()
                    .enumerate()
                    .filter_map(|(i, entry)| entry.map(|_| format!("{}", *min_tag + i as u8)))
                    .collect();
                if fallback.is_some() {
                    tags.push("…".to_string());
                }
                let desc = &tags.join(",");
                write!(f, "⑂<{desc}>")
            }
            Continuation::Update { index, .. } => {
                write!(f, "☇[ρ,{index}]")
            }
            Continuation::ApplyTo { args, .. } => {
                write!(f, "•(×{})", args.len())
            }
            Continuation::DeMeta { .. } => {
                write!(f, "ƒ(`,•)")
            }
        }
    }
}

impl GcScannable for Continuation {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn crate::eval::memory::collect::CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        match self {
            Continuation::Branch {
                branch_table,
                fallback,
                environment,
                ..
            } => {
                if marker.mark_array(branch_table) {
                    for branch in branch_table.iter().flatten() {
                        if marker.mark(*branch) {
                            out.push(ScanPtr::from_non_null(scope, *branch));
                        }
                    }
                }

                if let Some(fb) = fallback {
                    if marker.mark(*fb) {
                        out.push(ScanPtr::from_non_null(scope, *fb));
                    }
                }

                if marker.mark(*environment) {
                    out.push(ScanPtr::from_non_null(scope, *environment));
                }
            }
            Continuation::Update {
                environment,
                index: _,
            } => {
                if marker.mark(*environment) {
                    out.push(ScanPtr::from_non_null(scope, *environment));
                }
            }
            Continuation::ApplyTo { args, .. } => {
                if marker.mark_array(args) {
                    for arg in args.iter() {
                        out.push(ScanPtr::new(scope, arg));
                    }
                }
            }
            Continuation::DeMeta {
                handler,
                or_else,
                environment,
            } => {
                if marker.mark(*handler) {
                    out.push(ScanPtr::from_non_null(scope, *handler));
                }

                if marker.mark(*or_else) {
                    out.push(ScanPtr::from_non_null(scope, *or_else));
                }

                if marker.mark(*environment) {
                    out.push(ScanPtr::from_non_null(scope, *environment));
                }
            }
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        match self {
            Continuation::Branch {
                branch_table,
                fallback,
                environment,
                ..
            } => {
                for ptr in branch_table.iter_mut().flatten() {
                    if let Some(new) = heap.forwarded_to(*ptr) {
                        *ptr = new;
                    }
                }
                if let Some(ref mut fb) = fallback {
                    if let Some(new) = heap.forwarded_to(*fb) {
                        *fb = new;
                    }
                }
                if let Some(new) = heap.forwarded_to(*environment) {
                    *environment = new;
                }
            }
            Continuation::Update { environment, .. } => {
                if let Some(new) = heap.forwarded_to(*environment) {
                    *environment = new;
                }
            }
            Continuation::ApplyTo { args, .. } => {
                for closure in args.iter_mut() {
                    closure.scan_and_update(heap);
                }
            }
            Continuation::DeMeta {
                handler,
                or_else,
                environment,
            } => {
                if let Some(new) = heap.forwarded_to(*handler) {
                    *handler = new;
                }
                if let Some(new) = heap.forwarded_to(*or_else) {
                    *or_else = new;
                }
                if let Some(new) = heap.forwarded_to(*environment) {
                    *environment = new;
                }
            }
        }
    }
}
