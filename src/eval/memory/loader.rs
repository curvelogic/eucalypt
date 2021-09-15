//! Load STG compiled syntax into in-heap representation
//!

use std::rc::Rc;

use memory::syntax::HeapSyn;
use stg::syntax::StgSyn;

use crate::eval::stg;
use crate::eval::{error::ExecutionError, memory};

use super::string::HeapString;
use super::{alloc::ScopedAllocator, array::Array, syntax::RefPtr};

/// Load branch table into heap
fn load_branches<'scope, T: ScopedAllocator<'scope>>(
    mem: &'scope T,
    branches: &[(u8, Rc<StgSyn>)],
) -> Result<Array<(u8, RefPtr<HeapSyn>)>, ExecutionError> {
    let mut array = Array::with_capacity(mem, branches.len());
    for (tag, b) in branches {
        let branch = load(mem, b.clone())?;
        array.push(mem, (*tag, branch));
    }
    Ok(array)
}

/// Load syntax ref vector into heap
fn load_refvec<'scope, T: ScopedAllocator<'scope>>(
    mem: &'scope T,
    refs: &[stg::syntax::Ref],
) -> Result<Array<memory::syntax::Ref>, ExecutionError> {
    let mut array = Array::with_capacity(mem, refs.len());
    for r in refs {
        array.push(mem, stg_to_heap(mem, r));
    }
    Ok(array)
}

/// Load binding vector into heap
pub fn load_lambdavec<'scope, T: ScopedAllocator<'scope>>(
    mem: &'scope T,
    bindings: &[stg::syntax::LambdaForm],
) -> Result<Array<memory::syntax::LambdaForm>, ExecutionError> {
    let mut array = Array::with_capacity(mem, bindings.len());
    for b in bindings {
        let binding = match b {
            stg::syntax::LambdaForm::Lambda {
                bound,
                body,
                annotation,
            } => memory::syntax::LambdaForm::Lambda {
                bound: *bound,
                body: load(mem, body.clone())?,
                annotation: *annotation,
            },
            stg::syntax::LambdaForm::Thunk { body } => memory::syntax::LambdaForm::Thunk {
                body: load(mem, body.clone())?,
            },
            stg::syntax::LambdaForm::Value { body } => memory::syntax::LambdaForm::Value {
                body: load(mem, body.clone())?,
            },
        };
        array.push(mem, binding);
    }
    Ok(array)
}

/// Convert syntax Ref to in-heap representation
///
/// These type duplicates look crazy now but the intention is to alter
/// the in-heap representation but not the compile representation.
fn stg_to_heap<'scope, T: ScopedAllocator<'scope>>(
    mem: &'scope T,
    r: &stg::syntax::Ref,
) -> memory::syntax::Ref {
    match r {
        stg::syntax::Ref::L(n) => memory::syntax::Ref::L(*n),
        stg::syntax::Ref::G(n) => memory::syntax::Ref::G(*n),
        stg::syntax::Ref::V(stg::syntax::Native::Sym(s)) => {
            let ptr = mem
                .alloc(HeapString::from_str(mem, s.as_str()))
                .expect("alloc heap sym failure")
                .as_ptr();
            memory::syntax::Ref::V(memory::syntax::Native::Sym(ptr))
        }
        stg::syntax::Ref::V(stg::syntax::Native::Str(s)) => {
            let ptr = mem
                .alloc(HeapString::from_str(mem, s.as_str()))
                .expect("alloc heap str failure")
                .as_ptr();
            memory::syntax::Ref::V(memory::syntax::Native::Str(ptr))
        }
        stg::syntax::Ref::V(stg::syntax::Native::Num(n)) => {
            memory::syntax::Ref::V(memory::syntax::Native::Num(n.clone()))
        }
        stg::syntax::Ref::V(stg::syntax::Native::Zdt(d)) => {
            memory::syntax::Ref::V(memory::syntax::Native::Zdt(d.clone()))
        }
    }
}

/// Translate compiled syntax into in-heap representation using
/// supplied allocator
pub fn load<'scope, T: ScopedAllocator<'scope>>(
    view: &'scope T,
    syntax: Rc<StgSyn>,
) -> Result<RefPtr<HeapSyn>, ExecutionError> {
    match &*syntax {
        StgSyn::Atom { evaluand } => view.alloc(HeapSyn::Atom {
            evaluand: stg_to_heap(view, evaluand),
        }),
        StgSyn::Case {
            scrutinee,
            branches,
            fallback,
        } => view.alloc(HeapSyn::Case {
            scrutinee: load(view, scrutinee.clone())?,
            branches: load_branches(view, branches)?.clone(),
            fallback: match fallback {
                Some(f) => Some(load(view, f.clone())?),
                None => None,
            },
        }),
        StgSyn::Cons { tag, args } => view.alloc(HeapSyn::Cons {
            tag: *tag,
            args: load_refvec(view, args)?.clone(),
        }),
        StgSyn::App { callable, args } => view.alloc(HeapSyn::App {
            callable: stg_to_heap(view, callable),
            args: load_refvec(view, args)?.clone(),
        }),
        StgSyn::Bif { intrinsic, args } => view.alloc(HeapSyn::Bif {
            intrinsic: *intrinsic,
            args: load_refvec(view, args)?.clone(),
        }),
        StgSyn::Let { bindings, body } => view.alloc(HeapSyn::Let {
            bindings: load_lambdavec(view, bindings.as_slice())?.clone(),
            body: load(view, body.clone())?,
        }),
        StgSyn::LetRec { bindings, body } => view.alloc(HeapSyn::LetRec {
            bindings: load_lambdavec(view, bindings.as_slice())?.clone(),
            body: load(view, body.clone())?,
        }),
        StgSyn::Ann { smid, body } => view.alloc(HeapSyn::Ann {
            smid: *smid,
            body: load(view, body.clone())?,
        }),
        StgSyn::Meta { meta, body } => view.alloc(HeapSyn::Meta {
            meta: stg_to_heap(view, meta),
            body: stg_to_heap(view, body),
        }),
        StgSyn::DeMeta {
            scrutinee,
            handler,
            or_else,
        } => view.alloc(HeapSyn::DeMeta {
            scrutinee: load(view, scrutinee.clone())?,
            handler: load(view, handler.clone())?,
            or_else: load(view, or_else.clone())?,
        }),
        StgSyn::BlackHole => view.alloc(HeapSyn::BlackHole {}),
    }
    .map(|sp| sp.as_ptr())
}
