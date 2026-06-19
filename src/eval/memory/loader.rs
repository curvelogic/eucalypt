//! Load STG compiled syntax into in-heap representation
//!

use std::rc::Rc;

use memory::syntax::HeapSyn;
use stg::syntax::StgSyn;

use crate::eval::stg;
use crate::eval::{error::ExecutionError, memory};

use super::string::HeapString;
use super::symbol::SymbolPool;
use super::{alloc::ScopedAllocator, array::Array, syntax::RefPtr};

/// An indexed branch table: `(min_tag, table)` where `table[tag - min_tag]`
/// holds the handler for that tag, or `None` if no branch exists.
type BranchTable = (u8, Array<Option<RefPtr<HeapSyn>>>);

/// Load branch table into heap as an indexed table
fn load_branches<'scope, T: ScopedAllocator<'scope>>(
    mem: &'scope T,
    pool: &mut SymbolPool,
    branches: &[(u8, Rc<StgSyn>)],
) -> Result<BranchTable, ExecutionError> {
    if branches.is_empty() {
        return Ok((0, Array::default()));
    }
    let min_tag = branches.iter().map(|(t, _)| *t).min().unwrap();
    let max_tag = branches.iter().map(|(t, _)| *t).max().unwrap();
    let table_size = (max_tag - min_tag + 1) as usize;
    let mut table: Array<Option<RefPtr<HeapSyn>>> = Array::with_capacity(mem, table_size);
    for _ in 0..table_size {
        table.push(mem, None);
    }
    for (tag, b) in branches {
        let branch = load(mem, pool, b.clone())?;
        let index = (*tag - min_tag) as usize;
        // SAFETY: index is within [0, table_size) by construction
        unsafe { table.set_unchecked(index, Some(branch)) };
    }
    Ok((min_tag, table))
}

/// Load syntax ref vector into heap
fn load_refvec<'scope, T: ScopedAllocator<'scope>>(
    mem: &'scope T,
    pool: &mut SymbolPool,
    refs: &[stg::syntax::Ref],
) -> Result<Array<memory::syntax::Ref>, ExecutionError> {
    let mut array = Array::with_capacity(mem, refs.len());
    for r in refs {
        array.push(mem, stg_to_heap(mem, pool, r));
    }
    Ok(array)
}

/// Load binding vector into heap
pub fn load_lambdavec<'scope, T: ScopedAllocator<'scope>>(
    mem: &'scope T,
    pool: &mut SymbolPool,
    bindings: &[stg::syntax::LambdaForm],
) -> Result<Array<memory::syntax::LambdaForm>, ExecutionError> {
    let mut array = Array::with_capacity(mem, bindings.len());
    for b in bindings {
        let binding = match b {
            stg::syntax::LambdaForm::Lambda {
                bound,
                body,
                annotation,
            } => {
                memory::syntax::LambdaForm::new(*bound, load(mem, pool, body.clone())?, *annotation)
            }
            stg::syntax::LambdaForm::Thunk { body } => {
                memory::syntax::LambdaForm::thunk(load(mem, pool, body.clone())?)
            }
            stg::syntax::LambdaForm::Value { body } => {
                memory::syntax::LambdaForm::value(load(mem, pool, body.clone())?)
            }
        };
        array.push(mem, binding);
    }
    Ok(array)
}

/// Convert syntax Ref to in-heap representation
///
/// Symbols are interned into the pool rather than heap-allocated.
/// Strings are still heap-allocated as HeapString.
fn stg_to_heap<'scope, T: ScopedAllocator<'scope>>(
    mem: &'scope T,
    pool: &mut SymbolPool,
    r: &stg::syntax::Ref,
) -> memory::syntax::Ref {
    match r {
        stg::syntax::Ref::L(n) => memory::syntax::Ref::L(*n),
        stg::syntax::Ref::G(n) => memory::syntax::Ref::G(*n),
        stg::syntax::Ref::V(stg::syntax::Native::Sym(s)) => {
            let id = pool.intern(s.as_str());
            memory::syntax::Ref::V(memory::syntax::Native::Sym(id))
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
            memory::syntax::Ref::V(memory::syntax::Native::Zdt(*d))
        }
    }
}

/// Translate compiled syntax into in-heap representation using
/// supplied allocator. Symbols are interned into the pool.
pub fn load<'scope, T: ScopedAllocator<'scope>>(
    view: &'scope T,
    pool: &mut SymbolPool,
    syntax: Rc<StgSyn>,
) -> Result<RefPtr<HeapSyn>, ExecutionError> {
    match &*syntax {
        StgSyn::Atom { evaluand } => view.alloc(HeapSyn::Atom {
            evaluand: stg_to_heap(view, pool, evaluand),
        }),
        StgSyn::Case {
            scrutinee,
            branches,
            fallback,
        } => {
            let (min_tag, branch_table) = load_branches(view, pool, branches)?;
            view.alloc(HeapSyn::Case {
                scrutinee: load(view, pool, scrutinee.clone())?,
                min_tag,
                branch_table,
                fallback: match fallback {
                    Some(f) => Some(load(view, pool, f.clone())?),
                    None => None,
                },
            })
        }
        StgSyn::Cons { tag, args } => view.alloc(HeapSyn::Cons {
            tag: *tag,
            args: load_refvec(view, pool, args)?,
        }),
        StgSyn::App { callable, args } => view.alloc(HeapSyn::App {
            callable: stg_to_heap(view, pool, callable),
            args: load_refvec(view, pool, args)?,
        }),
        StgSyn::Bif { intrinsic, args } => view.alloc(HeapSyn::Bif {
            intrinsic: *intrinsic,
            args: load_refvec(view, pool, args)?,
        }),
        StgSyn::Let { bindings, body } => view.alloc(HeapSyn::Let {
            bindings: load_lambdavec(view, pool, bindings.as_slice())?,
            body: load(view, pool, body.clone())?,
        }),
        StgSyn::LetRec { bindings, body } => view.alloc(HeapSyn::LetRec {
            bindings: load_lambdavec(view, pool, bindings.as_slice())?,
            body: load(view, pool, body.clone())?,
        }),
        StgSyn::Ann { smid, body } => view.alloc(HeapSyn::Ann {
            smid: *smid,
            body: load(view, pool, body.clone())?,
        }),
        StgSyn::Meta { meta, body } => view.alloc(HeapSyn::Meta {
            meta: stg_to_heap(view, pool, meta),
            body: stg_to_heap(view, pool, body),
        }),
        StgSyn::DeMeta {
            scrutinee,
            handler,
            or_else,
        } => view.alloc(HeapSyn::DeMeta {
            scrutinee: load(view, pool, scrutinee.clone())?,
            handler: load(view, pool, handler.clone())?,
            or_else: load(view, pool, or_else.clone())?,
        }),
        StgSyn::BlackHole => view.alloc(HeapSyn::BlackHole {}),
    }
    .map(|sp| sp.as_ptr())
}
