//! Intrinsic traits

use std::rc::Rc;

use lru::LruCache;
use regex::Regex;

use serde_json::Number;

use crate::{
    common::sourcemap::Smid,
    eval::stg::wrap::wrap,
    eval::{
        bytecode::BcClosure,
        emit::Emitter,
        error::ExecutionError,
        intrinsics,
        memory::{
            alloc::ScopedAllocator,
            array::Array,
            infotable::InfoTable,
            mutator::MutatorHeapView,
            symbol::SymbolPool,
            syntax::{HeapSyn, Native, Ref, RefPtr, StgBuilder},
        },
        stg::{
            syntax::{dsl, StgSyn},
            tags::{DataConstructor, Tag},
        },
    },
};

use super::{
    env::{EnvFrame, SynClosure},
    vm::HeapNavigator,
};

/// A code-type-erased closure handle passed across the intrinsic ABI
/// (BV1 §5.5).
///
/// During the parallel-engine phase both closure kinds coexist: the
/// HeapSyn engine produces/consumes `Heap`, the bytecode engine `Byte`.
/// Intrinsics treat the handle opaquely; each engine downcasts to its own
/// variant in the `IntrinsicMachine` method impls. Phase 4 collapses this
/// to the sole surviving closure type.
#[derive(Clone)]
pub enum AbiClosure {
    Heap(SynClosure),
    Byte(BcClosure),
}

impl AbiClosure {
    /// Unwrap the HeapSyn closure, panicking on a bytecode handle. Used by
    /// the HeapSyn engine's ABI impls, which only ever see `Heap`.
    pub fn expect_heap(self) -> SynClosure {
        match self {
            AbiClosure::Heap(c) => c,
            AbiClosure::Byte(_) => {
                unreachable!("bytecode closure handed to the HeapSyn intrinsic engine")
            }
        }
    }

    /// Borrow the HeapSyn closure, panicking on a bytecode handle.
    pub fn as_heap(&self) -> &SynClosure {
        match self {
            AbiClosure::Heap(c) => c,
            AbiClosure::Byte(_) => {
                unreachable!("bytecode closure handed to the HeapSyn intrinsic engine")
            }
        }
    }

    /// The closure's arity (0 for a saturated/value closure). Works for
    /// both engine variants via their `InfoTable` impls.
    pub fn arity(&self) -> u8 {
        match self {
            AbiClosure::Heap(c) => c.arity(),
            AbiClosure::Byte(c) => c.arity(),
        }
    }
}

/// Machine interface exposed to intrinsic implementations
pub trait IntrinsicMachine {
    /// Access the regex cache
    fn rcache(&mut self) -> &mut LruCache<String, Regex>;

    /// Update closure after completion
    fn set_closure(&mut self, closure: SynClosure) -> Result<(), ExecutionError>;

    /// Get a navigator for resolving references
    fn nav<'guard>(&'guard self, view: MutatorHeapView<'guard>) -> HeapNavigator<'guard>;

    /// Constant empty environment
    fn root_env(&self) -> RefPtr<EnvFrame>;

    /// Environment of current closure
    fn env(&self, view: MutatorHeapView) -> RefPtr<EnvFrame>;

    /// Read-only access to the symbol pool
    fn symbol_pool(&self) -> &SymbolPool;

    /// Mutable access to the symbol pool for interning new symbols
    fn symbol_pool_mut(&mut self) -> &mut SymbolPool;

    /// Current source annotation for error reporting
    fn annotation(&self) -> Smid;

    // ── Code-type-neutral primitives (BV1 §5.5) ─────────────────────
    //
    // These express argument resolution and result construction without
    // naming the code type, so the bytecode engine can implement the same
    // intrinsic layer. The default impls below are the HeapSyn behaviour
    // (built from `nav`/`set_closure`); the bytecode engine overrides them
    // to produce `BcClosure`s over constructor templates. Once every
    // caller uses these, the `SynClosure`-typed methods above move to a
    // HeapSyn-only sub-trait (plan Phase 1.5, later increment).

    /// Resolve a ref to a native value, looking through atom indirections.
    fn resolve_native(
        &self,
        view: MutatorHeapView<'_>,
        arg: &Ref,
    ) -> Result<Native, ExecutionError> {
        self.nav(view).resolve_native(arg)
    }

    /// Set the machine result to a native value (scalar return).
    fn return_native(
        &mut self,
        view: MutatorHeapView<'_>,
        native: Native,
    ) -> Result<(), ExecutionError> {
        let env = self.root_env();
        self.set_closure(SynClosure::new(
            view.alloc(HeapSyn::Atom {
                evaluand: Ref::V(native),
            })?
            .as_ptr(),
            env,
        ))
    }

    /// Set the machine result to the unit value.
    fn return_unit(&mut self, view: MutatorHeapView<'_>) -> Result<(), ExecutionError> {
        let env = self.root_env();
        self.set_closure(SynClosure::new(view.unit()?.as_ptr(), env))
    }

    /// Set the machine result to a `BoxedNumber` data value.
    fn return_boxed_num(
        &mut self,
        view: MutatorHeapView<'_>,
        n: Number,
    ) -> Result<(), ExecutionError> {
        let env = self.root_env();
        let ptr = view
            .data(
                DataConstructor::BoxedNumber.tag(),
                Array::from_slice(&view, &[Ref::V(Native::Num(n))]),
            )?
            .as_ptr();
        self.set_closure(SynClosure::new(ptr, env))
    }

    /// Set the machine result to a boolean, reusing the global TRUE/FALSE
    /// closures rather than allocating a fresh data cell.
    fn return_bool(&mut self, view: MutatorHeapView<'_>, b: bool) -> Result<(), ExecutionError> {
        use std::sync::OnceLock;
        static TRUE_IDX: OnceLock<usize> = OnceLock::new();
        static FALSE_IDX: OnceLock<usize> = OnceLock::new();
        let idx = if b {
            *TRUE_IDX.get_or_init(|| intrinsics::index("TRUE").unwrap())
        } else {
            *FALSE_IDX.get_or_init(|| intrinsics::index("FALSE").unwrap())
        };
        let closure = self.nav(view).global(idx)?;
        self.set_closure(closure)
    }

    // ── Neutral closure operations (BV1 §5.5, Increment C) ──────────

    /// Resolve a ref to a closure handle (following the resolve rules:
    /// locals/globals looked up, `V` wrapped in an atom closure).
    fn resolve_closure(
        &self,
        view: MutatorHeapView<'_>,
        arg: &Ref,
    ) -> Result<AbiClosure, ExecutionError> {
        Ok(AbiClosure::Heap(self.nav(view).resolve(arg)?))
    }

    /// Resolve a ref to a closure that must be callable (errors on `V`).
    fn resolve_callable_closure(
        &self,
        view: MutatorHeapView<'_>,
        arg: &Ref,
    ) -> Result<AbiClosure, ExecutionError> {
        Ok(AbiClosure::Heap(self.nav(view).resolve_callable(arg)?))
    }

    /// Set the machine result to a previously-resolved closure handle.
    fn set_result(&mut self, closure: AbiClosure) -> Result<(), ExecutionError> {
        self.set_closure(closure.expect_heap())
    }

    /// Force a closure handle to WHNF.
    fn force(&mut self, closure: AbiClosure) -> Result<AbiClosure, ExecutionError> {
        Ok(AbiClosure::Heap(
            self.evaluate_to_whnf(closure.expect_heap())?,
        ))
    }

    /// The data-constructor tag of a (WHNF) closure handle, if it is a
    /// data constructor; `None` otherwise.
    fn data_tag(&self, view: MutatorHeapView<'_>, closure: &AbiClosure) -> Option<Tag> {
        let code = view.scoped(closure.as_heap().code());
        match &*code {
            HeapSyn::Cons { tag, .. } => Some(*tag),
            _ => None,
        }
    }

    /// The `idx`-th field of a data-constructor closure, resolved within
    /// the closure's environment; `None` if not a constructor / out of range.
    fn data_field(
        &self,
        view: MutatorHeapView<'_>,
        closure: &AbiClosure,
        idx: usize,
    ) -> Option<AbiClosure> {
        let sc = closure.as_heap();
        let field_ref = {
            let code = view.scoped(sc.code());
            match &*code {
                HeapSyn::Cons { args, .. } => args.get(idx)?,
                _ => return None,
            }
        };
        Some(AbiClosure::Heap(
            self.nav(view).resolve_in_closure(sc, field_ref)?,
        ))
    }

    /// The `idx`-th field of a data-constructor closure read as a native
    /// value (used by native-list iteration).
    fn field_native(
        &self,
        view: MutatorHeapView<'_>,
        closure: &AbiClosure,
        idx: usize,
    ) -> Option<Native> {
        let sc = closure.as_heap();
        let code = view.scoped(sc.code());
        match &*code {
            HeapSyn::Cons { args, .. } => {
                let field_ref = args.get(idx)?;
                Some(sc.navigate_local_native(&view, field_ref))
            }
            _ => None,
        }
    }

    /// Request an emitter capture for the given format.
    ///
    /// Sets a pending flag that `Machine::step()` reads to push a
    /// format-specific capture emitter.  All subsequent emit BIF
    /// output goes to the capture buffer until `CaptureEnd` fires.
    fn start_capture(&mut self, format: &str) -> Result<(), ExecutionError>;

    /// Push a `CaptureEnd` continuation onto the STG stack.
    fn push_capture_end(&mut self, view: MutatorHeapView<'_>) -> Result<(), ExecutionError>;

    /// Take the latest capture result string (set by `Machine::step()`
    /// after a `CaptureEnd` continuation fires).
    fn take_capture_result(&mut self) -> Result<String, ExecutionError>;

    /// Whether the machine is running in test mode.
    ///
    /// In test mode, `__EXPECT` failures return `false` instead of
    /// panicking, allowing test harnesses to collect results.
    fn test_mode(&self) -> bool;

    /// Evaluate a closure to WHNF, safely handling a non-empty continuation stack.
    ///
    /// Moves the current stack to a GC-visible location, runs the sub-evaluation,
    /// then restores the saved stack.  The result closure is returned in WHNF.
    ///
    /// Intrinsics that need to force thunks (e.g. to inspect values at
    /// runtime) should use this method.
    ///
    /// # Architecture note
    ///
    /// The default implementation panics.  A real implementation requires
    /// access to the heap and emitter, which are not available through
    /// `MachineState` alone.  The full `Machine` type provides a working
    /// implementation; future work may restructure the trait so that
    /// `Machine` itself implements `IntrinsicMachine` directly.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the sub-evaluation encounters a machine error or
    /// if it unexpectedly yields an IO constructor.
    fn evaluate_to_whnf(&mut self, closure: SynClosure) -> Result<SynClosure, ExecutionError> {
        let _ = closure;
        panic!(
            "evaluate_to_whnf is not available through IntrinsicMachine: \
             use Machine::evaluate_to_whnf directly from contexts with full machine access"
        )
    }
}

/// All intrinsics have an STG syntax wrapper
pub trait StgIntrinsic: Sync {
    /// The name of the intrinsic
    fn name(&self) -> &str;

    /// The STG wrapper for calling the intrinsic
    fn wrapper(&self, annotation: Smid) -> crate::eval::stg::syntax::LambdaForm {
        wrap(self.index(), self.info(), annotation)
    }

    /// Whether the compiler should inline the wrapper
    fn inlinable(&self) -> bool {
        true
    }

    /// Argument indices that are single-use (entered at most once).
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
    fn execute(
        &self,
        _machine: &mut dyn IntrinsicMachine,
        _heap: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        panic!("{} is STG-only", self.name());
    }

    /// A Ref to this global
    fn gref(&self) -> crate::eval::stg::syntax::Ref {
        dsl::gref(self.index())
    }
}

pub trait Const: StgIntrinsic {
    fn global(&self) -> Rc<StgSyn> {
        dsl::global(self.index())
    }
}

pub trait CallGlobal0: StgIntrinsic {
    fn global(&self) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![])
    }
}

pub trait CallGlobal1: StgIntrinsic {
    fn global(&self, x: crate::eval::stg::syntax::Ref) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x])
    }
}

pub trait CallGlobal2: StgIntrinsic {
    fn global(
        &self,
        x: crate::eval::stg::syntax::Ref,
        y: crate::eval::stg::syntax::Ref,
    ) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x, y])
    }
}

pub trait CallGlobal3: StgIntrinsic {
    fn global(
        &self,
        x: crate::eval::stg::syntax::Ref,
        y: crate::eval::stg::syntax::Ref,
        z: crate::eval::stg::syntax::Ref,
    ) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x, y, z])
    }
}

pub trait CallGlobal4: StgIntrinsic {
    fn global(
        &self,
        x: crate::eval::stg::syntax::Ref,
        y: crate::eval::stg::syntax::Ref,
        z: crate::eval::stg::syntax::Ref,
        w: crate::eval::stg::syntax::Ref,
    ) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x, y, z, w])
    }
}

pub trait CallGlobal7: StgIntrinsic {
    #[allow(clippy::too_many_arguments)]
    fn global(
        &self,
        x0: crate::eval::stg::syntax::Ref,
        x1: crate::eval::stg::syntax::Ref,
        x2: crate::eval::stg::syntax::Ref,
        x3: crate::eval::stg::syntax::Ref,
        x4: crate::eval::stg::syntax::Ref,
        x5: crate::eval::stg::syntax::Ref,
        x6: crate::eval::stg::syntax::Ref,
    ) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x0, x1, x2, x3, x4, x5, x6])
    }
}
