//! A variation of the STG machine
//!
//! This is in the process of morphing from something that is clearly
//! an interpreter to something more like a VM

use std::{cmp::Ordering, convert::TryInto, num::NonZeroUsize};

use itertools::Itertools;
use lru::LruCache;
use regex::Regex;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::{Emitter, NullEmitter},
        error::ExecutionError,
        intrinsics,
        machine::env::ScopeAndClosure,
        memory::{
            alloc::{ScopedAllocator, ScopedPtr},
            array::Array,
            collect::{self, CollectorHeapView, CollectorScope, GcScannable, ScanPtr},
            heap::{Heap, HeapStats},
            infotable::InfoTable,
            mutator::{Mutator, MutatorHeapView},
            syntax::{HeapSyn, Native, Ref, RefPtr, StgBuilder},
        },
        stg::tags::{DataConstructor, Tag},
    },
};

use super::{
    cont::{match_tag, Continuation},
    env::{EnvFrame, SynClosure},
    intrinsic::{IntrinsicMachine, StgIntrinsic},
    metrics::{Metrics, ThreadOccupation},
};
use super::{env_builder::EnvBuilder, metrics::Clock};

/// A utility for navigating chains of refs / pointers through the heap
pub struct HeapNavigator<'scope> {
    /// Local environment
    locals: ScopedPtr<'scope, EnvFrame>,
    /// Global environment
    globals: ScopedPtr<'scope, EnvFrame>,
    /// Mutator view of heap
    view: MutatorHeapView<'scope>,
}

impl<'scope> HeapNavigator<'scope> {
    /// Resolve a ref (creating atom closure if it resolves to native)
    pub fn resolve(&self, r: &Ref) -> Result<SynClosure, ExecutionError> {
        match r {
            Ref::L(index) => Ok(self.get(*index)?),
            Ref::G(index) => Ok(self.global(*index)?),
            Ref::V(_) => Ok(SynClosure::new(
                self.view
                    .alloc(HeapSyn::Atom {
                        evaluand: r.clone(),
                    })?
                    .as_ptr(),
                self.view.alloc(EnvFrame::default())?.as_ptr(),
            )),
        }
    }

    /// Index into current environment, retrieving pointer
    pub fn get(&self, index: usize) -> Result<SynClosure, ExecutionError> {
        (*self.locals)
            .get(&self.view, index)
            .ok_or(ExecutionError::BadEnvironmentIndex(index))
    }

    /// Index into globals
    pub fn global(&self, index: usize) -> Result<SynClosure, ExecutionError> {
        (*self.globals)
            .get(&self.view, index)
            .ok_or(ExecutionError::BadGlobalIndex(index))
    }

    /// Resolve a ref to a closure
    pub fn resolve_callable(&self, r: &Ref) -> Result<SynClosure, ExecutionError> {
        match r {
            Ref::L(index) => self.get(*index),
            Ref::G(index) => self.global(*index),
            Ref::V(_) => Err(ExecutionError::NotCallable(Smid::default())),
        }
    }

    /// Repeat resolves until we terminate with a native (vref)
    ///
    /// Should never have to burrow. But our forcing mechanism (CASE)
    /// currently wraps a bound ref to the value so we need unwrap again.
    pub fn resolve_native(&self, r: &Ref) -> Result<Native, ExecutionError> {
        let mut closure = match r {
            Ref::L(index) => self.get(*index)?,
            Ref::G(index) => self.global(*index)?,
            Ref::V(n) => return Ok(n.clone()),
        };

        let mut scoped_code = self.view.scoped(closure.code());

        while let HeapSyn::Atom { evaluand: r } = &*scoped_code {
            closure = match r {
                Ref::L(index) => {
                    let env = self.view.scoped(closure.env());
                    (*env)
                        .get(&self.view, *index)
                        .ok_or(ExecutionError::BadEnvironmentIndex(*index))?
                }
                Ref::G(index) => self.global(*index)?,
                Ref::V(n) => return Ok(n.clone()),
            };

            scoped_code = self.view.scoped(closure.code());
        }

        Err(ExecutionError::NotValue(Smid::default(), "".to_string()))
    }

    pub fn env_trace(&self) -> Vec<Smid> {
        (*self.locals).annotation_trace(&self.view)
    }
}

/// The state of the machine (stack / closure etc.)
pub struct MachineState {
    /// Root (empty) environment
    root_env: RefPtr<EnvFrame>,
    /// Current closure
    closure: SynClosure,
    /// Globals (primarily STG wrappers for intrinsics)
    globals: RefPtr<EnvFrame>,
    /// Stack of continuations (outside heap)
    stack: Vec<RefPtr<Continuation>>,
    /// Termination flag. Set when machine has terminated
    terminated: bool,
    /// Annotation to paint on any environments we create
    annotation: Smid,
    /// Cache compiled regexes
    rcache: LruCache<String, Regex>,
}

impl Default for MachineState {
    fn default() -> Self {
        Self {
            root_env: RefPtr::dangling(),
            closure: SynClosure::new(RefPtr::dangling(), RefPtr::dangling()),
            globals: RefPtr::dangling(),
            stack: Default::default(),
            terminated: Default::default(),
            annotation: Default::default(),
            rcache: LruCache::new(NonZeroUsize::new(100).unwrap()),
        }
    }
}

impl MachineState {
    /// Set global environment
    pub fn set_globals(&mut self, globals: RefPtr<EnvFrame>) -> Result<(), ExecutionError> {
        self.globals = globals;
        Ok(())
    }

    /// Has the machine terminated?
    pub fn terminated(&self) -> bool {
        self.terminated
    }

    /// Push a new continuation onto the stack
    fn push(&mut self, view: MutatorHeapView, cont: Continuation) -> Result<(), ExecutionError> {
        let ptr = view.alloc(cont)?.as_ptr();
        self.stack.push(ptr);
        Ok(())
    }

    /// Handle an instruction
    fn handle_instruction<'guard>(
        &mut self,
        view: MutatorHeapView<'guard>,
        emitter: &mut dyn Emitter,
        intrinsics: &[&'guard dyn StgIntrinsic],
        metrics: &mut Metrics,
    ) -> Result<(), ExecutionError> {
        // Load "op code"
        let code = (*view.scoped(self.closure.code())).clone();
        let environment = self.closure.env();
        let remaining_arity = self.closure.arity();

        // Set annotation to stamp on any allocations
        self.annotation = self.closure.annotation();

        if remaining_arity > 0 {
            return self.return_fun(view);
        }

        match code {
            HeapSyn::Atom { evaluand } => {
                match evaluand {
                    Ref::L(i) => {
                        self.closure = self.nav(view).get(i)?;
                        let updateable = self.closure.update();
                        if updateable {
                            self.push(
                                view,
                                Continuation::Update {
                                    environment,
                                    index: i,
                                },
                            )?;
                        }
                    }
                    Ref::G(i) => {
                        self.closure = self.nav(view).global(i)?; // TODO: update globals?
                    }
                    Ref::V(v) => {
                        self.return_native(view, &v)?;
                    }
                }
            }
            HeapSyn::Case {
                scrutinee,
                branches,
                fallback,
            } => {
                self.push(
                    view,
                    Continuation::Branch {
                        branches,
                        fallback,
                        environment,
                    },
                )?;
                self.closure = SynClosure::new(scrutinee, environment);
            }
            HeapSyn::Cons { tag, args } => {
                self.return_data(view, tag, args.as_slice())?;
            }
            HeapSyn::App { callable, args } => {
                let array = view.create_arg_array(args.as_slice(), environment)?;
                self.push(view, Continuation::ApplyTo { args: array })?;
                self.closure = self.nav(view).resolve_callable(&callable)?;
            }
            HeapSyn::Bif { intrinsic, args } => {
                let bif = intrinsics[intrinsic as usize];
                bif.execute(self, view, emitter, args.as_slice())?;
            }
            HeapSyn::Let { bindings, body } => {
                metrics.alloc(bindings.len());
                let new_env = view.from_let(bindings.as_slice(), environment, self.annotation);
                self.closure = SynClosure::new(body, new_env);
            }
            HeapSyn::LetRec { bindings, body } => {
                metrics.alloc(bindings.len());
                let new_env = view.from_letrec(bindings.as_slice(), environment, self.annotation);
                self.closure = SynClosure::new(body, new_env);
            }
            HeapSyn::Ann { smid, body } => {
                self.annotation = smid;
                self.closure = SynClosure::new(body, environment);
            }
            HeapSyn::Meta { meta, body } => {
                self.return_meta(view, &meta, &body)?;
            }
            HeapSyn::DeMeta {
                scrutinee,
                handler,
                or_else,
            } => {
                self.push(
                    view,
                    Continuation::DeMeta {
                        handler,
                        or_else,
                        environment,
                    },
                )?;
                self.closure = SynClosure::new(scrutinee, environment);
            }
            HeapSyn::BlackHole => return Err(ExecutionError::BlackHole),
        }

        Ok(())
    }

    /// Update environment index to point to current closure
    fn update(
        &mut self,
        view: MutatorHeapView,
        environment: RefPtr<EnvFrame>,
        index: usize,
    ) -> Result<(), ExecutionError> {
        let cont_env = view.scoped(environment);
        cont_env.update(&view, index, self.closure.clone())
    }

    /// Return meta into a demeta destructuring or just strip metadata
    /// and continue
    fn return_meta<'guard>(
        &mut self,
        view: MutatorHeapView<'guard>,
        meta: &Ref,
        body: &Ref,
    ) -> Result<(), ExecutionError> {
        if let Some(cont) = self.stack.pop() {
            let continuation = (*(view.scoped(cont))).clone();

            match continuation {
                Continuation::DeMeta {
                    handler,
                    environment,
                    ..
                } => {
                    self.closure = SynClosure::new(
                        handler,
                        view.from_closures(
                            [self.nav(view).resolve(meta)?, self.nav(view).resolve(body)?]
                                .iter()
                                .cloned(),
                            2,
                            environment,
                            self.annotation,
                        ),
                    );
                }
                Continuation::Update { environment, index } => {
                    self.update(view, environment, index)?;
                }
                _ => {
                    self.closure = self.nav(view).resolve(body)?;
                    self.stack.push(cont);
                }
            }
        } else {
            self.closure = self.nav(view).resolve(body)?;
            // Don't terminate at metadata, carrying processing
        }

        Ok(())
    }

    /// Return a native value into continuation or terminate
    fn return_native<'guard>(
        &mut self,
        view: MutatorHeapView<'guard>,
        value: &Native,
    ) -> Result<(), ExecutionError> {
        if let Some(cont) = self.stack.pop() {
            let continuation = (*(view.scoped(cont))).clone();

            match continuation {
                Continuation::Branch {
                    fallback,
                    environment,
                    ..
                } => {
                    // case fallbacks can handle natives
                    if let Some(fb) = fallback {
                        self.closure = SynClosure::new(
                            fb,
                            view.from_args(
                                &[Ref::vref(value.clone())],
                                environment,
                                self.annotation,
                            ),
                        );
                    } else {
                        return Err(ExecutionError::NoBranchForNative);
                    }
                }
                Continuation::Update { environment, index } => {
                    self.update(view, environment, index)?;
                }
                Continuation::ApplyTo { .. } => {
                    return Err(ExecutionError::NotCallable(Smid::default()));
                }
                Continuation::DeMeta {
                    or_else,
                    environment,
                    ..
                } => {
                    // demeta or_else can accept natives
                    self.closure = SynClosure::new(
                        or_else,
                        view.from_args(&[Ref::vref(value.clone())], environment, self.annotation),
                    );
                }
            }
        } else {
            self.terminated = true
        }

        Ok(())
    }

    /// Return data into an appropriate branch handler
    ///
    /// Data is destructured for tag handlers but not for the default
    /// handler which is used for natives and unknown data constructors.
    fn return_data<'guard>(
        &mut self,
        view: MutatorHeapView<'guard>,
        tag: Tag,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        if let Some(cont) = self.stack.pop() {
            let continuation = (*(view.scoped(cont))).clone();

            match continuation {
                Continuation::Branch {
                    branches,
                    fallback,
                    environment,
                } => {
                    if let Some(body) = match_tag(tag, branches.as_slice()) {
                        let closures = args
                            .iter()
                            .map(|r| self.nav(view).resolve(r))
                            .collect::<Result<Vec<SynClosure>, ExecutionError>>()?;
                        let len = closures.len();
                        self.closure = SynClosure::new(
                            body,
                            // TODO: skip empty frames
                            view.from_closures(
                                closures.into_iter(),
                                len,
                                environment,
                                self.annotation,
                            ),
                        );
                    } else if let Some(body) = fallback {
                        self.closure = SynClosure::new(
                            body,
                            view.from_closure(self.closure.clone(), environment, self.annotation),
                        );
                    } else {
                        return Err(ExecutionError::NoBranchForDataTag(tag as u8));
                    }
                }
                Continuation::Update { environment, index } => {
                    self.update(view, environment, index)?;
                }
                Continuation::ApplyTo { args } => {
                    // Block application - BLOCKs are a special case
                    // and can be applied as functions, calling
                    // __MERGE
                    //
                    // TODO: a more generic mechanism for applying
                    // data structures
                    if tag == DataConstructor::Block.tag() {
                        let mut args = Array::from_slice(&view, args.as_slice());
                        args.push(&view, self.closure.clone());
                        self.push(view, Continuation::ApplyTo { args })?;
                        self.closure = SynClosure::new(
                            view.atom(Ref::G(intrinsics::index("MERGE").unwrap()))?
                                .as_ptr(),
                            self.env(view),
                        );
                    } else {
                        return Err(ExecutionError::NotCallable(Smid::default()));
                    }
                }
                Continuation::DeMeta {
                    or_else,
                    environment,
                    ..
                } => {
                    self.closure = SynClosure::new(
                        or_else,
                        view.from_closure(self.closure.clone(), environment, self.annotation),
                    );
                }
            }
        } else {
            self.terminated = true
        }

        Ok(())
    }

    /// Return function to either apply to args or default case branch
    fn return_fun(&mut self, view: MutatorHeapView) -> Result<(), ExecutionError> {
        if let Some(cont) = self.stack.pop() {
            let continuation = (*(view.scoped(cont))).clone();

            match continuation {
                Continuation::ApplyTo { args } => {
                    let excess = args.len() as isize - self.closure.arity() as isize;

                    match excess.cmp(&0) {
                        Ordering::Equal => {
                            self.closure = view.saturate(&self.closure, args.as_slice())?;
                        }
                        Ordering::Less => {
                            self.closure = view.partially_apply(&self.closure, args.as_slice())?;
                        }
                        Ordering::Greater => {
                            let (quorum, surplus) =
                                args.as_slice().split_at(args.len() - excess as usize);
                            self.closure = view.saturate(&self.closure, quorum)?;
                            self.push(
                                view,
                                Continuation::ApplyTo {
                                    args: Array::from_slice(&view, surplus),
                                },
                            )?;
                        }
                    }
                }
                Continuation::Branch {
                    fallback,
                    environment,
                    ..
                } => {
                    // HACK: This impossible in the standard STG
                    // machine, but dynamic typing means we can't
                    // avoid it at compile time. Code may case
                    // something to determine its type and then
                    // discover it's a fn.
                    //
                    // All we can do at this stage is use the default
                    // branch with the evaluated scrutinee. The lack
                    // of progress risks looping though and in the
                    // general case it's not clear what the default
                    // handler can do with something that might be a
                    // native, an unmatched constructor or a lambda.
                    //
                    // Probably branch tables need complicating (they
                    // already omit primitive handlers from the
                    // standard STG machine.)
                    if let Some(body) = fallback {
                        self.closure = SynClosure::new(
                            body,
                            view.from_closure(self.closure.clone(), environment, self.annotation),
                        );
                    } else {
                        return Err(ExecutionError::CannotReturnFunToCase);
                    }
                }
                Continuation::Update { environment, index } => {
                    self.update(view, environment, index)?;
                }
                Continuation::DeMeta {
                    or_else,
                    environment,
                    ..
                } => {
                    self.closure = SynClosure::new(
                        or_else,
                        view.from_closure(self.closure.clone(), environment, self.annotation),
                    );
                }
            }
        } else {
            self.terminated = true
        }

        Ok(())
    }

    /// Trace of annotations in execution stack
    pub fn stack_trace<'guard>(&self, view: &'guard MutatorHeapView) -> Vec<Smid> {
        self.stack
            .iter()
            .rev()
            .filter_map(|cont| {
                let c = view.scoped(*cont);
                match &*c {
                Continuation::Branch { environment, .. }
                | Continuation::Update { environment, .. } // maybe not...
                    | Continuation::DeMeta { environment, .. } => {
			let cont_env = view.scoped(*environment);
			Some(cont_env.annotation())},
                _ => None,
		}
            })
            .filter(|smid| *smid != Smid::default())
            .dedup()
            .collect()
    }
}

impl IntrinsicMachine for MachineState {
    /// Return regex cache
    fn rcache(&mut self) -> &mut LruCache<String, Regex> {
        &mut self.rcache
    }

    /// Used by intrinsics to update the closure after execution
    fn set_closure(&mut self, closure: SynClosure) -> Result<(), ExecutionError> {
        self.closure = closure;
        Ok(())
    }

    /// Get a navigator for resolving references
    fn nav<'guard>(&'guard self, view: MutatorHeapView<'guard>) -> HeapNavigator<'guard> {
        HeapNavigator {
            locals: view.scoped(self.env(view)),
            globals: view.scoped(self.globals),
            view,
        }
    }

    /// The empty root environment
    fn root_env(&self) -> RefPtr<EnvFrame> {
        self.root_env
    }

    /// The current environment
    fn env(&self, _view: MutatorHeapView) -> RefPtr<EnvFrame> {
        self.closure.env()
    }
}

/// MachineState contains all the garbage collection roots
impl GcScannable for MachineState {
    fn scan<'a, 'b>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &'b mut CollectorHeapView<'a>,
    ) -> Vec<ScanPtr<'a>> {
        let mut grey = vec![];

        if marker.mark(self.globals) {
            grey.push(ScanPtr::from_non_null(scope, self.globals));
        }

        grey.push(ScanPtr::new(scope, &self.closure));

        for cont in &self.stack {
            if marker.mark(*cont) {
                grey.push(ScanPtr::from_non_null(scope, *cont));
            }
        }

        grey
    }
}

pub struct MachineSettings {
    pub trace_steps: bool,
    pub dump_heap: bool,
}

/// An STG machine variant using cactus environment
///
/// GC roots (outside heap, pointing in) are:
/// - closure
/// - globals
/// - stack
pub struct Machine<'a> {
    /// Main VM Memory - immutable ref and interior mutability
    heap: Heap,
    /// The current state of the machine - operated on as mutable ref
    state: MachineState,
    /// Intrinsics (actions with access into machine)
    intrinsics: Vec<&'a dyn StgIntrinsic>,
    /// Emitter to send output and error events to
    emitter: Box<dyn Emitter + 'a>,
    /// Whether to trace every step to stderr
    settings: MachineSettings,
    /// Metrics
    metrics: Metrics,
    /// Clock
    clock: Clock,
}

impl<'a> Machine<'a> {
    /// Construct a machine to evaluate `code`
    pub fn new(
        emitter: Box<dyn Emitter + 'a>,
        trace_steps: bool,
        heap_limit_mib: Option<usize>,
        dump_heap: bool,
    ) -> Self {
        Machine {
            heap: heap_limit_mib
                .map(Heap::with_limit)
                .unwrap_or_else(|| Heap::new()),
            state: Default::default(),
            intrinsics: vec![],
            emitter,
            settings: MachineSettings {
                trace_steps,
                dump_heap,
            },
            metrics: Metrics::default(),
            clock: Clock::default(),
        }
    }

    /// Access the heap for allocation
    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    /// Access the metrics (ticks, allocs, etc.)
    pub fn metrics(&self) -> &Metrics {
        &self.metrics
    }

    /// Get heap statistics
    pub fn heap_stats(&self) -> HeapStats {
        self.heap.stats()
    }

    /// Return clock for access to GC timings
    pub fn clock(&self) -> &Clock {
        &self.clock
    }

    /// Create a mutator heap view for heap access
    fn view(&'a self) -> MutatorHeapView<'a> {
        MutatorHeapView::new(&self.heap)
    }

    /// Split reference into separate facilities (state, heap)
    fn facilities(
        &mut self,
    ) -> (
        &mut MachineState,
        MutatorHeapView,
        &mut dyn Emitter,
        &mut Metrics,
        &MachineSettings,
        &[&dyn StgIntrinsic],
    ) {
        (
            &mut self.state,
            MutatorHeapView::new(&self.heap),
            self.emitter.as_mut(),
            &mut self.metrics,
            &self.settings,
            &self.intrinsics,
        )
    }

    /// Get a navigator for resolving references
    fn nav(&self) -> HeapNavigator {
        self.state.nav(MutatorHeapView::new(&self.heap))
    }

    /// Apply a mutation which needs heap access
    pub fn mutate<I, O, M>(&self, mutator: M, input: I) -> Result<O, ExecutionError>
    where
        I: Sized,
        O: Sized,
        M: Mutator<Input = I, Output = O>,
    {
        let view = MutatorHeapView::new(&self.heap);
        mutator.run(&view, input)
    }

    /// Initialise globals and starting closure
    pub fn initialise(
        &mut self,
        root_env: RefPtr<EnvFrame>,
        globals: RefPtr<EnvFrame>,
        closure: SynClosure,
        intrinsics: Vec<&'a dyn StgIntrinsic>,
    ) -> Result<(), ExecutionError> {
        self.clock.switch(ThreadOccupation::Initialisation);
        self.intrinsics = intrinsics.clone();
        self.state.root_env = root_env;
        self.state.set_globals(globals)?;
        self.state.set_closure(closure)
    }

    /// Execute one step
    pub fn step(&mut self) -> Result<(), ExecutionError> {
        let (state, view, emitter, metrics, settings, intrinsics) = self.facilities();

        if settings.trace_steps {
            let stack = state
                .stack
                .iter()
                .rev()
                .map(|p| (*(view.scoped(*p))).to_string())
                .format(":");
            eprintln!("M ⟪{}⟫ <{}>", ScopeAndClosure(&view, &state.closure), stack);
        }

        metrics.tick();
        metrics.stack(state.stack.len());

        state
            .handle_instruction(view, emitter, intrinsics, metrics)
            .map_err(|e| {
                ExecutionError::Traced(
                    Box::new(e),
                    state.nav(view).env_trace(),
                    state.stack_trace(&view),
                )
            })
    }

    /// Run the machine until termination or step limit
    pub fn run(&mut self, limit: Option<usize>) -> Result<Option<u8>, ExecutionError> {
        self.clock.switch(ThreadOccupation::Mutator);

        let gc_check_freq = 500;

        while !self.state.terminated {
            if let Some(limit) = limit {
                if self.metrics.ticks() as usize >= limit {
                    return Err(ExecutionError::DidntTerminate(limit));
                }
            }

            if self.metrics.ticks() % gc_check_freq == 0 && self.heap().policy_requires_collection() {
                collect::collect(
                    &self.state,
                    &mut self.heap,
                    &mut self.clock,
                    self.settings.dump_heap,
                );
                self.clock.switch(ThreadOccupation::Mutator);
            }

            self.step()?;
        }

        collect::collect(
            &self.state,
            &mut self.heap,
            &mut self.clock,
            self.settings.dump_heap,
        );

        self.clock.stop();

        Ok(self.exit_code())
    }

    /// Determine an exit code if the machine is terminated
    ///
    /// Numbers in 0-255 are used as is
    /// Unit becomes zero
    /// Anything else is 1
    pub fn exit_code(&self) -> Option<u8> {
        // A view for mutable heap access
        let view = self.view();

        let code = view.scoped(self.state.closure.code());

        if self.state.terminated() {
            Some(match &*code {
                HeapSyn::Atom { evaluand: r } => match self.nav().resolve_native(r) {
                    Ok(Native::Num(n)) => {
                        if let Some(integer) = n.as_i64() {
                            let rc: Result<u8, _> = integer.try_into();
                            rc.unwrap_or(1)
                        } else {
                            1
                        }
                    }
                    _ => 1,
                },
                HeapSyn::Cons { tag, .. } if *tag == 0 => 0,
                _ => 1,
            })
        } else {
            None
        }
    }

    /// Recover the emitter after a run
    pub fn take_emitter(&mut self) -> Box<dyn Emitter + 'a> {
        let mut ret: Box<dyn Emitter + 'a> = Box::new(NullEmitter);
        std::mem::swap(&mut ret, &mut self.emitter);
        ret
    }

    /// Has the machine terminated
    pub fn terminated(&self) -> bool {
        self.state.terminated()
    }

    /// Assertion helper for machine unit tests
    #[cfg(test)]
    pub fn native_return(&self) -> Option<Native> {
        let view = self.view();

        let code = view.scoped(self.state.closure.code());

        if self.state.terminated() {
            if let HeapSyn::Atom { evaluand: r } = &*code {
                self.nav().resolve_native(r).ok()
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Retrieve native string return value if it exists
    #[cfg(test)]
    pub fn string_return(&self) -> Option<String> {
        let view = self.view();

        let code = view.scoped(self.state.closure.code());

        if self.state.terminated() {
            if let HeapSyn::Atom { evaluand: r } = &*code {
                if let Ok(Native::Str(rp)) = self.nav().resolve_native(r) {
                    Some((*view.scoped(rp)).as_str().to_string())
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    #[cfg(test)]
    pub fn bool_return(&self) -> Option<bool> {
        let view = self.view();

        let code = view.scoped(self.state.closure.code());

        if self.state.terminated() {
            if let HeapSyn::Cons { tag, .. } = &*code {
                if *tag == DataConstructor::BoolTrue.tag() {
                    Some(true)
                } else if *tag == DataConstructor::BoolFalse.tag() {
                    Some(false)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    #[cfg(test)]
    pub fn unit_return(&self) -> bool {
        let view = self.view();

        let code = view.scoped(self.state.closure.code());

        if self.state.terminated() {
            if let HeapSyn::Cons { tag, .. } = &*code {
                *tag == DataConstructor::Unit.tag()
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Retrieve any events captured by the emitter
    #[cfg(test)]
    pub fn captures(&self) -> &[crate::eval::emit::Event] {
        self.emitter.captures()
    }
}

#[cfg(test)]
pub mod tests {

    use std::rc::Rc;

    use crate::eval::machine::env::{EnvFrame, SynClosure};
    use crate::eval::machine::intrinsic::StgIntrinsic;

    use crate::eval::memory::alloc::ScopedAllocator;
    use crate::eval::memory::loader::load;
    use crate::eval::memory::mutator::{Mutator, MutatorHeapView};
    use crate::eval::memory::syntax::{Native, RefPtr};
    use crate::eval::stg::syntax::{ex::*, StgSyn};
    use crate::eval::{emit::DebugEmitter, stg::syntax::dsl::*};

    use super::Machine;

    lazy_static! {
        static ref EMPTY_INTRINSICS: Vec<Box<dyn StgIntrinsic>> = vec![];
    }

    pub struct Init;

    impl Mutator for Init {
        type Input = ();
        type Output = RefPtr<EnvFrame>;

        fn run(
            &self,
            view: &MutatorHeapView,
            _input: Self::Input,
        ) -> Result<Self::Output, crate::eval::error::ExecutionError> {
            Ok(view.alloc(EnvFrame::default())?.as_ptr())
        }
    }

    pub struct Load {
        syntax: Rc<StgSyn>,
    }

    impl Mutator for Load {
        type Input = RefPtr<EnvFrame>;
        type Output = SynClosure;

        fn run(
            &self,
            view: &MutatorHeapView,
            input: Self::Input,
        ) -> Result<Self::Output, crate::eval::error::ExecutionError> {
            Ok(SynClosure::new(load(view, self.syntax.clone())?, input))
        }
    }

    fn machine(syn: Rc<StgSyn>) -> Machine<'static> {
        let mut m = Machine::new(Box::new(DebugEmitter::default()), true, None, false);
        let blank = m.mutate(Init, ()).unwrap();
        let closure = m.mutate(Load { syntax: syn }, blank).unwrap();
        m.initialise(blank, blank, closure, vec![]).unwrap();
        m
    }

    #[test]
    pub fn test_terminate_with_number() {
        let mut machine = machine(atom(num(9)));
        machine.step().unwrap();
        assert!(machine.terminated());
    }

    #[test]
    pub fn test_terminate_with_true() {
        let mut machine = machine(t());
        machine.step().unwrap();
        assert!(machine.terminated());
    }

    #[test]
    pub fn test_terminate_with_unit() {
        let mut machine = machine(unit());
        machine.step().unwrap();
        assert!(machine.terminated());
    }

    #[test]
    pub fn test_identity() {
        let mut m = machine(let_(vec![i()], app(lref(0), vec![num(42)])));
        m.run(Some(20)).unwrap();
        assert_eq!(m.native_return(), Some(Native::Num(42.into())));
    }

    #[test]
    pub fn test_ski() {
        let syn = letrec_(
            vec![
                i(),
                s(),
                k(),
                lambda(0, app(lref(1), vec![lref(2), lref(2)])), //s(k,k)
            ],
            app(lref(3), vec![str("foo")]),
        );

        let mut m = machine(syn);
        m.run(Some(30)).unwrap();
        assert_eq!(m.string_return(), Some("foo".to_string()));
    }

    #[test]
    pub fn test_update() {
        let syn = letrec_(
            vec![i(), value(unit()), thunk(app(lref(0), vec![lref(1)]))],
            local(2),
        );

        let mut m = machine(syn);
        m.run(Some(20)).unwrap();
        assert!(m.unit_return());
    }

    #[test]
    pub fn test_case_bool_toggle() {
        let syn = letrec_(vec![not(), value(t())], app(lref(0), vec![lref(1)]));
        let mut m = machine(syn);
        m.run(Some(20)).unwrap();
        assert_eq!(m.bool_return(), Some(false));
    }

    #[test]
    pub fn test_demeta_meta() {
        let syn = letrec_(
            vec![value(t()), value(with_meta(lref(0), num(9))), meta()],
            app(lref(2), vec![lref(1)]),
        );

        let mut m = machine(syn);
        m.run(Some(20)).unwrap();
        assert_eq!(m.bool_return(), Some(true));
    }

    #[test]
    pub fn test_demeta_nometa() {
        let syn = letrec_(
            vec![value(atom(num(9))), meta()],
            app(lref(1), vec![lref(0)]),
        );

        let mut m = machine(syn);
        m.run(Some(20)).unwrap();
        assert!(m.unit_return());
    }
}
