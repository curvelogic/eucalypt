//! Sketch of components of STG implementation
//!
//! A variation of the STG machine of the haskell eucalypt
//! implementation with several modifications:
//!
//! - remove metadata handling that was bodged into the machine in
//!   favour of a cleaner, transient metadata managed by special
//!   syntax
//! - eliminate the heap in favour of a mutable cactus stack approach
//! - box all natives for cleaner handling
use super::syntax::{dsl::global, tags, Native, Ref, StgSyn, Tag};
use super::{
    env::{Closure, EnvFrame},
    intrinsic::StgIntrinsic,
    syntax::dsl,
};
use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::{Emitter, NullEmitter},
        error::ExecutionError,
        intrinsics,
    },
};
use bacon_rajan_cc::{collect_cycles, number_of_roots_buffered, Cc};
use itertools::Itertools;
use std::{
    cmp::{max, Ordering},
    convert::TryInto,
    fmt,
    mem::swap,
    rc::Rc,
};

/// Continuations used on the stack to record how to handle returns
///
/// Three of the variants are standard STG machine continuations:
/// - Branch to record CASE continuations while the scrutinee is
///   evaluated
/// - Update to defer a thunk update for when the expression is
///   WHNF (though not in a heap - in the cactus environment)
/// - ApplyTo to hold args while the expression in call position is
///   evaluated
///
/// The last variant, DeMeta is a specialised case statement to
/// deconstruct a metadata / body pair (which is a data structured
/// encoded directly in the syntax rather than a Cons form).
pub enum Continuation {
    /// Expect a data constructor and match to a branch (or native to fallback)
    Branch {
        /// Branches for data constructor destructuring
        branches: Vec<(Tag, Rc<StgSyn>)>,
        /// Fallback for unmatched data or native
        fallback: Option<Rc<StgSyn>>,
        /// Environment of case statement
        environment: Cc<EnvFrame>,
    },
    /// Update thunk in environment at index i
    Update {
        environment: Cc<EnvFrame>,
        index: usize,
    },
    /// Once callable is evaluated, apply to args
    ApplyTo { args: Vec<Closure> },
    /// Catch metadata and pass it (with body) to handler
    DeMeta {
        /// handler receives metdata and body as bound args
        handler: Rc<StgSyn>,
        /// or_else receives the body as a bound arg
        or_else: Rc<StgSyn>,
        /// Environment of handlers
        environment: Cc<EnvFrame>,
    },
}

fn match_tag(tag: Tag, branches: &[(Tag, Rc<StgSyn>)]) -> Option<Rc<StgSyn>> {
    for (t, body) in branches {
        if *t == tag {
            return Some(body.clone());
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

/// Record some metrics as we execute code
#[derive(Default)]
pub struct Metrics {
    ticks: u64,
    allocs: u64,
    max_stack: usize,
}

impl Metrics {
    pub fn tick(&mut self) {
        self.ticks += 1;
    }

    pub fn ticks(&self) -> u64 {
        self.ticks
    }

    pub fn alloc(&mut self, count: usize) {
        self.allocs += count as u64;
    }

    pub fn allocs(&self) -> u64 {
        self.allocs
    }

    pub fn stack(&mut self, size: usize) {
        self.max_stack = max(self.max_stack, size);
    }

    pub fn max_stack(&self) -> usize {
        self.max_stack
    }
}

/// An STG machine variant using cactus environment
///
pub struct Machine<'a> {
    /// Current closure
    closure: Closure,
    /// Globals (primarily STG wrappers for intrinsics)
    globals: Cc<EnvFrame>,
    /// Intrinsics (actions with access into machine)
    intrinsics: Vec<&'a dyn StgIntrinsic>,
    /// Stack of continuations
    stack: Vec<Continuation>,
    /// Termination flag. Set when machine has terminated
    terminated: bool,
    /// Annotation to paint on any environments we create
    annotation: Smid,
    /// Emitter to send output and error events to
    emitter: Box<dyn Emitter + 'a>,
    /// Whether to trace every step to stderr
    trace_steps: bool,
    /// Metrics
    metrics: Metrics,
}

impl<'a> Machine<'a> {
    /// Construct a machine to evaluate `code`
    pub fn new(
        code: Rc<StgSyn>,
        environment: Cc<EnvFrame>,
        globals: Cc<EnvFrame>,
        intrinsics: Vec<&'a dyn StgIntrinsic>,
        emitter: Box<dyn Emitter + 'a>,
        trace_steps: bool,
    ) -> Self {
        Machine {
            closure: Closure::new(code, environment),
            globals,
            intrinsics,
            stack: Vec::new(),
            terminated: false,
            annotation: Smid::default(),
            emitter,
            trace_steps,
            metrics: Metrics::default(),
        }
    }

    /// The current environment
    pub fn env(&self) -> &Cc<EnvFrame> {
        self.closure.env()
    }

    /// The closure about to be entered
    pub fn closure(&self) -> &Closure {
        &self.closure
    }

    /// Access the emitter
    pub fn emitter(&mut self) -> &mut dyn Emitter {
        self.emitter.as_mut()
    }

    /// Access the metrics (ticks, allocs, etc.)
    pub fn metrics(&self) -> &Metrics {
        &self.metrics
    }

    /// Resolve a ref to a closure
    pub fn resolve_callable(&self, r: &Ref) -> Result<Closure, ExecutionError> {
        match r {
            Ref::L(index) => self.get(*index),
            Ref::G(index) => self.global(*index),
            Ref::V(_) => Err(ExecutionError::NotCallable(Smid::default())),
        }
    }

    /// Resolve a ref
    pub fn resolve(&self, r: &Ref) -> Result<Closure, ExecutionError> {
        match r {
            Ref::L(index) => self.get(*index),
            Ref::G(index) => self.global(*index),
            Ref::V(_) => Ok(Closure::new(
                dsl::atom(r.clone()),
                Cc::new(EnvFrame::default()),
            )),
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

        while let StgSyn::Atom { evaluand: r } = &**closure.code() {
            closure = match r {
                Ref::L(index) => closure
                    .env()
                    .get(*index)
                    .ok_or(ExecutionError::BadEnvironmentIndex(*index))?
                    .borrow()
                    .clone(),
                Ref::G(index) => self.global(*index)?,
                Ref::V(n) => return Ok(n.clone()),
            };
        }

        Err(ExecutionError::NotValue(Smid::default(), "".to_string()))
    }

    /// Index into current environment
    fn get(&self, index: usize) -> Result<Closure, ExecutionError> {
        self.env()
            .get(index)
            .map(|cell| cell.borrow().clone())
            .ok_or(ExecutionError::BadEnvironmentIndex(index))
    }

    /// Index into globals
    fn global(&self, index: usize) -> Result<Closure, ExecutionError> {
        self.globals
            .get(index)
            .map(|cell| cell.borrow().clone())
            .ok_or(ExecutionError::BadGlobalIndex(index))
    }

    /// Has the machine terminated?
    pub fn terminated(&self) -> bool {
        self.terminated
    }

    /// Determine an exit code if the machine is terminated
    ///
    /// Numbers in 0-255 are used as is
    /// Unit becomes zero
    /// Anything else is 1
    pub fn exit_code(&self) -> Option<u8> {
        let code = self.closure.code().clone();

        if self.terminated() {
            Some(match &*code {
                StgSyn::Atom { evaluand: r } => match self.resolve_native(r) {
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
                StgSyn::Cons { tag, .. } if *tag == 0 => 0,
                _ => 1,
            })
        } else {
            None
        }
    }

    /// Whether to collect garbage
    fn collect_garbage(&self) -> bool {
        number_of_roots_buffered() > 200
    }

    /// Execute one step
    pub fn step(&mut self) -> Result<(), ExecutionError> {
        self.metrics.tick();
        self.metrics.stack(self.stack.len());

        // GC cycles
        if self.collect_garbage() {
            collect_cycles();
        }

        let code = self.closure.code().clone();

        if self.closure.remaining_arity() > 0 {
            return self.return_fun();
        }

        match &*code {
            StgSyn::Atom { evaluand } => {
                match evaluand {
                    Ref::L(i) => {
                        let env = self.env().clone();
                        self.closure = self.get(*i)?;
                        if self.closure.updateable() {
                            self.stack.push(Continuation::Update {
                                environment: env,
                                index: *i, // TODO: optimise to directly enclosing env
                            });
                        }
                    }
                    Ref::G(i) => {
                        self.closure = self.global(*i)?; // TODO: update globals?
                    }
                    Ref::V(v) => {
                        self.return_native(&v)?;
                    }
                }
            }
            StgSyn::Case {
                scrutinee,
                branches,
                fallback,
            } => {
                self.stack.push(Continuation::Branch {
                    branches: branches.clone(),
                    fallback: fallback.clone(),
                    environment: self.env().clone(),
                });
                self.closure = Closure::new(scrutinee.clone(), self.env().clone());
            }
            StgSyn::Cons { tag, args } => {
                self.return_data(*tag, args)?;
            }
            StgSyn::App { callable, args } => {
                let arg_closures = args
                    .iter()
                    .map(|syn| Closure::new(dsl::atom(syn.clone()), self.env().clone()))
                    .collect();
                self.stack
                    .push(Continuation::ApplyTo { args: arg_closures });
                self.closure = self.resolve_callable(callable)?;
            }
            StgSyn::Bif { intrinsic, args } => {
                let bif = &self.intrinsics[*intrinsic as usize];
                bif.execute(self, args)?;
            }
            StgSyn::Let { bindings, body } => {
                self.metrics.alloc(bindings.len());
                let env = EnvFrame::from_let(bindings, self.env(), self.annotation);
                self.closure = Closure::new(body.clone(), env);
            }
            StgSyn::LetRec { bindings, body } => {
                self.metrics.alloc(bindings.len());
                let env = EnvFrame::from_letrec(bindings, self.env(), self.annotation);
                self.closure = Closure::new(body.clone(), env);
            }
            StgSyn::Ann { smid, body } => {
                self.annotation = *smid;
                self.closure = Closure::new(body.clone(), self.env().clone());
            }
            StgSyn::Meta { meta, body } => {
                self.return_meta(meta, body)?;
            }
            StgSyn::DeMeta {
                scrutinee,
                handler,
                or_else,
            } => {
                self.stack.push(Continuation::DeMeta {
                    handler: handler.clone(),
                    or_else: or_else.clone(),
                    environment: self.env().clone(),
                });
                self.closure = Closure::new(scrutinee.clone(), self.env().clone());
            }
            StgSyn::Pragma {} => {}
            StgSyn::BlackHole => return Err(ExecutionError::BlackHole),
        }
        Ok(())
    }

    /// Used by intrinsics to update the closure after execution
    pub fn set_closure(&mut self, closure: Closure) -> Result<(), ExecutionError> {
        self.closure = closure;
        Ok(())
    }

    /// Return meta into a demeta destructuring or just strip metadata
    /// and continue
    pub fn return_meta(&mut self, meta: &Ref, body: &Ref) -> Result<(), ExecutionError> {
        match self.stack.pop() {
            Some(Continuation::DeMeta {
                handler,
                environment,
                ..
            }) => {
                self.annotation = environment.annotation();
                self.closure = Closure::new(
                    handler,
                    EnvFrame::from_closures(
                        &[self.resolve(meta)?, self.resolve(body)?],
                        &environment,
                        self.annotation,
                    ),
                );
            }
            Some(Continuation::Update { environment, index }) => {
                environment.update(index, self.closure.clone())?;
            }
            Some(cont) => {
                self.closure = self.resolve(body)?;
                self.annotation = self.closure.env().annotation();
                self.stack.push(cont);
            }
            None => {
                // Don't terminate at metadata, carrying processing
                self.closure = self.resolve(body)?;
                self.annotation = self.closure.env().annotation();
            }
        }
        Ok(())
    }

    /// Return a native value into continuation or terminate
    pub fn return_native(&mut self, value: &Native) -> Result<(), ExecutionError> {
        match self.stack.pop() {
            Some(frame) => match frame {
                Continuation::Branch {
                    fallback,
                    environment,
                    ..
                } => {
                    if let Some(fb) = fallback {
                        self.annotation = environment.annotation();
                        self.closure = Closure::new(
                            fb,
                            EnvFrame::from_args(
                                &[dsl::vref(value.clone())],
                                &environment,
                                self.annotation,
                            ),
                        );
                    } else {
                        return Err(ExecutionError::NoBranchForNative);
                    }
                }
                Continuation::Update { environment, index } => {
                    environment.update(index, self.closure.clone())?;
                }
                Continuation::ApplyTo { .. } => {
                    return Err(ExecutionError::NotCallable(Smid::default()));
                }
                Continuation::DeMeta {
                    or_else,
                    environment,
                    ..
                } => {
                    self.annotation = environment.annotation();
                    self.closure = Closure::new(
                        or_else,
                        EnvFrame::from_args(
                            &[dsl::vref(value.clone())],
                            &environment,
                            self.annotation,
                        ),
                    );
                }
            },
            None => self.terminated = true,
        }
        Ok(())
    }

    /// Return data into an appropriate branch handler
    ///
    /// Data is destructured for tag handlers but not for the default
    /// handler which is used for natives and unknown data constructors.
    pub fn return_data(&mut self, tag: Tag, args: &[Ref]) -> Result<(), ExecutionError> {
        match self.stack.pop() {
            Some(frame) => match frame {
                Continuation::Branch {
                    branches,
                    fallback,
                    environment,
                } => {
                    if let Some(body) = match_tag(tag, &branches) {
                        let closures =
                            args.iter()
                                .map(|r| self.resolve(r))
                                .collect::<Result<Vec<Closure>, ExecutionError>>()?;
                        self.annotation = environment.annotation();
                        self.closure = Closure::new(
                            body,
                            // TODO: skip empty frames
                            EnvFrame::from_closures(&closures, &environment, self.annotation),
                        );
                    } else if let Some(body) = fallback {
                        self.annotation = environment.annotation();
                        self.closure = Closure::new(
                            body,
                            EnvFrame::from_closures(
                                &[self.closure.clone()],
                                &environment,
                                self.annotation,
                            ),
                        );
                    } else {
                        return Err(ExecutionError::NoBranchForDataTag(tag as u8));
                    }
                }
                Continuation::Update { environment, index } => {
                    environment.update(index, self.closure.clone())?;
                }
                Continuation::ApplyTo { mut args } => {
                    // Block application - BLOCKs are a special case
                    // and can be applied as functions, calling
                    // __MERGE
                    //
                    // TODO: a more generic mechanism for applying
                    // data structures
                    if tag == tags::BLOCK {
                        let call = Closure::new(
                            global(intrinsics::index("MERGE").unwrap()),
                            self.env().clone(),
                        );
                        args.push(self.closure.clone());
                        self.stack.push(Continuation::ApplyTo { args });
                        self.closure = call;
                    } else {
                        return Err(ExecutionError::NotCallable(Smid::default()));
                    }
                }
                Continuation::DeMeta {
                    or_else,
                    environment,
                    ..
                } => {
                    self.closure = Closure::new(
                        or_else,
                        EnvFrame::from_closures(
                            &[self.closure.clone()],
                            &environment,
                            self.annotation,
                        ),
                    );
                    self.annotation = environment.annotation();
                }
            },

            None => self.terminated = true,
        }
        Ok(())
    }

    /// Return function to either apply to args or default case branch
    pub fn return_fun(&mut self) -> Result<(), ExecutionError> {
        match self.stack.pop() {
            Some(frame) => match frame {
                Continuation::ApplyTo { mut args } => {
                    let excess = args.len() as isize - self.closure.remaining_arity() as isize;

                    match excess.cmp(&0) {
                        Ordering::Equal => {
                            self.closure.saturate(args);
                        }
                        Ordering::Less => {
                            self.closure.partially_apply(args);
                        }
                        Ordering::Greater => {
                            let surplus = args.split_off(excess as usize);
                            self.closure.saturate(args);
                            self.stack.push(Continuation::ApplyTo { args: surplus });
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
                        self.annotation = environment.annotation();
                        self.closure = Closure::new(
                            body,
                            EnvFrame::from_closures(
                                &[self.closure.clone()],
                                &environment,
                                self.annotation,
                            ),
                        );
                    } else {
                        return Err(ExecutionError::CannotReturnFunToCase);
                    }
                }
                Continuation::Update { environment, index } => {
                    environment.update(index, self.closure.clone())?;
                }
                Continuation::DeMeta {
                    or_else,
                    environment,
                    ..
                } => {
                    self.closure = Closure::new(
                        or_else,
                        EnvFrame::from_closures(
                            &[self.closure.clone()],
                            &environment,
                            self.annotation,
                        ),
                    );
                    self.annotation = environment.annotation()
                }
            },
            None => self.terminated = true,
        }
        Ok(())
    }

    /// Safe run that will limit execution to `limit` steps
    pub fn safe_run(&mut self, limit: usize) -> Result<(), ExecutionError> {
        // Debug Dump
        let mut i = 0;
        while !self.terminated {
            eprintln!("{}", self);
            if i >= limit {
                return Err(ExecutionError::DidntTerminate(limit));
            }
            self.step().map_err(|e| {
                ExecutionError::Traced(Box::new(e), self.env_trace(), self.stack_trace())
            })?;
            i += 1;
        }
        Ok(())
    }

    /// Safe run that will limit execution to `limit` steps
    pub fn run(&mut self) -> Result<Option<u8>, ExecutionError> {
        while !self.terminated {
            if self.trace_steps {
                eprintln!("{}", self);
            }
            self.step().map_err(|e| {
                ExecutionError::Traced(Box::new(e), self.env_trace(), self.stack_trace())
            })?;
        }
        Ok(self.exit_code())
    }

    /// Recover the emitter after a run
    pub fn take_emitter(&mut self) -> Box<dyn Emitter + 'a> {
        let mut ret: Box<dyn Emitter + 'a> = Box::new(NullEmitter);
        swap(&mut ret, &mut self.emitter);
        ret
    }

    /// Exit immediately
    pub fn panic(&mut self, message: &str) -> Result<(), ExecutionError> {
        self.terminated = true;
        panic!("{}", message); // TODO: implement panic
    }

    /// Trace of annotations in environment
    pub fn env_trace(&self) -> Vec<Smid> {
        self.env().annotation_trace()
    }

    /// Trace of annotations in execution stack
    pub fn stack_trace(&self) -> Vec<Smid> {
        self.stack
            .iter()
            .rev()
            .filter_map(|cont| match cont {
                Continuation::Branch { environment, .. }
                | Continuation::Update { environment, .. } // maybe not...
                | Continuation::DeMeta { environment, .. } => Some(environment.annotation()),
                _ => None,
            })
            .filter(|smid| *smid != Smid::default())
            .dedup()
            .collect()
    }
}

impl<'a> fmt::Display for Machine<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let stack = self.stack.iter().rev().format(":");
        write!(f, "M ⟪{}⟫ <{}>", self.closure, stack)
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::eval::machines::stg::syntax::ex::*;
    use crate::eval::{emit::DebugEmitter, machines::stg::syntax::dsl::*};

    lazy_static! {
        static ref EMPTY_INTRINSICS: Vec<Box<dyn StgIntrinsic>> = vec![];
    }

    fn assert_evals_to(syn: Rc<StgSyn>, expected: Rc<StgSyn>, limit: usize) {
        let env = EnvFrame::empty();
        let globals = EnvFrame::empty();
        let mut machine = Machine::new(
            syn,
            Cc::new(env),
            Cc::new(globals),
            EMPTY_INTRINSICS.iter().map(|b| b.as_ref()).collect(),
            Box::new(DebugEmitter::default()),
            true,
        );
        machine.safe_run(limit).unwrap();
        assert_eq!(*machine.closure.code(), expected);
    }

    fn machine(syn: Rc<StgSyn>) -> Machine<'static> {
        let env = EnvFrame::empty();
        let globals = EnvFrame::empty();
        Machine::new(
            syn,
            Cc::new(env),
            Cc::new(globals),
            EMPTY_INTRINSICS.iter().map(|b| b.as_ref()).collect(),
            Box::new(DebugEmitter::default()),
            true,
        )
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
        let syn = let_(vec![i()], app(lref(0), vec![num(42)]));
        assert_evals_to(syn, atom(num(42)), 10);
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

        assert_evals_to(syn, atom(str("foo")), 20);
    }

    #[test]
    pub fn test_update() {
        let syn = letrec_(
            vec![i(), value(unit()), thunk(app(lref(0), vec![lref(1)]))],
            local(2),
        );

        assert_evals_to(syn, unit(), 20);
    }

    #[test]
    pub fn test_case_bool_toggle() {
        let syn = letrec_(vec![not(), value(t())], app(lref(0), vec![lref(1)]));
        assert_evals_to(syn, f(), 20);
    }

    #[test]
    pub fn test_demeta_meta() {
        let syn = letrec_(
            vec![value(t()), value(with_meta(lref(0), num(9))), meta()],
            app(lref(2), vec![lref(1)]),
        );

        assert_evals_to(syn, t(), 20);
    }

    #[test]
    pub fn test_demeta_nometa() {
        let syn = letrec_(
            vec![value(atom(num(9))), meta()],
            app(lref(1), vec![lref(0)]),
        );

        assert_evals_to(syn, unit(), 20);
    }
}
