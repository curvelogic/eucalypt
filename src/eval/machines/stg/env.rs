//! The cactus environment heap used by the STG machine.
use super::syntax::{dsl, LambdaForm, Native, Ref, StgSyn};
use crate::{common::sourcemap::Smid, eval::error::ExecutionError};
use std::{cell::RefCell, fmt, rc::Rc};

/// Closure as stored in an environment
///
/// Closures are intended to be moved and replaced in refcells so
/// carry partially applied arguments inside them (as refcells ready
/// to drain into an EnvFrame on saturation). However, retrieval from
/// enviroments into the machine is done by clone so may be expensive
/// if partially applied closures are often stored and retrieved.
#[derive(Clone, Default)]
pub struct Closure {
    /// Code
    code: Rc<StgSyn>,
    /// Environment
    env: Rc<EnvFrame>,
    /// Pending arguments (for a partial application)
    pap_args: Vec<Rc<RefCell<Closure>>>,
    /// Total arity (subtract pap_args for remaining)
    arity: u8,
    /// Whether to update
    update: bool,
    /// Annotation to stamp on environment when saturated
    annotation: Smid,
}

impl Closure {
    /// A new non-callable closure of `code` over environment `env`
    pub fn new(code: Rc<StgSyn>, env: Rc<EnvFrame>) -> Self {
        Closure {
            code,
            env,
            pap_args: vec![],
            arity: 0,
            update: false,
            annotation: Smid::default(),
        }
    }

    /// Construct a closure from a lambda form
    pub fn close(lambda_form: &LambdaForm, env: &Rc<EnvFrame>) -> Self {
        Closure {
            code: lambda_form.body().clone(),
            env: env.clone(),
            pap_args: vec![],
            arity: lambda_form.arity(),
            update: lambda_form.update(),
            annotation: lambda_form.annotation(),
        }
    }

    /// Reference to the closure's environment
    pub fn env(&self) -> &Rc<EnvFrame> {
        &self.env
    }

    /// Reference to the closure's code
    pub fn code(&self) -> &Rc<StgSyn> {
        &self.code
    }

    /// Arity when partially applied args are taken into account
    pub fn remaining_arity(&self) -> u8 {
        self.arity - (self.pap_args.len() as u8)
    }

    /// Whether to update after evaluation
    pub fn updateable(&self) -> bool {
        self.update
    }

    /// Partially apply
    pub fn partially_apply(&mut self, mut args: Vec<Closure>) {
        self.pap_args.reserve_exact(self.arity as usize);
        self.pap_args
            .extend(args.drain(..).map(|c| Rc::new(RefCell::new(c))));
    }

    /// Saturate a lambda by creating a new bound argument environment
    /// from the existing partial application args (if any) and the
    /// provided arguments.
    pub fn saturate(&mut self, args: Vec<Closure>) {
        let pap_args = self.pap_args.split_off(0);
        self.env = EnvFrame::from_combined_args(pap_args, args, &self.env, self.annotation);
        self.arity = 0;
    }

    /// Unsafe means of navigating through closures by local Refs
    ///
    /// Used when read values of native lists that have been force
    /// evaluated ahead of time. Panics at the drop of a hat
    pub fn navigate_local(&self, arg: Ref) -> Closure {
        if let Ref::L(i) = arg {
            self.env.get(i).unwrap().borrow().clone()
        } else {
            panic!("non-local arg for str_list_arg")
        }
    }

    /// Unsafe means of navigating through closures by local Refs
    ///
    /// Used when read values of native lists that have been force
    /// evaluated ahead of time. Panics at the drop of a hat
    pub fn navigate_local_native(&self, arg: Ref) -> Native {
        let mut closure = match arg {
            Ref::L(_) => self.navigate_local(arg),
            Ref::G(_) => panic!("cannot navigate global"),
            Ref::V(n) => return n,
        };

        while let StgSyn::Atom { evaluand: r } = &**closure.code() {
            closure = match r {
                Ref::L(_) => closure.navigate_local(r.clone()),
                Ref::G(_) => panic!("cannot navigate global"),
                Ref::V(n) => return n.clone(),
            };
        }
        panic!("could not navigate to native")
    }
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.update {
            write!(f, "Th({}|{})", self.code, &*self.env)
        } else if self.arity > 0 {
            write!(
                f,
                "λ{{{}-{}}}({}|⒳→{})",
                self.arity,
                self.pap_args.len(),
                self.code,
                &*self.env
            )
        } else {
            write!(f, "({}|{})", self.code, &*self.env)
        }
    }
}

/// A 'frame' of bindings in our shared environment
///
/// Environment frames may represent let bindings, lambda bound args,
/// case bound args etc. Within a single scope args are placed in
/// natural order so `λ x y z. t` will result in t executed in a frame
/// containing [x y z] in that order.
///
/// When another frame is "pushed" (or scope wrapped around) it is
/// most convenient to think of it as placed to the left of the
/// original to see how the indexing works.
///
/// So `let foo = bar in λ x y z. t` will result in frames:
///
/// [foo] -> [x y z]
///
/// ...and any code operating in this context access the environment
/// locations
///
/// [foo] -> [x y z]
///   0       1 2 3
///
/// The compiler has to juggle these indexes and closures have no
/// record of the free variables they reference as in the standard
/// STG.
#[derive(Default)]
pub struct EnvFrame {
    /// Indexed bindings
    bindings: Vec<Rc<RefCell<Closure>>>,
    /// Source code annotation
    annotation: Smid,
    /// Reference to next environment
    next: Option<Rc<EnvFrame>>,
}

impl fmt::Display for EnvFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let len = self.bindings.len();

        match &self.next {
            None => {
                if len > 0 {
                    write!(f, "[×{}]→•", len)
                } else {
                    write!(f, "•")
                }
            }
            Some(env) => write!(f, "[×{}]→{}", len, &**env),
        }
    }
}

impl EnvFrame {
    /// Expose bindings
    pub fn print_bindings(&self) {
        for b in &self.bindings {
            println!("{}", *b.borrow());
        }
    }

    /// Create an empty environment
    pub fn empty() -> Self {
        Self::default()
    }

    /// Access any annotation
    pub fn annotation(&self) -> Smid {
        self.annotation
    }

    /// Gather trace of annotations from top to bottom
    pub fn annotation_trace(&self) -> Vec<Smid> {
        let mut trace = vec![];
        let mut frame = self;

        loop {
            let smid = frame.annotation();
            if smid != Smid::default() {
                trace.push(frame.annotation());
            }
            match &frame.next {
                Some(f) => frame = &f,
                None => return trace,
            }
        }
    }

    /// Zero-based closure access (from top of environment)
    pub fn get(&self, idx: usize) -> Option<&RefCell<Closure>> {
        let len = self.bindings.len();
        if idx < len {
            Some(&self.bindings[idx])
        } else {
            match self.next {
                Some(ref env) => env.get(idx - len),
                None => None,
            }
        }
    }

    /// Update in place
    pub fn update(&self, index: usize, closure: Closure) -> Result<(), ExecutionError> {
        match self.get(index) {
            Some(cell) => {
                cell.replace(closure);
                Ok(())
            }
            None => Err(ExecutionError::BadEnvironmentIndex(index)),
        }
    }

    /// From data constructor or lambda args
    pub fn from_args(args: &[Ref], next: &Rc<EnvFrame>, annotation: Smid) -> Rc<Self> {
        Rc::new(EnvFrame {
            bindings: args
                .iter()
                .map(|r| {
                    Rc::new(RefCell::new(Closure::new(
                        dsl::atom(r.clone()),
                        next.clone(),
                    )))
                }) // indirection
                .collect(),
            annotation,
            next: Some(next.clone()),
        })
    }

    /// From closures (to be clonded, expensive?)
    pub fn from_closures(closures: &[Closure], next: &Rc<EnvFrame>, annotation: Smid) -> Rc<Self> {
        Rc::new(EnvFrame {
            bindings: closures
                .iter()
                .map(|cl| Rc::new(RefCell::new(cl.clone())))
                .collect(),
            annotation,
            next: Some(next.clone()),
        })
    }

    /// From mixture of partially applied args and fresh args
    pub fn from_combined_args(
        mut existing: Vec<Rc<RefCell<Closure>>>,
        mut fresh: Vec<Closure>,
        next: &Rc<EnvFrame>,
        annotation: Smid,
    ) -> Rc<Self> {
        existing.reserve_exact(existing.len() + fresh.len());
        existing.extend(fresh.drain(..).map(|c| Rc::new(RefCell::new(c))));

        Rc::new(EnvFrame {
            bindings: existing,
            annotation,
            next: Some(next.clone()),
        })
    }

    /// "Allocate" let bindings in a new env
    pub fn from_let(bindings: &[LambdaForm], next: &Rc<EnvFrame>, annotation: Smid) -> Rc<Self> {
        Rc::new(EnvFrame {
            bindings: bindings
                .iter()
                .map(|pc| Rc::new(RefCell::new(Closure::close(pc, next))))
                .collect(),
            annotation,
            next: Some(next.clone()),
        })
    }

    /// "Allocate" let bindings in a new env
    ///
    /// NB this is where we're currently creating Rc cycles
    pub fn from_letrec(bindings: &[LambdaForm], next: &Rc<EnvFrame>, annotation: Smid) -> Rc<Self> {
        let frame = Rc::new(EnvFrame {
            bindings: vec![RefCell::new(Closure::default()); bindings.len()]
                .into_iter()
                .map(Rc::new)
                .collect(),
            annotation,
            next: Some(next.clone()),
        });

        {
            let EnvFrame {
                bindings: frame_bindings,
                ..
            } = &*frame;

            for (i, b) in bindings.iter().enumerate() {
                let cell = &*(frame_bindings[i]);
                cell.replace(Closure::close(b, &frame));
            }
        }

        frame
    }
}
