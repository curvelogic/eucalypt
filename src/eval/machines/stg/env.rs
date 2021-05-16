//! The cactus environment heap used by the STG machine.
use super::syntax::{dsl, LambdaForm, Native, Ref, StgSyn};
use crate::{common::sourcemap::Smid, eval::error::ExecutionError};
use bacon_rajan_cc::{Cc, Trace, Tracer};
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
    env: Cc<EnvFrame>,
    /// Pending arguments (for a partial application)
    pap_args: Vec<Cc<RefCell<Closure>>>,
    /// Total arity (subtract pap_args for remaining)
    arity: u8,
    /// Whether to update
    update: bool,
    /// Annotation to stamp on environment when saturated
    annotation: Smid,
}

impl Trace for Closure {
    fn trace(&self, tracer: &mut Tracer) {
        self.env.trace(tracer);
        self.pap_args.trace(tracer);
    }
}

impl Closure {
    /// A new non-callable closure of `code` over environment `env`
    pub fn new(code: Rc<StgSyn>, env: Cc<EnvFrame>) -> Self {
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
    pub fn close(lambda_form: &LambdaForm, env: &Cc<EnvFrame>) -> Self {
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
    pub fn env(&self) -> &Cc<EnvFrame> {
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
            .extend(args.drain(..).map(|c| Cc::new(RefCell::new(c))));
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
    bindings: Vec<Cc<RefCell<Closure>>>,
    /// Source code annotation
    annotation: Smid,
    /// Reference to next environment
    next: Option<Cc<EnvFrame>>,
}

impl Trace for EnvFrame {
    fn trace(&self, tracer: &mut Tracer) {
        self.next.trace(tracer);
        self.bindings.trace(tracer);
    }
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
    pub fn from_args(args: &[Ref], next: &Cc<EnvFrame>, annotation: Smid) -> Cc<Self> {
        Cc::new(EnvFrame {
            bindings: args
                .iter()
                .map(|r| {
                    Cc::new(RefCell::new(Closure::new(
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
    pub fn from_closures(closures: &[Closure], next: &Cc<EnvFrame>, annotation: Smid) -> Cc<Self> {
        Cc::new(EnvFrame {
            bindings: closures
                .iter()
                .map(|cl| Cc::new(RefCell::new(cl.clone())))
                .collect(),
            annotation,
            next: Some(next.clone()),
        })
    }

    /// From mixture of partially applied args and fresh args
    pub fn from_combined_args(
        mut existing: Vec<Cc<RefCell<Closure>>>,
        mut fresh: Vec<Closure>,
        next: &Cc<EnvFrame>,
        annotation: Smid,
    ) -> Cc<Self> {
        existing.reserve_exact(existing.len() + fresh.len());
        existing.extend(fresh.drain(..).map(|c| Cc::new(RefCell::new(c))));

        Cc::new(EnvFrame {
            bindings: existing,
            annotation,
            next: Some(next.clone()),
        })
    }

    /// "Allocate" let bindings in a new env
    pub fn from_let(bindings: &[LambdaForm], next: &Cc<EnvFrame>, annotation: Smid) -> Cc<Self> {
        Cc::new(EnvFrame {
            bindings: bindings
                .iter()
                .map(|pc| Cc::new(RefCell::new(Closure::close(pc, next))))
                .collect(),
            annotation,
            next: Some(next.clone()),
        })
    }

    /// "Allocate" let bindings in a new env
    ///
    /// NB this is where we're currently creating Rc cycles
    pub fn from_letrec(bindings: &[LambdaForm], next: &Cc<EnvFrame>, annotation: Smid) -> Cc<Self> {
        let frame = Cc::new(EnvFrame {
            bindings: vec![RefCell::new(Closure::default()); bindings.len()]
                .into_iter()
                .map(Cc::new)
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

#[cfg(test)]
pub mod tests {

    use super::*;
    use bacon_rajan_cc::collect_cycles;

    #[test]
    pub fn test_collect_letrec() {
        {
            let null_env = Cc::new(EnvFrame::default());

            {
                // |----------|
                // |          |->[ 99 ]--\
                // |----------|          |
                //      |   ^            |
                //      |   \------------/
                //      v
                //      •
                let _letrec = EnvFrame::from_letrec(
                    &[dsl::value(dsl::box_num(99))],
                    &null_env,
                    Smid::default(),
                );

                collect_cycles();
            }
            collect_cycles();
        }
        collect_cycles();
    }

    #[test]
    pub fn test_collect_let() {
        let null_env = Cc::new(EnvFrame::default());

        {
            // |----------|
            // |          |->[ 99 ]--\
            // |----------|          |
            //      |                |
            //      |	             |
            //      v                |
            //      •<---------------/
            let _letrec =
                EnvFrame::from_let(&[dsl::value(dsl::box_num(99))], &null_env, Smid::default());

            collect_cycles();
        }
        collect_cycles();
    }

    #[test]
    pub fn test_collect_from_args() {
        {
            let null_env = Cc::new(EnvFrame::default());

            {
                // |----------|
                // |          |->[ *99 ]--\
                // |----------|           |
                //      |                 |
                //      |	              |
                //      v                 |
                //      •<----------------/
                let _env = EnvFrame::from_args(&[dsl::gref(99)], &null_env, Smid::default());

                collect_cycles();
            }
            collect_cycles();
        }

        collect_cycles();
    }

    #[test]
    pub fn test_collect_from_combined_args() {
        {
            let null_env = Cc::new(EnvFrame::default());

            let existing = vec![Closure::new(dsl::box_num(99), null_env.clone())]
                .into_iter()
                .map(|c| Cc::new(RefCell::new(c)))
                .collect();

            let fresh = vec![Closure::new(dsl::box_num(100), null_env.clone())];

            {
                // |----------|
                // |          |->[ 99 ]---\
                // |----------|->[100 ]---|
                //      |                 |
                //      |	          |
                //      v                 |
                //      •<----------------/
                //
                let _env =
                    EnvFrame::from_combined_args(existing, fresh, &null_env, Smid::default());

                collect_cycles();
            }
            collect_cycles();
        }

        collect_cycles();
    }

    #[test]
    pub fn test_collect_from_closures() {
        let null_env = Cc::new(EnvFrame::default());

        {
            let closure = Closure::new(dsl::box_num(99), null_env.clone());

            {
                // |----------|
                // |          |->[ 99 ]--\
                // |----------|          |
                //      |                |
                //      |	         |
                //      v                |
                //      •<---------------/
                let _env = EnvFrame::from_closures(&[closure], &null_env, Smid::default());

                collect_cycles();
            }

            collect_cycles();
        }

        collect_cycles();
    }

    #[test]
    pub fn test_counterexample() {
        struct Env {
            pub closures: Vec<Cc<RefCell<Clos>>>,
            pub next: Option<Cc<Env>>,
        }
        impl Trace for Env {
            fn trace(&self, tracer: &mut Tracer) {
                self.closures.trace(tracer);
                self.next.trace(tracer);
            }
        }
        struct Clos {
            pub env: Cc<Env>,
        }
        impl Trace for Clos {
            fn trace(&self, tracer: &mut Tracer) {
                self.env.trace(tracer);
            }
        }

        let live_env = {
            let base_env = Cc::new(Env {
                closures: vec![],
                next: None,
            });

            let env_a = Cc::new(Env {
                closures: vec![Cc::new(RefCell::new(Clos {
                    env: base_env.clone(),
                }))],
                next: Some(base_env.clone()),
            });

            let circular_env = Cc::new(Env {
                closures: vec![Cc::new(RefCell::new(Clos {
                    env: base_env.clone(),
                }))],
                next: Some(env_a.clone()),
            });
            circular_env.closures[0].replace(Clos {
                env: circular_env.clone(),
            });

            let live_env = Cc::new(Env {
                closures: vec![],
                next: Some(env_a.clone()),
            });

            drop(base_env); // don't need the stack ref
            drop(env_a); // don't need the stack ref
            collect_cycles();

            drop(circular_env); // cycle root
            collect_cycles(); // <- incorrectly? frees env_a.
                              // mark_gray decrements env_a and does
                              // not reinstate (it's the root of the
                              // black region). collect_white frees
                              // circular_env, which decrements env_a
                              // again - to zero and frees it...

            live_env
        };

        if let Some(a) = &live_env.next {
            assert_eq!(a.closures.len(), 1);
        }
    }
}
