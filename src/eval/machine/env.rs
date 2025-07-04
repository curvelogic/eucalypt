//! The cactus environment heap used by the STG machine.

use std::fmt;

use crate::eval::memory::collect::{GcScannable, ScanPtr};
use crate::eval::memory::infotable::{InfoTable, InfoTagged};
use crate::{common::sourcemap::Smid, eval::error::ExecutionError};

use crate::eval::memory::{
    alloc::{MutatorScope, ScopedPtr, StgObject},
    array::Array,
    syntax::{HeapSyn, Native, Ref, RefPtr},
};

/// Closure as stored in an environment frame
///
/// A closure consist of a static part (InfoTable) that can be
/// statically compiled, and a pointer to an environment
#[derive(Clone)]
pub struct Closing<S>(InfoTagged<S>, RefPtr<EnvironmentFrame<Closing<S>>>)
where
    S: Copy;

impl<S> StgObject for Closing<S> where S: Copy {}

impl<S> InfoTable for Closing<S>
where
    S: Copy,
{
    /// Arity when partially applied args are taken into account
    fn arity(&self) -> u8 {
        self.0.arity()
    }

    /// Whether to update after evaluation
    fn update(&self) -> bool {
        self.0.update()
    }

    fn annotation(&self) -> Smid {
        self.0.annotation()
    }
}

impl<S: Copy> Closing<S> {
    /// A new non-callable closure of `code` over environment `env`
    pub fn new(code: S, env: RefPtr<EnvironmentFrame<Closing<S>>>) -> Self {
        Closing(InfoTagged::new(0, code, Smid::default()), env)
    }

    /// A new non-callable closure of `code` over environment `env`
    pub fn new_annotated(
        code: S,
        env: RefPtr<EnvironmentFrame<Closing<S>>>,
        annotation: Smid,
    ) -> Self {
        Closing(InfoTagged::new(0, code, annotation), env)
    }

    /// A new non-callable closure of `code` over environment `env`
    pub fn new_annotated_lambda(
        code: S,
        arity: u8,
        env: RefPtr<EnvironmentFrame<Closing<S>>>,
        annotation: Smid,
    ) -> Self {
        Closing(InfoTagged::new(arity, code, annotation), env)
    }

    /// Construct a closure from a lambda form
    pub fn close(lambda_form: &InfoTagged<S>, env: RefPtr<EnvironmentFrame<Closing<S>>>) -> Self {
        Closing(*lambda_form, env)
    }

    /// Reference to the closure's environment
    pub fn env(&self) -> RefPtr<EnvironmentFrame<Closing<S>>> {
        self.1
    }

    /// Reference to the closure's code
    pub fn code(&self) -> S {
        self.0.body()
    }

    /// Unsafe means of navigating through closures by local Refs
    ///
    /// Used when read values of native lists that have been force
    /// evaluated ahead of time. Panics at the drop of a hat
    pub fn navigate_local<'guard>(
        &'guard self,
        guard: &'guard dyn MutatorScope,
        arg: Ref,
    ) -> Closing<S> {
        if let Ref::L(i) = arg {
            let env = &*ScopedPtr::from_non_null(guard, self.env());
            if let Some(closure) = env.get(guard, i) {
                closure
            } else {
                panic!("invalid ref")
            }
        } else {
            panic!("non-local arg for str_list_arg")
        }
    }
}

impl Closing<RefPtr<HeapSyn>> {
    /// Unsafe means of navigating through closures by local Refs
    ///
    /// Used when read values of native lists that have been force
    /// evaluated ahead of time. Panics at the drop of a hat
    pub fn navigate_local_native(&self, guard: &dyn MutatorScope, arg: Ref) -> Native {
        let mut closure = match arg {
            Ref::L(_) => self.navigate_local(guard, arg),
            Ref::G(_) => panic!("cannot navigate global"),
            Ref::V(n) => return n,
        };

        let mut code_ptr = ScopedPtr::from_non_null(guard, closure.code());

        while let HeapSyn::Atom { evaluand: r } = &*code_ptr {
            closure = match r {
                Ref::L(_) => closure.navigate_local(guard, r.clone()),
                Ref::G(_) => panic!("cannot navigate global"),
                Ref::V(n) => return n.clone(),
            };

            code_ptr = ScopedPtr::from_non_null(guard, closure.code());
        }
        panic!("could not navigate to native")
    }
}

pub struct ScopeAndClosure<'guard>(pub &'guard dyn MutatorScope, pub &'guard SynClosure);

impl fmt::Display for ScopeAndClosure<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let code = ScopedPtr::from_non_null(self.0, self.1.code());
        let env = ScopedPtr::from_non_null(self.0, self.1.env());

        if self.1.update() {
            write!(f, "Th({code}|{env})")
        } else if self.1.arity() > 0 {
            write!(f, "λ{{{}}}({}|⒳→{})", self.1.arity(), code, env)
        } else {
            write!(f, "({code}|{env})")
        }
    }
}

impl fmt::Display for ScopedPtr<'_, SynClosure> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ScopeAndClosure(self, self).fmt(f)
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
pub struct EnvironmentFrame<C>
where
    C: Clone,
{
    /// Indexed bindings
    bindings: Array<C>,
    /// Source code annotation
    annotation: Smid,
    /// Reference to next environment
    next: Option<RefPtr<EnvironmentFrame<C>>>,
}

impl<C> Default for EnvironmentFrame<C>
where
    C: Clone,
{
    fn default() -> Self {
        Self {
            bindings: Default::default(),
            annotation: Default::default(),
            next: Default::default(),
        }
    }
}

impl<C> EnvironmentFrame<C>
where
    C: Clone,
{
    pub fn new(bindings: Array<C>, annotation: Smid, next: Option<RefPtr<Self>>) -> Self {
        debug_assert!(next.is_none() || (next.unwrap() != RefPtr::dangling()));
        Self {
            bindings,
            annotation,
            next,
        }
    }

    /// Navigate down the environment stack to find the referenced cell
    fn cell(&self, guard: &dyn MutatorScope, idx: usize) -> Option<(Array<C>, usize)> {
        let len = self.bindings.len();
        if idx < len {
            Some((self.bindings.clone(), idx))
        } else {
            match self.next {
                Some(ref env) => (*ScopedPtr::from_non_null(guard, *env)).cell(guard, idx - len),
                None => None,
            }
        }
    }

    /// Zero-based closure access (from top of environment)
    ///
    /// Uses guard for scoping derefs during navigation but then
    /// copies simple ref pointer out of RefCell for return. Therefore
    /// the returned value is not affected by subsequent updates and
    /// is useful mainly for immediate evaluation.
    pub fn get(&self, guard: &dyn MutatorScope, idx: usize) -> Option<C> {
        if let Some((arr, i)) = self.cell(guard, idx) {
            arr.get(i)
        } else {
            None
        }
    }

    /// Update in place
    pub fn update(
        &self,
        guard: &dyn MutatorScope,
        idx: usize,
        closure: C,
    ) -> Result<(), ExecutionError> {
        if let Some((mut arr, i)) = self.cell(guard, idx) {
            arr.set(i, closure)?;
            Ok(())
        } else {
            Err(ExecutionError::BadEnvironmentIndex(idx))
        }
    }

    /// Access any annotation
    pub fn annotation(&self) -> Smid {
        self.annotation
    }

    /// Gather trace of annotations from top to bottom
    pub fn annotation_trace(&self, guard: &dyn MutatorScope) -> Vec<Smid> {
        let mut trace = vec![];
        let mut frame = ScopedPtr::from_non_null(guard, unsafe {
            RefPtr::new_unchecked(self as *const Self as *mut Self)
        });

        loop {
            let smid = (*frame).annotation();
            if smid != Smid::default() {
                trace.push(smid);
            }
            match frame.next {
                Some(f) => frame = ScopedPtr::from_non_null(guard, f),
                None => return trace,
            }
        }
    }
}

impl<C> fmt::Display for ScopedPtr<'_, EnvironmentFrame<C>>
where
    C: Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let len = self.bindings.len();

        match self.next {
            None => {
                if len > 0 {
                    write!(f, "[×{len}]→•")
                } else {
                    write!(f, "•")
                }
            }
            Some(env) => {
                let env = ScopedPtr::from_non_null(self, env);
                write!(f, "[×{len}]→{env}")
            }
        }
    }
}

impl<C> fmt::Debug for ScopedPtr<'_, EnvironmentFrame<C>>
where
    C: Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let len = self.bindings.len();

        match self.next {
            None => {
                write!(f, "[{:p} × {}]→•", self.as_ptr(), len)
            }
            Some(env) => {
                let env = ScopedPtr::from_non_null(self, env);
                write!(f, "[{:p} × {}]→{:?}", self.as_ptr(), len, env)
            }
        }
    }
}

/// For now, a Closure is closing HeapSyn over an environment
pub type SynClosure = Closing<RefPtr<HeapSyn>>;

impl GcScannable for SynClosure {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn crate::eval::memory::collect::CollectorScope,
        marker: &mut crate::eval::memory::collect::CollectorHeapView<'a>,
    ) -> Vec<ScanPtr<'a>> {
        let mut grey = vec![];

        let code = self.code();
        if marker.mark(code) {
            grey.push(ScanPtr::from_non_null(scope, code));
        }

        let env = self.env();
        if marker.mark(env) {
            grey.push(ScanPtr::from_non_null(scope, env));
        }

        grey
    }
}

/// For now, an EnvFrame is an environment frame with HeapSyn Closures
pub type EnvFrame = EnvironmentFrame<SynClosure>;
impl StgObject for EnvFrame {}

impl GcScannable for EnvFrame {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn crate::eval::memory::collect::CollectorScope,
        marker: &mut crate::eval::memory::collect::CollectorHeapView<'a>,
    ) -> Vec<ScanPtr<'a>> {
        let mut grey = vec![];

        let bindings = &self.bindings;

        if marker.mark_array(bindings) {
            for binding in bindings.iter() {
                grey.push(ScanPtr::new(scope, binding));
            }
        }

        if let Some(next) = self.next {
            if marker.mark(next) {
                grey.push(ScanPtr::from_non_null(scope, next));
            }
        }

        grey
    }
}
