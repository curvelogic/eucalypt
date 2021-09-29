//! The cactus environment heap used by the STG machine.

use std::fmt;

use crate::eval::memory::infotable::InfoTable;
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
pub struct Closing<T>(InfoTable<T>, RefPtr<EnvironmentFrame<T>>)
where
    T: Copy;

impl<T: Copy> Closing<T> {
    /// A new non-callable closure of `code` over environment `env`
    pub fn new(code: T, env: RefPtr<EnvironmentFrame<T>>) -> Self {
        Closing(InfoTable::new(0, code, Smid::default()), env)
    }

    /// A new non-callable closure of `code` over environment `env`
    pub fn new_annotated(code: T, env: RefPtr<EnvironmentFrame<T>>, annotation: Smid) -> Self {
        Closing(InfoTable::new(0, code, annotation), env)
    }

    /// A new non-callable closure of `code` over environment `env`
    pub fn new_annotated_lambda(
        code: T,
        arity: u8,
        env: RefPtr<EnvironmentFrame<T>>,
        annotation: Smid,
    ) -> Self {
        Closing(InfoTable::new(arity, code, annotation), env)
    }

    /// Construct a closure from a lambda form
    pub fn close(lambda_form: &InfoTable<T>, env: RefPtr<EnvironmentFrame<T>>) -> Self {
        Closing(*lambda_form, env)
    }

    /// Reference to the closure's environment
    pub fn env(&self) -> RefPtr<EnvironmentFrame<T>> {
        self.1
    }

    /// Reference to the closure's code
    pub fn code(&self) -> T {
        self.0.body()
    }

    /// Arity when partially applied args are taken into account
    pub fn arity(&self) -> u8 {
        self.0.arity()
    }

    /// Whether to update after evaluation
    pub fn updateable(&self) -> bool {
        self.0.update()
    }

    pub fn annotation(&self) -> Smid {
        self.0.annotation()
    }

    /// Unsafe means of navigating through closures by local Refs
    ///
    /// Used when read values of native lists that have been force
    /// evaluated ahead of time. Panics at the drop of a hat
    pub fn navigate_local(&self, guard: &dyn MutatorScope, arg: Ref) -> RefPtr<Closing<T>> {
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

        let mut scoped_closure = ScopedPtr::from_non_null(guard, closure);
        let mut code_ptr = ScopedPtr::from_non_null(guard, scoped_closure.code());

        while let HeapSyn::Atom { evaluand: r } = &*code_ptr {
            closure = match r {
                Ref::L(_) => (*scoped_closure).navigate_local(guard, r.clone()),
                Ref::G(_) => panic!("cannot navigate global"),
                Ref::V(n) => return n.clone(),
            };

            scoped_closure = ScopedPtr::from_non_null(guard, closure);
            code_ptr = ScopedPtr::from_non_null(guard, scoped_closure.code());
        }
        panic!("could not navigate to native")
    }
}

impl<'guard> fmt::Display for ScopedPtr<'guard, Closure> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let code = ScopedPtr::from_non_null(self, self.code());
        let env = ScopedPtr::from_non_null(self, self.env());

        if self.updateable() {
            write!(f, "Th({}|{})", code, env)
        } else if self.arity() > 0 {
            write!(f, "λ{{{}}}({}|⒳→{})", self.arity(), code, env)
        } else {
            write!(f, "({}|{})", code, env)
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
pub struct EnvironmentFrame<T>
where
    T: Copy,
{
    /// Indexed bindings
    bindings: Array<RefPtr<Closing<T>>>,
    /// Source code annotation
    annotation: Smid,
    /// Reference to next environment
    next: Option<RefPtr<EnvironmentFrame<T>>>,
}

impl<T> Default for EnvironmentFrame<T>
where
    T: Copy,
{
    fn default() -> Self {
        Self {
            bindings: Default::default(),
            annotation: Default::default(),
            next: Default::default(),
        }
    }
}

impl<T> EnvironmentFrame<T>
where
    T: Copy,
{
    pub fn new(
        bindings: Array<RefPtr<Closing<T>>>,
        annotation: Smid,
        next: Option<RefPtr<Self>>,
    ) -> Self {
        Self {
            bindings,
            annotation,
            next,
        }
    }

    /// Navigate down the environment stack to find the referenced cell
    fn cell(
        &self,
        guard: &dyn MutatorScope,
        idx: usize,
    ) -> Option<(Array<RefPtr<Closing<T>>>, usize)> {
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
    pub fn get(&self, guard: &dyn MutatorScope, idx: usize) -> Option<RefPtr<Closing<T>>> {
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
        closure: RefPtr<Closing<T>>,
    ) -> Result<(), ExecutionError> {
        if let Some((mut arr, i)) = self.cell(guard, idx) {
            arr.set(i, closure);
            Ok(())
        } else {
            Err(ExecutionError::BadEnvironmentIndex(idx))
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
            match (*frame).next {
                Some(f) => frame = ScopedPtr::from_non_null(guard, f),
                None => return trace,
            }
        }
    }
}

impl<'guard, T> fmt::Display for ScopedPtr<'guard, EnvironmentFrame<T>>
where
    T: Copy,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let len = self.bindings.len();

        match (*self).next {
            None => {
                if len > 0 {
                    write!(f, "[×{}]→•", len)
                } else {
                    write!(f, "•")
                }
            }
            Some(env) => {
                let env = ScopedPtr::from_non_null(self, env);
                write!(f, "[×{}]→{}", len, env)
            }
        }
    }
}

/// For now, a Closure is closing HeapSyn over an environment
pub type Closure = Closing<RefPtr<HeapSyn>>;
impl StgObject for Closure {}
/// For now, an EnvFrame is an environment frame with HeapSyn Closures
pub type EnvFrame = EnvironmentFrame<RefPtr<HeapSyn>>;
impl StgObject for EnvFrame {}
