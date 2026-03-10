//! The cactus environment heap used by the STG machine.

use std::fmt;

use crate::eval::memory::collect::{CollectorHeapView, GcScannable, OpaqueHeapBytes, ScanPtr};
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
///
/// ## Shared-backing frames
///
/// A frame may share its backing `Array` with the constructor's
/// environment frame to preserve thunk memoisation.  In that case
/// `remap_len > 0` and the `remap` table maps logical index `i` to
/// physical index `remap[i]` in the shared array.  The shared array
/// always has `length == top_len` (the constructor frame's full
/// length) so the GC scans all slots regardless of which frame it
/// encounters first.
pub struct EnvironmentFrame<C>
where
    C: Clone,
{
    /// Indexed bindings (may share backing storage with another frame)
    bindings: Array<C>,
    /// Logical-to-physical index remap for shared-backing frames.
    ///
    /// When `remap_len > 0`, logical index `i` maps to physical index
    /// `remap[i]` in the shared backing array.  When `remap_len == 0`,
    /// logical index equals physical index (identity mapping).
    remap: [u8; 4],
    /// Number of active remap entries.  0 means identity mapping.
    remap_len: u8,
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
            remap: [0; 4],
            remap_len: 0,
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
            remap: [0; 4],
            remap_len: 0,
            annotation,
            next,
        }
    }

    /// Construct a shared-backing frame with an explicit logical→physical index remap.
    ///
    /// The `remap` slice must have at most 4 entries.  Logical index `i`
    /// maps to physical index `remap[i]` in the shared backing array.
    ///
    /// The `bindings` array **must** have `length == top_len` (the constructor
    /// frame's full binding count) so the GC scans all live slots regardless of
    /// which frame it encounters first.
    pub fn new_remapped(
        bindings: Array<C>,
        remap: &[u8],
        annotation: Smid,
        next: Option<RefPtr<Self>>,
    ) -> Self {
        debug_assert!(remap.len() <= 4);
        debug_assert!(next.is_none() || (next.unwrap() != RefPtr::dangling()));
        let mut map = [0u8; 4];
        map[..remap.len()].copy_from_slice(remap);
        Self {
            bindings,
            remap: map,
            remap_len: remap.len() as u8,
            annotation,
            next,
        }
    }

    /// Logical length of this frame for cactus-stack index chaining.
    ///
    /// When a remap is active, this is the number of logical bindings
    /// exposed by this frame (`remap_len`).  For normal frames it is
    /// the physical binding count.  Indices `>= logical_len()` chain
    /// through to the `next` frame.
    #[inline]
    pub(crate) fn logical_len(&self) -> usize {
        if self.remap_len > 0 {
            self.remap_len as usize
        } else {
            self.bindings.len()
        }
    }

    /// Translate a logical index to the physical index in the backing array.
    ///
    /// When a remap is active (`remap_len > 0`), logical index `i` maps to
    /// physical index `remap[i]`.  Otherwise the mapping is the identity.
    /// The caller must ensure `logical < logical_len()`.
    #[inline]
    pub fn physical_index(&self, logical: usize) -> usize {
        if self.remap_len > 0 {
            self.remap[logical] as usize
        } else {
            logical
        }
    }

    /// Physical (backing) length of this frame.
    ///
    /// Always equals `self.bindings.len()`, regardless of any remap table.
    /// This is the count of slots that the GC must trace and the count used
    /// when sharing the full backing array for GC correctness.
    #[inline]
    pub fn backing_len(&self) -> usize {
        self.bindings.len()
    }

    /// Return a shared view of the bindings array covering all physical slots.
    ///
    /// The returned `Array` shares backing storage with this frame, so
    /// mutations through the returned handle are visible through this frame
    /// and vice versa.  Used to create shared-backing environment frames for
    /// data constructor destructuring without copying thunks.
    ///
    /// The returned array always has `length == self.bindings.len()` so that the
    /// GC traces every live slot regardless of which frame it encounters first.
    pub fn shared_bindings_full(&self) -> Array<C> {
        self.bindings.clone()
    }

    /// Return a shared view of the bindings array with the given logical length.
    ///
    /// The returned `Array` shares backing storage with this frame, so
    /// mutations through the returned handle are visible through this frame
    /// and vice versa.  Used to create shared-backing environment frames for
    /// data constructor destructuring without copying thunks.
    ///
    /// For GC correctness the returned array length should equal the
    /// constructor frame's full binding count so all live slots are traced.
    pub fn shared_bindings(&self, n: usize) -> Array<C> {
        self.bindings.clone_with_length(n)
    }

    /// Navigate down the environment stack to find the referenced cell
    fn cell(&self, guard: &dyn MutatorScope, idx: usize) -> Option<(Array<C>, usize)> {
        let len = self.logical_len();
        if idx < len {
            Some((self.bindings.clone(), self.physical_index(idx)))
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
        let mut trace = Vec::with_capacity(16);
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
        let len = self.logical_len();
        let suffix = if self.remap_len > 0 { "R" } else { "" };

        match self.next {
            None => {
                if len > 0 {
                    write!(f, "[×{len}{suffix}]→•")
                } else {
                    write!(f, "•")
                }
            }
            Some(env) => {
                let env = ScopedPtr::from_non_null(self, env);
                write!(f, "[×{len}{suffix}]→{env}")
            }
        }
    }
}

impl<C> fmt::Debug for ScopedPtr<'_, EnvironmentFrame<C>>
where
    C: Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let len = self.logical_len();
        let suffix = if self.remap_len > 0 { "R" } else { "" };

        match self.next {
            None => {
                write!(f, "[{:p} × {}{suffix}]→•", self.as_ptr(), len)
            }
            Some(env) => {
                let env = ScopedPtr::from_non_null(self, env);
                write!(f, "[{:p} × {}{suffix}]→{:?}", self.as_ptr(), len, env)
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
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        let code = self.code();
        if marker.mark(code) {
            out.push(ScanPtr::from_non_null(scope, code));
        }

        let env = self.env();
        if marker.mark(env) {
            out.push(ScanPtr::from_non_null(scope, env));
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        if let Some(new_code) = heap.forwarded_to(self.code()) {
            self.0.set_body(new_code);
        }
        if let Some(new_env) = heap.forwarded_to(self.env()) {
            self.1 = new_env;
        }
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
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        let bindings = &self.bindings;

        if marker.mark_array(bindings) {
            // Push the backing allocation as a heap object so the evacuation
            // loop calls try_evacuate on it.  Without this, the backing stays
            // in the candidate block and is recycled after mark-state flip,
            // leaving the evacuated EnvFrame copy with a dangling data.ptr.
            if let Some(backing_ptr) = bindings.allocated_data() {
                out.push(ScanPtr::from_non_null(
                    scope,
                    backing_ptr.cast::<OpaqueHeapBytes>(),
                ));
            }
            for binding in bindings.iter() {
                out.push(ScanPtr::new(scope, binding));
            }
        }

        if let Some(next) = self.next {
            if marker.mark(next) {
                out.push(ScanPtr::from_non_null(scope, next));
            }
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        // If the bindings backing array was evacuated to a new block,
        // update the internal pointer before iterating.  Without this,
        // iter_mut() would walk the old (now-dead) backing memory.
        if let Some(old_ptr) = self.bindings.allocated_data() {
            if let Some(new_ptr) = heap.forwarded_to(old_ptr) {
                // SAFETY: new_ptr is a valid evacuated copy of the same
                // backing allocation, with identical capacity and element
                // layout.
                unsafe { self.bindings.set_backing_ptr(new_ptr.cast()) };
            }
        }
        for binding in self.bindings.iter_mut() {
            binding.scan_and_update(heap);
        }
        if let Some(ref mut next) = self.next {
            if let Some(new_next) = heap.forwarded_to(*next) {
                *next = new_next;
            }
        }
    }
}
