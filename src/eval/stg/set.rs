//! Set intrinsics for the eucalypt VM.
//!
//! These intrinsics operate on `Native::Set` values, providing
//! construction, membership, mutation, and set algebra. A set is a header
//! over the dict's persistent ordered tree (`memory::set::HeapSet`), so add,
//! remove and membership are O(log n) and each operation shares the
//! untouched parts of its input. Elements are numbers, strings, symbols
//! and lists of elements (design note OPEN 8), keyed exactly as dict keys
//! are.

use std::{collections::HashMap, convert::TryInto};

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{
            AbiClosure, CallGlobal0, CallGlobal1, CallGlobal2, IntrinsicMachine, StgIntrinsic,
        },
        memory::{
            dict::{Dict, DictKey, DictKeyList, Tree},
            mutator::MutatorHeapView,
            set::{set_unit, HeapSet},
            syntax::Ref,
        },
        stg::tags::DataConstructor,
        types::IntrinsicType,
    },
};

use super::{
    dict::{key_value, MAX_KEY_DEPTH},
    force::SeqKey,
    support::{
        closure_to_element, data_list_arg, machine_return_bool, machine_return_num,
        machine_return_set, native_to_element, set_arg,
    },
    syntax::{
        dsl::{app_bif, force, lambda, local, lref},
        LambdaForm,
    },
};

/// Create a wrapper for a 2-arg set intrinsic that forces arg 1 (the set)
/// and deep-forces arg 0 (the element) with `seqKey`, the dict's key
/// forcing: a boxed scalar comes back forced and unboxed, and a list is
/// rebuilt with every element deep-forced, so a list element (design note
/// OPEN 8) reaches `execute` fully evaluated. Anything else arrives at
/// WHNF, for `execute` to reject.
fn element_set_wrapper(index: usize) -> LambdaForm {
    let bif_index: u8 = index.try_into().unwrap();
    lambda(
        2, // [element, set]
        force(
            local(1),
            // [forced_set] [element, set]
            force(
                SeqKey.global(lref(1)),
                // [forced_element] [forced_set] [element, set]
                app_bif(bif_index, vec![lref(0), lref(1)]),
            ),
        ),
    )
}

/// The deepest nesting accepted in a list element, as for dict keys.
const MAX_ELEMENT_DEPTH: usize = MAX_KEY_DEPTH;

/// Convert an evaluated (deep-forced) set element to its key: a number,
/// string or symbol, or a list of elements (design note OPEN 8).
///
/// Anything else raises [`ExecutionError::InvalidElement`] at the
/// intrinsic's call site. A list holding something else is described as
/// such ("a list holding a block").
fn closure_to_set_key(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    item: &AbiClosure,
) -> Result<DictKey, ExecutionError> {
    match list_element_key(machine, view, item, 0)? {
        Some(Ok(key)) => Ok(key),
        Some(Err(found)) => Err(ExecutionError::InvalidElement(
            machine.annotation(),
            Box::new((IntrinsicType::Set, found)),
        )),
        None => Ok(closure_to_element(machine, view, item, &IntrinsicType::Set)?.to_key()),
    }
}

/// The key of `item` when it is a list: `None` when it is not a list,
/// `Some(Err(description))` when the list cannot be an element. Never
/// forces.
fn list_element_key(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    item: &AbiClosure,
    depth: usize,
) -> Result<Option<Result<DictKey, String>>, ExecutionError> {
    let is_list = |machine: &dyn IntrinsicMachine, v: &AbiClosure| {
        let tag = machine.data_tag(view, v);
        tag == Some(DataConstructor::ListCons.tag()) || tag == Some(DataConstructor::ListNil.tag())
    };
    if !is_list(machine, item) {
        return Ok(None);
    }
    if depth >= MAX_ELEMENT_DEPTH {
        return Ok(Some(Err(format!(
            "a list nested more than {MAX_ELEMENT_DEPTH} deep"
        ))));
    }
    let smid = machine.annotation();
    let malformed =
        || ExecutionError::Panic(smid, "malformed cons cell in set element".to_string());
    let mut elements = Vec::new();
    let mut cur = item.clone();
    while machine.data_tag(view, &cur) == Some(DataConstructor::ListCons.tag()) {
        let head = machine.data_field(view, &cur, 0).ok_or_else(malformed)?;
        let key = match list_element_key(machine, view, &head, depth + 1)? {
            Some(Ok(key)) => key,
            Some(Err(found)) => return Ok(Some(Err(found))),
            None => match closure_to_element(machine, view, &head, &IntrinsicType::Set) {
                Ok(prim) => prim.to_key(),
                Err(ExecutionError::InvalidElement(_, b)) => {
                    return Ok(Some(Err(format!("a list holding {}", b.1))))
                }
                Err(e) => return Err(e),
            },
        };
        elements.push(key);
        cur = machine.data_field(view, &cur, 1).ok_or_else(malformed)?;
    }
    Ok(Some(Ok(DictKey::List(DictKeyList::alloc(
        &view, &elements,
    )?))))
}

/// [`closure_to_set_key`] for an intrinsic argument.
fn arg_to_set_key(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<DictKey, ExecutionError> {
    if let Ok(native) = machine.resolve_native(view, arg) {
        return Ok(native_to_element(machine.annotation(), &native, &IntrinsicType::Set)?.to_key());
    }
    let closure = machine.resolve_closure(view, arg)?;
    closure_to_set_key(machine, view, &closure)
}

/// SET.EMPTY — return an empty set
pub struct SetEmpty;

impl StgIntrinsic for SetEmpty {
    fn name(&self) -> &str {
        "SET.EMPTY"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        machine_return_set(machine, view, None)
    }
}

impl CallGlobal0 for SetEmpty {}

/// SET.OF — build a set from a list of primitives in one pass
pub struct SetOf;

impl StgIntrinsic for SetOf {
    fn name(&self) -> &str {
        "SET.OF"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        let bif_index: u8 = self.index().try_into().unwrap();
        // Deep-force the list and every element with `seqKey`, so list
        // elements arrive fully evaluated.
        lambda(
            1, // [list]
            force(
                SeqKey.global(lref(0)),
                // [forced_list] [list]
                app_bif(bif_index, vec![lref(0)]),
            ),
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // Each element raises the same error SET.ADD raises for it.
        let items = data_list_arg(machine, view, args[0].clone())?;
        let mut keys = Vec::with_capacity(items.len());
        for item in &items {
            keys.push(closure_to_set_key(machine, view, item)?);
        }
        // One sort + dedup and a bottom-up build: O(n log n), not n
        // repeated insertions.
        let root = HeapSet::from_keys(&Dict::new(view, machine.symbol_pool()), keys)?.root();
        machine_return_set(machine, view, root)
    }
}

impl CallGlobal1 for SetOf {}

/// SET.TO_LIST — convert a set to a sorted list of boxed values (and
/// lists, for list elements)
pub struct SetToList;

impl StgIntrinsic for SetToList {
    fn name(&self) -> &str {
        "SET.TO_LIST"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let set = set_arg(machine, view, &args[0])?;
        // Copy the (Copy) keys out so no borrow of the set spans the
        // allocations below.
        let sorted = set.elements(&Dict::new(view, machine.symbol_pool()));

        // Box each element (a cons-list for a list element) and assemble
        // the sorted list via the neutral ABI.
        let mut items = Vec::with_capacity(sorted.len());
        for key in &sorted {
            items.push(key_value(machine, view, key)?);
        }

        machine.return_closure_list(view, items)
    }
}

impl CallGlobal1 for SetToList {}

/// SET.ADD — add an element to a set
pub struct SetAdd;

impl StgIntrinsic for SetAdd {
    fn name(&self) -> &str {
        "SET.ADD"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        element_set_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let key = arg_to_set_key(machine, view, &args[0])?;
        let root = set_arg(machine, view, &args[1])?.root();
        let d = Dict::new(view, machine.symbol_pool());
        // A member leaves the tree as it is, unless `key` is the integer
        // form of a stored float, which replaces it (`Dict::canonical_key`).
        // The header is always fresh, even then: set `=` compares by
        // identity, so returning the input would change it.
        let root = match d.lookup(root, &key) {
            Some(p) if d.prefers(&key, &d.node(p).key()) => d.insert(root, key, set_unit())?,
            Some(_) => root,
            None => d.insert(root, key, set_unit())?,
        };
        machine_return_set(machine, view, root)
    }
}

impl CallGlobal2 for SetAdd {}

/// SET.REMOVE — remove an element from a set
pub struct SetRemove;

impl StgIntrinsic for SetRemove {
    fn name(&self) -> &str {
        "SET.REMOVE"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        element_set_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let key = arg_to_set_key(machine, view, &args[0])?;
        let root = set_arg(machine, view, &args[1])?.root();
        // Always a fresh header, even when `key` is not a member (see
        // SET.ADD).
        let root = Dict::new(view, machine.symbol_pool()).delete(root, &key)?;
        machine_return_set(machine, view, root)
    }
}

impl CallGlobal2 for SetRemove {}

/// SET.CONTAINS — check if an element is in a set
///
/// Returns false for values that can never be set members (blocks,
/// functions, lists holding them, ...).
pub struct SetContains;

impl StgIntrinsic for SetContains {
    fn name(&self) -> &str {
        "SET.CONTAINS"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        element_set_wrapper(self.index())
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // A value that cannot be an element (a block, a function, a list
        // holding one, ...) is never a member, so return false rather than
        // erroring.
        let key = match arg_to_set_key(machine, view, &args[0]) {
            Ok(k) => k,
            Err(ExecutionError::InvalidElement(..)) => {
                return machine_return_bool(machine, view, false)
            }
            Err(e) => return Err(e),
        };
        let root = set_arg(machine, view, &args[1])?.root();
        let found = Dict::new(view, machine.symbol_pool())
            .lookup(root, &key)
            .is_some();
        machine_return_bool(machine, view, found)
    }
}

impl CallGlobal2 for SetContains {}

/// SET.SIZE — return the number of elements
pub struct SetSize;

impl StgIntrinsic for SetSize {
    fn name(&self) -> &str {
        "SET.SIZE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let set = set_arg(machine, view, &args[0])?;
        let n = serde_json::Number::from(set.len() as u64);
        machine_return_num(machine, view, n)
    }
}

impl CallGlobal1 for SetSize {}

/// SET.UNION — return the union of two sets
pub struct SetUnion;

impl StgIntrinsic for SetUnion {
    fn name(&self) -> &str {
        "SET.UNION"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // Pipeline: `a set.union(b)` → SET.UNION(b, a) → args[0]=b, args[1]=a
        let a = set_arg(machine, view, &args[1])?.root();
        let b = set_arg(machine, view, &args[0])?.root();
        let d = Dict::new(view, machine.symbol_pool());
        let root = d.union_with(a, b, &mut |va, _| Ok(va.clone()))?;
        machine_return_set(machine, view, root)
    }
}

impl CallGlobal2 for SetUnion {}

/// SET.INTERSECT — return the intersection of two sets
pub struct SetIntersect;

impl StgIntrinsic for SetIntersect {
    fn name(&self) -> &str {
        "SET.INTERSECT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // Pipeline: `a set.intersect(b)` → SET.INTERSECT(b, a) → args[0]=b, args[1]=a
        let a = set_arg(machine, view, &args[1])?.root();
        let b = set_arg(machine, view, &args[0])?.root();
        let d = Dict::new(view, machine.symbol_pool());
        let root = d.intersect(a, b)?;
        machine_return_set(machine, view, root)
    }
}

impl CallGlobal2 for SetIntersect {}

/// SET.DIFF — return the difference (a - b)
pub struct SetDiff;

impl StgIntrinsic for SetDiff {
    fn name(&self) -> &str {
        "SET.DIFF"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // Pipeline: `a set.diff(b)` → SET.DIFF(b, a) → args[0]=b, args[1]=a
        let a = set_arg(machine, view, &args[1])?.root();
        let b = set_arg(machine, view, &args[0])?.root();
        let d = Dict::new(view, machine.symbol_pool());
        let root = d.difference(a, b)?;
        machine_return_set(machine, view, root)
    }
}

impl CallGlobal2 for SetDiff {}

/// SET.SAMPLE — pick k random elements from a set using random floats
/// from an external source (the random stream).
///
/// Args: [floats_list, set] where floats_list is a list of k floats
/// in [0,1). Converts set to a sorted vec internally, performs partial
/// Fisher-Yates, and returns a new set of the sampled elements.
pub struct SetSample;

impl StgIntrinsic for SetSample {
    fn name(&self) -> &str {
        "SET.SAMPLE"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        use super::force::SeqNumList;
        use super::syntax::dsl::{app_bif, force, lambda, local, lref};

        let bif_index: u8 = self.index().try_into().unwrap();
        // lambda args: [floats_list, set]
        // We need to force floats_list via SeqNumList.
        // After force: env is [forced_floats] [floats_list, set]
        // forced_floats is local(0), set is local(2)
        //
        // But the default wrapper already handles strict args.
        // The problem is that strict forcing uses unbox_num on num args,
        // and just force on unk args. For a list of floats, we need
        // SeqNumList to deep-force the list structure.
        //
        // Override: force arg 0 (floats) via SeqNumList, then force arg 1
        // (set) normally, then call bif.
        lambda(
            2, // [floats, set]
            force(
                local(1), // force set first (it's unk, just WHNF)
                // [forced_set] [floats, set]
                force(
                    SeqNumList.global(lref(1)),
                    // [forced_floats] [forced_set] [floats, set]
                    app_bif(bif_index, vec![lref(0), lref(1)]),
                ),
            ),
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        use super::support::collect_num_list;

        let floats = collect_num_list(machine, view, args[0].clone())?;
        let root = set_arg(machine, view, &args[1])?.root();
        let d = Dict::new(view, machine.symbol_pool());
        let len = d.size(root) as usize;
        let count = floats.len().min(len);

        // Partial Fisher-Yates over ranks, recording only the swapped
        // positions: the same draws as shuffling a dense index array, in
        // O(k log n) rather than O(n).
        let mut swapped: HashMap<usize, usize> = HashMap::new();
        let mut picked = Vec::with_capacity(count);
        for (i, &f) in floats.iter().take(count).enumerate() {
            let remaining = len - i;
            let j = i + (f * remaining as f64) as usize % remaining;
            let at_i = *swapped.get(&i).unwrap_or(&i);
            let at_j = *swapped.get(&j).unwrap_or(&j);
            swapped.insert(j, at_i);
            picked.push(at_j);
        }

        // The sample is a subset: sorting the ranks restores set order.
        picked.sort_unstable();
        let mut entries = Vec::with_capacity(count);
        for rank in picked {
            let node = d.nth(root, rank as u64).ok_or_else(|| {
                ExecutionError::Panic(
                    machine.annotation(),
                    "set sample rank out of range".to_string(),
                )
            })?;
            entries.push((d.node(node).key(), set_unit()));
        }
        let sampled: Tree = d.build_sorted(&entries)?;
        machine_return_set(machine, view, sampled)
    }
}

impl CallGlobal2 for SetSample {}
