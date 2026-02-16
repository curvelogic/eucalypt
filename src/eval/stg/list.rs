//! List intrinsics

use std::convert::TryInto;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal1, CallGlobal2, Const, IntrinsicMachine, StgIntrinsic},
        memory::{mutator::MutatorHeapView, syntax::Ref},
    },
};

use super::{
    force::SeqNumList,
    panic::Panic,
    support::{data_list_arg, machine_return_bool},
    syntax::{
        dsl::{annotated_lambda, app_bif, case, data, force, local, lref, str, value},
        LambdaForm,
    },
    tags::DataConstructor,
};

/// A constant for CONS
pub struct Cons;

impl StgIntrinsic for Cons {
    fn name(&self) -> &str {
        "CONS"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            2, // [h t]
            data(DataConstructor::ListCons.tag(), vec![lref(0), lref(1)]),
            annotation,
        )
    }
}

impl CallGlobal2 for Cons {}

/// A constant for NIL
pub struct Nil;

impl StgIntrinsic for Nil {
    fn name(&self) -> &str {
        "NIL"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        value(data(DataConstructor::ListNil.tag(), vec![]))
    }
}

impl Const for Nil {}

/// (Unsafe) list TAIL
pub struct Tail;

impl StgIntrinsic for Tail {
    fn name(&self) -> &str {
        "TAIL"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1,
            case(
                local(0),
                vec![(DataConstructor::ListCons.tag(), local(1))],
                Panic.global(str("TAIL on empty list")),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for Tail {}

/// (Unsafe) list HEAD
pub struct Head;

impl StgIntrinsic for Head {
    fn name(&self) -> &str {
        "HEAD"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1,
            case(
                local(0),
                vec![(DataConstructor::ListCons.tag(), local(0))],
                Panic.global(str("HEAD on empty list")),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for Head {}

/// ISLIST(value)
///
/// Return true if the value is a list (cons or nil), false otherwise
pub struct IsList;

impl StgIntrinsic for IsList {
    fn name(&self) -> &str {
        "ISLIST"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        use crate::eval::memory::syntax;
        let closure = machine.nav(view).resolve(&args[0])?;
        let code = view.scoped(closure.code());
        let is_list = matches!(
            &*code,
            syntax::HeapSyn::Cons { tag, .. }
                if *tag == DataConstructor::ListCons.tag()
                    || *tag == DataConstructor::ListNil.tag()
        );
        machine_return_bool(machine, view, is_list)
    }
}

impl CallGlobal1 for IsList {}

/// SORT_NUM_LIST — sort a list of numbers in Rust
///
/// The wrapper first applies SeqNumList to force and unbox all elements,
/// then calls the execute method which sorts in Rust and returns a
/// sorted list of boxed numbers.
pub struct SortNumList;

impl StgIntrinsic for SortNumList {
    fn name(&self) -> &str {
        "SORT_NUM_LIST"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        let bif_index: u8 = self.index().try_into().unwrap();
        annotated_lambda(
            1, // [xs]
            force(
                SeqNumList.global(lref(0)),
                // [concrete_list] [xs]
                app_bif(bif_index, vec![lref(0)]),
            ),
            annotation,
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        use crate::eval::memory::{
            alloc::ScopedAllocator,
            array::Array,
            syntax::{HeapSyn, LambdaForm, Native, StgBuilder},
        };

        // Walk the concrete list and collect numbers
        let iter = data_list_arg(machine, view, args[0].clone())?;
        let mut numbers: Vec<f64> = Vec::new();
        for item_result in iter {
            let item_closure = item_result?;
            let code = view.scoped(item_closure.code());
            match &*code {
                HeapSyn::Atom { evaluand } => {
                    let native = item_closure.navigate_local_native(&view, evaluand.clone());
                    match native {
                        Native::Num(n) => {
                            numbers.push(n.as_f64().unwrap_or(0.0));
                        }
                        _ => {
                            return Err(ExecutionError::Panic(
                                "non-numeric value in sort".to_string(),
                            ))
                        }
                    }
                }
                HeapSyn::Cons {
                    tag: _,
                    args: cargs,
                } => {
                    // Handle boxed numbers
                    let inner_ref = cargs.get(0).ok_or_else(|| {
                        ExecutionError::Panic("empty boxed value in sort".to_string())
                    })?;
                    let native = item_closure.navigate_local_native(&view, inner_ref.clone());
                    match native {
                        Native::Num(n) => {
                            numbers.push(n.as_f64().unwrap_or(0.0));
                        }
                        _ => {
                            return Err(ExecutionError::Panic(
                                "non-numeric value in sort".to_string(),
                            ))
                        }
                    }
                }
                _ => {
                    return Err(ExecutionError::Panic(
                        "unexpected value in sort".to_string(),
                    ))
                }
            }
        }

        // Sort
        numbers.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        // Build sorted list of boxed numbers in reverse
        let mut bindings = vec![LambdaForm::value(
            view.alloc(HeapSyn::Cons {
                tag: DataConstructor::ListNil.tag(),
                args: Array::default(),
            })?
            .as_ptr(),
        )];

        for &num in numbers.iter().rev() {
            let n =
                serde_json::Number::from_f64(num).unwrap_or_else(|| serde_json::Number::from(0));
            // Box the number
            bindings.push(LambdaForm::value(
                view.alloc(HeapSyn::Cons {
                    tag: DataConstructor::BoxedNumber.tag(),
                    args: Array::from_slice(&view, &[Ref::V(Native::Num(n))]),
                })?
                .as_ptr(),
            ));
            // Cons it onto the list
            let len = bindings.len();
            bindings.push(LambdaForm::value(
                view.alloc(HeapSyn::Cons {
                    tag: DataConstructor::ListCons.tag(),
                    args: Array::from_slice(&view, &[Ref::L(len - 1), Ref::L(len - 2)]),
                })?
                .as_ptr(),
            ));
        }

        let list_index = bindings.len() - 1;
        let syn = view
            .letrec(
                Array::from_slice(&view, &bindings),
                view.atom(Ref::L(list_index))?,
            )?
            .as_ptr();
        machine.set_closure(crate::eval::machine::env::SynClosure::new(
            syn,
            machine.root_env(),
        ))
    }
}

impl CallGlobal1 for SortNumList {}

/// KRUSKAL_LAST — run Kruskal's MST algorithm in Rust
///
/// Takes two arguments: sorted pairs (encoded numbers) and
/// x-coordinates of sorted points. Runs union-find with path
/// compression, returns x[i] * x[j] for the last pair that merges
/// two components.
pub struct KruskalLast;

impl KruskalLast {
    /// Extract f64 values from a concrete (forced) list
    fn collect_numbers(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        list_ref: Ref,
    ) -> Result<Vec<f64>, ExecutionError> {
        use crate::eval::memory::syntax::{HeapSyn, Native};

        let iter = data_list_arg(machine, view, list_ref)?;
        let mut numbers = Vec::new();
        for item_result in iter {
            let item_closure = item_result?;
            let code = view.scoped(item_closure.code());
            match &*code {
                HeapSyn::Atom { evaluand } => {
                    let native = item_closure.navigate_local_native(&view, evaluand.clone());
                    match native {
                        Native::Num(n) => numbers.push(n.as_f64().unwrap_or(0.0)),
                        _ => {
                            return Err(ExecutionError::Panic(
                                "non-numeric value in kruskal".to_string(),
                            ))
                        }
                    }
                }
                HeapSyn::Cons {
                    tag: _,
                    args: cargs,
                } => {
                    let inner_ref = cargs.get(0).ok_or_else(|| {
                        ExecutionError::Panic("empty boxed value in kruskal".to_string())
                    })?;
                    let native = item_closure.navigate_local_native(&view, inner_ref.clone());
                    match native {
                        Native::Num(n) => numbers.push(n.as_f64().unwrap_or(0.0)),
                        _ => {
                            return Err(ExecutionError::Panic(
                                "non-numeric value in kruskal".to_string(),
                            ))
                        }
                    }
                }
                _ => {
                    return Err(ExecutionError::Panic(
                        "unexpected value in kruskal".to_string(),
                    ))
                }
            }
        }
        Ok(numbers)
    }
}

impl StgIntrinsic for KruskalLast {
    fn name(&self) -> &str {
        "KRUSKAL_LAST"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        let bif_index: u8 = self.index().try_into().unwrap();
        annotated_lambda(
            2, // [pairs, xcords]
            force(
                SeqNumList.global(lref(0)),
                // [concrete_pairs] [pairs, xcords]
                force(
                    SeqNumList.global(lref(2)),
                    // [concrete_xcords, concrete_pairs] [pairs, xcords]
                    app_bif(bif_index, vec![lref(1), lref(0)]),
                ),
            ),
            annotation,
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // Collect sorted pairs and x-coordinates
        let pairs = self.collect_numbers(machine, view, args[0].clone())?;
        let xcords = self.collect_numbers(machine, view, args[1].clone())?;
        let n = xcords.len();

        // Union-Find with path compression
        let mut parent: Vec<usize> = (0..n).collect();

        fn find(parent: &mut [usize], mut x: usize) -> usize {
            while parent[x] != x {
                parent[x] = parent[parent[x]]; // path halving
                x = parent[x];
            }
            x
        }

        let mut components = n;
        let mut last_i = 0usize;
        let mut last_j = 0usize;

        for &encoded in &pairs {
            let key = encoded as i64;
            let i = ((key / 1000) % 1000) as usize;
            let j = (key % 1000) as usize;

            if i >= n || j >= n {
                continue;
            }

            let ri = find(&mut parent, i);
            let rj = find(&mut parent, j);

            if ri != rj {
                parent[ri] = rj;
                components -= 1;
                last_i = i;
                last_j = j;
                if components == 1 {
                    break;
                }
            }
        }

        let result = xcords[last_i] * xcords[last_j];
        let n = serde_json::Number::from_f64(result).unwrap_or_else(|| serde_json::Number::from(0));

        use crate::eval::memory::{
            alloc::ScopedAllocator,
            array::Array,
            syntax::{HeapSyn, LambdaForm, Native, StgBuilder},
        };

        // Return a boxed number
        let boxed = LambdaForm::value(
            view.alloc(HeapSyn::Cons {
                tag: DataConstructor::BoxedNumber.tag(),
                args: Array::from_slice(&view, &[Ref::V(Native::Num(n))]),
            })?
            .as_ptr(),
        );

        let syn = view
            .letrec(Array::from_slice(&view, &[boxed]), view.atom(Ref::L(0))?)?
            .as_ptr();
        machine.set_closure(crate::eval::machine::env::SynClosure::new(
            syn,
            machine.root_env(),
        ))
    }
}

impl CallGlobal2 for KruskalLast {}
