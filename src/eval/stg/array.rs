//! Array (tensor) intrinsic implementations
//!
//! Provides construction, access, query, and transformation operations
//! on n-dimensional arrays backed by ndarray.

use std::convert::TryInto;

use serde_json::Number;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{
            CallGlobal1, CallGlobal2, CallGlobal3, IntrinsicMachine, StgIntrinsic,
        },
        memory::{
            mutator::MutatorHeapView,
            ndarray::HeapNdArray,
            syntax::{Native, Ref},
        },
    },
};

use super::{
    force::SeqNumList,
    support::{
        machine_return_bool, machine_return_ndarray, machine_return_num, ndarray_arg, num_arg,
    },
    syntax::{
        dsl::{annotated_lambda, app_bif, data, force, let_, local, lref, unbox_num, value},
        LambdaForm,
    },
    tags::DataConstructor,
};

// ---------------------------------------------------------------------------
// Construction
// ---------------------------------------------------------------------------

/// ARRAY.ZEROS(shape_list) — create an array of zeros
pub struct ArrayZeros;

impl StgIntrinsic for ArrayZeros {
    fn name(&self) -> &str {
        "ARRAY.ZEROS"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        // Force SeqNumList on the shape arg before calling execute
        let bif_index: u8 = self.index().try_into().unwrap();
        annotated_lambda(
            1, // [shape]
            force(
                SeqNumList.global(lref(0)),
                // [concrete_shape] [shape]
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
        let shape = num_list_to_usize_vec(machine, view, &args[0])?;
        machine_return_ndarray(machine, view, HeapNdArray::zeros(&shape))
    }
}

impl CallGlobal1 for ArrayZeros {}

/// ARRAY.FILL(shape_list, value) — create an array filled with value
pub struct ArrayFill;

impl StgIntrinsic for ArrayFill {
    fn name(&self) -> &str {
        "ARRAY.FILL"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        // Force SeqNumList on shape arg, then unbox+force value arg.
        //
        // Env trace (force uses from_closure, unbox uses env_from_data_args):
        // lambda:              lref(0)=shape, lref(1)=val
        // force(SeqNumList(shape)): lref(0)=concrete_shape, lref(1)=shape, lref(2)=val
        // unbox_num(val=lref(2)):   lref(0)=inner_num, lref(1)=concrete_shape, lref(2)=shape, lref(3)=val
        // force(inner_num):         lref(0)=raw_num, lref(1)=inner_num, lref(2)=concrete_shape, ...
        //
        // execute args: (shape=lref(2), val=lref(0))
        let bif_index: u8 = self.index().try_into().unwrap();
        annotated_lambda(
            2, // [shape, val]
            force(
                SeqNumList.global(lref(0)),
                unbox_num(
                    local(2),
                    force(local(0), app_bif(bif_index, vec![lref(2), lref(0)])),
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
        let shape = num_list_to_usize_vec(machine, view, &args[0])?;
        let value = num_arg(machine, view, &args[1])?.as_f64().unwrap_or(0.0);
        machine_return_ndarray(machine, view, HeapNdArray::filled(&shape, value))
    }
}

impl CallGlobal2 for ArrayFill {}

/// ARRAY.FROM_FLAT(shape_list, values_list) — create from flat data
pub struct ArrayFromFlat;

impl StgIntrinsic for ArrayFromFlat {
    fn name(&self) -> &str {
        "ARRAY.FROM_FLAT"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        // Force SeqNumList on both shape and values args.
        // After first force: lref(0)=concrete_shape, lref(1)=shape, lref(2)=vals
        // After second force: lref(0)=concrete_vals, lref(1)=concrete_shape, lref(2)=shape, lref(3)=vals
        let bif_index: u8 = self.index().try_into().unwrap();
        annotated_lambda(
            2, // [shape, vals]
            force(
                SeqNumList.global(lref(0)),
                force(
                    SeqNumList.global(lref(2)),
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
        let shape = num_list_to_usize_vec(machine, view, &args[0])?;
        let values = num_list_to_f64_vec(machine, view, &args[1])?;
        match HeapNdArray::from_flat(&shape, values) {
            Some(arr) => machine_return_ndarray(machine, view, arr),
            None => Err(ExecutionError::Panic(
                "array shape does not match data length".to_string(),
            )),
        }
    }
}

impl CallGlobal2 for ArrayFromFlat {}

// ---------------------------------------------------------------------------
// Access and query
// ---------------------------------------------------------------------------

/// ARRAY.GET(coords_list, array) — get element at coordinates
///
/// Arg order is (coords, array) so pipeline style works:
/// `my_array arr.get([0, 1])` = `ARRAY.GET([0, 1], my_array)`
pub struct ArrayGet;

impl StgIntrinsic for ArrayGet {
    fn name(&self) -> &str {
        "ARRAY.GET"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        // SeqNumList on coords (lref(0)), force array (lref(1)).
        // After SeqNumList force: lref(0)=concrete_coords, lref(1)=coords, lref(2)=array
        // After force(array):     lref(0)=forced_array, lref(1)=concrete_coords, lref(2)=coords, lref(3)=array
        //
        // execute args: (coords=lref(1), array=lref(0))
        // Result is boxed as BoxedNumber to match the default wrapper convention.
        let bif_index: u8 = self.index().try_into().unwrap();
        annotated_lambda(
            2, // [coords, array]
            force(
                SeqNumList.global(lref(0)),
                force(
                    local(2),
                    let_(
                        vec![value(app_bif(bif_index, vec![lref(1), lref(0)]))],
                        data(DataConstructor::BoxedNumber.tag(), vec![lref(0)]),
                    ),
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
        let coords = num_list_to_usize_vec(machine, view, &args[0])?;
        let arr = ndarray_arg(machine, view, &args[1])?;
        match arr.get(&coords) {
            Some(val) => machine_return_num(
                machine,
                view,
                Number::from_f64(val).unwrap_or_else(|| Number::from(0)),
            ),
            None => Err(ExecutionError::Panic(
                "array index out of bounds".to_string(),
            )),
        }
    }
}

impl CallGlobal2 for ArrayGet {}

/// ARRAY.SET(array, coords_list, value) — return new array with element set
pub struct ArraySet;

impl StgIntrinsic for ArraySet {
    fn name(&self) -> &str {
        "ARRAY.SET"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        // Arg order: (coords, val, array) for pipeline use.
        // SeqNumList on coords (lref(0)), unbox+force val (lref(1)), force array (lref(2)).
        //
        // Env trace:
        // lambda:                    lref(0)=coords, lref(1)=val, lref(2)=array
        // force(SeqNumList(coords)): lref(0)=concrete_coords, lref(1)=coords, lref(2)=val, lref(3)=array
        // unbox_num(val=lref(2)):    lref(0)=inner_num, lref(1)=concrete_coords, lref(2)=coords, lref(3)=val, lref(4)=array
        // force(inner_num):          lref(0)=raw_num, lref(1)=inner_num, lref(2)=concrete_coords, lref(3)=coords, lref(4)=val, lref(5)=array
        // force(array=lref(5)):      lref(0)=forced_array, lref(1)=raw_num, lref(2)=inner_num, lref(3)=concrete_coords, ...
        //
        // execute args: (coords=lref(3), val=lref(1), array=lref(0))
        let bif_index: u8 = self.index().try_into().unwrap();
        annotated_lambda(
            3, // [coords, val, array]
            force(
                SeqNumList.global(lref(0)),
                unbox_num(
                    local(2),
                    force(
                        local(0),
                        force(
                            local(5),
                            app_bif(bif_index, vec![lref(3), lref(1), lref(0)]),
                        ),
                    ),
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
        let coords = num_list_to_usize_vec(machine, view, &args[0])?;
        let value = num_arg(machine, view, &args[1])?.as_f64().unwrap_or(0.0);
        let arr = ndarray_arg(machine, view, &args[2])?;
        match arr.with_set(&coords, value) {
            Some(new_arr) => machine_return_ndarray(machine, view, new_arr),
            None => Err(ExecutionError::Panic(
                "array index out of bounds".to_string(),
            )),
        }
    }
}

impl CallGlobal3 for ArraySet {}

/// ARRAY.SHAPE(array) — return shape as a list of numbers
pub struct ArrayShape;

impl StgIntrinsic for ArrayShape {
    fn name(&self) -> &str {
        "ARRAY.SHAPE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let arr = ndarray_arg(machine, view, &args[0])?;
        let shape = arr.shape();
        usize_slice_to_list(machine, view, shape)
    }
}

impl CallGlobal1 for ArrayShape {}

/// ARRAY.RANK(array) — return number of dimensions
pub struct ArrayRank;

impl StgIntrinsic for ArrayRank {
    fn name(&self) -> &str {
        "ARRAY.RANK"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let arr = ndarray_arg(machine, view, &args[0])?;
        machine_return_num(machine, view, Number::from(arr.rank() as u64))
    }
}

impl CallGlobal1 for ArrayRank {}

/// ARRAY.LENGTH(array) — return total number of elements
pub struct ArrayLength;

impl StgIntrinsic for ArrayLength {
    fn name(&self) -> &str {
        "ARRAY.LENGTH"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let arr = ndarray_arg(machine, view, &args[0])?;
        machine_return_num(machine, view, Number::from(arr.len() as u64))
    }
}

impl CallGlobal1 for ArrayLength {}

/// ARRAY.TO_LIST(array) — return flat list of numbers
pub struct ArrayToList;

impl StgIntrinsic for ArrayToList {
    fn name(&self) -> &str {
        "ARRAY.TO_LIST"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let arr = ndarray_arg(machine, view, &args[0])?;
        let values = arr.to_flat_vec();
        f64_vec_to_list(machine, view, &values)
    }
}

impl CallGlobal1 for ArrayToList {}

/// ARRAY.ISARRAY(x) — check if argument is an array
pub struct ArrayIsArray;

impl StgIntrinsic for ArrayIsArray {
    fn name(&self) -> &str {
        "ARRAY.ISARRAY"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // resolve_native returns Err for non-natives (e.g. lists, blocks).
        // In that case, the value is definitely not an array.
        let is_array = machine
            .nav(view)
            .resolve_native(&args[0])
            .map(|n| matches!(n, Native::NdArray(_)))
            .unwrap_or(false);
        machine_return_bool(machine, view, is_array)
    }
}

impl CallGlobal1 for ArrayIsArray {}

// ---------------------------------------------------------------------------
// Transformations
// ---------------------------------------------------------------------------

/// ARRAY.TRANSPOSE(array) — reverse all axes
pub struct ArrayTranspose;

impl StgIntrinsic for ArrayTranspose {
    fn name(&self) -> &str {
        "ARRAY.TRANSPOSE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let arr = ndarray_arg(machine, view, &args[0])?;
        machine_return_ndarray(machine, view, arr.transpose())
    }
}

impl CallGlobal1 for ArrayTranspose {}

/// ARRAY.RESHAPE(array, new_shape) — reshape to new dimensions
pub struct ArrayReshape;

impl StgIntrinsic for ArrayReshape {
    fn name(&self) -> &str {
        "ARRAY.RESHAPE"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        // Arg order: (new_shape, array) for pipeline use.
        // SeqNumList on new_shape (lref(0)), force array (lref(1)).
        //
        // Env trace:
        // lambda:                       lref(0)=new_shape, lref(1)=array
        // force(SeqNumList(new_shape)): lref(0)=concrete_shape, lref(1)=new_shape, lref(2)=array
        // force(array=lref(2)):         lref(0)=forced_array, lref(1)=concrete_shape, lref(2)=new_shape, lref(3)=array
        //
        // execute args: (new_shape=lref(1), array=lref(0))
        let bif_index: u8 = self.index().try_into().unwrap();
        annotated_lambda(
            2, // [new_shape, array]
            force(
                SeqNumList.global(lref(0)),
                force(local(2), app_bif(bif_index, vec![lref(1), lref(0)])),
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
        let new_shape = num_list_to_usize_vec(machine, view, &args[0])?;
        let arr = ndarray_arg(machine, view, &args[1])?;
        match arr.reshape(&new_shape) {
            Some(reshaped) => machine_return_ndarray(machine, view, reshaped),
            None => Err(ExecutionError::Panic(
                "reshape: new shape does not match total element count".to_string(),
            )),
        }
    }
}

impl CallGlobal2 for ArrayReshape {}

/// ARRAY.SLICE(array, axis, index) — slice along axis, reducing rank
pub struct ArraySlice;

impl StgIntrinsic for ArraySlice {
    fn name(&self) -> &str {
        "ARRAY.SLICE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let arr = ndarray_arg(machine, view, &args[0])?;
        let axis = num_arg(machine, view, &args[1])?.as_f64().unwrap_or(0.0) as usize;
        let index = num_arg(machine, view, &args[2])?.as_f64().unwrap_or(0.0) as usize;
        match arr.slice_along(axis, index) {
            Some(sliced) => machine_return_ndarray(machine, view, sliced),
            None => Err(ExecutionError::Panic(
                "slice: axis or index out of bounds".to_string(),
            )),
        }
    }
}

impl CallGlobal3 for ArraySlice {}

/// ARRAY.ADD(a, b) — element-wise addition (array+array or array+scalar)
pub struct ArrayAdd;

impl StgIntrinsic for ArrayAdd {
    fn name(&self) -> &str {
        "ARRAY.ADD"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        array_binop(machine, view, &args[0], &args[1], |a, b| a + b)
    }
}

impl CallGlobal2 for ArrayAdd {}

/// ARRAY.SUB(a, b) — element-wise subtraction
pub struct ArraySub;

impl StgIntrinsic for ArraySub {
    fn name(&self) -> &str {
        "ARRAY.SUB"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        array_binop(machine, view, &args[0], &args[1], |a, b| a - b)
    }
}

impl CallGlobal2 for ArraySub {}

/// ARRAY.MUL(a, b) — element-wise multiplication
pub struct ArrayMul;

impl StgIntrinsic for ArrayMul {
    fn name(&self) -> &str {
        "ARRAY.MUL"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        array_binop(machine, view, &args[0], &args[1], |a, b| a * b)
    }
}

impl CallGlobal2 for ArrayMul {}

/// ARRAY.DIV(a, b) — element-wise division
pub struct ArrayDiv;

impl StgIntrinsic for ArrayDiv {
    fn name(&self) -> &str {
        "ARRAY.DIV"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        array_binop(machine, view, &args[0], &args[1], |a, b| a / b)
    }
}

impl CallGlobal2 for ArrayDiv {}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Extract a list of numbers from a cons-list, converting to usize
fn num_list_to_usize_vec(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<Vec<usize>, ExecutionError> {
    let nums = super::support::collect_num_list(machine, view, arg.clone())?;
    Ok(nums.into_iter().map(|n| n as usize).collect())
}

/// Extract a list of numbers as f64 from a cons-list
fn num_list_to_f64_vec(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<Vec<f64>, ExecutionError> {
    super::support::collect_num_list(machine, view, arg.clone())
}

/// Build a cons-list of numbers from a slice of usize
fn usize_slice_to_list(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    values: &[usize],
) -> Result<(), ExecutionError> {
    let nums: Vec<f64> = values.iter().map(|&n| n as f64).collect();
    super::support::machine_return_num_list(machine, view, nums)
}

/// Build a cons-list of numbers from a Vec of f64
fn f64_vec_to_list(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    values: &[f64],
) -> Result<(), ExecutionError> {
    super::support::machine_return_num_list(machine, view, values.to_vec())
}

/// Dispatch a binary operation on arrays, supporting array+array and array+scalar
fn array_binop<F: Fn(f64, f64) -> f64>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    a_ref: &Ref,
    b_ref: &Ref,
    f: F,
) -> Result<(), ExecutionError> {
    let a_native = machine.nav(view).resolve_native(a_ref)?;
    let b_native = machine.nav(view).resolve_native(b_ref)?;

    match (a_native, b_native) {
        (Native::NdArray(a_ptr), Native::NdArray(b_ptr)) => {
            let a = view.scoped(a_ptr);
            let b = view.scoped(b_ptr);
            match a.zip_with(&b, f) {
                Some(result) => machine_return_ndarray(machine, view, result),
                None => Err(ExecutionError::Panic(
                    "array shape mismatch in element-wise operation".to_string(),
                )),
            }
        }
        (Native::NdArray(a_ptr), Native::Num(n)) => {
            let a = view.scoped(a_ptr);
            let scalar = n.as_f64().unwrap_or(0.0);
            machine_return_ndarray(machine, view, a.scalar_op(scalar, f))
        }
        (Native::Num(n), Native::NdArray(b_ptr)) => {
            let b = view.scoped(b_ptr);
            let scalar = n.as_f64().unwrap_or(0.0);
            machine_return_ndarray(machine, view, b.scalar_op(scalar, |x, s| f(s, x)))
        }
        _ => Err(ExecutionError::Panic(
            "array arithmetic requires at least one array operand".to_string(),
        )),
    }
}
