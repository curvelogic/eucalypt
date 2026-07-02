//! Intrinsics for type-data (`s"..."`) value semantics.
//!
//! Provides:
//! - `TYPE_TO_DATA` — project a `BoxedTypeData` value to a `t-*` tagged-list structure
//! - `TYPE_FROM_STRING` — wrap a canonical type-DSL string as `BoxedTypeData`
//!
//! Together these power the `to-data` / `from-data` round-trip in the prelude.

use crate::eval::memory::syntax::{Native, StgBuilder};
use crate::{
    common::sourcemap::Smid,
    core::{
        expr::{self, RcExpr},
        typecheck::{parse::parse_type, types::Type},
    },
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal1, IntrinsicMachine, StgIntrinsic},
        memory::{mutator::MutatorHeapView, syntax::Ref},
        stg::{
            data_literal,
            support::{resolve_native_unboxing, str_arg},
            tags::DataConstructor,
        },
    },
};

/// Convert a `Type` to a `t-*` tagged-list core expression.
///
/// The mapping is documented in the SV1 spec (eu-3a9w).  Primitives map to
/// `[:t-prim :name]`; composite types carry their sub-types recursively.
fn type_to_rcexpr(ty: &Type) -> RcExpr {
    let s = Smid::default();

    let sym = |name: &str| expr::core::sym(s, name);

    match ty {
        Type::Number => expr::core::list(s, vec![sym("t-prim"), sym("number")]),
        Type::String => expr::core::list(s, vec![sym("t-prim"), sym("string")]),
        Type::Symbol => expr::core::list(s, vec![sym("t-prim"), sym("symbol")]),
        Type::Bool => expr::core::list(s, vec![sym("t-prim"), sym("bool")]),
        Type::Null => expr::core::list(s, vec![sym("t-prim"), sym("null")]),
        Type::DateTime => expr::core::list(s, vec![sym("t-prim"), sym("datetime")]),
        Type::Any => expr::core::list(s, vec![sym("t-prim"), sym("any")]),
        Type::Top => expr::core::list(s, vec![sym("t-prim"), sym("top")]),
        Type::Never => expr::core::list(s, vec![sym("t-prim"), sym("never")]),
        Type::ExecutionError => expr::core::list(s, vec![sym("t-prim"), sym("error")]),
        Type::Set => expr::core::list(s, vec![sym("t-prim"), sym("set")]),
        Type::Vec => expr::core::list(s, vec![sym("t-prim"), sym("vec")]),
        Type::Array => expr::core::list(s, vec![sym("t-prim"), sym("array")]),

        Type::Con(name) => expr::core::list(s, vec![sym("t-con"), sym(name)]),

        Type::App(f, x) => {
            // Re-sugar `App(Con("List"), T)` → `[:t-list T']`
            if let Type::Con(name) = f.as_ref() {
                if name == "List" {
                    return expr::core::list(s, vec![sym("t-list"), type_to_rcexpr(x)]);
                }
            }
            expr::core::list(s, vec![sym("t-app"), type_to_rcexpr(f), type_to_rcexpr(x)])
        }

        Type::Tuple(ts) => {
            let mut elems = vec![sym("t-tuple")];
            elems.extend(ts.iter().map(type_to_rcexpr));
            expr::core::list(s, elems)
        }

        Type::Record {
            fields,
            open: _,
            rows: _,
        } => {
            let field_exprs: Vec<(String, RcExpr)> = fields
                .iter()
                .map(|(k, fp)| {
                    let presence_sym = if fp.is_optional() {
                        "optional"
                    } else {
                        "required"
                    };
                    let inner = type_to_rcexpr(fp.ty());
                    let wrapper =
                        expr::core::list(s, vec![sym("t-field"), sym(presence_sym), inner]);
                    (k.clone(), wrapper)
                })
                .collect();
            let block_expr = expr::core::block(s, field_exprs);
            expr::core::list(s, vec![sym("t-record"), block_expr])
        }

        Type::Function(a, b) => {
            expr::core::list(s, vec![sym("t-fn"), type_to_rcexpr(a), type_to_rcexpr(b)])
        }

        Type::Union(ts) => {
            // Re-sugar `Union([T, ExecutionError])` or `Union([ExecutionError, T])`
            // to `[:t-partial T']` — matching the `T?` display sugar.
            if ts.len() == 2 && ts[1] == Type::ExecutionError {
                return expr::core::list(s, vec![sym("t-partial"), type_to_rcexpr(&ts[0])]);
            }
            if ts.len() == 2 && ts[0] == Type::ExecutionError {
                return expr::core::list(s, vec![sym("t-partial"), type_to_rcexpr(&ts[1])]);
            }
            let mut elems = vec![sym("t-union")];
            elems.extend(ts.iter().map(type_to_rcexpr));
            expr::core::list(s, elems)
        }

        Type::LiteralSymbol(name) => expr::core::list(s, vec![sym("t-lit-sym"), sym(name)]),

        Type::LiteralString(val) => {
            expr::core::list(s, vec![sym("t-lit-str"), expr::core::str(s, val)])
        }

        Type::Var(id, _kind) => expr::core::list(s, vec![sym("t-var"), sym(&id.0)]),

        Type::Mu(id, body) => {
            // Display for Mu only shows the variable name (opaque alias).
            // We store the body for round-trip fidelity.
            expr::core::list(s, vec![sym("t-mu"), sym(&id.0), type_to_rcexpr(body)])
        }

        Type::Forall(vars, body) => {
            if vars.len() == 1 {
                expr::core::list(
                    s,
                    vec![sym("t-forall"), sym(&vars[0].0 .0), type_to_rcexpr(body)],
                )
            } else {
                let var_list = expr::core::list(s, vars.iter().map(|(id, _)| sym(&id.0)).collect());
                expr::core::list(s, vec![sym("t-forall"), var_list, type_to_rcexpr(body)])
            }
        }

        Type::Lam(id, body) => {
            expr::core::list(s, vec![sym("t-lam"), sym(&id.0), type_to_rcexpr(body)])
        }
    }
}

// ── TYPE_TO_DATA ─────────────────────────────────────────────────────────────

/// `TYPE_TO_DATA(type_data_val)` — project a `BoxedTypeData` to a `t-*` tagged list.
///
/// 1. Extracts the canonical type-DSL string from the `BoxedTypeData` wrapper.
/// 2. Parses it via `parse_type`.
/// 3. Converts the resulting `Type` to the `t-*` vocabulary using `type_to_rcexpr`.
/// 4. Builds the resulting core expression directly as a value (BV1 §5.5,
///    `data_literal::build_value`) — `type_to_rcexpr` emits only data
///    constructors (literals/lists/blocks), never function application, so
///    no STG compilation or runtime code loading is needed.
pub struct TypeToData;

impl StgIntrinsic for TypeToData {
    fn name(&self) -> &str {
        "TYPE_TO_DATA"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let smid = machine.annotation();

        // Extract the string from the BoxedTypeData wrapper.
        let native = resolve_native_unboxing(machine, view, &args[0])?;
        let type_str = match native {
            Native::Str(ptr) => (*view.scoped(ptr)).as_str().to_string(),
            other => {
                return Err(ExecutionError::Panic(
                    smid,
                    format!("TYPE_TO_DATA: expected type-data value, got {other:?}"),
                ));
            }
        };

        // Parse — always valid since the desugarer validates at compile time.
        let ty = parse_type(&type_str)
            .map_err(|e| ExecutionError::Panic(smid, format!("TYPE_TO_DATA: parse error: {e}")))?;

        // Convert to t-* core expression and build it directly as a value.
        let core_expr = type_to_rcexpr(&ty);
        let value = data_literal::build_value(machine, view, &core_expr)?;
        machine.set_result(value)
    }
}

impl CallGlobal1 for TypeToData {}

// ── TYPE_FROM_STRING ──────────────────────────────────────────────────────────

/// `TYPE_FROM_STRING(str)` — wrap a type-DSL string as a validated `BoxedTypeData`.
///
/// 1. Validates the input string via `parse_type`.
/// 2. Normalises to canonical form via the `Type` `Display` impl.
/// 3. Returns a `BoxedTypeData` (tag 17) wrapping the canonical string, built
///    directly via the neutral data-construction ABI (BV1 §5.5).
///
/// Used by the prelude `from-data` function as the final wrapping step.
pub struct TypeFromString;

impl StgIntrinsic for TypeFromString {
    fn name(&self) -> &str {
        "TYPE_FROM_STRING"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let smid = machine.annotation();
        let input = str_arg(machine, view, &args[0])?;

        // Validate and canonicalise.
        let ty = parse_type(&input).map_err(|e| {
            ExecutionError::Panic(smid, format!("from-data: invalid type string: {e}"))
        })?;
        let canonical = format!("{ty}");

        // Build BoxedTypeData as a value via the neutral ABI.
        let Ref::V(native) = view.str_ref(canonical)? else {
            unreachable!("str_ref yields a value ref")
        };
        let boxed = machine.native_value(view, native)?;
        let value = machine.data_value(view, DataConstructor::BoxedTypeData.tag(), &[boxed])?;
        machine.set_result(value)
    }
}

impl CallGlobal1 for TypeFromString {}
