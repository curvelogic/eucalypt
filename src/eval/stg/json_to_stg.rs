//! Convert serde_json::Value to STG syntax
//!
//! Used by stream producers to convert JSON values into STG syntax
//! that can be loaded directly into the VM heap.

use std::rc::Rc;

use serde_json::Value;

use super::syntax::{
    dsl::{self, block, cons, letrec_, local, lref, nil, pair, unit, value},
    LambdaForm, StgSyn,
};

/// Convert a serde_json Value to STG syntax.
pub fn json_to_stg(val: &Value) -> Rc<StgSyn> {
    match val {
        Value::Null => unit(),
        Value::Bool(b) => dsl::bool_(*b),
        Value::Number(n) => dsl::box_num(n.clone()),
        Value::String(s) => dsl::box_str(s.as_str()),
        Value::Array(items) => json_array_to_stg(items),
        Value::Object(map) => json_object_to_stg(map),
    }
}

/// Convert a JSON array to an STG list.
fn json_array_to_stg(items: &[Value]) -> Rc<StgSyn> {
    if items.is_empty() {
        return nil();
    }

    // Build as let-bindings: [nil, item_n, cons_n, ..., item_0, cons_0]
    let mut bindings: Vec<LambdaForm> = vec![value(nil())];
    for item in items.iter().rev() {
        let item_stg = json_to_stg(item);
        bindings.push(value(item_stg));
        let len = bindings.len();
        bindings.push(value(cons(lref(len - 1), lref(len - 2))));
    }
    let list_idx = bindings.len() - 1;
    letrec_(bindings, local(list_idx))
}

/// Convert a JSON object to an STG block.
fn json_object_to_stg(map: &serde_json::Map<String, Value>) -> Rc<StgSyn> {
    if map.is_empty() {
        // Empty block: BLOCK(NIL, no-index)
        let bindings: Vec<LambdaForm> = vec![value(nil()), value(block(lref(0)))];
        return letrec_(bindings, local(1));
    }

    // Build pair list then wrap in BLOCK
    let mut bindings: Vec<LambdaForm> = vec![value(nil())];
    for (key, val) in map.iter().rev() {
        let val_stg = json_to_stg(val);
        bindings.push(value(val_stg));
        let val_idx = bindings.len() - 1;
        bindings.push(value(pair(key.as_str(), lref(val_idx))));
        let pair_idx = bindings.len() - 1;
        let prev_list = pair_idx - 2;
        bindings.push(value(cons(lref(pair_idx), lref(prev_list))));
    }
    let list_idx = bindings.len() - 1;
    bindings.push(value(block(lref(list_idx))));
    let block_idx = bindings.len() - 1;
    letrec_(bindings, local(block_idx))
}
