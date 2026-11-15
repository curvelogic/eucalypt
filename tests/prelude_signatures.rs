//! Gate: no prelude type signature may contain a union with an arrow member
//! unless it is an allow-listed overload (eu-oaz72.137).
//!
//! In type strings `|` binds looser than `→`, so `a → b | c` means
//! `(a → b) | c`, not `a → (b | c)`. A union-of-arrows signature is almost
//! always a missing pair of parentheses. Intentional overload signatures
//! are listed in `OVERLOADS` verbatim.

use eucalypt::core::typecheck::parse::parse_scheme;
use eucalypt::core::typecheck::types::Type;
use std::fs;

/// Signatures that deliberately express alternative arrow types.
const OVERLOADS: &[&str] = &[
    "number → number → number | array → array → array",
    "number → number → bool | string → string → bool | symbol → symbol → bool | datetime → datetime → bool",
    "[a] → number → a | array → [number] → number",
];

fn union_of_arrows(t: &Type) -> bool {
    match t {
        Type::Union(vs) => {
            vs.iter().any(|v| matches!(v, Type::Function(..))) || vs.iter().any(union_of_arrows)
        }
        Type::Function(a, b) | Type::App(a, b) => union_of_arrows(a) || union_of_arrows(b),
        Type::Tuple(ts) => ts.iter().any(union_of_arrows),
        Type::PrefixList { prefix, tail } => {
            prefix.iter().any(union_of_arrows) || union_of_arrows(tail)
        }
        Type::Mu(_, b) | Type::Forall(_, b) => union_of_arrows(b),
        _ => false,
    }
}

fn signatures(src: &str) -> Vec<String> {
    let mut out = vec![];
    for marker in ["type: s\"", "type: \""] {
        let mut rest = src;
        while let Some(i) = rest.find(marker) {
            rest = &rest[i + marker.len()..];
            if let Some(j) = rest.find('"') {
                out.push(rest[..j].to_string());
                rest = &rest[j..];
            }
        }
    }
    out
}

#[test]
fn prelude_signatures_have_no_unparenthesised_union_of_arrows() {
    let mut bad = vec![];
    let mut checked = 0;
    for entry in fs::read_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/lib")).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().is_none_or(|e| e != "eu") {
            continue;
        }
        let src = fs::read_to_string(&path).unwrap();
        for sig in signatures(&src) {
            let Ok((ty, _)) = parse_scheme(&sig) else {
                continue;
            };
            checked += 1;
            if union_of_arrows(&ty) && !OVERLOADS.contains(&sig.as_str()) {
                bad.push(format!("{}: {sig}", path.display()));
            }
        }
    }
    assert!(checked > 300, "only {checked} signatures checked");
    assert!(
        bad.is_empty(),
        "union of arrows (missing parentheses after →?):\n{}",
        bad.join("\n")
    );
}
