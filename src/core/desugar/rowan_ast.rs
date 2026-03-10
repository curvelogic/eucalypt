//! Desugarable implementations for Rowan AST elements
//!
//! This mirrors the logic in ast.rs but uses Rowan's method-based API
//! instead of direct pattern matching on data structures.

use super::{
    desugarable::Desugarable, desugarer::Desugarer, rowan_disembed::core_from_rowan_embedding,
};
use crate::core::metadata::{
    normalise_metadata, strip_desugar_phase_metadata, DesugarPhaseBlockMetadata,
    DesugarPhaseDeclarationMetadata, ReadMetadata,
};
use crate::{
    common::sourcemap::{HasSmid, Smid},
    core::{
        anaphora::{BLOCK_ANAPHORA, EXPR_ANAPHORA},
        error::CoreError,
        expr::*,
        rt,
        transform::dynamise,
    },
    syntax::rowan::{
        ast::{self as rowan_ast, AstToken, Element, HasSoup},
        lex,
    },
};
use codespan::{ByteIndex, Span};
use moniker::{Binder, Embed, Rec, Scope};
use rowan::{ast::AstNode, TextRange};

/// Convert a TextRange to a Span
fn text_range_to_span(range: TextRange) -> Span {
    let start = ByteIndex(range.start().into());
    let end = ByteIndex(range.end().into());
    Span::new(start, end)
}

/// Return type of `desugar_declaration_body_with_patterns`:
/// `(body_expr, lambda_param_names, lambda_param_vars)`
type PatternBodyResult = (RcExpr, Vec<String>, Vec<moniker::FreeVar<String>>);

/// Entry for a single field binding in a destructuring let.
struct FieldBinding {
    /// Name of the field in the source block (for Lookup)
    field_name: String,
    /// FreeVar for the binding name in the body
    binding_var: moniker::FreeVar<String>,
}

/// Entry for a destructuring block let: synthetic param + field bindings.
struct DestructureEntry {
    /// Synthetic parameter name (e.g. `__p0`)
    synthetic_name: String,
    /// Field bindings to generate
    fields: Vec<FieldBinding>,
}

/// Entry for a single list element binding.
struct ListElementBinding {
    /// Zero-based index of the element
    index: usize,
    /// FreeVar for the binding name in the body
    binding_var: moniker::FreeVar<String>,
}

/// Entry for the tail binding in a head/tail list pattern.
struct ListTailBinding {
    /// Number of head elements to skip (i.e., the drop count)
    drop_count: usize,
    /// FreeVar for the tail binding name in the body
    binding_var: moniker::FreeVar<String>,
}

/// Entry for a destructuring list let: synthetic param + element bindings.
struct ListDestructureEntry {
    /// Synthetic parameter name (e.g. `__p0`)
    synthetic_name: String,
    /// Element bindings at each position
    elements: Vec<ListElementBinding>,
    /// Optional tail binding for head/tail patterns (`[x : xs]`)
    tail: Option<ListTailBinding>,
}

/// A parsed parameter pattern in a function declaration
enum ParamPattern {
    /// Simple identifier: `x`
    Simple(String),
    /// Block destructuring: `{x y}` or `{x: a  y: b}`.
    ///
    /// Each entry is `(field_name, binding_name)`. For shorthand `{x}`,
    /// field and binding are both `"x"`. For rename `{x: a}`,
    /// field is `"x"` and binding is `"a"`.
    Block(Vec<(String, String)>),
    /// Fixed-length list destructuring: `[a, b, c]`, or head/tail pattern
    /// `[x : xs]` or `[a, b : rest]`.
    ///
    /// Contains the binding names for each head element, plus an optional
    /// tail binding name. The tail is `None` for fixed-length patterns.
    List(Vec<String>, Option<String>),
}

/// Parse a block parameter pattern `{x y}` or `{x: a  y: b}` from a
/// Rowan `Block` AST node in a function parameter position.
///
/// Block patterns may contain:
/// - Shorthand names in block metadata: `{x y}` → `[(x, x), (y, y)]`
/// - Rename declarations: `{x: a  y: b}` → `[(x, a), (y, b)]`
/// - Mixed: `{x  y: b}` → `[(x, x), (y, b)]`
fn parse_block_pattern(block: &rowan_ast::Block) -> Result<Vec<(String, String)>, CoreError> {
    let mut fields: Vec<(String, String)> = Vec::new();

    // Extract shorthand names from block metadata soup (e.g. `x y` in `{x y}`)
    if let Some(meta) = block.meta() {
        if let Some(soup) = meta.soup() {
            for element in soup.elements() {
                if let rowan_ast::Element::Name(name) = element {
                    if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) = name.identifier()
                    {
                        let field_name = normal.text().to_string();
                        // Shorthand: field name = binding name
                        fields.push((field_name.clone(), field_name));
                    }
                }
            }
        }
    }

    // Extract rename fields from declarations (e.g. `x: a  y: b`)
    for decl in block.declarations() {
        if let Some(head) = decl.head() {
            let kind = head.classify_declaration();
            if let rowan_ast::DeclarationKind::Property(prop) = kind {
                let field_name = prop.text().to_string();
                // Check if there's a body (rename) or not
                let binding_name = if let Some(body) = decl.body() {
                    if let Some(body_soup) = body.soup() {
                        // Body should be a single normal identifier (the binding name)
                        if let Some(rowan_ast::Element::Name(name)) = body_soup.singleton() {
                            if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) =
                                name.identifier()
                            {
                                normal.text().to_string()
                            } else {
                                // Not a normal identifier — use field name as fallback
                                field_name.clone()
                            }
                        } else {
                            // Complex expression in body — use field name as fallback
                            field_name.clone()
                        }
                    } else {
                        field_name.clone()
                    }
                } else {
                    // No body in declaration — field name = binding name (shorthand)
                    field_name.clone()
                };
                fields.push((field_name, binding_name));
            }
        }
    }

    Ok(fields)
}

/// Parse a list parameter pattern from a Rowan `List` AST node in a
/// function parameter position.
///
/// Handles both fixed-length patterns `[a, b, c]` and head/tail
/// patterns `[x : xs]` and `[a, b : rest]`.
///
/// Returns `(head_elements, tail)` where `tail` is `None` for
/// fixed-length patterns and `Some(tail_name)` for head/tail patterns.
fn parse_list_pattern(list: &rowan_ast::List) -> Option<(Vec<String>, Option<String>)> {
    let has_colon = list.has_colon();
    let all_items: Vec<_> = list.items().collect();

    if has_colon {
        // Head/tail pattern: items before the colon are heads,
        // the last item after the colon is the tail.
        // The List node's items() returns all Soup children in order:
        // for [a, b : rest], items are: a, b, rest (colon is a token, not a child node).
        // We collect all items; the last is the tail, the rest are heads.
        if all_items.len() < 2 {
            return None; // Need at least one head and one tail
        }
        let mut heads = Vec::new();
        for item in &all_items[..all_items.len() - 1] {
            if let Some(rowan_ast::Element::Name(name)) = item.singleton() {
                if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) = name.identifier() {
                    heads.push(normal.text().to_string());
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
        // Last item is the tail binding
        let tail_item = all_items.last().unwrap();
        if let Some(rowan_ast::Element::Name(name)) = tail_item.singleton() {
            if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) = name.identifier() {
                let tail = normal.text().to_string();
                Some((heads, Some(tail)))
            } else {
                None
            }
        } else {
            None
        }
    } else {
        // Fixed-length pattern: all items are positional bindings
        let mut elements = Vec::new();
        for item in &all_items {
            if let Some(rowan_ast::Element::Name(name)) = item.singleton() {
                if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) = name.identifier() {
                    elements.push(normal.text().to_string());
                } else {
                    return None; // Not a normal identifier — invalid pattern
                }
            } else {
                return None; // Not a single name — invalid pattern
            }
        }
        if elements.is_empty() {
            None // Empty list pattern not valid
        } else {
            Some((elements, None))
        }
    }
}

/// Parse a function parameter soup into a `ParamPattern`.
///
/// Returns `None` if the soup is not a valid single-element parameter.
fn parse_param_pattern(soup: &rowan_ast::Soup) -> Option<ParamPattern> {
    match soup.singleton() {
        Some(rowan_ast::Element::Name(name)) => {
            if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) = name.identifier() {
                Some(ParamPattern::Simple(normal.text().to_string()))
            } else {
                None
            }
        }
        Some(rowan_ast::Element::Block(block)) => {
            parse_block_pattern(&block).ok().map(ParamPattern::Block)
        }
        Some(rowan_ast::Element::List(list)) => {
            parse_list_pattern(&list).map(|(heads, tail)| ParamPattern::List(heads, tail))
        }
        _ => None,
    }
}

/// Ordered record of a destructuring let to emit after the body is desugared.
enum DestructureLet {
    /// Block destructuring: lookup each field by name.
    Block(DestructureEntry),
    /// List destructuring: index each element with LIST.NTH.
    List(ListDestructureEntry),
}

impl DestructureLet {
    fn synthetic_name(&self) -> &str {
        match self {
            DestructureLet::Block(e) => &e.synthetic_name,
            DestructureLet::List(e) => &e.synthetic_name,
        }
    }
}

/// Desugar a function body with support for destructuring parameter patterns.
///
/// For simple parameter patterns, this behaves like `desugar_declaration_body`.
/// For block patterns, a synthetic parameter (`__pN`) is introduced as the
/// lambda binder, and let bindings for each field are added around the body.
/// For list patterns, the same synthetic-parameter approach is used, with
/// `LIST.NTH` calls for each positional element.
///
/// Returns `(body_expr, lambda_param_names, lambda_param_vars)`.
fn desugar_declaration_body_with_patterns(
    decl: &rowan_ast::Declaration,
    desugarer: &mut Desugarer,
    patterns: &[ParamPattern],
    span: Span,
) -> Result<PatternBodyResult, CoreError> {
    // Build the complete list of names to push into the environment,
    // tracking which ones are lambda binders (synthetic params) and
    // which are only binding names for let bindings.
    let mut all_env_names: Vec<String> = Vec::new();
    let mut lambda_param_names: Vec<String> = Vec::new();
    // Raw data for block patterns: (synthetic_name, [(field_name, binding_name)])
    let mut block_raw: Vec<(String, Vec<(String, String)>)> = Vec::new();
    // Raw data for list patterns: (synthetic_name, [head_names], tail_name?)
    let mut list_raw: Vec<(String, Vec<String>, Option<String>)> = Vec::new();
    // Ordered sequence of lets to emit — preserves argument order for correct nesting.
    let mut let_order: Vec<DestructureLet> = Vec::new();
    let mut synthetic_counter = 0usize;

    for pattern in patterns {
        match pattern {
            ParamPattern::Simple(name) => {
                all_env_names.push(name.clone());
                lambda_param_names.push(name.clone());
            }
            ParamPattern::Block(fields) => {
                let synthetic_name = format!("__p{}", synthetic_counter);
                synthetic_counter += 1;
                all_env_names.push(synthetic_name.clone());
                lambda_param_names.push(synthetic_name.clone());
                for (_, binding_name) in fields {
                    all_env_names.push(binding_name.clone());
                }
                block_raw.push((synthetic_name.clone(), fields.clone()));
                // Placeholder — resolved after env push
                let_order.push(DestructureLet::Block(DestructureEntry {
                    synthetic_name,
                    fields: Vec::new(),
                }));
            }
            ParamPattern::List(element_names, tail_name) => {
                let synthetic_name = format!("__p{}", synthetic_counter);
                synthetic_counter += 1;
                all_env_names.push(synthetic_name.clone());
                lambda_param_names.push(synthetic_name.clone());
                for binding_name in element_names {
                    all_env_names.push(binding_name.clone());
                }
                if let Some(tail) = tail_name {
                    all_env_names.push(tail.clone());
                }
                list_raw.push((
                    synthetic_name.clone(),
                    element_names.clone(),
                    tail_name.clone(),
                ));
                // Placeholder — resolved after env push
                let_order.push(DestructureLet::List(ListDestructureEntry {
                    synthetic_name,
                    elements: Vec::new(),
                    tail: None,
                }));
            }
        }
    }

    // Push all names into the environment as a single frame
    if !all_env_names.is_empty() {
        desugarer.env_mut().push_keys(all_env_names.iter().cloned());
    }

    // Collect FreeVars for lambda binders (before desugaring body)
    let lambda_param_vars: Vec<moniker::FreeVar<String>> = lambda_param_names
        .iter()
        .map(|name| desugarer.env().get(name).unwrap().clone())
        .collect();

    // Resolve block destructure entries now that names are in the environment.
    let mut block_raw_iter = block_raw.into_iter();
    let mut list_raw_iter = list_raw.into_iter();
    for slot in &mut let_order {
        match slot {
            DestructureLet::Block(entry) => {
                let (_, fields) = block_raw_iter.next().unwrap();
                entry.fields = fields
                    .iter()
                    .map(|(field_name, binding_name)| {
                        let binding_var = desugarer.env().get(binding_name).unwrap().clone();
                        FieldBinding {
                            field_name: field_name.clone(),
                            binding_var,
                        }
                    })
                    .collect();
            }
            DestructureLet::List(entry) => {
                let (_, element_names, tail_name) = list_raw_iter.next().unwrap();
                let drop_count = element_names.len();
                entry.elements = element_names
                    .iter()
                    .enumerate()
                    .map(|(index, binding_name)| {
                        let binding_var = desugarer.env().get(binding_name).unwrap().clone();
                        ListElementBinding { index, binding_var }
                    })
                    .collect();
                entry.tail = tail_name.as_ref().map(|tail_name| {
                    let binding_var = desugarer.env().get(tail_name).unwrap().clone();
                    ListTailBinding {
                        drop_count,
                        binding_var,
                    }
                });
            }
        }
    }

    // Desugar body with all names in scope
    let mut body = if let Some(body_node) = decl.body() {
        if let Some(body_soup) = body_node.soup() {
            body_soup.desugar(desugarer)?
        } else {
            return Err(CoreError::InvalidEmbedding(
                "empty declaration body".to_string(),
                desugarer.new_smid(span),
            ));
        }
    } else {
        return Err(CoreError::InvalidEmbedding(
            "missing declaration body".to_string(),
            desugarer.new_smid(span),
        ));
    };

    // Apply varify to convert Name expressions to Var expressions
    body = desugarer.varify(body);

    // Pop environment frame
    if !all_env_names.is_empty() {
        desugarer.env_mut().pop();
    }

    // Wrap body in destructuring lets for each pattern, innermost first
    // (last pattern wraps outermost), preserving argument order.
    for slot in let_order.iter().rev() {
        // Look up the synthetic param FreeVar from the lambda_param_vars
        // (the env frame has already been popped)
        let synthetic_fv = lambda_param_vars
            .iter()
            .zip(lambda_param_names.iter())
            .find(|(_, n)| **n == slot.synthetic_name())
            .map(|(fv, _)| fv.clone())
            .unwrap();

        let smid = desugarer.new_smid(span);
        let synthetic_var = RcExpr::from(Expr::Var(smid, moniker::Var::Free(synthetic_fv)));

        match slot {
            DestructureLet::Block(entry) => {
                // Each binding is: binding_var = Lookup(Var(synthetic), "field_name", None)
                let bindings: Vec<(Binder<String>, Embed<RcExpr>)> = entry
                    .fields
                    .iter()
                    .map(|fb| {
                        let lookup = RcExpr::from(Expr::Lookup(
                            smid,
                            synthetic_var.clone(),
                            fb.field_name.clone(),
                            None,
                        ));
                        (Binder(fb.binding_var.clone()), Embed(lookup))
                    })
                    .collect();

                body = RcExpr::from(Expr::Let(
                    smid,
                    Scope::new(Rec::new(bindings), body),
                    LetType::DestructureBlockLet,
                ));
            }
            DestructureLet::List(entry) => {
                // Each binding uses HEAD/TAIL chaining:
                //   a = HEAD(__p0)
                //   b = HEAD(TAIL(__p0))
                //   c = HEAD(TAIL(TAIL(__p0)))
                //
                // For head/tail patterns, the tail binding uses TAIL applied
                // `drop_count` times:
                //   xs = TAIL(TAIL(__p0))   (for [a, b : xs])
                //
                // This correctly handles lists built by the STG compiler
                // where the nil tail is a global ref, not a local ref.
                let mut bindings: Vec<(Binder<String>, Embed<RcExpr>)> = entry
                    .elements
                    .iter()
                    .map(|eb| {
                        // Build TAIL applied `index` times to synthetic_var
                        let mut list_expr = synthetic_var.clone();
                        for _ in 0..eb.index {
                            list_expr = core::app(smid, core::bif(smid, "TAIL"), vec![list_expr]);
                        }
                        // Apply HEAD to get the element at this position
                        let head_call = core::app(smid, core::bif(smid, "HEAD"), vec![list_expr]);
                        (Binder(eb.binding_var.clone()), Embed(head_call))
                    })
                    .collect();

                // Add the tail binding if present
                if let Some(tb) = &entry.tail {
                    let mut tail_expr = synthetic_var.clone();
                    for _ in 0..tb.drop_count {
                        tail_expr = core::app(smid, core::bif(smid, "TAIL"), vec![tail_expr]);
                    }
                    bindings.push((Binder(tb.binding_var.clone()), Embed(tail_expr)));
                }

                body = RcExpr::from(Expr::Let(
                    smid,
                    Scope::new(Rec::new(bindings), body),
                    LetType::DestructureListLet,
                ));
            }
        }
    }

    Ok((body, lambda_param_names, lambda_param_vars))
}

/// Literals desugar into core Primitives
impl Desugarable for rowan_ast::Literal {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        let span = text_range_to_span(self.syntax().text_range());

        if let Some(value) = self.value() {
            let primitive = match value {
                rowan_ast::LiteralValue::Sym(sym) => {
                    if let Some(s) = sym.value() {
                        Primitive::Sym(s.to_string())
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid symbol literal".to_string(),
                            desugarer.new_smid(span),
                        ));
                    }
                }
                rowan_ast::LiteralValue::Str(s) => {
                    if let Some(text) = s.value() {
                        Primitive::Str(text.to_string())
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid string literal".to_string(),
                            desugarer.new_smid(span),
                        ));
                    }
                }
                rowan_ast::LiteralValue::CStr(s) => {
                    if let Some(raw_text) = s.raw_value() {
                        // Process escape sequences for c-strings
                        use super::escape;
                        match escape::process_escapes(raw_text) {
                            Ok(processed) => Primitive::Str(processed),
                            Err(e) => {
                                return Err(CoreError::InvalidEmbedding(
                                    format!("invalid c-string escape: {}", e),
                                    desugarer.new_smid(span),
                                ));
                            }
                        }
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid c-string literal".to_string(),
                            desugarer.new_smid(span),
                        ));
                    }
                }
                rowan_ast::LiteralValue::RawStr(s) => {
                    // Raw strings are literal - no escape processing
                    if let Some(text) = s.value() {
                        Primitive::Str(text.to_string())
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid raw string literal".to_string(),
                            desugarer.new_smid(span),
                        ));
                    }
                }
                rowan_ast::LiteralValue::TStr(s) => {
                    // ZDT literal desugars to ZDT.PARSE(normalized_content)
                    if let Some(content) = s.value() {
                        let smid = desugarer.new_smid(span);
                        let normalized = lex::normalize_zdt_for_parse(content);
                        return Ok(core::app(
                            smid,
                            core::bif(smid, "ZDT.PARSE"),
                            vec![core::str(smid, normalized)],
                        ));
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid ZDT literal".to_string(),
                            desugarer.new_smid(span),
                        ));
                    }
                }
                rowan_ast::LiteralValue::Num(n) => {
                    if let Some(num) = n.value() {
                        Primitive::Num(num)
                    } else {
                        return Err(CoreError::InvalidEmbedding(
                            "invalid number literal".to_string(),
                            desugarer.new_smid(span),
                        ));
                    }
                }
            };
            Ok(RcExpr::from(Expr::Literal(
                desugarer.new_smid(span),
                primitive,
            )))
        } else {
            Err(CoreError::InvalidEmbedding(
                "malformed literal".to_string(),
                desugarer.new_smid(span),
            ))
        }
    }
}

/// Extract a MonadSpec from a desugared block body used in a bracket pair definition.
///
/// Expects the body to be a desugared block like:
/// ```eucalypt
/// { :monad bind: rand-bind  return: rand-return }
/// { :monad namespace: maybe }
/// ```
/// The desugared form is `Expr::Let(bindings, body)`.
/// Returns `Some(MonadSpec)` if a valid spec is found.
fn extract_monad_spec_from_body(
    body: &RcExpr,
    smid: Smid,
) -> Result<Option<super::desugarer::MonadSpec>, CoreError> {
    use super::desugarer::MonadSpec;
    let has_monad_marker = has_monad_block_metadata(body);
    let spec = find_monad_spec_in_bindings(body);
    match (has_monad_marker, spec) {
        (true, Some(spec)) => Ok(Some(spec)),
        (true, None) => Ok(None),
        (
            false,
            Some(MonadSpec::Explicit {
                bind_name,
                return_name,
            }),
        ) => Err(CoreError::MonadSpecMissingMarker(
            bind_name,
            return_name,
            smid,
        )),
        (false, Some(MonadSpec::Namespace(_))) => Ok(None),
        (false, None) => Ok(None),
    }
}

/// Check whether a desugared block body has `:monad` block metadata.
fn has_monad_block_metadata(expr: &RcExpr) -> bool {
    match &*expr.inner {
        Expr::Meta(_, inner, meta) => {
            if matches!(&*meta.inner, Expr::Literal(_, Primitive::Sym(s)) if s == "monad") {
                true
            } else {
                has_monad_block_metadata(inner)
            }
        }
        _ => false,
    }
}

/// Find a MonadSpec from the let-bindings of a desugared block.
///
/// Handles both explicit (bind + return keys) and namespace (namespace key) forms.
fn find_monad_spec_in_bindings(expr: &RcExpr) -> Option<super::desugarer::MonadSpec> {
    use super::desugarer::MonadSpec;
    use crate::core::metadata::extract_function_name_from_expr;
    match &*expr.inner {
        Expr::Let(_, scope, _) => {
            let bindings = scope.unsafe_pattern.unsafe_pattern.clone();
            let mut bind_name = None;
            let mut return_name = None;
            let mut namespace = None;
            for (binder, embed) in &bindings {
                let key = binder.0.pretty_name.as_deref().unwrap_or("");
                let val = extract_function_name_from_expr(&embed.0);
                match key {
                    "bind" => bind_name = val,
                    "return" => return_name = val,
                    "namespace" => namespace = val,
                    _ => {}
                }
            }
            if let Some(ns) = namespace {
                Some(MonadSpec::Namespace(ns))
            } else if let (Some(b), Some(r)) = (bind_name, return_name) {
                Some(MonadSpec::Explicit {
                    bind_name: b,
                    return_name: r,
                })
            } else {
                None
            }
        }
        Expr::Meta(_, inner, _) => find_monad_spec_in_bindings(inner),
        _ => None,
    }
}

/// Extract a MonadSpec from the raw rowan AST metadata of a regular block element.
///
/// Handles these block metadata forms:
/// 1. `:name` — bare symbol, treated as namespace reference
/// 2. `{ monad: name }` — block with `monad` key, treated as namespace reference
/// 3. `{ :monad namespace: name }` — :monad-tagged block with `namespace` key
/// 4. `{ :monad bind: f return: r }` — :monad-tagged block with explicit bind/return
///
/// Returns `None` if no monadic metadata is detected.
fn extract_block_monad_spec_from_raw(
    block: &rowan_ast::Block,
) -> Option<super::desugarer::MonadSpec> {
    use super::desugarer::MonadSpec;

    let meta = block.meta()?;
    let soup = meta.soup()?;
    let elements: Vec<rowan_ast::Element> = soup.elements().collect();

    // Form 1: bare symbol `:name` — namespace reference
    if elements.len() == 1 {
        if let rowan_ast::Element::Lit(lit) = &elements[0] {
            let text = lit.syntax().text().to_string();
            if let Some(name) = text.strip_prefix(':') {
                // Exclude known non-monad desugar-phase metadata symbols
                if !matches!(name, "main" | "suppress") {
                    return Some(MonadSpec::Namespace(name.to_string()));
                }
            }
        }
    }

    // Forms 2, 3, 4: metadata is a block expression
    if let Some(rowan_ast::Element::Block(meta_block)) = elements.first() {
        let mut monad_key: Option<String> = None;
        let mut namespace_val: Option<String> = None;
        let mut bind_val: Option<String> = None;
        let mut return_val: Option<String> = None;
        let mut has_monad_marker = false;

        // Check for :monad marker in meta_block's own metadata
        if let Some(meta_meta) = meta_block.meta() {
            if let Some(meta_soup) = meta_meta.soup() {
                let meta_elems: Vec<rowan_ast::Element> = meta_soup.elements().collect();
                if meta_elems.len() == 1 {
                    if let rowan_ast::Element::Lit(lit) = &meta_elems[0] {
                        if lit.syntax().text() == ":monad" {
                            has_monad_marker = true;
                        }
                    }
                }
            }
        }

        // Read declarations from the meta_block
        for decl in meta_block.declarations() {
            if let Some(head) = decl.head() {
                if let rowan_ast::DeclarationKind::Property(prop) = head.classify_declaration() {
                    let key = prop.text().to_string();
                    let val = decl.body().and_then(|body| body.soup()).and_then(|s| {
                        let elems: Vec<rowan_ast::Element> = s.elements().collect();
                        if elems.len() == 1 {
                            match &elems[0] {
                                rowan_ast::Element::Lit(lit) => {
                                    let text = lit.syntax().text().to_string();
                                    Some(
                                        text.strip_prefix(':')
                                            .map(|s| s.to_string())
                                            .unwrap_or(text),
                                    )
                                }
                                rowan_ast::Element::Name(name) => {
                                    name.identifier().map(|id| id.text().to_string())
                                }
                                _ => None,
                            }
                        } else {
                            None
                        }
                    });
                    match key.as_str() {
                        "monad" => monad_key = val,
                        "namespace" => namespace_val = val,
                        "bind" => bind_val = val,
                        "return" => return_val = val,
                        _ => {}
                    }
                }
            }
        }

        // Form 2: `{ monad: name }` — namespace reference via `monad` key (no :monad marker)
        if !has_monad_marker {
            if let Some(ns) = monad_key {
                return Some(MonadSpec::Namespace(ns));
            }
            return None;
        }

        // Forms 3 and 4: has :monad marker
        if let Some(ns) = namespace_val {
            return Some(MonadSpec::Namespace(ns));
        }
        if let (Some(b), Some(r)) = (bind_val, return_val) {
            return Some(MonadSpec::Explicit {
                bind_name: b,
                return_name: r,
            });
        }
    }

    None
}

/// Return true if the raw metadata soup of a `Unit` begins with a block-like
/// element (`Block`, `BracketBlock`, or `ParenExpr`).
///
/// Used to distinguish genuine bare-expression evaluands such as
/// `{ :io r: cmd }.r.stdout` (first element: Block) from erroneous
/// assignment-style declarations like `result = 42` (first element: Name).
fn unit_meta_starts_with_block(unit: &rowan_ast::Unit) -> bool {
    unit.meta()
        .and_then(|m| m.soup())
        .and_then(|s| s.elements().next())
        .map(|e| {
            matches!(
                e,
                rowan_ast::Element::Block(_)
                    | rowan_ast::Element::BracketBlock(_)
                    | rowan_ast::Element::ParenExpr(_)
            )
        })
        .unwrap_or(false)
}

/// Desugar a monadic block using a monad spec.
///
/// Given a list of declarations `a: ma  b: mb`, a return element, and a monad spec,
/// produces:
/// ```text
/// bind(ma, λa. bind(mb, λb. return(return_expr)))
/// ```
/// For namespace specs (`MonadSpec::Namespace("ns")`), emits lookup expressions:
/// ```text
/// ns.bind(ma, λa. ns.bind(mb, λb. ns.return(return_expr)))
/// ```
///
/// All declarations are desugared as monadic bind steps,
/// with each declaration name becoming a lambda parameter.
/// The return expression (from `.expr` after the block) is wrapped in `return`.
/// Desugar a slice of soup elements that form a chained return expression.
///
/// Handles `name`, `name.field`, `name.field.subfield`, and parenthesised
/// expressions such as `(name.field)`. The slice must be non-empty and must
/// follow the pattern `primary (. name)*` where the dots and names alternate.
///
/// The bind names introduced by the monadic block must already be in scope
/// in `desugarer` when this function is called.
fn desugar_return_chain(
    elements: &[Element],
    smid: Smid,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    if elements.is_empty() {
        return Err(CoreError::InvalidEmbedding(
            "empty monadic return expression".to_string(),
            smid,
        ));
    }
    // Desugar the primary expression (first element)
    let primary = elements[0].desugar(desugarer)?;
    let mut result = desugarer.varify(primary);

    // Walk the rest: expected to be pairs of (`.`, name)
    let mut i = 1;
    while i + 1 < elements.len() {
        // We expect a `.` operator followed by a name
        let is_dot = elements[i]
            .as_operator_identifier()
            .map(|op| op.text() == ".")
            .unwrap_or(false);
        if !is_dot {
            break;
        }
        let field_span = text_range_to_span(elements[i + 1].syntax().text_range());
        let field_smid = desugarer.new_smid(field_span);
        match &elements[i + 1] {
            Element::Name(name_elem) => {
                if let Some(id) = name_elem.identifier() {
                    let field = id.text().to_string();
                    result = core::lookup(field_smid, result, &field, None);
                } else {
                    break;
                }
            }
            _ => break,
        }
        i += 2;
    }

    Ok(result)
}

fn desugar_monadic_block(
    smid: Smid,
    decls: Vec<rowan_ast::Declaration>,
    return_elems: &[Element],
    spec: &super::desugarer::MonadSpec,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    if decls.is_empty() {
        return Err(CoreError::EmptyMonadicBlock(smid));
    }

    // We need to push all bind-names into the environment before desugaring
    // any bodies, so that later bodies can reference earlier names.
    // All declaration names become bind names (the return expr is separate).
    let bind_names: Vec<String> = decls
        .iter()
        .map(extract_declaration_name)
        .collect::<Result<Vec<_>, CoreError>>()?;

    // Push bind names into environment so bodies can reference earlier bindings
    if !bind_names.is_empty() {
        desugarer.env_mut().push_keys(bind_names.iter().cloned());
    }

    // Collect bind FreeVars while names are in scope
    let bind_vars: Vec<moniker::FreeVar<String>> = bind_names
        .iter()
        .map(|name| desugarer.env().get(name).unwrap().clone())
        .collect();

    // Desugar all declaration bodies (in order) with bind names in scope
    let mut name_value_pairs: Vec<(String, RcExpr)> = Vec::with_capacity(decls.len());
    for (i, decl) in decls.iter().enumerate() {
        let decl_name = bind_names[i].clone();
        let span = text_range_to_span(decl.syntax().text_range());
        let value = if let Some(body) = decl.body() {
            if let Some(soup) = body.soup() {
                let expr = soup.desugar(desugarer)?;
                desugarer.varify(expr)
            } else {
                if !bind_names.is_empty() {
                    desugarer.env_mut().pop();
                }
                return Err(CoreError::InvalidEmbedding(
                    "empty monadic block declaration body".to_string(),
                    desugarer.new_smid(span),
                ));
            }
        } else {
            if !bind_names.is_empty() {
                desugarer.env_mut().pop();
            }
            return Err(CoreError::InvalidEmbedding(
                "missing monadic block declaration body".to_string(),
                desugarer.new_smid(span),
            ));
        };
        name_value_pairs.push((decl_name, value));
    }

    // Desugar the return expression (potentially a chained lookup chain like
    // `result.stdout`) with bind names in scope.
    let return_expr = desugar_return_chain(return_elems, smid, desugarer)?;

    // Pop bind names from environment
    if !bind_names.is_empty() {
        desugarer.env_mut().pop();
    }

    // Build the bind chain right-to-left.
    // For Explicit spec: call bind_name(value, lambda) and return_name(expr)
    // For Namespace spec: emit lookup expressions ns.bind(value, lambda) and ns.return(expr)
    use super::desugarer::MonadSpec;
    let mut result = match spec {
        MonadSpec::Explicit { return_name, .. } => {
            let return_fn = desugarer.varify(RcExpr::from(Expr::Name(smid, return_name.clone())));
            RcExpr::from(Expr::App(smid, return_fn, vec![return_expr]))
        }
        MonadSpec::Namespace(ns) => {
            // ns.return(return_expr)
            let ns_var = desugarer.varify(RcExpr::from(Expr::Name(smid, ns.clone())));
            core::app(
                smid,
                core::lookup(smid, ns_var, "return", None),
                vec![return_expr],
            )
        }
    };

    // Build bind chain from right-to-left using all pairs
    for ((_, value), bind_var) in name_value_pairs
        .into_iter()
        .zip(bind_vars.into_iter())
        .rev()
    {
        let lambda = core::lam(smid, vec![bind_var], result);
        result = match spec {
            MonadSpec::Explicit { bind_name, .. } => {
                let bind_fn = desugarer.varify(RcExpr::from(Expr::Name(smid, bind_name.clone())));
                RcExpr::from(Expr::App(smid, bind_fn, vec![value, lambda]))
            }
            MonadSpec::Namespace(ns) => {
                // ns.bind(value, lambda)
                let ns_var = desugarer.varify(RcExpr::from(Expr::Name(smid, ns.clone())));
                core::app(
                    smid,
                    core::lookup(smid, ns_var, "bind", None),
                    vec![value, lambda],
                )
            }
        };
    }

    Ok(result)
}

/// Element (equivalent to Expression) desugaring
impl Desugarable for Element {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        match self {
            Element::Lit(lit) => lit.desugar(desugarer),
            Element::Block(block) => block.desugar(desugarer),
            Element::List(list) => {
                let span = text_range_to_span(list.syntax().text_range());
                let items: Result<Vec<RcExpr>, CoreError> = list
                    .items()
                    .map(|soup| {
                        // Always desugar through the soup path so that anaphora
                        // in list items are handled by cook_soup's
                        // fill_gaps/wrap_lambda logic.
                        //
                        // Wrap bare ExprAnaphor results in Soup so the cooker
                        // calls cook_soup on them and can wrap a lambda.
                        let expr = soup.desugar(desugarer)?;
                        let expr = ensure_anaphor_in_soup(expr);
                        // Apply varify to convert Name expressions to Var expressions
                        Ok(desugarer.varify(expr))
                    })
                    .collect();

                Ok(RcExpr::from(Expr::List(desugarer.new_smid(span), items?)))
            }
            Element::ParenExpr(paren) => {
                // ParenExpr contains a soup
                if let Some(soup) = paren.soup() {
                    soup.desugar(desugarer)
                } else {
                    let span = text_range_to_span(paren.syntax().text_range());
                    Err(CoreError::InvalidEmbedding(
                        "empty parentheses".to_string(),
                        desugarer.new_smid(span),
                    ))
                }
            }
            Element::Name(name) => {
                let span = text_range_to_span(name.syntax().text_range());
                if let Some(id) = name.identifier() {
                    desugar_rowan_name(span, &id, desugarer)
                } else {
                    Err(CoreError::InvalidEmbedding(
                        "invalid name".to_string(),
                        desugarer.new_smid(span),
                    ))
                }
            }
            Element::StringPattern(pattern) => {
                let span = text_range_to_span(pattern.syntax().text_range());
                desugar_rowan_string_pattern(
                    span,
                    pattern.chunks(),
                    StringPatternMode::Plain,
                    desugarer,
                )
            }
            Element::CStringPattern(pattern) => {
                let span = text_range_to_span(pattern.syntax().text_range());
                desugar_rowan_string_pattern(
                    span,
                    pattern.chunks(),
                    StringPatternMode::CString,
                    desugarer,
                )
            }
            Element::RawStringPattern(pattern) => {
                let span = text_range_to_span(pattern.syntax().text_range());
                desugar_rowan_string_pattern(
                    span,
                    pattern.chunks(),
                    StringPatternMode::Raw,
                    desugarer,
                )
            }
            Element::BracketExpr(bracket) => {
                let span = text_range_to_span(bracket.syntax().text_range());
                let smid = desugarer.new_smid(span);

                let pair_name = bracket.bracket_pair_name().ok_or_else(|| {
                    CoreError::InvalidEmbedding(
                        "bracket expression missing bracket characters".to_string(),
                        smid,
                    )
                })?;

                // Simple expression mode: ⟦ x ⟧ → ⟦⟧(x)
                //
                // Desugar the inner soup and apply the bracket pair function.
                // Both the function name and the argument must be varified so
                // that Name nodes are resolved to Var references before the
                // STG compiler sees them.
                let inner = if let Some(soup) = bracket.soup() {
                    soup.desugar(desugarer)?
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "empty bracket expression".to_string(),
                        smid,
                    ));
                };

                let bracket_fn_name = RcExpr::from(Expr::Name(smid, pair_name));
                let bracket_fn = desugarer.varify(bracket_fn_name);
                let arg = desugarer.varify(inner);
                Ok(RcExpr::from(Expr::App(smid, bracket_fn, vec![arg])))
            }
            Element::BracketBlock(_) => {
                // BracketBlock elements appearing in isolation (not preceded by soup context)
                // should not occur — they should be consumed by desugar_rowan_soup lookahead.
                // If we reach here, it means the BracketBlock has no following .expr.
                let span = text_range_to_span(self.syntax().text_range());
                let smid = desugarer.new_smid(span);
                Err(CoreError::InvalidEmbedding(
                    "monadic bracket block requires a return expression (e.g. ⟦ ... ⟧.expr)"
                        .to_string(),
                    smid,
                ))
            }
            Element::ApplyTuple(tuple) => {
                let span = text_range_to_span(tuple.syntax().text_range());
                let args: Result<Vec<RcExpr>, CoreError> = tuple
                    .items()
                    .map(|soup| {
                        // Always desugar through the soup path so that anaphora
                        // in args (e.g. `map(_)`, `filter(_ > 0)`) are handled
                        // by cook_soup's fill_gaps/wrap_lambda logic.
                        //
                        // Additionally, if the result is a bare ExprAnaphor (from a
                        // single-element soup like `(_)` or just `_`), we wrap it in
                        // Expr::Soup so the cooker sees a Soup and calls cook_soup,
                        // which is where fill_gaps and lambda-wrapping happen.
                        // Without this, a bare `_` in `f(_)` would bypass cook_soup
                        // and become an orphaned free variable.
                        let expr = soup.desugar(desugarer)?;
                        let expr = ensure_anaphor_in_soup(expr);
                        // Apply varify to convert Name expressions to Var expressions
                        Ok(desugarer.varify(expr))
                    })
                    .collect();

                Ok(RcExpr::from(Expr::ArgTuple(
                    desugarer.new_smid(span),
                    args?,
                )))
            }
        }
    }
}

/// Rowan equivalent of DeclarationComponents for extracting declaration information
struct RowanDeclarationComponents {
    pub span: Span,
    pub metadata: Option<RcExpr>,
    pub name: String,
    pub args: Vec<String>,
    pub body: RcExpr,
    pub arg_vars: Vec<moniker::FreeVar<String>>,
    pub is_operator: bool,
    pub fixity: Option<crate::core::expr::Fixity>,
}

/// Helper to desugar declaration body with arguments in scope
/// Returns the desugared body and the FreeVars for the arguments
fn desugar_declaration_body(
    decl: &rowan_ast::Declaration,
    desugarer: &mut Desugarer,
    args: &[String],
    span: Span,
) -> Result<(RcExpr, Vec<moniker::FreeVar<String>>), CoreError> {
    // Push args to environment if any and collect the FreeVars
    let arg_vars = if !args.is_empty() {
        desugarer.env_mut().push_keys(args.iter().cloned());
        // Get the FreeVars that were created
        args.iter()
            .map(|name| desugarer.env().get(name).unwrap().clone())
            .collect()
    } else {
        Vec::new()
    };

    // Desugar body
    let mut body = if let Some(body) = decl.body() {
        if let Some(body_soup) = body.soup() {
            body_soup.desugar(desugarer)?
        } else {
            return Err(CoreError::InvalidEmbedding(
                "empty declaration body".to_string(),
                desugarer.new_smid(span),
            ));
        }
    } else {
        return Err(CoreError::InvalidEmbedding(
            "missing declaration body".to_string(),
            desugarer.new_smid(span),
        ));
    };

    // Apply varify to convert Name expressions to Var expressions
    body = desugarer.varify(body);

    // Pop args from environment if any
    if !args.is_empty() {
        desugarer.env_mut().pop();
    }

    Ok((body, arg_vars))
}

/// Extract declaration components from a Rowan Declaration
fn extract_rowan_declaration_components(
    decl: &rowan_ast::Declaration,
    desugarer: &mut Desugarer,
) -> Result<RowanDeclarationComponents, CoreError> {
    let span = text_range_to_span(decl.syntax().text_range());

    // Extract metadata first
    let metadata = if let Some(meta) = decl.meta() {
        if let Some(meta_soup) = meta.soup() {
            Some(meta_soup.desugar(desugarer)?)
        } else {
            None
        }
    } else {
        None
    };

    // Extract head information (name, args, operator status) BEFORE body
    // so we can push args to environment for body desugaring
    if let Some(head) = decl.head() {
        let kind = head.classify_declaration();

        match kind {
            rowan_ast::DeclarationKind::Property(prop) => {
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &[], span)?;

                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: prop.text().to_string(),
                    args: vec![],
                    body,
                    arg_vars,
                    is_operator: false,
                    fixity: None,
                })
            }
            rowan_ast::DeclarationKind::Function(func, args_tuple) => {
                // Parse each argument into a ParamPattern (simple name or
                // destructuring pattern). Fall back to simple name extraction
                // for any arg that doesn't parse as a pattern (shouldn't
                // happen after Task 2 validation, but be defensive).
                let patterns: Vec<ParamPattern> = args_tuple
                    .items()
                    .filter_map(|soup| parse_param_pattern(&soup))
                    .collect();

                // If all patterns are Simple (no destructuring), use the
                // existing fast path so as not to disturb existing behaviour.
                let all_simple = patterns
                    .iter()
                    .all(|p| matches!(p, ParamPattern::Simple(_)));

                if all_simple {
                    let arg_names: Vec<String> = patterns
                        .into_iter()
                        .map(|p| match p {
                            ParamPattern::Simple(n) => n,
                            _ => unreachable!(),
                        })
                        .collect();
                    let (body, arg_vars) =
                        desugar_declaration_body(decl, desugarer, &arg_names, span)?;
                    Ok(RowanDeclarationComponents {
                        span,
                        metadata,
                        name: func.text().to_string(),
                        args: arg_names,
                        body,
                        arg_vars,
                        is_operator: false,
                        fixity: None,
                    })
                } else {
                    // At least one destructuring pattern — use pattern-aware
                    // desugaring which injects synthetic params and let bindings.
                    let (body, lambda_param_names, lambda_param_vars) =
                        desugar_declaration_body_with_patterns(decl, desugarer, &patterns, span)?;
                    Ok(RowanDeclarationComponents {
                        span,
                        metadata,
                        name: func.text().to_string(),
                        args: lambda_param_names,
                        body,
                        arg_vars: lambda_param_vars,
                        is_operator: false,
                        fixity: None,
                    })
                }
            }
            rowan_ast::DeclarationKind::Prefix(_, op, arg) => {
                let args = vec![arg.text().to_string()];
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &args, span)?;

                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: op.text().to_string(),
                    args,
                    body,
                    arg_vars,
                    is_operator: true,
                    fixity: Some(crate::core::expr::Fixity::UnaryPrefix),
                })
            }
            rowan_ast::DeclarationKind::Postfix(_, arg, op) => {
                let args = vec![arg.text().to_string()];
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &args, span)?;

                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: op.text().to_string(),
                    args,
                    body,
                    arg_vars,
                    is_operator: true,
                    fixity: Some(crate::core::expr::Fixity::UnaryPostfix),
                })
            }
            rowan_ast::DeclarationKind::Binary(_, left, op, right) => {
                let args = vec![left.text().to_string(), right.text().to_string()];
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &args, span)?;

                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: op.text().to_string(),
                    args,
                    body,
                    arg_vars,
                    is_operator: true,
                    fixity: None,
                })
            }
            rowan_ast::DeclarationKind::Nullary(_, op) => {
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &[], span)?;

                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: op.text().to_string(),
                    args: vec![],
                    body,
                    arg_vars,
                    is_operator: true,
                    fixity: Some(crate::core::expr::Fixity::Nullary),
                })
            }
            rowan_ast::DeclarationKind::BracketPair(_, bracket_expr, param) => {
                let pair_name = bracket_expr.bracket_pair_name().ok_or_else(|| {
                    CoreError::InvalidEmbedding(
                        "bracket pair declaration has no bracket pair name".to_string(),
                        desugarer.new_smid(span),
                    )
                })?;
                let args = vec![param.text().to_string()];
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &args, span)?;

                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: pair_name,
                    args,
                    body,
                    arg_vars,
                    is_operator: false,
                    fixity: None,
                })
            }
            rowan_ast::DeclarationKind::BracketBlockDef(_, bracket_expr) => {
                let pair_name = bracket_expr.bracket_pair_name().ok_or_else(|| {
                    CoreError::InvalidEmbedding(
                        "bracket block definition has no bracket pair name".to_string(),
                        desugarer.new_smid(span),
                    )
                })?;
                // No formal args — the body is the monad spec block.
                let (body, arg_vars) = desugar_declaration_body(decl, desugarer, &[], span)?;

                Ok(RowanDeclarationComponents {
                    span,
                    metadata,
                    name: pair_name,
                    args: vec![],
                    body,
                    arg_vars,
                    is_operator: false,
                    fixity: None,
                })
            }
            rowan_ast::DeclarationKind::MalformedHead(_) => Err(CoreError::InvalidEmbedding(
                "malformed declaration head".to_string(),
                desugarer.new_smid(span),
            )),
        }
    } else {
        Err(CoreError::InvalidEmbedding(
            "missing declaration head".to_string(),
            desugarer.new_smid(span),
        ))
    }
}

/// Translate special operator names (".") to operators but all other
/// names to Expr::Name for further analysis
fn desugar_rowan_name(
    span: Span,
    id: &rowan_ast::Identifier,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    match id {
        rowan_ast::Identifier::OperatorIdentifier(op) => {
            let name = op.text();
            if name == "." {
                Ok(RcExpr::from(ops::dot()))
            } else {
                Ok(RcExpr::from(Expr::Name(
                    desugarer.new_smid(span),
                    name.to_string(),
                )))
            }
        }
        rowan_ast::Identifier::NormalIdentifier(normal) => {
            // Use the parent Identifier's name() method to properly extract name content (strips quotes)
            let full_id = rowan_ast::Identifier::NormalIdentifier(normal.clone());
            let name = full_id.name().unwrap_or("");
            if name.starts_with("__") && name.chars().nth(2).is_some_and(|c| c.is_uppercase()) {
                Ok(RcExpr::from(Expr::Intrinsic(
                    desugarer.new_smid(span),
                    name[2..].to_string(),
                )))
            } else if BLOCK_ANAPHORA.is_anaphor(name) {
                let smid = desugarer.new_smid(span);
                Ok(RcExpr::from(Expr::BlockAnaphor(
                    smid,
                    BLOCK_ANAPHORA.to_explicit_anaphor(smid, name),
                )))
            } else if EXPR_ANAPHORA.is_anaphor(name) {
                let smid = desugarer.new_smid(span);
                Ok(RcExpr::from(Expr::ExprAnaphor(
                    smid,
                    EXPR_ANAPHORA.to_explicit_anaphor(smid, name),
                )))
            } else {
                Ok(RcExpr::from(Expr::Name(
                    desugarer.new_smid(span),
                    name.to_string(),
                )))
            }
        }
    }
}

/// Mode for string pattern processing
#[derive(Clone, Copy, PartialEq, Eq)]
enum StringPatternMode {
    /// Plain string - no backslash escape processing
    Plain,
    /// C-string - process backslash escape sequences
    CString,
    /// Raw string - no backslash escape processing (same as plain)
    Raw,
}

/// Desugar string pattern directly from Rowan AST
fn desugar_rowan_string_pattern(
    span: Span,
    chunks: rowan_ast::AstChildren<rowan_ast::StringChunk>,
    mode: StringPatternMode,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    // Desugar chunks directly without converting to legacy AST
    let mut translated_chunks = Vec::new();

    for chunk in chunks {
        let chunk_expr = match chunk {
            rowan_ast::StringChunk::LiteralContent(content) => {
                if let Some(text) = content.value() {
                    let chunk_span = text_range_to_span(content.syntax().text_range());
                    // Process escapes for c-strings
                    let processed_text = if mode == StringPatternMode::CString {
                        use super::escape;
                        match escape::process_escapes(&text) {
                            Ok(processed) => processed,
                            Err(e) => {
                                return Err(CoreError::InvalidEmbedding(
                                    format!("invalid c-string escape: {}", e),
                                    desugarer.new_smid(chunk_span),
                                ));
                            }
                        }
                    } else {
                        text.to_string()
                    };
                    RcExpr::from(Expr::Literal(
                        desugarer.new_smid(chunk_span),
                        Primitive::Str(processed_text),
                    ))
                } else {
                    RcExpr::from(Expr::Literal(
                        desugarer.new_smid(span),
                        Primitive::Str("".to_string()),
                    ))
                }
            }
            rowan_ast::StringChunk::Interpolation(interp) => {
                let chunk_span = text_range_to_span(interp.syntax().text_range());

                // Extract target expression using the exact same approach as legacy AST
                let mut target_expr = if let Some(soup) = interp.soup() {
                    // Extract names from the soup (equivalent to Vec<Name> in legacy AST)
                    let elements: Vec<_> = soup.elements().collect();
                    let names: Vec<_> = elements
                        .iter()
                        .filter_map(|elem| match elem {
                            rowan_ast::Element::Name(name) => {
                                // Exclude dots - they're parsed as names but aren't actual identifiers
                                if name.syntax().text() == "." {
                                    None
                                } else {
                                    Some(name)
                                }
                            }
                            _ => None,
                        })
                        .collect();

                    // Apply the exact legacy algorithm for InterpolationTarget::Reference
                    let smid = desugarer.new_smid(chunk_span);
                    if names.len() == 1 {
                        // Single name case: just desugar the name and varify it
                        let name = &names[0];
                        let name_text = name.syntax().text().to_string();
                        let name_expr = core::name(smid, name_text);
                        desugarer.varify(name_expr)
                    } else {
                        // Multiple names case: build [name, dot, name, dot, name] soup
                        let mut v = Vec::new();
                        let mut items: Vec<_> = names.to_vec();

                        // Process from end to beginning, just like legacy AST
                        while items.len() > 1 {
                            let name = items.pop().unwrap();
                            let name_text = name.syntax().text().to_string();
                            v.push(core::name(smid, name_text));
                            v.push(acore::dot());
                        }

                        // Add the final name (the first one in the original order)
                        if let Some(final_name) = items.pop() {
                            let final_name_text = final_name.syntax().text().to_string();
                            let name_expr = core::name(smid, final_name_text);
                            v.push(desugarer.varify(name_expr));
                        }

                        // Reverse the entire vector, just like legacy AST
                        v.reverse();
                        core::soup(smid, v)
                    }
                } else if let Some(target_token) = interp.target() {
                    if let Some(target_text) = target_token.value() {
                        if target_text.is_empty() {
                            // Empty {} means anonymous positional parameter
                            let smid = desugarer.new_smid(chunk_span);
                            let fv = desugarer
                                .add_pending_string_anaphor(Anaphor::ExplicitAnonymous(smid));
                            core::var(smid, fv)
                        } else if let Ok(num) = target_text.parse::<i32>() {
                            // Numeric index like {0}, {1}, {2}
                            let smid = desugarer.new_smid(chunk_span);
                            let fv = desugarer
                                .add_pending_string_anaphor(Anaphor::ExplicitNumbered(num));
                            core::var(smid, fv)
                        } else {
                            // Variable reference like {name}
                            core::var(desugarer.new_smid(chunk_span), desugarer.var(&target_text))
                        }
                    } else {
                        // No value means anonymous positional parameter
                        let smid = desugarer.new_smid(chunk_span);
                        let fv =
                            desugarer.add_pending_string_anaphor(Anaphor::ExplicitAnonymous(smid));
                        core::var(smid, fv)
                    }
                } else {
                    // No target means anonymous positional parameter
                    let smid = desugarer.new_smid(chunk_span);
                    let fv = desugarer.add_pending_string_anaphor(Anaphor::ExplicitAnonymous(smid));
                    core::var(smid, fv)
                };

                // Apply format spec if present
                if let Some(format_spec) = interp.format_spec() {
                    if let Some(format) = format_spec.value() {
                        if !format.is_empty() {
                            if format.as_bytes()[0] as char == '%' {
                                target_expr =
                                    acore::app(rt::fmt(), vec![target_expr, acore::str(format)]);
                            } else {
                                let smid = desugarer.new_smid(chunk_span);
                                target_expr = acore::app(
                                    core::var(smid, desugarer.var(&format)),
                                    vec![target_expr],
                                );
                            }
                        }
                    }
                }

                // Apply default string conversion for interpolated expressions
                acore::app(rt::str(), vec![target_expr])
            }
            rowan_ast::StringChunk::EscapedOpen(_) => {
                // Escaped {{ becomes literal {
                let chunk_span = text_range_to_span(chunk.syntax().text_range());
                RcExpr::from(Expr::Literal(
                    desugarer.new_smid(chunk_span),
                    Primitive::Str("{".to_string()),
                ))
            }
            rowan_ast::StringChunk::EscapedClose(_) => {
                // Escaped }} becomes literal }
                let chunk_span = text_range_to_span(chunk.syntax().text_range());
                RcExpr::from(Expr::Literal(
                    desugarer.new_smid(chunk_span),
                    Primitive::Str("}".to_string()),
                ))
            }
        };
        translated_chunks.push(chunk_expr);
    }

    // Create the final string expression using JOIN intrinsic
    let smid = desugarer.new_smid(span);
    let mut expr = core::app(
        smid,
        rt::join(),
        vec![core::list(smid, translated_chunks), acore::str("")],
    );

    // If there are any anaphora wrap into a lambda
    if desugarer.has_pending_string_anaphora() {
        use crate::core::anaphora;
        use crate::core::transform::succ;
        let binders = anaphora::to_binding_pattern(desugarer.pending_string_anaphora())?;
        desugarer.clear_pending_string_anaphora();
        expr = core::lam(expr.smid(), binders, succ::succ(&expr)?);
    }

    Ok(expr)
}

/// Soup desugaring
impl Desugarable for rowan_ast::Soup {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        let span = text_range_to_span(self.syntax().text_range());
        let elements: Vec<Element> = self.elements().collect();

        // If there's only one element, desugar it and apply variable resolution
        if elements.len() == 1 {
            let expr = elements[0].desugar(desugarer)?;
            // Apply varify to ensure Name expressions become Var expressions
            Ok(desugarer.varify(expr))
        } else {
            // Multiple elements - desugar as operator soup
            desugar_rowan_soup(span, elements, desugarer)
        }
    }
}

/// Ensure a bare `ExprAnaphor` is wrapped in an `Expr::Soup` so that the
/// cooker's `cook_soup` is called on it and can wrap a lambda around it.
///
/// When a single-element soup (e.g. `(_)` or bare `_`) is desugared, the
/// `Soup::desugar` fast-path returns the element directly without wrapping it
/// in an `Expr::Soup`. The cooker only calls `cook_soup` (where `fill_gaps`
/// and lambda-wrapping happen) when it encounters `Expr::Soup` nodes. A bare
/// `ExprAnaphor` outside a `Soup` hits `cook_expr_anaphor` instead, which
/// records the anaphor but never produces the wrapping lambda.
///
/// This helper is used when desugaring `ApplyTuple` args and list items so
/// that `map(_)` and `[_]` work correctly.
fn ensure_anaphor_in_soup(expr: RcExpr) -> RcExpr {
    if matches!(&*expr.inner, crate::core::expr::Expr::ExprAnaphor(_, _)) {
        let smid = expr.smid();
        RcExpr::from(crate::core::expr::Expr::Soup(smid, vec![expr]))
    } else {
        expr
    }
}

/// Desugar operator soup - mirrors the logic from ast.rs
fn desugar_rowan_soup(
    span: Span,
    elements: Vec<Element>,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    // Define PendingLookup enum locally since we removed the legacy AST module
    #[derive(Debug, Clone, PartialEq)]
    enum PendingLookup {
        None,
        Static,
        Dynamic,
    }

    let mut soup: Vec<RcExpr> = Vec::with_capacity(elements.len() * 2);
    let mut lookup = PendingLookup::None;

    let mut idx = 0;
    while idx < elements.len() {
        // Check for BracketBlock — needs lookahead to consume the return expr
        if let Element::BracketBlock(ref bracket) = elements[idx] {
            let bracket_span = text_range_to_span(bracket.syntax().text_range());
            let smid = desugarer.new_smid(bracket_span);

            let pair_name = bracket.bracket_pair_name().ok_or_else(|| {
                CoreError::InvalidEmbedding(
                    "bracket block missing bracket pair name".to_string(),
                    smid,
                )
            })?;

            // Consume return expression: expect `.expr` where expr may be a
            // chained dot-lookup like `result.stdout`.
            // We collect all consecutive `.name` continuations so that
            // `{ :io r: cmd }.r.field` desugars to `io.bind(cmd, λ(r).
            // io.return(r.field))` rather than `io.bind(...).field`.
            let return_elems_slice = if idx + 2 < elements.len() {
                // Next should be dot operator, then an expression
                let dot_elem = &elements[idx + 1];
                let is_dot = dot_elem
                    .as_operator_identifier()
                    .map(|op| op.text() == ".")
                    .unwrap_or(false);
                if is_dot {
                    let ret_start = idx + 2;
                    // Consume all consecutive `.name` continuations into
                    // the return expression.
                    let mut ret_end = ret_start + 1;
                    while ret_end + 1 < elements.len() {
                        let maybe_dot = &elements[ret_end];
                        let is_chain_dot = maybe_dot
                            .as_operator_identifier()
                            .map(|op| op.text() == ".")
                            .unwrap_or(false);
                        if is_chain_dot && ret_end + 1 < elements.len() {
                            // Peek at what follows the dot: only consume if
                            // it is a simple name (not another block/bracket/etc.)
                            let after_dot = &elements[ret_end + 1];
                            if after_dot.as_normal_identifier().is_some()
                                || matches!(after_dot, Element::ParenExpr(_))
                            {
                                ret_end += 2;
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    idx = ret_end; // advance past all consumed elements
                    &elements[ret_start..ret_end]
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "monadic bracket block requires a return expression (e.g. ⟦ ... ⟧.expr)"
                            .to_string(),
                        smid,
                    ));
                }
            } else {
                return Err(CoreError::InvalidEmbedding(
                    "monadic bracket block requires a return expression (e.g. ⟦ ... ⟧.expr)"
                        .to_string(),
                    smid,
                ));
            };

            // Look up the monad spec and desugar the bind chain
            let spec = desugarer
                .monad_spec(&pair_name)
                .cloned()
                .ok_or_else(|| CoreError::NoMonadSpec(pair_name.clone(), smid))?;

            let bracket_decls: Vec<rowan_ast::Declaration> = bracket.declarations().collect();
            let monadic_expr =
                desugar_monadic_block(smid, bracket_decls, return_elems_slice, &spec, desugarer)?;
            soup.push(monadic_expr);
            continue;
        }

        // Check for Block element with monadic metadata followed by .expr
        if let Element::Block(ref block) = elements[idx] {
            if let Some(spec) = extract_block_monad_spec_from_raw(block) {
                let block_span = text_range_to_span(block.syntax().text_range());
                let smid = desugarer.new_smid(block_span);

                // Check for .expr pattern following the block.
                // Consume all consecutive `.name` continuations into the
                // return expression so that `{ :io r: cmd }.r.field`
                // desugars to `io.bind(cmd, λ(r). io.return(r.field))`.
                if idx + 2 < elements.len() {
                    let dot_elem = &elements[idx + 1];
                    let is_dot = dot_elem
                        .as_operator_identifier()
                        .map(|op| op.text() == ".")
                        .unwrap_or(false);
                    if is_dot {
                        let ret_start = idx + 2;
                        let mut ret_end = ret_start + 1;
                        while ret_end + 1 < elements.len() {
                            let maybe_dot = &elements[ret_end];
                            let is_chain_dot = maybe_dot
                                .as_operator_identifier()
                                .map(|op| op.text() == ".")
                                .unwrap_or(false);
                            if is_chain_dot && ret_end + 1 < elements.len() {
                                let after_dot = &elements[ret_end + 1];
                                if after_dot.as_normal_identifier().is_some()
                                    || matches!(after_dot, Element::ParenExpr(_))
                                {
                                    ret_end += 2;
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                        idx = ret_end;
                        let block_decls: Vec<rowan_ast::Declaration> =
                            block.declarations().collect();
                        let monadic_expr = desugar_monadic_block(
                            smid,
                            block_decls,
                            &elements[ret_start..ret_end],
                            &spec,
                            desugarer,
                        )?;
                        soup.push(monadic_expr);
                        continue;
                    }
                }
                // No .expr following — fall through to normal block desugaring
            }
        }

        idx += 1;
        let expr = elements[idx - 1].desugar(desugarer)?;
        let top: Option<RcExpr> = soup.last().cloned();

        match &*expr.inner {
            // Insert explicit call ops
            Expr::ArgTuple(_, _) => {
                soup.push(RcExpr::from(ops::call()));
                soup.push(expr.clone());
            }
            // Initial name in lookup chains must become a var
            Expr::Name(s, ref n) => {
                if lookup == PendingLookup::Static {
                    // simple static lookup
                    soup.pop();
                    if let Some(dlet) = soup.pop() {
                        let rebodied = dlet.rebody(core::var(*s, desugarer.var(n)));
                        // Convert to OtherLet to prevent subsequent static lookups
                        let fixed_rebodied = match &*rebodied.inner {
                            Expr::Let(smid, scope, LetType::DefaultBlockLet) => {
                                RcExpr::from(Expr::Let(*smid, scope.clone(), LetType::OtherLet))
                            }
                            _ => rebodied,
                        };
                        soup.push(fixed_rebodied);
                    } else {
                        panic!("Expected default let");
                    }
                    lookup = PendingLookup::None;
                } else if lookup == PendingLookup::Dynamic {
                    // simple dynamic lookup
                    soup.push(expr.clone());
                    lookup = PendingLookup::None;
                } else {
                    soup.push(core::var(*s, desugarer.var(n)))
                }
            }
            Expr::Operator(s, _, _, ref body) => {
                // If we're propagating a dot, we must classify it first
                if body.inner.is_pseudodot() {
                    if soup.is_empty() {
                        soup.push(RcExpr::from(Expr::ExprAnaphor(
                            *s,
                            Anaphor::Implicit(*s, ImplicitAnaphorSide::Left),
                        )));
                        lookup = PendingLookup::Dynamic;
                    } else if top.unwrap().inner.is_default_let() {
                        lookup = PendingLookup::Static;
                    } else {
                        lookup = PendingLookup::Dynamic;
                    }
                }
                soup.push(expr.clone());
            }
            _ => {
                if lookup == PendingLookup::Dynamic {
                    soup.push(dynamise::dynamise(&expr)?);
                    lookup = PendingLookup::None;
                } else if lookup == PendingLookup::Static {
                    soup.pop();
                    if let Some(dlet) = soup.pop() {
                        let rebodied = dlet.rebody(expr.clone());
                        // Convert to OtherLet to prevent subsequent static lookups
                        let fixed_rebodied = match &*rebodied.inner {
                            Expr::Let(smid, scope, LetType::DefaultBlockLet) => {
                                RcExpr::from(Expr::Let(*smid, scope.clone(), LetType::OtherLet))
                            }
                            _ => rebodied,
                        };
                        soup.push(fixed_rebodied);
                    } else {
                        panic!("Expected default let");
                    }
                    lookup = PendingLookup::None;
                } else {
                    soup.push(expr.clone());
                }
            }
        }
    }

    // Optimise away the soup wrapping in cases where it is definitely
    // not needed but leave vars for cooking phase (in case of sections)
    if soup.len() == 1 {
        match &*soup.first().unwrap().inner {
            Expr::Var(_, _) => Ok(RcExpr::from(Expr::Soup(desugarer.new_smid(span), soup))),
            _ => Ok(soup.first().unwrap().clone()),
        }
    } else {
        Ok(RcExpr::from(Expr::Soup(desugarer.new_smid(span), soup)))
    }
}

/// Extract just the declaration name without desugaring body
fn extract_declaration_name(decl: &rowan_ast::Declaration) -> Result<String, CoreError> {
    if let Some(head) = decl.head() {
        let kind = head.classify_declaration();
        match kind {
            rowan_ast::DeclarationKind::Property(prop) => Ok(prop.text().to_string()),
            rowan_ast::DeclarationKind::Function(func, _) => Ok(func.text().to_string()),
            rowan_ast::DeclarationKind::Prefix(_, op, _) => Ok(op.text().to_string()),
            rowan_ast::DeclarationKind::Postfix(_, _, op) => Ok(op.text().to_string()),
            rowan_ast::DeclarationKind::Binary(_, _, op, _) => Ok(op.text().to_string()),
            rowan_ast::DeclarationKind::Nullary(_, op) => Ok(op.text().to_string()),
            rowan_ast::DeclarationKind::BracketPair(_, bracket_expr, _) => {
                bracket_expr.bracket_pair_name().ok_or_else(|| {
                    CoreError::InvalidEmbedding(
                        "bracket pair declaration has no bracket pair name".to_string(),
                        Smid::default(),
                    )
                })
            }
            rowan_ast::DeclarationKind::BracketBlockDef(_, bracket_expr) => {
                bracket_expr.bracket_pair_name().ok_or_else(|| {
                    CoreError::InvalidEmbedding(
                        "bracket block definition has no bracket pair name".to_string(),
                        Smid::default(),
                    )
                })
            }
            rowan_ast::DeclarationKind::MalformedHead(_) => Err(CoreError::InvalidEmbedding(
                "malformed declaration head".to_string(),
                Smid::default(),
            )),
        }
    } else {
        Err(CoreError::InvalidEmbedding(
            "missing declaration head".to_string(),
            Smid::default(),
        ))
    }
}

/// Convert Rowan declaration to binding for let expression
fn rowan_declaration_to_binding(
    decl: &rowan_ast::Declaration,
    desugarer: &mut Desugarer,
) -> Result<(Binder<String>, Embed<RcExpr>), CoreError> {
    // Extract declaration name first and push to stack before body desugaring
    let name = extract_declaration_name(decl)?;
    desugarer.push(&name);

    let components = extract_rowan_declaration_components(decl, desugarer)?;

    // Process metadata for desugar-phase information
    let (core_meta, metadata) = match components.metadata {
        Some(m) => {
            let mut core_meta = normalise_metadata(&m);
            let metadata: DesugarPhaseDeclarationMetadata =
                core_meta.read_metadata().unwrap_or_default();
            (Some(core_meta), metadata)
        }
        None => {
            let metadata = DesugarPhaseDeclarationMetadata::default();
            (None, metadata)
        }
    };

    // Handle documentation if in base file
    if let Some(doc) = &metadata.doc {
        if desugarer.in_base_file() {
            desugarer.record_doc(doc.clone(), &components.name, components.args.clone());
        }
    }

    // Handle target metadata
    if let Some(target) = &metadata.target {
        desugarer.record_target(
            target.to_string(),
            metadata.doc.unwrap_or_default(),
            metadata.format,
            metadata.validations.unwrap_or_default(),
        );
    }

    // Handle imports
    let mut imports = Vec::new();
    if let Some(inputs) = metadata.imports {
        for input in inputs {
            let import_smid = desugarer.new_smid(components.span);
            imports.push(
                desugarer
                    .translate_import(import_smid, input)
                    .expect("failure translating import"),
            );
        }
    }

    // Register monad spec from BracketBlockDef body (new-style: ⟦{}⟧: { :monad bind: f return: r })
    // The body is a block with :monad block metadata and bind/return declarations.
    if let Some(head) = decl.head() {
        if matches!(
            head.classify_declaration(),
            rowan_ast::DeclarationKind::BracketBlockDef(_, _)
        ) {
            let spec_smid = desugarer.new_smid(components.span);
            let spec = extract_monad_spec_from_body(&components.body, spec_smid)?;
            if let Some(monad_spec) = spec {
                desugarer.register_monad_spec(components.name.clone(), monad_spec);
            }
        }
    }

    // Note: argument names were already resolved during body desugaring
    // We need to get the original FreeVars that were used, not create new ones

    // Desugar the body expression
    let mut expr = if let Some(embedding) = &metadata.embedding {
        if embedding == "core" {
            // For core embedding, we use the raw Rowan body before desugaring
            let raw_body = if let Some(body) = decl.body() {
                if let Some(body_soup) = body.soup() {
                    body_soup
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "empty declaration body in core embedding".to_string(),
                        desugarer.new_smid(components.span),
                    ));
                }
            } else {
                return Err(CoreError::InvalidEmbedding(
                    "missing declaration body in core embedding".to_string(),
                    desugarer.new_smid(components.span),
                ));
            };

            core_from_rowan_embedding(desugarer, &raw_body)?
        } else {
            return Err(CoreError::UnknownEmbedding(embedding.clone()));
        }
    } else {
        // Body is already desugared and variable resolution was done during soup processing
        components.body
    };

    // Wrap in lambda if there are arguments
    if !components.args.is_empty() {
        expr = core::lam(
            desugarer.new_annotated_smid(components.span, &components.name),
            components.arg_vars,
            expr.clone(),
        );
    }

    // Wrap in operator if this is an operator declaration
    if components.is_operator {
        expr = RcExpr::from(Expr::Operator(
            desugarer.new_smid(components.span),
            components
                .fixity
                .or(metadata.fixity)
                .unwrap_or(Fixity::InfixLeft),
            metadata.precedence.unwrap_or(50),
            expr.clone(),
        ));
    }

    // Attach metadata if present
    if let Some(m) = core_meta {
        let stripped_meta = strip_desugar_phase_metadata(&m);
        if !matches!(&*stripped_meta.inner, Expr::ErrEliminated) {
            expr = RcExpr::from(Expr::Meta(
                desugarer.new_smid(components.span),
                expr,
                stripped_meta,
            ));
        }
    }

    // Get the declared variable from environment
    let declared_var = desugarer
        .env()
        .get(&components.name)
        .expect("declaration var should have been prepared in block")
        .clone();

    // Embed in imports if required
    expr = imports.iter().rfold(expr, |acc, import| import.rebody(acc));

    let ret = (Binder(declared_var), Embed(expr));

    // Note: Don't pop the environment here - it's shared across all declarations
    // The caller (Unit or Block) will pop it after processing all declarations
    desugarer.pop();
    Ok(ret)
}

/// Block desugaring - proper implementation following legacy architecture
impl Desugarable for rowan_ast::Block {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        let span = text_range_to_span(self.syntax().text_range());

        // Transform metadata for attachment later
        let mut metadata = if let Some(meta) = self.meta() {
            if let Some(meta_soup) = meta.soup() {
                Some(normalise_metadata(&meta_soup.desugar(desugarer)?))
            } else {
                None
            }
        } else {
            None
        };

        // Extract block metadata for desugar-phase processing
        let block_meta: DesugarPhaseBlockMetadata = match metadata {
            Some(ref mut concrete_meta) => concrete_meta.read_metadata().unwrap_or_default(),
            None => DesugarPhaseBlockMetadata::default(),
        };

        // Handle imports
        let mut imports = Vec::new();
        if let Some(inputs) = block_meta.imports {
            let meta_smid = metadata.as_ref().unwrap().smid();
            for input in inputs {
                imports.push(
                    desugarer
                        .translate_import(meta_smid, input)
                        .expect("failure translating import"),
                );
            }
        }

        // Collect declaration names for environment
        let keys: Vec<_> = self
            .declarations()
            .filter_map(|decl| {
                if let Some(head) = decl.head() {
                    let kind = head.classify_declaration();
                    match kind {
                        rowan_ast::DeclarationKind::Property(prop) => Some(prop.text().to_string()),
                        rowan_ast::DeclarationKind::Function(func, _) => {
                            Some(func.text().to_string())
                        }
                        rowan_ast::DeclarationKind::Prefix(_, op, _) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::Postfix(_, _, op) => {
                            Some(op.text().to_string())
                        }
                        rowan_ast::DeclarationKind::Binary(_, _, op, _) => {
                            Some(op.text().to_string())
                        }
                        rowan_ast::DeclarationKind::Nullary(_, op) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::BracketPair(_, bracket_expr, _) => {
                            bracket_expr.bracket_pair_name()
                        }
                        rowan_ast::DeclarationKind::BracketBlockDef(_, bracket_expr) => {
                            bracket_expr.bracket_pair_name()
                        }
                        rowan_ast::DeclarationKind::MalformedHead(_) => None,
                    }
                } else {
                    None
                }
            })
            .collect();

        desugarer.env_mut().push_keys(keys);

        // Convert declarations to bindings
        let bindings = self
            .declarations()
            .map(|decl| rowan_declaration_to_binding(&decl, desugarer))
            .collect::<Result<Vec<_>, CoreError>>()?;

        // Create block body mapping names to variables
        let body_elements: BlockMap<RcExpr> = bindings
            .iter()
            .map(|(binder, embed)| {
                (
                    binder.0.clone().pretty_name.unwrap(),
                    core::var(embed.0.smid(), binder.0.clone()),
                )
            })
            .collect();

        // Create body - check for parse-embed override
        let body = if let Some(embed_key) = block_meta.parse_embed {
            let fv_opt = desugarer.env().get(&embed_key).cloned();
            match fv_opt {
                Some(fv) => core::var(desugarer.new_smid(span), fv),
                None => {
                    let meta_smid = metadata.as_ref().map(|m| m.smid()).unwrap_or_default();
                    RcExpr::from(Expr::ErrUnresolved(meta_smid, embed_key))
                }
            }
        } else {
            RcExpr::from(Expr::Block(desugarer.new_smid(span), body_elements))
        };

        // Create let expression with bindings
        let mut expr = RcExpr::from(Expr::Let(
            desugarer.new_smid(span),
            Scope::new(Rec::new(bindings), body),
            LetType::DefaultBlockLet,
        ));

        // Attach metadata if present
        if let Some(m) = metadata {
            let stripped_meta = strip_desugar_phase_metadata(&m);
            if !matches!(&*stripped_meta.inner, Expr::ErrEliminated) {
                expr = RcExpr::from(Expr::Meta(desugarer.new_smid(span), expr, stripped_meta));
            }
        }

        // Embed in imports if required
        expr = imports.iter().rfold(expr, |acc, import| import.rebody(acc));

        desugarer.env_mut().pop();
        Ok(expr)
    }
}

/// Unit desugaring — proper implementation following legacy architecture
impl Desugarable for rowan_ast::Unit {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        let span = text_range_to_span(self.syntax().text_range());

        // Transform metadata for attachment later
        let mut metadata = if let Some(meta) = self.meta() {
            if let Some(meta_soup) = meta.soup() {
                Some(normalise_metadata(&meta_soup.desugar(desugarer)?))
            } else {
                None
            }
        } else {
            None
        };

        // Extract unit metadata for desugar-phase processing
        let unit_meta: DesugarPhaseBlockMetadata = match metadata {
            Some(ref mut concrete_meta) => concrete_meta.read_metadata().unwrap_or_default(),
            None => DesugarPhaseBlockMetadata::default(),
        };

        // Handle imports
        let mut imports = Vec::new();
        if let Some(inputs) = unit_meta.imports {
            let meta_smid = metadata.as_ref().unwrap().smid();
            for input in inputs {
                imports.push(
                    desugarer
                        .translate_import(meta_smid, input)
                        .expect("failure translating import"),
                );
            }
        }

        // Collect declaration names for environment
        let keys: Vec<_> = self
            .declarations()
            .filter_map(|decl| {
                if let Some(head) = decl.head() {
                    let kind = head.classify_declaration();
                    match kind {
                        rowan_ast::DeclarationKind::Property(prop) => Some(prop.text().to_string()),
                        rowan_ast::DeclarationKind::Function(func, _) => {
                            Some(func.text().to_string())
                        }
                        rowan_ast::DeclarationKind::Prefix(_, op, _) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::Postfix(_, _, op) => {
                            Some(op.text().to_string())
                        }
                        rowan_ast::DeclarationKind::Binary(_, _, op, _) => {
                            Some(op.text().to_string())
                        }
                        rowan_ast::DeclarationKind::Nullary(_, op) => Some(op.text().to_string()),
                        rowan_ast::DeclarationKind::BracketPair(_, bracket_expr, _) => {
                            bracket_expr.bracket_pair_name()
                        }
                        rowan_ast::DeclarationKind::BracketBlockDef(_, bracket_expr) => {
                            bracket_expr.bracket_pair_name()
                        }
                        rowan_ast::DeclarationKind::MalformedHead(_) => None,
                    }
                } else {
                    None
                }
            })
            .collect();

        desugarer.env_mut().push_keys(keys);

        // Convert declarations to bindings
        let bindings = self
            .declarations()
            .map(|decl| rowan_declaration_to_binding(&decl, desugarer))
            .collect::<Result<Vec<_>, CoreError>>()?;

        // Create unit body mapping names to variables
        let body_elements: BlockMap<RcExpr> = bindings
            .iter()
            .map(|(binder, embed)| {
                (
                    binder.0.clone().pretty_name.unwrap(),
                    core::var(embed.0.smid(), binder.0.clone()),
                )
            })
            .collect();

        // Create body - check for parse-embed override
        let body = if let Some(embed_key) = unit_meta.parse_embed {
            let fv_opt = desugarer.env().get(&embed_key).cloned();
            match fv_opt {
                Some(fv) => core::var(desugarer.new_smid(span), fv),
                None => {
                    let meta_smid = metadata.as_ref().map(|m| m.smid()).unwrap_or_default();
                    RcExpr::from(Expr::ErrUnresolved(meta_smid, embed_key))
                }
            }
        } else {
            RcExpr::from(Expr::Block(desugarer.new_smid(span), body_elements.clone()))
        };

        // Create let expression with bindings
        let mut expr = RcExpr::from(Expr::Let(
            desugarer.new_smid(span),
            Scope::new(Rec::new(bindings), body),
            LetType::DefaultBlockLet,
        ));

        // Attach metadata if present.
        //
        // Special case: when a file contains only a bare expression (no
        // declarations) and the parsed content is a real evaluable expression
        // (not a simple doc/target/import annotation block), use that expression
        // as the unit body rather than attaching it as metadata.  This allows
        // single-expression files such as
        //
        //   { :io result: io.shell("echo hello") }.result.stdout
        //
        // to behave like an `-e` evaluand.
        if let Some(m) = metadata {
            let stripped_meta = strip_desugar_phase_metadata(&m);
            if !matches!(&*stripped_meta.inner, Expr::ErrEliminated) {
                // Only treat the unit as a bare evaluand when it has no
                // declarations AND the raw metadata soup starts with a
                // block-like element.  This distinguishes genuine
                // block-dot evaluands (`{ :io r: cmd }.r.stdout`) from
                // erroneous forms such as `result = 42` whose first raw
                // element is a name, not a block.
                let is_bare_expression = body_elements.is_empty()
                    && !matches!(&*stripped_meta.inner, Expr::Block(_, _))
                    && unit_meta_starts_with_block(self);
                if is_bare_expression {
                    // Replace the empty-body let with one whose body is the
                    // bare expression, making it the evaluand.
                    expr = RcExpr::from(Expr::Let(
                        desugarer.new_smid(span),
                        Scope::new(Rec::new(vec![]), stripped_meta),
                        LetType::DefaultBlockLet,
                    ));
                } else {
                    expr = RcExpr::from(Expr::Meta(desugarer.new_smid(span), expr, stripped_meta));
                }
            }
        }

        // Embed in imports if required
        expr = imports.iter().rfold(expr, |acc, import| import.rebody(acc));

        desugarer.env_mut().pop();
        Ok(expr)
    }
}
