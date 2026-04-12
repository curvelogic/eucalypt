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
        binding::{BoundVar, Scope, Var},
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
use rowan::{ast::AstNode, TextRange};

/// Convert a TextRange to a Span
fn text_range_to_span(range: TextRange) -> Span {
    let start = ByteIndex(range.start().into());
    let end = ByteIndex(range.end().into());
    Span::new(start, end)
}

/// Return type of `desugar_declaration_body_with_patterns`:
/// `(body_expr, lambda_param_names, lambda_param_vars)`
type PatternBodyResult = (RcExpr, Vec<String>, Vec<String>);

/// Entry for a single field binding in a destructuring let.
struct FieldBinding {
    /// Name of the field in the source block (for Lookup)
    field_name: String,
    /// Name of the binding in the body
    binding_var: String,
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
    /// Name of the binding in the body
    binding_var: String,
}

/// Entry for the tail binding in a head/tail list pattern.
struct ListTailBinding {
    /// Number of head elements to skip (i.e., the drop count)
    drop_count: usize,
    /// Name of the tail binding in the body
    binding_var: String,
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
#[derive(Clone)]
enum ParamPattern {
    /// Simple identifier: `x`
    Simple(String),
    /// Block destructuring: `{x y}` or `{x: a  y: b}` or `{x: [a, b]}`.
    ///
    /// Each entry is `(field_name, value_pattern)`. For shorthand `{x}`,
    /// field is `"x"` and pattern is `Simple("x")`. For rename `{x: a}`,
    /// field is `"x"` and pattern is `Simple("a")`. For nested `{x: [a, b]}`,
    /// field is `"x"` and pattern is `List([Simple("a"), Simple("b")], None)`.
    Block(Vec<(String, ParamPattern)>),
    /// Fixed-length list destructuring: `[a, b, c]`, or head/tail pattern
    /// `[x : xs]` or `[a, b : rest]`.
    ///
    /// Elements are `ParamPattern` to support nesting: `[a, [b: c]]` has
    /// a `Simple("a")` and a `List(["b"], Some("c"))` element.
    /// The tail (after `:`) is always a simple name if present.
    List(Vec<ParamPattern>, Option<String>),
}

/// Parse a block parameter pattern `{x y}` or `{x: a  y: b}` from a
/// Rowan `Block` AST node in a function parameter position.
///
/// Block patterns may contain:
/// - Shorthand names in block metadata: `{x y}` → `[(x, Simple(x)), (y, Simple(y))]`
/// - Rename declarations: `{x: a  y: b}` → `[(x, Simple(a)), (y, Simple(b))]`
/// - Nested patterns: `{x: [a, b]}` → `[(x, List([Simple(a), Simple(b)], None))]`
/// - Mixed: `{x  y: b}` → `[(x, Simple(x)), (y, Simple(b))]`
fn parse_block_pattern(block: &rowan_ast::Block) -> Result<Vec<(String, ParamPattern)>, CoreError> {
    let mut fields: Vec<(String, ParamPattern)> = Vec::new();

    // Extract shorthand names from block metadata soup (e.g. `x y` in `{x y}`)
    if let Some(meta) = block.meta() {
        if let Some(soup) = meta.soup() {
            for element in soup.elements() {
                if let rowan_ast::Element::Name(name) = element {
                    if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) = name.identifier()
                    {
                        let field_name = normal.value().to_string();
                        // Shorthand: field name = binding name
                        fields.push((field_name.clone(), ParamPattern::Simple(field_name)));
                    }
                }
            }
        }
    }

    // Extract rename/nested fields from declarations (e.g. `x: a  y: b  z: [a, b]`)
    for decl in block.declarations() {
        if let Some(head) = decl.head() {
            let kind = head.classify_declaration();
            if let rowan_ast::DeclarationKind::Property(prop) = kind {
                let field_name = prop.value().to_string();
                let value_pattern = if let Some(body) = decl.body() {
                    if let Some(body_soup) = body.soup() {
                        match body_soup.singleton() {
                            Some(rowan_ast::Element::Name(name)) => {
                                if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) =
                                    name.identifier()
                                {
                                    ParamPattern::Simple(normal.value().to_string())
                                } else {
                                    // Not a normal identifier — use field name as fallback
                                    ParamPattern::Simple(field_name.clone())
                                }
                            }
                            Some(rowan_ast::Element::List(list)) => {
                                // Nested list pattern: `{x: [a, b]}`
                                match parse_list_pattern(&list)? {
                                    Some((heads, tail)) => ParamPattern::List(heads, tail),
                                    None => ParamPattern::Simple(field_name.clone()),
                                }
                            }
                            Some(rowan_ast::Element::Block(nested_block)) => {
                                // Nested block pattern: `{x: {a b}}`
                                let nested_fields = parse_block_pattern(&nested_block)?;
                                ParamPattern::Block(nested_fields)
                            }
                            _ => {
                                // Complex expression in body — use field name as fallback
                                ParamPattern::Simple(field_name.clone())
                            }
                        }
                    } else {
                        ParamPattern::Simple(field_name.clone())
                    }
                } else {
                    // No body in declaration — field name = binding name (shorthand)
                    ParamPattern::Simple(field_name.clone())
                };
                fields.push((field_name, value_pattern));
            }
        }
    }

    Ok(fields)
}

/// Parsed list pattern: (head elements, optional tail name).
type ListPattern = (Vec<ParamPattern>, Option<String>);

/// Parse a list parameter pattern from a Rowan `List` AST node in a
/// function parameter position.
///
/// Handles both fixed-length patterns `[a, b, c]` and head/tail
/// patterns `[x : xs]` and `[a, b : rest]`.
///
/// Returns `(head_elements, tail)` where `tail` is `None` for
/// fixed-length patterns and `Some(tail_name)` for head/tail patterns.
fn parse_list_pattern(list: &rowan_ast::List) -> Result<Option<ListPattern>, CoreError> {
    let has_colon = list.has_colon();
    let all_items: Vec<_> = list.items().collect();

    if has_colon {
        // Head/tail pattern: items before the colon are heads,
        // the last item after the colon is the tail.
        if all_items.len() < 2 {
            return Ok(None); // Need at least one head and one tail
        }
        let mut heads = Vec::new();
        for item in &all_items[..all_items.len() - 1] {
            match parse_soup_as_pattern(item)? {
                Some(pat) => heads.push(pat),
                None => return Ok(None),
            }
        }
        // Last item (tail) is always a simple name
        let tail_item = all_items.last().unwrap();
        if let Some(rowan_ast::Element::Name(name)) = tail_item.singleton() {
            if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) = name.identifier() {
                let tail = normal.value().to_string();
                Ok(Some((heads, Some(tail))))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    } else {
        // Fixed-length pattern: elements may be names or nested patterns
        let mut elements = Vec::new();
        for item in &all_items {
            match parse_soup_as_pattern(item)? {
                Some(pat) => elements.push(pat),
                None => return Ok(None),
            }
        }
        if elements.is_empty() {
            Ok(None)
        } else {
            Ok(Some((elements, None)))
        }
    }
}

/// Parse a single soup element as a pattern (name, nested list, or nested block).
///
/// Returns `Err` for unsupported nesting, `Ok(None)` for non-pattern elements.
fn parse_soup_as_pattern(soup: &rowan_ast::Soup) -> Result<Option<ParamPattern>, CoreError> {
    match soup.singleton() {
        Some(rowan_ast::Element::Name(name)) => {
            if let Some(rowan_ast::Identifier::NormalIdentifier(normal)) = name.identifier() {
                Ok(Some(ParamPattern::Simple(normal.value().to_string())))
            } else {
                Ok(None)
            }
        }
        Some(rowan_ast::Element::List(list)) => {
            Ok(parse_list_pattern(&list)?.map(|(heads, tail)| ParamPattern::List(heads, tail)))
        }
        Some(rowan_ast::Element::Block(block)) => {
            parse_block_pattern(&block).map(|fields| Some(ParamPattern::Block(fields)))
        }
        _ => Ok(None),
    }
}

/// Parse a function parameter soup into a `ParamPattern`.
///
/// Returns `Err` for unsupported nesting, `Ok(None)` for non-pattern elements.
fn parse_param_pattern(soup: &rowan_ast::Soup) -> Result<Option<ParamPattern>, CoreError> {
    parse_soup_as_pattern(soup)
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
    // Raw data for block patterns: (synthetic_name, [(field_name, binding_var_name)])
    // binding_var_name is either a user-visible name (Simple) or a synthetic __pN (nested).
    let mut block_raw: Vec<(String, Vec<(String, String)>)> = Vec::new();
    // Raw data for list patterns: (synthetic_name, [element_var_names], tail_name?)
    let mut list_raw: Vec<(String, Vec<String>, Option<String>)> = Vec::new();
    // Ordered sequence of lets to emit — preserves argument order for correct nesting.
    let mut let_order: Vec<DestructureLet> = Vec::new();
    let mut synthetic_counter = 0usize;

    // Work queue: (synthetic_name, pattern_to_expand).
    // Top-level non-Simple patterns seed the queue; nested patterns add to it.
    // Using a VecDeque for BFS ordering, which preserves intuitive argument order.
    let mut work_queue: std::collections::VecDeque<(String, ParamPattern)> =
        std::collections::VecDeque::new();

    // Initial pass: top-level patterns become lambda binders.
    for pattern in patterns {
        match pattern {
            ParamPattern::Simple(name) => {
                all_env_names.push(name.clone());
                lambda_param_names.push(name.clone());
            }
            nested => {
                let synthetic_name = format!("__p{}", synthetic_counter);
                synthetic_counter += 1;
                all_env_names.push(synthetic_name.clone());
                lambda_param_names.push(synthetic_name.clone());
                work_queue.push_back((synthetic_name, nested.clone()));
            }
        }
    }

    // Drain the work queue — handles arbitrary nesting depth.
    while let Some((syn_name, pat)) = work_queue.pop_front() {
        match pat {
            ParamPattern::List(element_patterns, tail_name) => {
                let mut element_names = Vec::new();
                for elem in &element_patterns {
                    match elem {
                        ParamPattern::Simple(name) => {
                            all_env_names.push(name.clone());
                            element_names.push(name.clone());
                        }
                        nested => {
                            let nested_name = format!("__p{}", synthetic_counter);
                            synthetic_counter += 1;
                            all_env_names.push(nested_name.clone());
                            element_names.push(nested_name.clone());
                            work_queue.push_back((nested_name, nested.clone()));
                        }
                    }
                }
                if let Some(tail) = &tail_name {
                    all_env_names.push(tail.clone());
                }
                list_raw.push((syn_name.clone(), element_names, tail_name));
                let_order.push(DestructureLet::List(ListDestructureEntry {
                    synthetic_name: syn_name,
                    elements: Vec::new(),
                    tail: None,
                }));
            }
            ParamPattern::Block(fields) => {
                let mut field_var_names: Vec<(String, String)> = Vec::new();
                for (field_name, value_pat) in fields {
                    match value_pat {
                        ParamPattern::Simple(binding_name) => {
                            all_env_names.push(binding_name.clone());
                            field_var_names.push((field_name, binding_name));
                        }
                        nested => {
                            let nested_name = format!("__p{}", synthetic_counter);
                            synthetic_counter += 1;
                            all_env_names.push(nested_name.clone());
                            field_var_names.push((field_name, nested_name.clone()));
                            work_queue.push_back((nested_name, nested));
                        }
                    }
                }
                block_raw.push((syn_name.clone(), field_var_names));
                let_order.push(DestructureLet::Block(DestructureEntry {
                    synthetic_name: syn_name,
                    fields: Vec::new(),
                }));
            }
            ParamPattern::Simple(_) => unreachable!("Simple patterns handled in initial pass"),
        }
    }

    // Push all names into the environment as a single frame
    if !all_env_names.is_empty() {
        desugarer.env_mut().push_keys(all_env_names.iter().cloned());
    }

    // Collect var names for lambda binders (before desugaring body)
    let lambda_param_vars: Vec<String> = lambda_param_names
        .iter()
        .map(|name| desugarer.env().get(name).unwrap().clone())
        .collect();

    // Collect FreeVars for ALL env names (needed for nested destructure synthetics)
    let all_env_vars: std::collections::HashMap<String, String> = all_env_names
        .iter()
        .map(|name| (name.clone(), desugarer.env().get(name).unwrap().clone()))
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
        // Look up the synthetic param/binding FreeVar (the env frame has
        // already been popped, so we use the pre-collected map).
        let synthetic_fv = all_env_vars
            .get(slot.synthetic_name())
            .expect("synthetic name must be in env")
            .clone();

        let smid = desugarer.new_smid(span);
        let synthetic_var = RcExpr::from(Expr::Var(smid, Var::Free(synthetic_fv)));

        match slot {
            DestructureLet::Block(entry) => {
                // Each binding is: binding_var = Lookup(Var(synthetic), "field_name", None)
                let bindings: Vec<(String, RcExpr)> = entry
                    .fields
                    .iter()
                    .map(|fb| {
                        let lookup = RcExpr::from(Expr::Lookup(
                            smid,
                            synthetic_var.clone(),
                            fb.field_name.clone(),
                            None,
                        ));
                        (fb.binding_var.clone(), lookup)
                    })
                    .collect();

                body = RcExpr::from(Expr::Let(
                    smid,
                    close_let_scope(bindings, body),
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
                let mut bindings: Vec<(String, RcExpr)> = entry
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
                        (eb.binding_var.clone(), head_call)
                    })
                    .collect();

                // Add the tail binding if present
                if let Some(tb) = &entry.tail {
                    let mut tail_expr = synthetic_var.clone();
                    for _ in 0..tb.drop_count {
                        tail_expr = core::app(smid, core::bif(smid, "TAIL"), vec![tail_expr]);
                    }
                    bindings.push((tb.binding_var.clone(), tail_expr));
                }

                body = RcExpr::from(Expr::Let(
                    smid,
                    close_let_scope(bindings, body),
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

/// Return `true` if the raw Rowan declaration has `monad: true` in its
/// backtick metadata block.
///
/// This is used to detect monad namespace declarations such as:
/// ```eucalypt
/// ` { monad: true }
/// io: monad{…} { … }
/// ```
fn has_monad_true_in_raw_meta(decl: &rowan_ast::Declaration) -> bool {
    let meta = match decl.meta() {
        Some(m) => m,
        None => return false,
    };
    let soup = match meta.soup() {
        Some(s) => s,
        None => return false,
    };
    let elements: Vec<rowan_ast::Element> = soup.elements().collect();

    // The metadata soup must contain exactly one Block element.
    if let Some(rowan_ast::Element::Block(meta_block)) = elements.first() {
        for inner_decl in meta_block.declarations() {
            if let Some(head) = inner_decl.head() {
                if let rowan_ast::DeclarationKind::Property(prop) = head.classify_declaration() {
                    if prop.text() != "monad" {
                        continue;
                    }
                    // Check that the value is the identifier `true`.
                    if let Some(body) = inner_decl.body() {
                        if let Some(body_soup) = body.soup() {
                            let elems: Vec<rowan_ast::Element> = body_soup.elements().collect();
                            if elems.len() == 1 {
                                if let rowan_ast::Element::Name(name_elem) = &elems[0] {
                                    if let Some(id) = name_elem.identifier() {
                                        if id.text() == "true" {
                                            return true;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    false
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
            let bindings = scope.pattern.clone();
            let mut bind_name = None;
            let mut return_name = None;
            let mut namespace = None;
            for (key, val_expr) in &bindings {
                let val = extract_function_name_from_expr(val_expr);
                match key.as_str() {
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
                    let key = prop.value().to_string();
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
                                    name.identifier().map(|id| id.value().to_string())
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

/// Desugar a slice of soup elements forming a (possibly dot-chained) return
/// expression for a monadic block.
///
/// Handles:
/// - A single element: `name`, `(expr)`, a literal, etc.
/// - A dot chain: `name . field` or `name . field . subfield`
///
/// For a dot chain, the primary element is desugared first, then each
/// consecutive `. name` pair is applied as a `Lookup` on the result.
/// This allows `{ :io r: cmd }.r.stdout` to desugar the return
/// expression as `r.stdout` (i.e., `Lookup(Lookup(var(r), "stdout"), ...)`)
/// rather than leaving `.stdout` outside the `io.return` wrapper.
///
/// The bind names introduced by the enclosing monadic block must already be
/// in scope in `desugarer` when this function is called.
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
    // Desugar the primary expression (first element).
    let primary = elements[0].desugar(desugarer)?;
    let mut result = desugarer.varify(primary);

    // Walk the rest: expected pairs of (`.` operator, name).
    let mut i = 1;
    while i + 1 < elements.len() {
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
                    let field = id.value().to_string();
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

/// Desugar a monadic block using a monad spec.
///
/// Given a list of declarations `a: ma  b: mb`, a return element slice, and a
/// monad spec, produces:
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

    // Collect bind var names while names are in scope
    let bind_vars: Vec<String> = bind_names
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

    // Desugar the return expression (potentially a dot chain) now that
    // bind names are in scope.
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

/// Desugar a monadic block with implicit return semantics (no `.expr`).
///
/// Like `desugar_monadic_block`, but synthesises the return expression as a
/// block of all non-underscore bind names:
///
/// ```text
/// { :io r: cmd }   →   io.bind(cmd, λr. io.return({ r: r }))
/// ```
///
/// Bind names that are exactly `"_"` are excluded from the return block.
/// If all names are `"_"`, an empty block `{}` is returned instead.
fn desugar_monadic_block_implicit(
    smid: Smid,
    decls: Vec<rowan_ast::Declaration>,
    spec: &super::desugarer::MonadSpec,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    if decls.is_empty() {
        return Err(CoreError::EmptyMonadicBlock(smid));
    }

    // Push all bind names into scope first.
    let bind_names: Vec<String> = decls
        .iter()
        .map(extract_declaration_name)
        .collect::<Result<Vec<_>, CoreError>>()?;

    if !bind_names.is_empty() {
        desugarer.env_mut().push_keys(bind_names.iter().cloned());
    }

    // Collect the bind var names while names are in scope.
    let bind_vars: Vec<String> = bind_names
        .iter()
        .map(|name| desugarer.env().get(name).unwrap().clone())
        .collect();

    // Desugar all declaration bodies.
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

    // Synthesise the implicit return block from all non-underscore bind names.
    //
    // We need: return({a: a_λ, b: b_λ}) where a_λ, b_λ are the lambda-bound
    // variables from the bind chain.
    //
    // A bare Expr::Block({a: a_λ, b: b_λ}) compiles to a letrec whose bindings
    // shadow the lambda params — {a: a} self-references.  core::let_ and
    // core::default_let also self-reference because close_let_scope closes
    // binding values over the let's own names.
    //
    // Fix: build Let + Block manually.  The Let binding values are the
    // lambda-bound FreeVars (NOT closed over the Let's names).  The Block body
    // references the Let bindings via BoundVar(scope=0).  This way the Let
    // captures the lambda vars, and the Block reads from the Let.
    let non_underscore: Vec<(String, String)> = bind_names
        .iter()
        .zip(bind_vars.iter())
        .filter(|(name, _)| name.as_str() != "_")
        .map(|(name, fv)| (name.clone(), fv.clone()))
        .collect();

    let return_expr = if non_underscore.is_empty() {
        core::block(smid, std::iter::empty::<(String, RcExpr)>())
    } else {
        // Let bindings: each value is the lambda-bound FreeVar.
        // These must NOT be closed over the Let's own names.
        let let_bindings: Vec<(String, RcExpr)> = non_underscore
            .iter()
            .map(|(name, fv)| (name.clone(), core::var(smid, fv.clone())))
            .collect();

        // Body: Block where each key reads from the Let (BoundVar scope=0).
        let body_block = core::block(
            smid,
            non_underscore.iter().enumerate().map(|(i, (name, _))| {
                (
                    name.clone(),
                    RcExpr::from(Expr::Var(
                        smid,
                        Var::Bound(BoundVar {
                            scope: 0,
                            binder: i as u32,
                            name: Some(name.clone()),
                        }),
                    )),
                )
            }),
        );

        // Construct the Let Scope directly — skip close_let_scope which
        // would close the binding FreeVars over the Let's own names.
        RcExpr::from(Expr::Let(
            smid,
            Scope {
                pattern: let_bindings,
                body: body_block,
            },
            LetType::OtherLet,
        ))
    };

    // Pop bind names from scope.
    if !bind_names.is_empty() {
        desugarer.env_mut().pop();
    }

    // Build the bind chain using the same logic as desugar_monadic_block.
    use super::desugarer::MonadSpec;
    let mut result = match spec {
        MonadSpec::Explicit { return_name, .. } => {
            let return_fn = desugarer.varify(RcExpr::from(Expr::Name(smid, return_name.clone())));
            RcExpr::from(Expr::App(smid, return_fn, vec![return_expr]))
        }
        MonadSpec::Namespace(ns) => {
            let ns_var = desugarer.varify(RcExpr::from(Expr::Name(smid, ns.clone())));
            core::app(
                smid,
                core::lookup(smid, ns_var, "return", None),
                vec![return_expr],
            )
        }
    };

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
            Element::Block(ref block) => {
                // Check for monadic block with implicit return (no following .expr).
                // When a block is desugared as a single-element soup, we must apply
                // the same implicit-return logic as desugar_rowan_soup's Block path.
                if let Some(spec) = extract_block_monad_spec_from_raw(block) {
                    let ns_registered = if let super::desugarer::MonadSpec::Namespace(ref ns) = spec
                    {
                        desugarer.monad_namespace_spec(ns).is_some()
                    } else {
                        true
                    };
                    if ns_registered {
                        let span = text_range_to_span(block.syntax().text_range());
                        let smid = desugarer.new_smid(span);
                        let block_decls: Vec<rowan_ast::Declaration> =
                            block.declarations().collect();
                        return desugar_monadic_block_implicit(smid, block_decls, &spec, desugarer);
                    }
                }
                block.desugar(desugarer)
            }
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

                // Idiot brackets: desugar inner soup with bracket flag.
                //
                // Uses desugar_rowan_soup_bracket which processes elements
                // normally (grouping calls, resolving lookups) but produces
                // Soup with bracket=true.  The cook phase will then split
                // at catenation boundaries and collect items into a List.
                let inner = if let Some(soup) = bracket.soup() {
                    let elements: Vec<Element> = soup.elements().collect();
                    desugar_rowan_soup_bracket(span, elements, desugarer)?
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "empty bracket expression".to_string(),
                        smid,
                    ));
                };

                let bracket_fn_name = RcExpr::from(Expr::Name(smid, pair_name));
                let bracket_fn = desugarer.varify(bracket_fn_name);
                Ok(RcExpr::from(Expr::App(smid, bracket_fn, vec![inner])))
            }
            Element::BracketBlock(bracket) => {
                // BracketBlock element appearing in isolation (single-element soup, no .expr).
                // Use implicit return: synthesise { k1: k1, k2: k2, … } from bind names,
                // excluding any underscore-prefixed names.
                let span = text_range_to_span(self.syntax().text_range());
                let smid = desugarer.new_smid(span);

                let pair_name = bracket.bracket_pair_name().ok_or_else(|| {
                    CoreError::InvalidEmbedding(
                        "bracket block missing bracket pair name".to_string(),
                        smid,
                    )
                })?;

                let spec = desugarer
                    .monad_spec(&pair_name)
                    .cloned()
                    .ok_or_else(|| CoreError::NoMonadSpec(pair_name.clone(), smid))?;

                let bracket_decls: Vec<rowan_ast::Declaration> = bracket.declarations().collect();
                desugar_monadic_block_implicit(smid, bracket_decls, &spec, desugarer)
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
    pub arg_vars: Vec<String>,
    pub is_operator: bool,
    pub fixity: Option<crate::core::expr::Fixity>,
}

/// Helper to desugar declaration body with arguments in scope
/// Returns the desugared body and the var names for the arguments
fn desugar_declaration_body(
    decl: &rowan_ast::Declaration,
    desugarer: &mut Desugarer,
    args: &[String],
    span: Span,
) -> Result<(RcExpr, Vec<String>), CoreError> {
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
                    name: prop.value().to_string(),
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
                    .filter_map(|soup| parse_param_pattern(&soup).transpose())
                    .collect::<Result<Vec<_>, _>>()?;

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
                        name: func.value().to_string(),
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
                        name: func.value().to_string(),
                        args: lambda_param_names,
                        body,
                        arg_vars: lambda_param_vars,
                        is_operator: false,
                        fixity: None,
                    })
                }
            }
            rowan_ast::DeclarationKind::Prefix(_, op, arg) => {
                let args = vec![arg.value().to_string()];
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
                let args = vec![arg.value().to_string()];
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
                let args = vec![left.value().to_string(), right.value().to_string()];
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
            rowan_ast::DeclarationKind::BracketPair(_, bracket_expr, param_soup) => {
                let pair_name = bracket_expr.bracket_pair_name().ok_or_else(|| {
                    CoreError::InvalidEmbedding(
                        "bracket pair declaration has no bracket pair name".to_string(),
                        desugarer.new_smid(span),
                    )
                })?;
                // Parse the bracket parameter as a pattern (simple name,
                // list destructuring, or block destructuring)
                let pattern = parse_param_pattern(&param_soup)?.ok_or_else(|| {
                    CoreError::InvalidEmbedding(
                        "invalid bracket parameter pattern".to_string(),
                        desugarer.new_smid(span),
                    )
                })?;

                let (body, args, arg_vars) =
                    desugar_declaration_body_with_patterns(decl, desugarer, &[pattern], span)?;

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
                        let name_text = name
                            .identifier()
                            .map(|id| id.value().to_string())
                            .unwrap_or_else(|| name.syntax().text().to_string());
                        let name_expr = core::name(smid, name_text);
                        desugarer.varify(name_expr)
                    } else {
                        // Multiple names case: build [name, dot, name, dot, name] soup
                        let mut v = Vec::new();
                        let mut items: Vec<_> = names.to_vec();

                        // Process from end to beginning, just like legacy AST
                        while items.len() > 1 {
                            let name = items.pop().unwrap();
                            let name_text = name
                                .identifier()
                                .map(|id| id.value().to_string())
                                .unwrap_or_else(|| name.syntax().text().to_string());
                            v.push(core::name(smid, name_text));
                            v.push(acore::dot());
                        }

                        // Add the final name (the first one in the original order)
                        if let Some(final_name) = items.pop() {
                            let final_name_text = final_name
                                .identifier()
                                .map(|id| id.value().to_string())
                                .unwrap_or_else(|| final_name.syntax().text().to_string());
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
        RcExpr::from(crate::core::expr::Expr::Soup(smid, vec![expr], false))
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
    desugar_rowan_soup_inner(span, elements, desugarer, false)
}

fn desugar_rowan_soup_bracket(
    span: Span,
    elements: Vec<Element>,
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    desugar_rowan_soup_inner(span, elements, desugarer, true)
}

fn desugar_rowan_soup_inner(
    span: Span,
    elements: Vec<Element>,
    desugarer: &mut Desugarer,
    bracket: bool,
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

            // Look up the monad spec first.
            let spec = desugarer
                .monad_spec(&pair_name)
                .cloned()
                .ok_or_else(|| CoreError::NoMonadSpec(pair_name.clone(), smid))?;

            // Check for an explicit `.expr` return expression following the block.
            // Collect all consecutive `.name` continuations so that
            // `⟦ r: cmd ⟧.r.field` desugars to
            // `bind(cmd, λ(r). return(r.field))`.
            let has_dot_return = idx + 2 <= elements.len() && {
                let dot_elem = &elements[idx + 1];
                dot_elem
                    .as_operator_identifier()
                    .map(|op| op.text() == ".")
                    .unwrap_or(false)
            };

            let bracket_decls: Vec<rowan_ast::Declaration> = bracket.declarations().collect();

            let monadic_expr = if has_dot_return {
                // Explicit .expr — consume the return expression.
                let ret_start = idx + 2;
                let mut ret_end = ret_start + 1;
                while ret_end + 1 < elements.len() {
                    let is_chain_dot = elements[ret_end]
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
                desugar_monadic_block(
                    smid,
                    bracket_decls,
                    &elements[ret_start..ret_end],
                    &spec,
                    desugarer,
                )?
            } else {
                // Implicit return — synthesise { k1: k1, k2: k2, … } from bind names.
                idx += 1;
                desugar_monadic_block_implicit(smid, bracket_decls, &spec, desugarer)?
            };

            soup.push(monadic_expr);
            continue;
        }

        // Check for Block element with monadic metadata.
        //
        // When the monadic block is in generalised lookup position
        // (preceded by `.`), it is desugared with implicit return and
        // fed into the lookup handler — the LHS block's bindings are
        // visible inside the monadic block.
        //
        // When standalone (not preceded by `.`):
        // - If followed by `.expr`, desugar with explicit return
        // - Otherwise use implicit return if the namespace is registered
        if let Element::Block(ref block) = elements[idx] {
            if let Some(spec) = extract_block_monad_spec_from_raw(block) {
                let block_span = text_range_to_span(block.syntax().text_range());
                let smid = desugarer.new_smid(block_span);

                let ns_registered = if let super::desugarer::MonadSpec::Namespace(ref ns) = spec {
                    desugarer.monad_namespace_spec(ns).is_some()
                } else {
                    true
                };

                // In lookup position: desugar with implicit return
                // and fall through to the generalised lookup handler.
                if lookup != PendingLookup::None && ns_registered {
                    idx += 1;
                    let block_decls: Vec<rowan_ast::Declaration> = block.declarations().collect();
                    let monadic_expr =
                        desugar_monadic_block_implicit(smid, block_decls, &spec, desugarer)?;

                    // Feed into the generalised lookup handler
                    if lookup == PendingLookup::Dynamic {
                        soup.push(dynamise::dynamise(&monadic_expr)?);
                    } else if lookup == PendingLookup::Static {
                        soup.pop(); // remove dot
                        if let Some(dlet) = soup.pop() {
                            let rebodied = dlet.rebody(monadic_expr);
                            let fixed = match &*rebodied.inner {
                                Expr::Let(s, scope, LetType::DefaultBlockLet) => {
                                    RcExpr::from(Expr::Let(*s, scope.clone(), LetType::OtherLet))
                                }
                                _ => rebodied,
                            };
                            soup.push(fixed);
                        } else {
                            panic!("Expected default let for static monadic lookup");
                        }
                    }
                    lookup = PendingLookup::None;
                    continue;
                }

                let has_dot_return = idx + 2 <= elements.len() && {
                    let dot_elem = &elements[idx + 1];
                    dot_elem
                        .as_operator_identifier()
                        .map(|op| op.text() == ".")
                        .unwrap_or(false)
                };

                if has_dot_return {
                    // Explicit .expr — consume the return expression.
                    let ret_start = idx + 2;
                    let mut ret_end = ret_start + 1;
                    while ret_end + 1 < elements.len() {
                        let is_chain_dot = elements[ret_end]
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
                    let block_decls: Vec<rowan_ast::Declaration> = block.declarations().collect();
                    let monadic_expr = desugar_monadic_block(
                        smid,
                        block_decls,
                        &elements[ret_start..ret_end],
                        &spec,
                        desugarer,
                    )?;
                    soup.push(monadic_expr);
                    continue;
                } else if ns_registered {
                    // No .expr, standalone — use implicit return.
                    idx += 1;
                    let block_decls: Vec<rowan_ast::Declaration> = block.declarations().collect();
                    let monadic_expr =
                        desugar_monadic_block_implicit(smid, block_decls, &spec, desugarer)?;
                    soup.push(monadic_expr);
                    continue;
                }
                // Namespace not registered — fall through to normal block desugaring.
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
                    // Simple lookup (.name) on a static block literal.
                    //
                    // Emit a Lookup node restricted to the block's
                    // own bindings.  Outer scope must NOT be
                    // consulted — `.name` is key lookup, not scope
                    // resolution.
                    soup.pop(); // remove dot
                    if let Some(dlet) = soup.pop() {
                        soup.push(core::lookup(*s, dlet, n, None));
                    } else {
                        panic!("Expected default let for static lookup");
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
    // not needed but leave vars for cooking phase (in case of sections).
    // For bracket content, always wrap so the cook phase produces a List.
    if soup.len() == 1 && !bracket {
        match &*soup.first().unwrap().inner {
            Expr::Var(_, _) => Ok(RcExpr::from(Expr::Soup(
                desugarer.new_smid(span),
                soup,
                bracket,
            ))),
            _ => Ok(soup.first().unwrap().clone()),
        }
    } else {
        Ok(RcExpr::from(Expr::Soup(
            desugarer.new_smid(span),
            soup,
            bracket,
        )))
    }
}

/// Extract just the declaration name without desugaring body
fn extract_declaration_name(decl: &rowan_ast::Declaration) -> Result<String, CoreError> {
    if let Some(head) = decl.head() {
        let kind = head.classify_declaration();
        match kind {
            rowan_ast::DeclarationKind::Property(prop) => Ok(prop.value().to_string()),
            rowan_ast::DeclarationKind::Function(func, _) => Ok(func.value().to_string()),
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
) -> Result<(String, RcExpr), CoreError> {
    // Extract declaration name first and push to stack before body desugaring
    let name = extract_declaration_name(decl)?;

    // Register monad namespace BEFORE desugaring body so that subsequent
    // monadic blocks in the same file can find the spec.  Only register at
    // the top level (stack empty before push) to avoid spurious registrations
    // from nested declarations.
    if desugarer.is_top_level() && has_monad_true_in_raw_meta(decl) {
        let spec = super::desugarer::MonadSpec::Namespace(name.clone());
        desugarer.register_monad_namespace(name.clone(), spec);
    }

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
            if let Some(import_expr) = desugarer
                .translate_import(import_smid, input)
                .expect("failure translating import")
            {
                imports.push(import_expr);
            }
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

    let ret = (declared_var, expr);

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
                if let Some(import_expr) = desugarer
                    .translate_import(meta_smid, input)
                    .expect("failure translating import")
                {
                    imports.push(import_expr);
                }
            }
        }

        // Collect declaration names for environment
        let keys: Vec<_> = self
            .declarations()
            .filter_map(|decl| {
                if let Some(head) = decl.head() {
                    let kind = head.classify_declaration();
                    match kind {
                        rowan_ast::DeclarationKind::Property(prop) => {
                            Some(prop.value().to_string())
                        }
                        rowan_ast::DeclarationKind::Function(func, _) => {
                            Some(func.value().to_string())
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
            .map(|(name, expr)| (name.clone(), core::var(expr.smid(), name.clone())))
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
            close_let_scope(bindings, body),
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

/// Return true if the raw metadata soup of a `Unit` begins with a block-like
/// element (`Block`, `BracketBlock`, or `ParenExpr`).
///
/// Used to distinguish genuine bare-expression evaluands such as
/// `{ :io r: cmd }.(r.stdout)` (first element: Block) from erroneous
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

/// Unit desugaring - proper implementation following legacy architecture
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
                if let Some(import_expr) = desugarer
                    .translate_import(meta_smid, input)
                    .expect("failure translating import")
                {
                    imports.push(import_expr);
                }
            }
        }

        // Collect declaration names for environment
        let keys: Vec<_> = self
            .declarations()
            .filter_map(|decl| {
                if let Some(head) = decl.head() {
                    let kind = head.classify_declaration();
                    match kind {
                        rowan_ast::DeclarationKind::Property(prop) => {
                            Some(prop.value().to_string())
                        }
                        rowan_ast::DeclarationKind::Function(func, _) => {
                            Some(func.value().to_string())
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
            .map(|(name, expr)| (name.clone(), core::var(expr.smid(), name.clone())))
            .collect();

        // Remember whether there are any declarations before body_elements is moved.
        let has_no_declarations = body_elements.is_empty();

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
            RcExpr::from(Expr::Block(desugarer.new_smid(span), body_elements))
        };

        // Create let expression with bindings
        let mut expr = RcExpr::from(Expr::Let(
            desugarer.new_smid(span),
            close_let_scope(bindings, body),
            LetType::DefaultBlockLet,
        ));

        // Attach metadata if present.
        //
        // Special case: when a file contains only a bare block-dot expression
        // (no declarations) and the raw metadata soup starts with a block-like
        // element, use that expression directly as the unit body.  This allows
        // single-expression files such as
        //
        //   { :io result: io.shell("echo hello") }.(result.stdout)
        //
        // to behave like an `-e` evaluand.  The raw-element check restricts
        // this path to soups that start with `{`, `[`, or `(`, excluding
        // assignment-style mistakes like `result = 42` whose first element is
        // a name.
        if let Some(m) = metadata {
            let stripped_meta = strip_desugar_phase_metadata(&m);
            if !matches!(&*stripped_meta.inner, Expr::ErrEliminated) {
                let is_bare_expression = has_no_declarations
                    && !matches!(&*stripped_meta.inner, Expr::Block(_, _))
                    && unit_meta_starts_with_block(self);
                if is_bare_expression {
                    expr = RcExpr::from(Expr::Let(
                        desugarer.new_smid(span),
                        close_let_scope(vec![], stripped_meta),
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
