//! This module contains a compiler for translating core expressions
//! to STG syntax for evaluation in the machine.
use std::{convert::TryInto, rc::Rc};

use crate::{
    common::sourcemap::{HasSmid, Smid, SourceMap},
    core::expr::{BlockMap, Expr, LamScope, LetScope, Primitive, RcExpr},
    eval::{
        intrinsics,
        machine::intrinsic::{CallGlobal1, CallGlobal3, Const, StgIntrinsic},
    },
};
use codespan_reporting::diagnostic::Diagnostic;
use moniker::{BoundVar, Embed, Var};

use thiserror::Error;

use super::{
    block::{panic_key_not_found, LookupOr},
    constant::KEmptyList,
    optimiser,
    render::{Render, RenderDoc},
    runtime::NativeVariant,
    syntax::{
        dsl::{self, gref},
        LambdaForm, Ref, StgSyn,
    },
    RenderType,
};

/// Errors found during compilation
#[derive(Debug, Error)]
pub enum CompileError {
    #[error("unresolved free variable reference {0}")]
    FreeVar(Smid),
    #[error("unresolvable bound variable reference")]
    BoundVarOverflowsContext,
    #[error("exceed maximum number of lambda args")]
    MaxLambdaArgs,
    #[error("unknown intrinsic {0}")]
    UnknownIntrinsic(String),
    #[error("encountered an uncompileable expression (operator soup)")]
    BadSoupExpression(Smid),
    #[error("encountered an uncompileable expression (arg tuple)")]
    BadArgTupleExpression(Smid),
}

impl HasSmid for CompileError {
    fn smid(&self) -> Smid {
        use self::CompileError::*;

        match *self {
            FreeVar(s) => s,
            BadSoupExpression(s) => s,
            BadArgTupleExpression(s) => s,
            _ => Smid::default(),
        }
    }
}

impl CompileError {
    pub fn to_diagnostic(&self, source_map: &SourceMap) -> Diagnostic<usize> {
        source_map.diagnostic(self)
    }
}

/// A linked list approach to context
pub struct Context<'a> {
    /// If this context scope has arisen from a core letrec
    scope: Option<RcExpr>,

    /// Indices corresponding to core let bindings (bound vars)
    var_refs: Vec<Ref>,

    /// What offset we need to count back to cross this scope
    size: usize,

    /// Next context frame.
    ///
    /// Note that this may not be the enclosing, if we are embedded in
    /// the binding of a non-recursive let, that let is skipped as it
    /// does not contribute bindings to our context.
    next: Option<&'a Context<'a>>,
}

impl Context<'_> {
    /// The core expression representing an underlying scope, if not synthetic
    pub fn scope(&self) -> Option<&RcExpr> {
        self.scope.as_ref()
    }

    /// Size (or offset required to traverse this scope)
    pub fn size(&self) -> usize {
        self.size
    }

    /// Is a purely synthetic context, created during compile, rather
    /// than corresponding to an underlying scope in core syntax.
    pub fn is_synthetic(&self) -> bool {
        self.scope().is_none()
    }

    /// Next or error
    pub fn next(&self) -> Result<&Context, CompileError> {
        match self.next {
            Some(frame) => Ok(frame),
            None => Err(CompileError::BoundVarOverflowsContext),
        }
    }

    pub fn local(&self, bound_var: &BoundVar<String>) -> Result<Rc<StgSyn>, CompileError> {
        Ok(dsl::atom(self.lookup(bound_var)?))
    }

    /// Generate a STG syntax local ref corresponding to the specified
    /// Core bound variable
    pub fn lookup(&self, bound_var: &BoundVar<String>) -> Result<Ref, CompileError> {
        let BoundVar { scope, binder, .. } = bound_var;

        if self.is_synthetic() {
            self.next()?.lookup(bound_var).map(|r| r.bump(self.size))
        } else if scope.0 == 0 {
            Ok(self.var_refs[binder.to_usize()].clone())
        } else {
            let adjusted_var = BoundVar {
                scope: scope.pred().ok_or(CompileError::BoundVarOverflowsContext)?,
                binder: bound_var.binder,
                pretty_name: None,
            };
            self.next()?
                .lookup(&adjusted_var)
                .map(|r| r.bump(self.size))
        }
    }
}

/// Compiler for translating core to STG
///
/// References intrinsics to support inlining
pub struct Compiler<'rt> {
    /// Whether to generate source annotations in let bindings
    generate_annotations: bool,
    /// Whether to wrap a render
    render_type: RenderType,
    /// Suppress updates by generating values instead of thunks
    suppress_updates: bool,
    /// Turn inlining off
    suppress_inlining: bool,
    /// Turn optimisations off
    suppress_optimiser: bool,
    /// Intrinsics
    intrinsics: Vec<&'rt dyn StgIntrinsic>,
}

/// An item which can be converted to STG syntax once the context in known
///
/// Each Proto form should implement at least one of `take_lambda_form`
/// and `take_syntax`
pub trait ProtoSyntax {
    /// This syntax is used only once and therefore probably will not
    /// benefit from a thunk. Default false.
    fn single_use(&self) -> bool {
        false
    }

    /// Convert into lambda form (destroying the proto form)
    ///
    /// Default wraps the `take_syntax` result in value or thunk
    fn take_lambda_form(
        &mut self,
        compiler: &Compiler,
        context: &Context,
    ) -> Result<LambdaForm, CompileError> {
        let syntax = self.take_syntax(compiler, context)?;
        if self.single_use() || syntax.is_whnf() || compiler.suppress_updates {
            Ok(dsl::value(syntax))
        } else {
            Ok(dsl::thunk(syntax))
        }
    }

    /// Convert into STG syntax (destroying the proto form)
    ///
    /// Default allocates the `take_lambda_form` result in a synthetic
    /// letrec and references the binding.
    fn take_syntax(
        &mut self,
        compiler: &Compiler,
        context: &Context,
    ) -> Result<Rc<StgSyn>, CompileError> {
        // we'll use a plain let so the LambdaForm does not need to
        // refer to the let's context
        let lambda_form = self.take_lambda_form(compiler, context)?;

        // the body would need to take account of the let context but
        // we can trivially hardcode the reference
        let body = dsl::local(0);

        Ok(dsl::let_(vec![lambda_form], body))
    }
}

/// An item which can be converted to a StgSyn once a context is available
pub trait ProtoReference {
    /// Convert into STG reference (destroying the proto form)
    fn take_reference(&mut self, context: &Context) -> Result<Ref, CompileError>;
}

/// We don't know the size of the environment as we add bindings to a
/// let so we track them in order to generate the entire let once we
/// are complete.
#[derive(Default)]
pub struct LetBinder<'a> {
    /// If this context scope has arisen from
    scope: Option<RcExpr>,
    /// Whether this should compile to LETREC as opposed to LET
    recursive: bool,
    /// Bindings in construction
    bindings: Vec<Box<dyn ProtoSyntax>>,
    /// If this corresponds to a Core let (i.e. has a scope), track
    /// the indices in our bindings which correspond to the scope
    /// bindings (as opposed to extra supporting bindings, generated)
    /// or potentially if bindings have been optimised away, the
    /// globals that replace them
    var_refs: Vec<Ref>,
    /// Body in construction
    body: Option<Box<dyn ProtoSyntax>>,
    /// When frozen, the number of bindings added
    size: Option<usize>,
    /// Next
    context: Option<&'a Context<'a>>,
}

impl<'a> LetBinder<'a> {
    /// A letrec scope for the root context
    pub fn root() -> Self {
        LetBinder {
            scope: None,
            recursive: true,
            bindings: vec![],
            var_refs: vec![],
            body: None,
            size: None,
            context: None,
        }
    }

    /// A let scope to catch any required intermediate bindings
    /// without a corresponding Core scope
    pub fn synthetic_let(context: &'a Context) -> Self {
        LetBinder {
            scope: None,
            recursive: false,
            bindings: vec![],
            var_refs: vec![],
            body: None,
            size: None,
            context: Some(context),
        }
    }

    /// A recursive let binder to catch any required intermediate
    /// bindings without a corresponding Core scope
    pub fn synthetic_letrec(context: &'a Context) -> Self {
        LetBinder {
            scope: None,
            recursive: true,
            bindings: vec![],
            var_refs: vec![],
            body: None,
            size: None,
            context: Some(context),
        }
    }

    /// A new letrec binder corresponding to a scope in the Core syntax
    pub fn for_scope(expr: RcExpr, context: &'a Context) -> Self {
        LetBinder {
            scope: Some(expr),
            recursive: true,
            bindings: vec![],
            var_refs: vec![],
            body: None,
            size: None,
            context: Some(context),
        }
    }

    /// If we already know what reference a bound var index
    /// corresponds to, we can generate a ref right away and optimise
    /// away a binding.
    pub fn running_ref(&self, index: usize) -> Option<Ref> {
        if self.recursive {
            self.var_refs.get(index).cloned()
        } else {
            None
        }
    }

    /// Stop accepting bindings and crystalise size
    pub fn freeze(&mut self) {
        self.size = Some(self.bindings.len())
    }

    /// Ensure that this binder will now generate a LETREC.
    ///
    /// If we're adding inter-referring bindings we need to ensure
    /// that however the binder was created, it will generate a
    /// LETREC.
    pub fn ensure_recursive(&mut self) {
        self.recursive = true;
    }

    /// Once a let binder has been frozen, so the size of the bindings
    /// is known and it has a body, it can be converted into an STG LetRec
    pub fn into_stg(mut self, compiler: &Compiler) -> Result<Rc<StgSyn>, CompileError> {
        let recursive_let = self.recursive;

        if self.size.is_none() {
            panic!("attempt to realise an unfrozen let binder");
        }

        if self.body.is_none() {
            panic!("attempt to realise let binder with no body")
        }

        let body_context = Context {
            scope: self.scope,
            var_refs: self.var_refs,
            size: self.size.unwrap(),
            next: self.context,
        };

        let binding_context = if recursive_let {
            &body_context
        } else {
            self.context.as_ref().unwrap()
        };

        let bindings: Vec<LambdaForm> = self
            .bindings
            .drain(0..)
            .map(|mut b| b.take_lambda_form(compiler, binding_context))
            .collect::<Result<Vec<LambdaForm>, CompileError>>()?;

        let mut proto_body = self.body.take().unwrap();
        let body = proto_body.take_syntax(compiler, &body_context)?;

        if bindings.is_empty() {
            Ok(body)
        } else if recursive_let {
            Ok(Rc::new(StgSyn::LetRec { bindings, body }))
        } else {
            Ok(Rc::new(StgSyn::Let { bindings, body }))
        }
    }

    /// Add a deferred binding which will be realised when the number
    /// of bindings is known. Free vars (i.e. refs into environments
    /// below the nascent letrec) will need accessing with indexes
    /// which include the count of bound vars.
    pub fn add_deferred(&mut self, form: Box<dyn ProtoSyntax>) -> Result<Ref, CompileError> {
        if self.size.is_some() {
            panic!("binding is frozen")
        } else {
            self.bindings.push(form);
            Ok(dsl::lref(self.bindings.len() - 1))
        }
    }

    /// Add a binding to syntax which is already determined
    /// (regardless of unknown context)
    pub fn add(&mut self, syntax: Rc<StgSyn>) -> Result<Ref, CompileError> {
        self.add_deferred(Box::new(Holder::new(syntax)))
    }

    /// Add a ref as an equivalent to an underlying core let binding
    pub fn add_var_index(&mut self, reference: Ref) {
        self.var_refs.push(reference);
    }

    /// Add a fn to calculate the body once the context is known
    pub fn set_body(&mut self, body: Box<dyn ProtoSyntax>) -> Result<(), CompileError> {
        self.body = Some(body);
        Ok(())
    }
}

struct ProtoLet {
    expr: RcExpr,
}

impl ProtoLet {
    pub fn new(expr: RcExpr) -> Self {
        assert!(matches!(&*expr.inner, Expr::Let(_, _, _)));
        ProtoLet { expr }
    }

    /// Reference the scope inside the let expression
    fn scope(&self) -> &LetScope<RcExpr> {
        if let Expr::Let(_, scope, _) = &*self.expr.inner {
            scope
        } else {
            unreachable!()
        }
    }
}

impl ProtoSyntax for ProtoLet {
    fn take_syntax(
        &mut self,
        compiler: &Compiler,
        context: &Context,
    ) -> Result<Rc<StgSyn>, CompileError> {
        let scope = self.scope();
        let mut binder = LetBinder::for_scope(self.expr.clone(), context);
        for (_, Embed(ref value)) in scope.unsafe_pattern.unsafe_pattern.iter() {
            let annotation = value.smid();
            let index = compiler.compile_binding(&mut binder, value.clone(), annotation, false)?;
            binder.add_var_index(index);
        }

        let body = compiler.compile_body(&mut binder, scope.unsafe_body.clone())?;
        binder.set_body(body)?;
        binder.freeze();

        binder.into_stg(compiler)
    }
}

/// ProtoVars become references into the environment once a context is
/// available
struct ProtoVar {
    bound_var: BoundVar<String>,
}

impl ProtoVar {
    pub fn new(bound_var: BoundVar<String>) -> Self {
        ProtoVar { bound_var }
    }
}

impl ProtoReference for ProtoVar {
    fn take_reference(&mut self, context: &Context) -> Result<Ref, CompileError> {
        context.lookup(&self.bound_var)
    }
}

impl ProtoSyntax for ProtoVar {
    fn take_syntax(
        &mut self,
        _compiler: &Compiler,
        context: &Context,
    ) -> Result<Rc<StgSyn>, CompileError> {
        context.local(&self.bound_var)
    }
}

/// A variable reference that resolves to a simple offset in the
/// current context
pub struct ProtoRef {
    index: Ref,
}

impl ProtoRef {
    pub fn new(index: Ref) -> Self {
        ProtoRef { index }
    }
}

impl ProtoReference for ProtoRef {
    fn take_reference(&mut self, _context: &Context) -> Result<Ref, CompileError> {
        Ok(self.index.clone())
    }
}

impl ProtoSyntax for ProtoRef {
    fn take_syntax(
        &mut self,
        _compiler: &Compiler,
        _context: &Context,
    ) -> Result<Rc<StgSyn>, CompileError> {
        Ok(Rc::new(StgSyn::Atom {
            evaluand: self.index.clone(),
        }))
    }
}

/// Holder just wraps up the StgSyn that will be delivered later when
/// the context is available
pub struct Holder {
    syntax: Option<Rc<StgSyn>>,
    single_use: bool,
}

impl Holder {
    pub fn new(syntax: Rc<StgSyn>) -> Self {
        Holder {
            syntax: Some(syntax),
            single_use: false,
        }
    }

    pub fn new_single_use(syntax: Rc<StgSyn>) -> Self {
        Holder {
            syntax: Some(syntax),
            single_use: true,
        }
    }
}

impl ProtoSyntax for Holder {
    fn single_use(&self) -> bool {
        self.single_use
    }
    fn take_syntax(
        &mut self,
        _compiler: &Compiler,
        _context: &Context,
    ) -> Result<Rc<StgSyn>, CompileError> {
        Ok(self.syntax.take().unwrap())
    }
}

/// A function application using local let bindings
pub struct ProtoAppGroup {
    f: RcExpr,
    args: Vec<RcExpr>,
    single_use: bool,
    smid: Smid,
}

impl ProtoAppGroup {
    pub fn new(f: RcExpr, args: Vec<RcExpr>, single_use: bool, smid: Smid) -> Self {
        Self {
            f,
            args,
            single_use,
            smid,
        }
    }
}

impl ProtoSyntax for ProtoAppGroup {
    fn single_use(&self) -> bool {
        self.single_use
    }

    fn take_syntax(
        &mut self,
        compiler: &Compiler,
        context: &Context,
    ) -> Result<Rc<StgSyn>, CompileError> {
        let mut intrinsic_index = None;
        let mut strict_args = &vec![];
        let mut local_binder = LetBinder::synthetic_let(context);

        // Find a reference for the function
        let f_index: Box<dyn ProtoReference> = match &*self.f.inner {
            Expr::Var(s, v) => Box::new(ProtoVar::new(extract_bound_var(s, v)?.clone())),
            Expr::Intrinsic(_, bif) => {
                intrinsic_index = intrinsics::index(bif);
                let n =
                    intrinsic_index.ok_or_else(|| CompileError::UnknownIntrinsic(bif.clone()))?;
                let info = intrinsics::intrinsic(n);
                strict_args = info.strict_args();
                Box::new(ProtoRef::new(gref(n)))
            }
            _ => Box::new(ProtoRef::new(compiler.compile_binding(
                &mut local_binder,
                self.f.clone(),
                self.smid,
                false,
            )?)),
        };

        // Get references for args, compiling into the local binder if necessary
        let mut arg_indexes: Vec<Box<dyn ProtoReference>> = vec![];
        for (i, arg) in self.args.iter().enumerate() {
            match &*arg.inner {
                Expr::Var(s, v) => {
                    arg_indexes.push(Box::new(ProtoVar::new(extract_bound_var(s, v)?.clone())))
                }
                Expr::Intrinsic(_, bif) => {
                    let global_index = intrinsics::index(bif)
                        .ok_or_else(|| CompileError::UnknownIntrinsic(bif.clone()))?;
                    arg_indexes.push(Box::new(ProtoRef::new(gref(global_index))))
                }
                _ => {
                    // if the argument position is strict, assume
                    // it'll be evaluated once only and so won't
                    // benefit from a thunk
                    let index = compiler.compile_binding(
                        &mut local_binder,
                        arg.clone(),
                        self.smid,
                        strict_args.contains(&i),
                    )?;
                    arg_indexes.push(Box::new(ProtoRef::new(index)));
                }
            }
        }

        // If it's an intrinsic, check whether we should inline the
        // wrapper
        match intrinsic_index.and_then(|index| compiler.intrinsics.get(index)) {
            Some(bif)
                if !compiler.suppress_inlining
                    && bif.inlinable()
                    && bif.info().arity() == arg_indexes.len() =>
            {
                let inline_body = bif.wrapper(Smid::default()).body().clone();
                local_binder.set_body(Box::new(ProtoInline::new(arg_indexes, inline_body)))?;
            }
            _ => {
                local_binder.set_body(ProtoApp::boxed(f_index, arg_indexes, self.single_use))?;
            }
        }

        local_binder.freeze();
        local_binder.into_stg(compiler)
    }
}

/// A function application that resolves all locals when context is available
pub struct ProtoApp {
    f: Box<dyn ProtoReference>,
    args: Vec<Box<dyn ProtoReference>>,
    single_use: bool,
}

impl ProtoApp {
    pub fn boxed(
        f: Box<dyn ProtoReference>,
        args: Vec<Box<dyn ProtoReference>>,
        single_use: bool,
    ) -> Box<dyn ProtoSyntax> {
        Box::new(ProtoApp {
            f,
            args,
            single_use,
        })
    }
}

impl ProtoSyntax for ProtoApp {
    fn single_use(&self) -> bool {
        self.single_use
    }

    fn take_syntax(
        &mut self,
        _compiler: &Compiler,
        context: &Context,
    ) -> Result<Rc<StgSyn>, CompileError> {
        let callable = self.f.take_reference(context)?;
        let args = self
            .args
            .drain(0..)
            .map(|mut a| a.take_reference(context))
            .collect::<Result<Vec<Ref>, CompileError>>()?;
        Ok(Rc::new(StgSyn::App { callable, args }))
    }
}

/// Extract reference to bound var
pub fn extract_bound_var<'a>(
    smid: &'a Smid,
    var: &'a Var<String>,
) -> Result<&'a BoundVar<String>, CompileError> {
    match var {
        Var::Bound(bound_var) => Ok(bound_var),
        Var::Free(_) => Err(CompileError::FreeVar(*smid)),
    }
}

/// Compile literal into a consistent boxed representation
pub fn compile_boxed_literal(prim: &Primitive) -> Rc<StgSyn> {
    match prim {
        Primitive::Str(s) => dsl::box_str(s),
        Primitive::Sym(s) => dsl::box_sym(s),
        Primitive::Num(n) => dsl::box_num(n.clone()),
        Primitive::Bool(b) => dsl::bool_(*b),
        Primitive::Null => dsl::unit(),
    }
}

pub struct ProtoLambda {
    expr: RcExpr,
    annotation: Smid,
}

impl ProtoLambda {
    pub fn new(expr: RcExpr, annotation: Smid) -> Self {
        assert!(matches!(&*expr.inner, Expr::Lam(_, _, _)));
        ProtoLambda { expr, annotation }
    }

    /// Reference the scope inside the lambda expression
    fn scope(&self) -> &LamScope<RcExpr> {
        if let Expr::Lam(_, _, scope) = &*self.expr.inner {
            scope
        } else {
            unreachable!()
        }
    }
}

impl ProtoSyntax for ProtoLambda {
    /// Compiling a lambda introduced a context for the bound
    /// variables and a synthetic let binder for the body in case it
    /// is required.
    fn take_lambda_form(
        &mut self,
        compiler: &Compiler,
        context: &Context,
    ) -> Result<LambdaForm, CompileError> {
        let scope = self.scope();
        let args = scope.unsafe_pattern.len();

        let lambda_context = Context {
            scope: Some(self.expr.clone()),
            var_refs: (0..args).map(Ref::L).collect(),
            size: args,
            next: Some(context),
        };

        let mut binder = LetBinder::synthetic_let(&lambda_context);

        let body = compiler.compile_body(&mut binder, scope.unsafe_body.clone())?;
        binder.set_body(body)?;
        binder.freeze();
        let mut body = binder.into_stg(compiler)?;

        if compiler.generate_annotations() {
            body = dsl::ann(self.annotation, body);
        }

        Ok(dsl::lambda(
            args.try_into().or(Err(CompileError::MaxLambdaArgs))?,
            body,
        ))
    }
}

pub struct ProtoInline {
    args: Vec<Box<dyn ProtoReference>>,
    body: Rc<StgSyn>,
}

impl ProtoInline {
    pub fn new(args: Vec<Box<dyn ProtoReference>>, body: Rc<StgSyn>) -> Self {
        ProtoInline { args, body }
    }
}

impl ProtoSyntax for ProtoInline {
    /// Inline an application by creating a let binding to hold the
    /// args then substituting the lambda body.
    ///
    /// We need an optimisation pass to eliminate needless lets as
    /// they're only refs to elsewhere
    fn take_syntax(
        &mut self,
        _compiler: &Compiler,
        context: &Context,
    ) -> Result<Rc<StgSyn>, CompileError> {
        let body = self.body.clone();

        let refs = self
            .args
            .drain(..)
            .map(|mut pr| pr.take_reference(context))
            .collect::<Result<Vec<Ref>, CompileError>>()?;

        let bindings = refs.into_iter().map(|r| dsl::value(dsl::atom(r))).collect();

        Ok(Rc::new(StgSyn::Let { bindings, body }))
    }
}

impl<'rt> Compiler<'rt> {
    /// Temporary pending a builder pattern....
    pub fn new(
        generate_annotations: bool,
        render_type: RenderType,
        suppress_updates: bool,
        suppress_inlining: bool,
        suppress_optimiser: bool,
        intrinsics: Vec<&'rt dyn StgIntrinsic>,
    ) -> Self {
        Compiler {
            generate_annotations,
            render_type,
            suppress_updates,
            suppress_inlining,
            suppress_optimiser,
            intrinsics,
        }
    }

    /// Whether to generate source annotations
    pub fn generate_annotations(&self) -> bool {
        self.generate_annotations
    }

    /// Compile a core expression into STG syntax
    pub fn compile(&self, expr: RcExpr) -> Result<Rc<StgSyn>, CompileError> {
        let mut binder = LetBinder::root();
        match self.render_type {
            RenderType::Headless => {
                let body = self.compile_body(&mut binder, expr)?;
                binder.set_body(body)?;
            }
            RenderType::RenderDoc => {
                let index = self.compile_binding(&mut binder, expr.clone(), expr.smid(), false)?;
                binder.set_body(Box::new(Holder::new(RenderDoc.global(index))))?;
            }
            RenderType::RenderFragment => {
                let index = self.compile_binding(&mut binder, expr.clone(), expr.smid(), false)?;
                binder.set_body(Box::new(Holder::new(Render.global(index))))?;
            }
        }
        binder.freeze();
        let compiled = binder.into_stg(self);

        if self.suppress_optimiser {
            compiled
        } else {
            compiled.map(|c| optimiser::AllocationPruner::default().apply(c))
        }
    }

    /// Compile a let body or standalone expression
    pub fn compile_body(
        &self,
        binder: &mut LetBinder,
        expr: RcExpr,
    ) -> Result<Box<dyn ProtoSyntax>, CompileError> {
        match &*expr.inner {
            Expr::Let(_, _, _) => Ok(Box::new(ProtoLet::new(expr))),
            Expr::Var(s, v) => Ok(Box::new(ProtoVar::new(extract_bound_var(s, v)?.clone()))),
            Expr::App(s, f, args) => {
                let proto_app = self.compile_application(binder, *s, f, args, false)?;
                Ok(proto_app)
            }
            Expr::Literal(_, n) => Ok(Box::new(Holder::new(compile_boxed_literal(n)))),
            Expr::Lam(s, _, _) => Ok(Box::new(self.compile_lambda(&expr, *s)?)),
            Expr::List(s, xs) => {
                let list = self.compile_list_body(binder, *s, xs)?;
                Ok(Box::new(list))
            }
            Expr::Block(s, map) => {
                let block = self.compile_block(binder, *s, map)?;
                Ok(Box::new(block))
            }
            Expr::Intrinsic(_, name) => {
                let index = intrinsics::index(name)
                    .ok_or_else(|| CompileError::UnknownIntrinsic(name.clone()))?;
                Ok(Box::new(Holder::new(dsl::atom(gref(index)))))
            }
            Expr::Lookup(s, obj, k, d) => {
                let lookup = self.compile_lookup(binder, obj, k, d, *s)?;
                Ok(Box::new(lookup))
            }
            Expr::Meta(s, body, meta) => {
                let m = self.compile_binding(binder, meta.clone(), *s, false)?;
                let b = self.compile_binding(binder, body.clone(), *s, false)?;
                Ok(Box::new(Holder::new(dsl::with_meta(m, b))))
            }
            Expr::Operator(_, _, _, body) => self.compile_body(binder, body.clone()),
            x => {
                panic!("bad core syntax during compile: {:?}", x)
            }
        }
    }

    /// Compile an expression as a binding, returning index in the binder
    /// that the final expression is located at
    pub fn compile_binding(
        &self,
        binder: &mut LetBinder,
        expr: RcExpr,
        annotation: Smid,
        single_use: bool,
    ) -> Result<Ref, CompileError> {
        match &*expr.inner {
            Expr::Var(s, v) => {
                let bound_var = extract_bound_var(s, v)?.clone();
                if bound_var.scope.0 == 0 {
                    if let Some(r) = binder.running_ref(bound_var.binder.to_usize()) {
                        Ok(r)
                    } else {
                        binder.add_deferred(Box::new(ProtoVar::new(bound_var)))
                    }
                } else {
                    binder.add_deferred(Box::new(ProtoVar::new(bound_var)))
                }
            }
            Expr::Let(_, _, _) => binder.add_deferred(Box::new(ProtoLet::new(expr))),
            Expr::Lam(_, _, _) => {
                binder.add_deferred(Box::new(self.compile_lambda(&expr, annotation)?))
            }
            Expr::App(s, f, args) => {
                let proto_app = self.compile_application(binder, *s, f, args, single_use)?;
                binder.add_deferred(proto_app)
            }
            Expr::Literal(_, n) => binder.add(compile_boxed_literal(n)),
            Expr::List(s, xs) => self.compile_list_binding(binder, *s, xs),
            Expr::Block(s, map) => {
                let block = self.compile_block(binder, *s, map)?;
                binder.add_deferred(Box::new(block))
            }
            Expr::Intrinsic(_, name) => {
                let index = intrinsics::index(name)
                    .ok_or_else(|| CompileError::UnknownIntrinsic(name.clone()))?;
                Ok(dsl::gref(index))
            }
            Expr::Lookup(s, obj, k, d) => {
                let lookup = self.compile_lookup(binder, obj, k, d, *s)?;
                binder.add_deferred(Box::new(lookup))
            }
            Expr::Name(_, _) => {
                todo!()
            }
            Expr::Meta(s, body, meta) => {
                let m = self.compile_binding(binder, meta.clone(), *s, false)?;
                let b = self.compile_binding(binder, body.clone(), *s, false)?;
                binder.add(dsl::with_meta(m, b))
            }
            Expr::ArgTuple(s, _) => Err(CompileError::BadArgTupleExpression(*s)),
            Expr::Soup(s, _) => Err(CompileError::BadSoupExpression(*s)),
            Expr::Operator(s, _, _, body) => self.compile_binding(binder, body.clone(), *s, false),
            _ => {
                panic!("bad core syntax during compile")
            }
        }
    }

    /// Compile a lookup (with or without default) to a LOOKUPOR call
    pub fn compile_lookup(
        &self,
        binder: &mut LetBinder,
        obj: &RcExpr,
        key: &str,
        dft: &Option<RcExpr>,
        annotation: Smid,
    ) -> Result<Holder, CompileError> {
        binder.ensure_recursive();

        let obj = self.compile_binding(binder, obj.clone(), obj.smid(), false)?;
        let dft = match dft {
            // Tolerate free vars in lookup default for dynamic gen
            // lookup
            // HACK: get this right in desugar phase instead
            Some(expr) => match self.compile_binding(binder, expr.clone(), annotation, false) {
                Ok(expr) => Ok(expr),
                Err(CompileError::FreeVar(_)) => binder.add(panic_key_not_found(key)),
                Err(e) => Err(e),
            },
            None => binder.add(panic_key_not_found(key)),
        }?;
        Ok(Holder::new(LookupOr(NativeVariant::Unboxed).global(
            dsl::sym(key),
            dft,
            obj,
        )))
    }

    /// Compile a lambda to a lambda form
    pub fn compile_lambda(
        &self,
        expr: &RcExpr,
        annotation: Smid,
    ) -> Result<ProtoLambda, CompileError> {
        Ok(ProtoLambda::new(expr.clone(), annotation))
    }

    /// Compile a list into a chain of cons cells in the environment
    /// together with the compiled contents
    pub fn compile_list_body(
        &self,
        binder: &mut LetBinder,
        smid: Smid,
        members: &[RcExpr],
    ) -> Result<Holder, CompileError> {
        binder.ensure_recursive();

        let mut last_cons = None;

        for item in members.iter().rev() {
            let last_index = match last_cons {
                Some(data) => binder.add(data)?,
                None => KEmptyList.gref(),
            };
            let item_index = self.compile_binding(binder, item.clone(), smid, false)?;
            last_cons = Some(dsl::cons(item_index, last_index));
        }

        match last_cons {
            Some(data) => Ok(Holder::new(data)),
            None => Ok(Holder::new(KEmptyList.global())),
        }
    }

    /// Compile a list into a chain of cons cells in the environment
    /// together with the compiled contents
    pub fn compile_list_binding(
        &self,
        binder: &mut LetBinder,
        smid: Smid,
        members: &[RcExpr],
    ) -> Result<Ref, CompileError> {
        binder.ensure_recursive();

        let mut last_cons = None;

        for item in members.iter().rev() {
            let last_index = match last_cons {
                Some(data) => binder.add(data)?,
                None => KEmptyList.gref(),
            };
            let item_index = self.compile_binding(binder, item.clone(), smid, false)?;
            last_cons = Some(dsl::cons(item_index, last_index));
        }

        match last_cons {
            Some(data) => binder.add(data),
            None => Ok(KEmptyList.gref()),
        }
    }

    /// Compile a block
    pub fn compile_block(
        &self,
        binder: &mut LetBinder,
        smid: Smid,
        block_map: &BlockMap<RcExpr>,
    ) -> Result<Holder, CompileError> {
        binder.ensure_recursive();

        let mut index = KEmptyList.gref(); // binder.add(dsl::nil())?; // TODO: to CAF
        for (k, v) in block_map.iter().rev() {
            let v_index = self.compile_binding(binder, v.clone(), smid, false)?;
            let kv_index = binder.add(dsl::pair(k, v_index))?;
            index = binder.add(dsl::cons(kv_index, index))?;
        }
        Ok(Holder::new(dsl::block(index)))
    }

    /// Compile a function application
    ///
    /// This may create a let binding for temporaries in function or
    /// argument position
    pub fn compile_application(
        &self,
        _binder: &mut LetBinder,
        smid: Smid,
        f: &RcExpr,
        args: &[RcExpr],
        single_use: bool,
    ) -> Result<Box<dyn ProtoSyntax>, CompileError> {
        Ok(Box::new(ProtoAppGroup::new(
            f.clone(),
            args.to_vec(),
            single_use,
            smid,
        )))
    }
}
#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{
        core::expr::{acore, free},
        eval::stg::tags::DataConstructor,
    };

    fn compile(expr: RcExpr) -> Result<Rc<StgSyn>, CompileError> {
        Compiler::new(true, RenderType::Headless, false, false, false, vec![]).compile(expr)
    }

    #[test]
    pub fn test_compile_literal() {
        assert_eq!(
            compile(acore::num(99)).unwrap(),
            dsl::data(DataConstructor::BoxedNumber.tag(), vec![dsl::num(99)])
        );
    }

    #[test]
    pub fn test_simple_letrec() {
        let x = free("x");
        let y = free("y");

        let core = acore::let_(
            vec![(x.clone(), acore::num(2)), (y, acore::num(3))],
            acore::var(x),
        );

        assert_eq!(
            compile(core).unwrap(),
            dsl::letrec_(
                vec![
                    dsl::value(compile_boxed_literal(&Primitive::Num(2.into()))),
                    dsl::value(compile_boxed_literal(&Primitive::Num(3.into()))),
                ],
                dsl::local(0)
            )
        );
    }

    #[test]
    pub fn test_nested_letrecs() {
        let x = free("x");
        let y = free("y");
        let z = free("z");

        let core = acore::let_(
            vec![
                (
                    x.clone(),
                    acore::let_(vec![(z, acore::num(2))], acore::var(y.clone())),
                ),
                (y, acore::num(3)),
            ],
            acore::var(x),
        );

        assert_eq!(
            compile(core).unwrap(),
            dsl::letrec_(
                vec![
                    dsl::thunk(dsl::letrec_(
                        vec![dsl::value(compile_boxed_literal(&Primitive::Num(2.into())))],
                        dsl::local(2) // 0 is this letrec,
                                      // 1, 2 are outer
                    )),
                    dsl::value(compile_boxed_literal(&Primitive::Num(3.into()))),
                ],
                dsl::local(0)
            )
        );
    }

    #[test]
    pub fn test_lambdas() {
        let x = free("x");
        let y = free("y");
        let z = free("z");
        let f = free("f");

        let core = acore::let_(
            vec![
                (f.clone(), acore::lam(vec![x.clone(), y], acore::var(x))),
                (
                    z.clone(),
                    acore::app(acore::var(f), vec![acore::num(999), acore::num(-999)]),
                ),
            ],
            acore::var(z),
        );

        let expected = dsl::letrec_(
            vec![
                dsl::lambda(2, dsl::ann(Smid::default(), dsl::local(0))),
                dsl::thunk(dsl::let_(
                    vec![
                        dsl::value(compile_boxed_literal(&Primitive::Num(999.into()))),
                        dsl::value(compile_boxed_literal(&Primitive::Num((-999).into()))),
                    ],
                    dsl::app(dsl::lref(2), vec![dsl::lref(0), dsl::lref(1)]),
                )),
            ],
            dsl::local(1),
        );

        assert_eq!(compile(core).unwrap(), expected);
    }

    #[test]
    pub fn test_compile_list() {
        let core = acore::list(vec![acore::sym("x"), acore::sym("y"), acore::sym("z")]);
        let syntax = dsl::letrec_(
            vec![
                dsl::value(dsl::box_sym("z")),
                dsl::value(dsl::cons(dsl::lref(0), KEmptyList.gref())),
                dsl::value(dsl::box_sym("y")),
                dsl::value(dsl::cons(dsl::lref(2), dsl::lref(1))),
                dsl::value(dsl::box_sym("x")),
            ],
            dsl::cons(dsl::lref(4), dsl::lref(3)),
        );
        assert_eq!(compile(core).unwrap(), syntax);
    }

    #[test]
    pub fn test_compile_block() {
        let core = acore::block(vec![
            ("x".to_string(), acore::num(20)),
            ("y".to_string(), acore::num(30)),
        ]);
        let syntax = dsl::letrec_(
            vec![
                dsl::value(dsl::box_num(30)),
                dsl::value(dsl::pair("y", dsl::lref(0))),
                dsl::value(dsl::cons(dsl::lref(1), KEmptyList.gref())),
                dsl::value(dsl::box_num(20)),
                dsl::value(dsl::pair("x", dsl::lref(3))),
                dsl::value(dsl::cons(dsl::lref(4), dsl::lref(2))),
            ],
            dsl::block(dsl::lref(5)),
        );

        assert_eq!(compile(core).unwrap(), syntax);
    }
}
