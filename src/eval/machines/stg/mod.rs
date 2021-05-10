//! An implementation of a spineless tagless G-machine variant plus a
//! compiler for core syntax that targets it.
//!
pub mod arith;
pub mod block;
pub mod boolean;
pub mod compiler;
pub mod constant;
pub mod emit;
pub mod env;
pub mod eq;
pub mod force;
pub mod intrinsic;
pub mod list;
pub mod machine;
pub mod meta;
pub mod null;
pub mod panic;
pub mod pretty;
pub mod printf;
pub mod render;
pub mod runtime;
pub mod string;
pub mod syntax;
pub mod time;

use gcmodule::Cc;
use std::rc::Rc;

use crate::{
    common::{prettify::prettify, sourcemap::SourceMap},
    core::expr::RcExpr,
    eval::{emit::Emitter, error::ExecutionError},
};

use self::{
    compiler::CompileError,
    runtime::{NativeVariant, Runtime},
    syntax::StgSyn,
};

lazy_static! {
    static ref STANDARD_RUNTIME: Box<runtime::StandardRuntime> = {
        let mut rt = runtime::StandardRuntime::default();
        rt.add(Box::new(arith::Add));
        rt.add(Box::new(arith::Sub));
        rt.add(Box::new(arith::Mul));
        rt.add(Box::new(arith::Div));
        rt.add(Box::new(arith::Gt));
        rt.add(Box::new(arith::Lt));
        rt.add(Box::new(arith::Gte));
        rt.add(Box::new(arith::Lte));
        rt.add(Box::new(eq::Eq));
        rt.add(Box::new(boolean::True));
        rt.add(Box::new(boolean::False));
        rt.add(Box::new(boolean::And));
        rt.add(Box::new(boolean::Or));
        rt.add(Box::new(boolean::Not));
        rt.add(Box::new(boolean::If));
        rt.add(Box::new(panic::Panic));
        rt.add(Box::new(block::Block));
        rt.add(Box::new(block::Kv));
        rt.add(Box::new(block::Dekv));
        rt.add(Box::new(block::Elements));
        rt.add(Box::new(block::MatchesKey));
        rt.add(Box::new(block::ExtractValue));
        rt.add(Box::new(block::ExtractKey));
        rt.add(Box::new(block::PackPair));
        rt.add(Box::new(block::BlockPair));
        rt.add(Box::new(block::Merge));
        rt.add(Box::new(block::MergeWith));
        rt.add(Box::new(block::DeepMerge));
        rt.add(Box::new(block::LookupOr(NativeVariant::Boxed)));
        rt.add(Box::new(block::LookupOr(NativeVariant::Unboxed)));
        rt.add(Box::new(block::Lookup));
        rt.add(Box::new(emit::Emit0));
        rt.add(Box::new(emit::EmitT));
        rt.add(Box::new(emit::EmitF));
        rt.add(Box::new(emit::EmitNative));
        rt.add(Box::new(emit::EmitSeqStart));
        rt.add(Box::new(emit::EmitSeqEnd));
        rt.add(Box::new(emit::EmitBlockStart));
        rt.add(Box::new(emit::EmitBlockEnd));
        rt.add(Box::new(emit::EmitDocStart));
        rt.add(Box::new(emit::EmitDocEnd));
        rt.add(Box::new(render::Render));
        rt.add(Box::new(render::RenderItems));
        rt.add(Box::new(render::RenderBlockItems));
        rt.add(Box::new(render::RenderKv));
        rt.add(Box::new(render::RenderDoc));
        rt.add(Box::new(render::Saturated));
        rt.add(Box::new(render::Suppresses));
        rt.add(Box::new(null::Null));
        rt.add(Box::new(list::Cons));
        rt.add(Box::new(list::Tail));
        rt.add(Box::new(list::Head));
        rt.add(Box::new(list::Nil));
        rt.add(Box::new(string::Sym));
        rt.add(Box::new(string::Str));
        rt.add(Box::new(string::Join));
        rt.add(Box::new(string::Match));
        rt.add(Box::new(string::Matches));
        rt.add(Box::new(string::Split));
        rt.add(Box::new(string::NumParse));
        rt.add(Box::new(string::Fmt));
        rt.add(Box::new(string::Letters));
        rt.add(Box::new(force::SeqStrList));
        rt.add(Box::new(meta::Meta));
        rt.add(Box::new(meta::WithMeta));
        rt.add(Box::new(time::Zdt));
        rt.add(Box::new(time::ZdtWrap));
        rt.add(Box::new(time::ZdtUnwrap));
        rt.add(Box::new(time::ZdtFromEpoch));
        rt.add(Box::new(time::ZdtFields));
        rt.add(Box::new(time::ZdtIFields));
        rt.add(Box::new(time::ZdtParse8601));
        rt.add(Box::new(time::ZdtFormat8601));
        rt.add(Box::new(constant::KNil));
        rt.add(Box::new(constant::KEmptyList));
        rt.add(Box::new(constant::KEmptyBlock));
        Box::new(rt)
    };
}

/// What type of render-wrapping to apply to the compiled code
#[derive(Copy, Debug, Clone)]
pub enum RenderType {
    /// No rendering - calculate only
    Headless,
    /// Render a fragment (suitable for -e flag)
    RenderFragment,
    /// Render an entire document (source must be a block)
    RenderDoc,
}

/// Settings to control compilation and execution of STG code
pub struct StgSettings {
    /// Generate annotations for stack traces in compiled output
    pub generate_annotations: bool,
    /// Trace steps during execution
    pub trace_steps: bool,
    /// Whether to generate a top-level render / render-doc call
    pub render_type: RenderType,
    /// Suppress thunks and updates (i.e call-by-name)
    pub suppress_updates: bool,
}

/// Dump STG for all globals.
pub fn dump_runtime() -> String {
    prettify(STANDARD_RUNTIME.as_ref())
}

/// Compile core syntax to STG ready for execution
pub fn compile(settings: &StgSettings, expr: RcExpr) -> Result<Rc<StgSyn>, CompileError> {
    let compiler = compiler::Compiler::new(
        settings.generate_annotations,
        settings.render_type,
        settings.suppress_updates,
    );
    compiler.compile(expr)
}

/// Create a standard machine for evaluating the provided STG syntax.
///
/// Use source_map for annotating synthetic runtime source
/// Use emitter for output events
pub fn standard_machine<'a>(
    settings: &StgSettings,
    syntax: Rc<StgSyn>,
    emitter: Box<dyn Emitter + 'a>,
    source_map: &mut SourceMap,
) -> Result<machine::Machine<'a>, ExecutionError> {
    let env = env::EnvFrame::default();
    Ok(machine::Machine::new(
        syntax,
        Cc::new(env),
        STANDARD_RUNTIME.globals(source_map),
        STANDARD_RUNTIME.intrinsics(),
        emitter,
        settings.trace_steps,
    ))
}
