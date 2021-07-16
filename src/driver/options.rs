//! Command line argument handling.
use crate::{driver::error::EucalyptError, eval::machines::stg::StgSettings};
use crate::{
    eval::machines::stg::RenderType,
    syntax::input::{Input, Locator},
};
use atty::Stream;
use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

use super::project;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CommandLineMode {
    Ergonomic,
    Batch,
}

impl Default for CommandLineMode {
    fn default() -> Self {
        CommandLineMode::Ergonomic
    }
}

impl CommandLineMode {
    fn from_batch_flag(b: bool) -> Self {
        if b {
            CommandLineMode::Batch
        } else {
            CommandLineMode::Ergonomic
        }
    }
}

#[derive(StructOpt, Clone, Debug, Default)]
#[structopt(name = "reu")]
pub struct EucalyptOptions {
    /// Batch mode (no .eucalypt.d)
    #[structopt(short = "B", long = "batch", parse(from_flag=CommandLineMode::from_batch_flag))]
    mode: CommandLineMode,

    #[structopt(flatten)]
    command: EucalyptCommandOption,

    /// Target to run (identified by target metadata in eucalypt source)
    #[structopt(short, long)]
    target: Option<String>,

    /// Output file to export to
    #[structopt(short, parse(from_os_str))]
    output: Option<PathBuf>,

    /// Don't load the standard prelude
    #[structopt(short = "Q", long)]
    no_prelude: bool,

    /// Expression to evaluate
    #[structopt(short, long)]
    evaluate: Option<String>,

    #[structopt(short = "c", long)]
    collect_as: Option<String>,

    #[structopt(short = "N", long)]
    name_inputs: bool,

    /// Turn on debug features
    #[structopt(short, long)]
    debug: bool,

    /// Add directory to lib path
    #[structopt(short = "L", long, number_of_values = 1, multiple = true)]
    lib_path: Vec<PathBuf>,

    /// When outputing AST or Core expressions, quote-embed as
    /// eucalypt
    #[structopt(long)]
    quote_embed: bool,

    /// When outputing AST or Core expressions, quote as
    /// debug print of structure
    #[structopt(long)]
    quote_debug: bool,

    /// Print metrics to stderr before exiting
    #[structopt(short = "S", long)]
    statistics: bool,

    /// Shortcut for `-x json``
    #[structopt(short)]
    json: bool,

    /// Format to export output in (e.g. yaml, json, toml, text)
    #[structopt(short = "x", long)]
    export_type: Option<String>,

    #[structopt(flatten)]
    stg_settings: StgSettings,

    /// Explicit source code / data inputs (in order)
    explicit_inputs: Vec<Input>,

    /// Prologue inputs (inputs added prior to explicit inputs)
    #[structopt(skip)]
    prologue_inputs: Vec<Input>,

    /// Epilogue inputs (inputs added after explicit inputs)
    #[structopt(skip)]
    epilogue_inputs: Vec<Input>,
}

impl EucalyptOptions {
    /// If the user has specified an evaluand (`-e`), we render a
    /// fragment rather than doc
    pub fn is_fragment(&self) -> bool {
        self.evaluate.is_some()
    }

    /// Set up options for a test
    pub fn for_test(mut self, target: String, format: String) -> Self {
        self.target = if target.is_empty() {
            None
        } else {
            Some(target)
        };
        self.export_type = Some(format);
        self
    }

    pub fn with_explicit_inputs(mut self, inputs: Vec<Input>) -> Self {
        self.explicit_inputs = inputs;
        self
    }

    pub fn with_target(mut self, target: Option<String>) -> Self {
        self.target = target;
        self
    }

    pub fn with_collect_as(mut self, collect_as: Option<String>) -> Self {
        self.collect_as = collect_as;
        self
    }

    pub fn with_lib_path(mut self, lib_path: Vec<PathBuf>) -> Self {
        self.lib_path = lib_path;
        self
    }

    pub fn with_output(mut self, output: PathBuf) -> Self {
        self.output = Some(output);
        self
    }

    pub fn with_export_type(mut self, format: String) -> Self {
        self.export_type = Some(format);
        self
    }

    pub fn without_prelude(mut self) -> Self {
        self.no_prelude = true;
        self
    }

    pub fn with_debug(mut self) -> Self {
        self.debug = true;
        self
    }

    /// The test flag indicates shallow desugaring only for test plan
    /// analysis. Actual execution requires the flag is reset.
    pub fn without_test_flag(mut self) -> Self {
        self.command.test = false;
        self
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn to_evaluate<S: Into<String>>(mut self, evaluand: S) -> Self {
        self.evaluate = Some(evaluand.into());
        self
    }

    pub fn build(mut self) -> Self {
        self.process_defaults().unwrap();
        self
    }

    /// Produce a dry run explanation of the selected options
    pub fn explanation(&self) -> String {
        let mut explanation = String::new();

        // command
        explanation.push_str("Eucalypt will ");
        explanation.push_str(self.command.explain());
        explanation.push_str("\n\n");

        // targets
        if let Some(t) = &self.target {
            explanation.push_str(&format!("Target: {}\n\n", t));
        }
        if let Some(e) = &self.evaluate {
            explanation.push_str(&format!("Evaluand: {}\n\n", e));
        }

        // output
        let o = self
            .output
            .as_ref()
            .and_then(|p| p.to_str())
            .unwrap_or("stdout");
        let f = self
            .export_type
            .clone()
            .unwrap_or_else(|| "yaml".to_string());
        explanation.push_str(&format!("Render output: {} to {}\n\n", f, o));

        // inputs
        explanation.push_str("Inputs:\n");
        for i in &self.prologue_inputs {
            explanation.push_str(&format!(" • {}\n", i));
        }
        if let Some(name) = self.collection() {
            explanation.push_str(&format!(" • {}=\n", name));
        }
        for i in &self.explicit_inputs {
            if self.collection().is_some() {
                explanation.push_str("  ");
            }
            explanation.push_str(&format!(" • {}\n", i));
        }
        for i in &self.epilogue_inputs {
            explanation.push_str(&format!(" • {}\n", i));
        }
        explanation.push('\n');

        // library path
        if !self.lib_path.is_empty() {
            explanation.push_str("Library path:\n");
            for p in &self.lib_path {
                explanation.push_str(&format!(" • {}\n", p.as_path().to_str().unwrap_or("?")));
            }
        }
        explanation.push('\n');

        // options
        if self.no_prelude || self.debug || self.quote_embed || self.quote_debug || self.statistics
        {
            explanation.push_str("Other options:\n");
        }
        if self.no_prelude {
            explanation.push_str(" • the standard prelude is excluded\n")
        }
        if self.debug {
            explanation.push_str(" • debug mode is ON \n")
        }
        if self.quote_embed {
            explanation.push_str(" • dumps will use embedding format \n")
        }
        if self.quote_debug {
            explanation.push_str(" • dumps will use debug format \n")
        }
        if self.statistics {
            explanation.push_str(" • print statistics to stderr")
        }

        explanation
    }
}

/// Option to select the command actually run
#[derive(StructOpt, Debug, Clone, Default)]
pub struct EucalyptCommandOption {
    /// Explain command (do not run)
    #[structopt(short = "n", long = "explain")]
    explain: bool,
    /// Explain command (do not run)
    #[structopt(short = "v", long = "version")]
    version: bool,
    /// List targets defined in the source
    #[structopt(short = "l", long = "list-targets")]
    list_targets: bool,
    /// Run file as test
    #[structopt(short = "T", long)]
    test: bool,
    /// Parse only
    #[structopt(short, long = "parse")]
    parse: bool,
    /// Dump core expression as initially translated from syntax tree
    #[structopt(long)]
    dump_desugared: bool,
    /// Dump core expression once operator soup has been analysed for precedence
    #[structopt(long)]
    dump_cooked: bool,
    /// Dump core expression once inliner pass has run
    #[structopt(long)]
    dump_inlined: bool,
    /// Dump core expression once dead ocde has been eliminated
    #[structopt(long)]
    dump_pruned: bool,
    /// Dump compiled STG syntax
    #[structopt(long)]
    dump_stg: bool,
    /// Dump code for runtime globals
    #[structopt(long)]
    dump_runtime: bool,
}

impl EucalyptCommandOption {
    pub fn run(&self) -> bool {
        !self.explain
            && !self.list_targets
            && !self.test
            && !self.parse
            && !self.dump_desugared
            && !self.dump_cooked
            && !self.dump_inlined
            && !self.dump_pruned
            && !self.dump_stg
            && !self.dump_runtime
    }

    pub fn explain(&self) -> &str {
        if self.version {
            "print its own version"
        } else if self.list_targets {
            "list the targets available in the inputs"
        } else if self.test {
            "evaluate the inputs (or all inputs in the directory) in test mode"
        } else if self.parse {
            "parse the inputs into syntax trees and dump to standard out"
        } else if self.dump_desugared {
            "parse inputs and translate to initial core representation then dump to standard out"
        } else if self.dump_cooked {
            "parse inputs, translate to core, resolve operator precedence and dump to standard out"
        } else if self.dump_inlined {
            "process inputs up to inlining then dump to standard out"
        } else if self.dump_pruned {
            "process inputs and simplify to final core representation and dump to standard out"
        } else if self.dump_stg {
            "process inputs through core phase and compile to STG code and dump to standard out"
        } else if self.dump_runtime {
            "dump all STG intrinsic functions to standard out"
        } else {
            "evaluate the inputs in production mode"
        }
    }
}

impl EucalyptOptions {
    pub fn evaluand(&self) -> Option<&String> {
        self.evaluate.as_ref()
    }

    pub fn collection(&self) -> Option<&String> {
        self.collect_as.as_ref()
    }

    pub fn name_inputs(&self) -> bool {
        self.name_inputs
    }

    pub fn explain(&self) -> bool {
        self.command.explain
    }

    pub fn parse_only(&self) -> bool {
        self.command.parse
    }

    pub fn dump_desugared(&self) -> bool {
        self.command.dump_desugared
    }

    pub fn dump_cooked(&self) -> bool {
        self.command.dump_cooked
    }

    pub fn dump_inlined(&self) -> bool {
        self.command.dump_inlined
    }

    pub fn dump_pruned(&self) -> bool {
        self.command.dump_pruned
    }

    pub fn dump_stg(&self) -> bool {
        self.command.dump_stg
    }

    pub fn dump_runtime(&self) -> bool {
        self.command.dump_runtime
    }

    pub fn list_targets(&self) -> bool {
        self.command.list_targets
    }

    pub fn show_version(&self) -> bool {
        self.command.version
    }

    pub fn test(&self) -> bool {
        self.command.test
    }

    pub fn run(&self) -> bool {
        self.command.run()
    }

    pub fn target(&self) -> Option<&str> {
        self.target.as_deref()
    }

    pub fn debug(&self) -> bool {
        self.debug
    }

    pub fn prologue_inputs(&self) -> &[Input] {
        &self.prologue_inputs
    }

    pub fn explicit_inputs(&self) -> &[Input] {
        &self.explicit_inputs
    }

    pub fn epilogue_inputs(&self) -> &[Input] {
        &self.epilogue_inputs
    }

    pub fn inputs(&self) -> Vec<Input> {
        let mut inputs = vec![];
        inputs.extend_from_slice(&self.prologue_inputs.as_slice());
        inputs.extend_from_slice(&self.explicit_inputs.as_slice());
        inputs.extend_from_slice(&self.epilogue_inputs.as_slice());
        inputs
    }

    pub fn lib_path(&self) -> &[PathBuf] {
        &self.lib_path
    }

    pub fn output(&self) -> Option<&PathBuf> {
        self.output.as_ref()
    }

    pub fn export_type(&self) -> Option<&String> {
        self.export_type.as_ref()
    }

    pub fn quote_embed(&self) -> bool {
        self.quote_embed
    }

    pub fn quote_debug(&self) -> bool {
        self.quote_debug
    }

    pub fn statistics(&self) -> bool {
        self.statistics
    }

    pub fn stg_settings(&self) -> &StgSettings {
        &self.stg_settings
    }

    /// Prepend an input if it is not already in the input list
    fn prepend_input(&mut self, input: Input) {
        if !self.explicit_inputs.iter().any(|i| i == &input) {
            self.prologue_inputs.insert(0, input);
        }
    }

    /// Insert extra items into lib path and inputs, reconcile
    /// shortcut options
    pub fn process_defaults(&mut self) -> Result<(), EucalyptError> {
        // Version is provided by reading eu.build.banner
        if self.show_version() {
            self.evaluate = Some("eu.build.banner".to_string());
            self.export_type = Some("text".to_string());
            self.no_prelude = false;
        }

        // if an output file is specified, default an export type if
        // not set
        if let Some(outfile) = self.output() {
            if self.export_type.is_none() {
                let format = match outfile.extension().and_then(|s| s.to_str()) {
                    Some("yaml") | Some("yml") => Some("yaml".to_string()),
                    Some("json") => Some("json".to_string()),
                    Some("txt") => Some("text".to_string()),
                    _ => None,
                };
                self.export_type = format;
            }
        }

        // The explicit -j json flag overrides all other export
        // settings
        if self.json {
            self.export_type = Some("json".to_string());
        }

        // add current working directory as a lib path
        self.lib_path.insert(0, std::env::current_dir()?);

        // For pipes, default json stdin
        if self.explicit_inputs.is_empty() && !atty::is(Stream::Stdin) {
            self.prepend_input(Input::from_str("-").unwrap());
        }

        // Prepend project Eufile
        if let Some(eufile) = project::eufile() {
            self.prepend_input(Input::new(Locator::Fs(eufile), None, "eu"));
        }

        // In ergonomic mode prepend user .eucalypt
        if self.mode == CommandLineMode::Ergonomic {
            if let Some(dotfile) = project::dotfile() {
                self.prepend_input(Input::new(Locator::Fs(dotfile), None, "eu"));
            }
        }

        // add prelude by default
        if !self.no_prelude {
            self.prepend_input(Input::new(
                Locator::Resource("prelude".to_string()),
                None,
                "eu",
            ));
        }

        // add build metadata as __build and io data as __io
        self.prepend_input(Input::new(
            Locator::Resource("build-meta".to_string()),
            Some("__build".to_string()),
            "yaml",
        ));
        self.prepend_input(Input::new(
            Locator::Pseudo("io".to_string()),
            Some("__io".to_string()),
            "core",
        ));

        // Add CLI evaluand as final input if it exists
        if self.evaluate.is_some() {
            self.epilogue_inputs.push(Input::from(Locator::Cli(
                self.evaluate.as_ref().unwrap().to_string(),
            )));
        }

        // Set default STG settings based on debug & other settings
        self.stg_settings.generate_annotations = true;
        if self.debug() {
            self.stg_settings.trace_steps = true;
        }
        if self.is_fragment() && self.stg_settings.render_type != RenderType::Headless {
            self.stg_settings.render_type = RenderType::RenderFragment;
        }

        Ok(())
    }
}
