//! Command line argument handling with clap v4 and subcommands.

use crate::{driver::error::EucalyptError, eval::stg::StgSettings};
use crate::{
    eval::stg::RenderType,
    syntax::input::{Input, Locator},
};
use clap::{Args, Parser, Subcommand};
use std::io::IsTerminal;
use std::path::PathBuf;
use std::str::FromStr;

use super::project;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum CommandLineMode {
    #[default]
    Ergonomic,
    Batch,
}

/// Eucalypt - A functional language for structured data
#[derive(Parser, Debug, Clone)]
#[command(name = "eu")]
#[command(about = "A functional language for structured data")]
#[command(version)]
pub struct EucalyptCli {
    /// Add directory to lib path
    #[arg(short = 'L', long = "lib-path", action = clap::ArgAction::Append)]
    pub lib_path: Vec<PathBuf>,

    /// Don't load the standard prelude
    #[arg(short = 'Q', long = "no-prelude")]
    pub no_prelude: bool,

    /// Batch mode (no .eucalypt.d)
    #[arg(short = 'B', long = "batch")]
    pub batch: bool,

    /// Turn on debug features
    #[arg(short = 'd', long = "debug")]
    pub debug: bool,

    /// Print metrics to stderr before exiting
    #[arg(short = 'S', long = "statistics")]
    pub statistics: bool,

    #[command(subcommand)]
    pub command: Option<Commands>,

    /// Files to process (used when no subcommand specified)
    #[arg(value_name = "FILES")]
    pub files: Vec<String>,
}

#[derive(Subcommand, Debug, Clone)]
pub enum Commands {
    /// Evaluate eucalypt code (default)
    Run(RunArgs),
    /// Run tests
    Test(TestArgs),
    /// Dump intermediate representations
    Dump(DumpArgs),
    /// Show version information
    Version,
    /// Explain what would be executed
    Explain(ExplainArgs),
    /// List targets defined in the source
    ListTargets(ListTargetsArgs),
    /// Format eucalypt source files
    Fmt(FmtArgs),
}

#[derive(Args, Debug, Clone)]
pub struct RunArgs {
    /// Target to run (identified by target metadata in eucalypt source)
    #[arg(short = 't', long = "target")]
    pub target: Option<String>,

    /// Expression to evaluate
    #[arg(short = 'e', long = "eval")]
    pub evaluate: Option<String>,

    /// Output file to export to
    #[arg(short = 'o', long = "output")]
    pub output: Option<PathBuf>,

    /// Format to export output in (e.g. yaml, json, toml, text)
    #[arg(short = 'x', long = "export")]
    pub export_type: Option<String>,

    /// Shortcut for `-x json`
    #[arg(short = 'j', long = "json")]
    pub json: bool,

    /// Collect inputs as named collection
    #[arg(short = 'c', long = "collect-as")]
    pub collect_as: Option<String>,

    /// Name inputs in collection
    #[arg(short = 'N', long = "name-inputs")]
    pub name_inputs: bool,

    /// Add directory to lib path
    #[arg(short = 'L', long = "lib-path", action = clap::ArgAction::Append)]
    pub lib_path: Vec<PathBuf>,

    /// Don't load the standard prelude
    #[arg(short = 'Q', long = "no-prelude")]
    pub no_prelude: bool,

    /// Batch mode (no .eucalypt.d)
    #[arg(short = 'B', long = "batch")]
    pub batch: bool,

    /// Turn on debug features
    #[arg(short = 'd', long = "debug")]
    pub debug: bool,

    /// Print metrics to stderr before exiting
    #[arg(short = 'S', long = "statistics")]
    pub statistics: bool,

    /// Files to process
    #[arg(value_name = "FILES")]
    pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct TestArgs {
    /// Target to test (identified by target metadata in eucalypt source)
    #[arg(short = 't', long = "target")]
    pub target: Option<String>,

    /// Output file for test report
    #[arg(short = 'o', long = "output")]
    pub output: Option<PathBuf>,

    /// Open a browser window on the test report if appropriate
    #[arg(long = "open")]
    pub open_browser: bool,

    /// Files to test
    #[arg(value_name = "FILES")]
    pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct DumpArgs {
    /// Phase to dump
    #[arg(value_enum)]
    pub phase: DumpPhase,

    /// When outputting AST or Core expressions, quote-embed as eucalypt
    #[arg(long = "embed")]
    pub quote_embed: bool,

    /// When outputting AST or Core expressions, quote as debug print of structure
    #[arg(long = "debug-format")]
    pub quote_debug: bool,

    /// Files to dump
    #[arg(value_name = "FILES")]
    pub files: Vec<String>,
}

#[derive(clap::ValueEnum, Debug, Clone)]
pub enum DumpPhase {
    /// Parse and dump syntax tree
    Ast,
    /// Dump core expression as initially translated from syntax tree
    Desugared,
    /// Dump core expression once operator soup has been analysed for precedence
    Cooked,
    /// Dump core expression once inliner pass has run
    Inlined,
    /// Dump core expression once dead code has been eliminated
    Pruned,
    /// Dump compiled STG syntax
    Stg,
    /// Dump code for runtime globals
    Runtime,
}

#[derive(Args, Debug, Clone)]
pub struct ExplainArgs {
    /// Files (when explaining default run behavior)
    #[arg(value_name = "FILES")]
    pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct ListTargetsArgs {
    /// Files to analyze for targets
    #[arg(value_name = "FILES")]
    pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct FmtArgs {
    /// Files to format
    #[arg(value_name = "FILES")]
    pub files: Vec<String>,

    /// Line width for formatting (default: 80)
    #[arg(short = 'w', long = "width", default_value = "80")]
    pub width: usize,

    /// Modify files in place
    #[arg(long = "write")]
    pub write: bool,

    /// Check if files are formatted (exit 1 if not)
    #[arg(long = "check")]
    pub check: bool,

    /// Full reformatting mode (instead of conservative)
    #[arg(long = "reformat")]
    pub reformat: bool,

    /// Indent size in spaces (default: 2)
    #[arg(long = "indent", default_value = "2")]
    pub indent: usize,
}

/// Combined options structure that maintains compatibility
#[derive(Debug, Clone, Default)]
pub struct EucalyptOptions {
    // Global options
    pub mode: CommandLineMode,
    pub lib_path: Vec<PathBuf>,
    pub no_prelude: bool,
    pub debug: bool,
    pub statistics: bool,

    // Command-specific options (flattened from subcommands)
    pub target: Option<String>,
    pub output: Option<PathBuf>,
    pub evaluate: Option<String>,
    pub export_type: Option<String>,
    pub json: bool,
    pub collect_as: Option<String>,
    pub name_inputs: bool,
    pub quote_embed: bool,
    pub quote_debug: bool,
    pub open_browser: bool,

    // Command type flags (for backward compatibility)
    pub explain: bool,
    pub version: bool,
    pub list_targets: bool,
    pub test: bool,
    pub parse: bool,
    pub dump_desugared: bool,
    pub dump_cooked: bool,
    pub dump_inlined: bool,
    pub dump_pruned: bool,
    pub dump_stg: bool,
    pub dump_runtime: bool,

    // Format command options
    pub format: bool,
    pub format_width: usize,
    pub format_write: bool,
    pub format_check: bool,
    pub format_reformat: bool,
    pub format_indent: usize,

    // STG settings (flattened)
    pub stg_settings: StgSettings,

    // Input handling
    pub explicit_inputs: Vec<Input>,
    pub prologue_inputs: Vec<Input>,
    pub epilogue_inputs: Vec<Input>,
}

impl From<EucalyptCli> for EucalyptOptions {
    fn from(cli: EucalyptCli) -> Self {
        // Convert files to inputs
        let mut explicit_inputs = Vec::new();
        let files = match &cli.command {
            Some(Commands::Run(args)) => &args.files,
            Some(Commands::Test(args)) => &args.files,
            Some(Commands::Dump(args)) => &args.files,
            Some(Commands::Explain(args)) => &args.files,
            Some(Commands::ListTargets(args)) => &args.files,
            Some(Commands::Fmt(args)) => &args.files,
            None => &cli.files,
            _ => &Vec::new(),
        };

        for file in files {
            if let Ok(input) = Input::from_str(file) {
                explicit_inputs.push(input);
            }
        }

        // Extract command-specific options and override global settings
        let (
            target,
            output,
            evaluate,
            export_type,
            json,
            collect_as,
            name_inputs,
            quote_embed,
            quote_debug,
            open_browser,
            cmd_lib_path,
            cmd_no_prelude,
            cmd_batch,
            cmd_debug,
            cmd_statistics,
        ) = match &cli.command {
            Some(Commands::Run(args)) => (
                args.target.clone(),
                args.output.clone(),
                args.evaluate.clone(),
                args.export_type.clone(),
                args.json,
                args.collect_as.clone(),
                args.name_inputs,
                false,
                false,
                false,
                Some(args.lib_path.clone()),
                Some(args.no_prelude),
                Some(args.batch),
                Some(args.debug),
                Some(args.statistics),
            ),
            Some(Commands::Test(args)) => (
                args.target.clone(),
                args.output.clone(),
                None,
                None,
                false,
                None,
                false,
                false,
                false,
                args.open_browser,
                None,
                None,
                None,
                None,
                None,
            ),
            Some(Commands::Dump(args)) => (
                None,
                None,
                None,
                None,
                false,
                None,
                false,
                args.quote_embed,
                args.quote_debug,
                false,
                None,
                None,
                None,
                None,
                None,
            ),
            Some(Commands::ListTargets(_)) => (
                None, None, None, None, false, None, false, false, false, false, None, None, None,
                None, None,
            ),
            _ => (
                None, None, None, None, false, None, false, false, false, false, None, None, None,
                None, None,
            ),
        };

        // Set command type flags for backward compatibility
        let (
            explain,
            version,
            test,
            parse,
            dump_desugared,
            dump_cooked,
            dump_inlined,
            dump_pruned,
            dump_stg,
            dump_runtime,
            list_targets,
        ) = match &cli.command {
            Some(Commands::Version) => (
                false, true, false, false, false, false, false, false, false, false, false,
            ),
            Some(Commands::Explain(_)) => (
                true, false, false, false, false, false, false, false, false, false, false,
            ),
            Some(Commands::Test(_)) => (
                false, false, true, false, false, false, false, false, false, false, false,
            ),
            Some(Commands::ListTargets(_)) => (
                false, false, false, false, false, false, false, false, false, false, true,
            ),
            Some(Commands::Dump(args)) => match args.phase {
                DumpPhase::Ast => (
                    false, false, false, true, false, false, false, false, false, false, false,
                ),
                DumpPhase::Desugared => (
                    false, false, false, false, true, false, false, false, false, false, false,
                ),
                DumpPhase::Cooked => (
                    false, false, false, false, false, true, false, false, false, false, false,
                ),
                DumpPhase::Inlined => (
                    false, false, false, false, false, false, true, false, false, false, false,
                ),
                DumpPhase::Pruned => (
                    false, false, false, false, false, false, false, true, false, false, false,
                ),
                DumpPhase::Stg => (
                    false, false, false, false, false, false, false, false, true, false, false,
                ),
                DumpPhase::Runtime => (
                    false, false, false, false, false, false, false, false, false, true, false,
                ),
            },
            _ => (
                false, false, false, false, false, false, false, false, false, false, false,
            ),
        };

        // Extract format options
        let (format, format_width, format_write, format_check, format_reformat, format_indent) =
            match &cli.command {
                Some(Commands::Fmt(args)) => (
                    true,
                    args.width,
                    args.write,
                    args.check,
                    args.reformat,
                    args.indent,
                ),
                _ => (false, 80, false, false, false, 2),
            };

        EucalyptOptions {
            mode: if cmd_batch.unwrap_or(cli.batch) {
                CommandLineMode::Batch
            } else {
                CommandLineMode::Ergonomic
            },
            lib_path: cmd_lib_path.unwrap_or(cli.lib_path),
            no_prelude: cmd_no_prelude.unwrap_or(cli.no_prelude),
            debug: cmd_debug.unwrap_or(cli.debug),
            statistics: cmd_statistics.unwrap_or(cli.statistics),
            target,
            output,
            evaluate,
            export_type,
            json,
            collect_as,
            name_inputs,
            quote_embed,
            quote_debug,
            open_browser,
            explain,
            version,
            list_targets,
            test,
            parse,
            dump_desugared,
            dump_cooked,
            dump_inlined,
            dump_pruned,
            dump_stg,
            dump_runtime,
            format,
            format_width,
            format_write,
            format_check,
            format_reformat,
            format_indent,
            stg_settings: StgSettings::default(),
            explicit_inputs,
            prologue_inputs: Vec::new(),
            epilogue_inputs: Vec::new(),
        }
    }
}

// Parsing logic to handle default subcommand
impl EucalyptCli {
    pub fn parse_with_fallback() -> Self {
        let args: Vec<String> = std::env::args().collect();

        // If no args after program name, just parse normally
        if args.len() <= 1 {
            return Self::parse();
        }

        // Check if first arg is a known subcommand or help/version flag
        const SUBCOMMANDS: &[&str] = &[
            "run",
            "test",
            "dump",
            "version",
            "explain",
            "list-targets",
            "fmt",
            "help",
        ];
        if SUBCOMMANDS.contains(&args[1].as_str())
            || args[1] == "--help"
            || args[1] == "-h"
            || args[1] == "--version"
            || args[1] == "-V"
        {
            return Self::parse();
        }

        // If first arg starts with '-', or doesn't match subcommands, assume it's for run
        let mut modified_args = vec![args[0].clone(), "run".to_string()];
        modified_args.extend_from_slice(&args[1..]);

        Self::try_parse_from(modified_args).unwrap_or_else(|e| {
            e.exit();
        })
    }
}

impl EucalyptOptions {
    /// If the user has specified an evaluand (`-e`), we render a
    /// fragment rather than doc
    pub fn is_fragment(&self) -> bool {
        self.evaluate.is_some()
    }

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
        self.explain
    }

    pub fn parse_only(&self) -> bool {
        self.parse
    }

    pub fn dump_desugared(&self) -> bool {
        self.dump_desugared
    }

    pub fn dump_cooked(&self) -> bool {
        self.dump_cooked
    }

    pub fn dump_inlined(&self) -> bool {
        self.dump_inlined
    }

    pub fn dump_pruned(&self) -> bool {
        self.dump_pruned
    }

    pub fn dump_stg(&self) -> bool {
        self.dump_stg
    }

    pub fn dump_runtime(&self) -> bool {
        self.dump_runtime
    }

    pub fn format(&self) -> bool {
        self.format
    }

    pub fn format_width(&self) -> usize {
        self.format_width
    }

    pub fn format_write(&self) -> bool {
        self.format_write
    }

    pub fn format_check(&self) -> bool {
        self.format_check
    }

    pub fn format_reformat(&self) -> bool {
        self.format_reformat
    }

    pub fn format_indent(&self) -> usize {
        self.format_indent
    }

    pub fn list_targets(&self) -> bool {
        self.list_targets
    }

    pub fn show_version(&self) -> bool {
        self.version
    }

    pub fn test(&self) -> bool {
        self.test
    }

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
            && !self.format
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
        inputs.extend_from_slice(self.prologue_inputs.as_slice());
        inputs.extend_from_slice(self.explicit_inputs.as_slice());
        inputs.extend_from_slice(self.epilogue_inputs.as_slice());
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

    pub fn open_browser(&self) -> bool {
        self.open_browser
    }

    pub fn statistics(&self) -> bool {
        self.statistics
    }

    pub fn stg_settings(&self) -> &StgSettings {
        &self.stg_settings
    }

    /// Parse command line arguments using the new clap v4 structure
    pub fn from_args() -> Self {
        let cli = EucalyptCli::parse_with_fallback();
        let mut options = EucalyptOptions::from(cli);
        options.process_defaults().unwrap();
        options
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
        self.test = false;
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
                    Some("edn") => Some("edn".to_string()),
                    Some("toml") => Some("toml".to_string()),
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
        if self.explicit_inputs.is_empty() && !std::io::stdin().is_terminal() {
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

    /// Produce a dry run explanation of the selected options
    pub fn explanation(&self) -> String {
        let mut explanation = String::new();

        // command
        explanation.push_str("Eucalypt will ");
        if self.version {
            explanation.push_str("print its own version");
        } else if self.list_targets {
            explanation.push_str("list the targets available in the inputs");
        } else if self.test {
            explanation
                .push_str("evaluate the inputs (or all inputs in the directory) in test mode");
        } else if self.parse {
            explanation.push_str("parse the inputs into syntax trees and dump to standard out");
        } else if self.dump_desugared {
            explanation.push_str("parse inputs and translate to initial core representation then dump to standard out");
        } else if self.dump_cooked {
            explanation.push_str("parse inputs, translate to core, resolve operator precedence and dump to standard out");
        } else if self.dump_inlined {
            explanation.push_str("process inputs up to inlining then dump to standard out");
        } else if self.dump_pruned {
            explanation.push_str(
                "process inputs and simplify to final core representation and dump to standard out",
            );
        } else if self.dump_stg {
            explanation.push_str("process inputs through core phase and compile to STG code and dump to standard out");
        } else if self.dump_runtime {
            explanation.push_str("dump all STG intrinsic functions to standard out");
        } else {
            explanation.push_str("evaluate the inputs in production mode");
        }
        explanation.push_str("\n\n");

        // targets
        if let Some(t) = &self.target {
            explanation.push_str(&format!("Target: {t}\n\n"));
        }
        if let Some(e) = &self.evaluate {
            explanation.push_str(&format!("Evaluand: {e}\n\n"));
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
        explanation.push_str(&format!("Render output: {f} to {o}\n\n"));

        // inputs
        explanation.push_str("Inputs:\n");
        for i in &self.prologue_inputs {
            explanation.push_str(&format!(" • {i}\n"));
        }
        if let Some(name) = self.collection() {
            explanation.push_str(&format!(" • {name}=\n"));
        }
        for i in &self.explicit_inputs {
            if self.collection().is_some() {
                explanation.push_str("  ");
            }
            explanation.push_str(&format!(" • {i}\n"));
        }
        for i in &self.epilogue_inputs {
            explanation.push_str(&format!(" • {i}\n"));
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
