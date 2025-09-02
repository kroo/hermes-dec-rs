use clap::{Parser, Subcommand};
use miette::{miette, Result};
use std::path::PathBuf;

use hermes_dec_rs::cli;

#[derive(Parser)]
#[command(name = "hermes-dec-rs")]
#[command(about = "Rust-based high-level decompiler for Hermes bytecode")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Inspect HBC file header and tables
    Inspect {
        /// Input HBC file
        input: PathBuf,

        /// Output format (json, text)
        #[arg(short, long, default_value = "json")]
        format: String,
    },

    /// Disassemble HBC file to flat instruction list
    Disasm {
        /// Input HBC file
        input: PathBuf,

        /// Output file (defaults to stdout)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Include program counter annotations
        #[arg(long)]
        annotate_pc: bool,
    },

    /// Decompile HBC file to JavaScript/TypeScript
    Decompile {
        /// Input HBC file
        input: PathBuf,

        /// Function index to decompile (required for now)
        #[arg(long)]
        function: usize,

        /// Output file (defaults to stdout)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Output format (js, ts)
        #[arg(short, long, default_value = "js")]
        format: String,

        /// Include comments (pc, reg, instructions, ssa, none)
        #[arg(long, default_value = "none")]
        comments: String,

        /// Minify output
        #[arg(long)]
        minify: bool,

        /// HBC version (auto-detected if not specified)
        #[arg(long)]
        hbc_version: Option<u32>,

        /// Skip validation of block processing
        #[arg(long)]
        skip_validation: bool,

        /// Decompile nested function definitions (experimental)
        #[arg(long)]
        decompile_nested: bool,

        /// Inline constant values that are used only once
        #[arg(long)]
        inline_constants: Option<bool>,

        /// Aggressively inline all constant values regardless of usage count
        #[arg(long)]
        inline_all_constants: Option<bool>,
        
        /// Inline property access chains that are used only once
        #[arg(long)]
        inline_property_access: Option<bool>,
        
        /// Aggressively inline all property access chains regardless of usage count
        #[arg(long)]
        inline_all_property_access: Option<bool>,
        
        /// Inline all uses of globalThis (defaults to true if any other inlining is enabled)
        #[arg(long)]
        inline_global_this: Option<bool>,
        
        /// Simplify call patterns like fn.call(undefined, ...) to fn(...)
        #[arg(long)]
        simplify_calls: Option<bool>,
        
        /// Unsafely simplify method calls (e.g., obj.fn.call(obj, args) -> obj.fn(args))
        /// Warning: This transformation is not semantics-preserving in all cases
        #[arg(long)]
        unsafe_simplify_calls: Option<bool>,
    },

    /// Generate unified instruction definitions from Hermes source
    Generate {
        /// Force regeneration even if files exist
        #[arg(short, long)]
        force: bool,
    },

    /// Build and analyze control flow graphs
    Cfg {
        /// Input HBC file
        input: PathBuf,
        /// Function index to analyze (optional, analyzes all if not specified)
        #[arg(short, long)]
        function: Option<usize>,
        /// Output DOT file for visualization (optional)
        #[arg(short, long)]
        dot: Option<PathBuf>,
        /// Generate DOT file with loop analysis visualization (optional)
        #[arg(long)]
        loops: Option<PathBuf>,
        /// Generate DOT file with comprehensive analysis visualization (optional)
        #[arg(long)]
        analysis: Option<PathBuf>,
    },

    /// Analyze control flow structures (conditionals, loops, etc.)
    AnalyzeCfg {
        /// Input HBC file
        input: PathBuf,
        /// Function index to analyze
        #[arg(short, long)]
        function: usize,
        /// Show verbose analysis (dominance frontiers, liveness)
        #[arg(short, long)]
        verbose: bool,
    },
}

fn main() -> Result<()> {
    // Initialize logging
    env_logger::init();

    let cli = Cli::parse();

    match cli.command {
        Commands::Inspect { input, format: _ } => {
            cli::inspect::inspect(&input).map_err(|e| miette!("{}", e))
        }
        Commands::Disasm {
            input,
            output: _,
            annotate_pc: _,
        } => cli::disasm::disasm(&input).map_err(|e| miette!("{}", e)),
        Commands::Decompile {
            input,
            function,
            output,
            comments,
            skip_validation,
            decompile_nested,
            inline_constants,
            inline_all_constants,
            inline_property_access,
            inline_all_property_access,
            inline_global_this,
            simplify_calls,
            unsafe_simplify_calls,
            format: _,
            minify: _,
            hbc_version: _,
        } => {
            let args = cli::decompile::DecompileArgs {
                input_path: input,
                function_index: function,
                output_path: output,
                comments,
                skip_validation,
                decompile_nested,
                inline_constants,
                inline_all_constants,
                inline_property_access,
                inline_all_property_access,
                inline_global_this,
                simplify_calls,
                unsafe_simplify_calls,
            };
            cli::decompile::decompile(&args).map_err(|e| miette!("{}", e))
        }
        Commands::Generate { force: _ } => {
            cli::generate::generate_instructions().map_err(|e| miette!("{}", e))
        }
        Commands::Cfg {
            input,
            function,
            dot,
            loops,
            analysis,
        } => cli::cfg::cfg(
            &input,
            function,
            dot.as_ref().map(|v| &**v),
            loops.as_ref().map(|v| &**v),
            analysis.as_ref().map(|v| &**v),
        )
        .map_err(|e| miette!("{}", e)),
        Commands::AnalyzeCfg {
            input,
            function,
            verbose,
        } => cli::analyze_cfg::analyze_cfg(&input, function, verbose).map_err(|e| miette!("{}", e)),
    }
}
