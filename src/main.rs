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

        /// Enable all safe optimizations (equivalent to setting all safe optimization flags)
        #[arg(long, conflicts_with_all = &["optimize_all"])]
        optimize_safe: bool,

        /// Enable ALL optimizations including unsafe ones (USE WITH CAUTION)
        #[arg(long, conflicts_with_all = &["optimize_safe"])]
        optimize_all: bool,

        /// Inline constant values that are used only once
        #[arg(long)]
        inline_constants: bool,

        /// Aggressively inline all constant values regardless of usage count
        #[arg(long)]
        inline_all_constants: bool,

        /// Inline property access chains that are used only once
        #[arg(long)]
        inline_property_access: bool,

        /// Aggressively inline all property access chains regardless of usage count
        #[arg(long)]
        inline_all_property_access: bool,

        /// Inline all uses of globalThis
        #[arg(long)]
        inline_global_this: bool,

        /// Simplify call patterns like fn.call(undefined, ...) to fn(...)
        #[arg(long)]
        simplify_calls: bool,

        /// Unsafely simplify method calls (e.g., obj.fn.call(obj, args) -> obj.fn(args))
        /// Warning: This transformation is not semantics-preserving in all cases
        #[arg(long)]
        unsafe_simplify_calls: bool,

        /// Inline parameter references to use original parameter names (this, arg0, arg1, etc.)
        #[arg(long)]
        inline_parameters: bool,

        /// Inline constructor calls (CreateThis/Construct/SelectObject pattern to new Constructor(...))
        #[arg(long)]
        inline_constructor_calls: bool,

        /// Inline object literals when safe (experimental)
        #[arg(long)]
        inline_object_literals: bool,
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

        /// Enable all safe optimizations (equivalent to setting all safe optimization flags)
        #[arg(long, conflicts_with_all = &["optimize_all"])]
        optimize_safe: bool,

        /// Enable ALL optimizations including unsafe ones (USE WITH CAUTION)
        #[arg(long, conflicts_with_all = &["optimize_safe"])]
        optimize_all: bool,

        /// Inline constant values that are used only once
        #[arg(long)]
        inline_constants: bool,

        /// Aggressively inline all constant values regardless of usage count
        #[arg(long)]
        inline_all_constants: bool,

        /// Inline property access chains that are used only once
        #[arg(long)]
        inline_property_access: bool,

        /// Aggressively inline all property access chains regardless of usage count
        #[arg(long)]
        inline_all_property_access: bool,

        /// Inline all uses of globalThis
        #[arg(long)]
        inline_global_this: bool,

        /// Simplify call patterns like fn.call(undefined, ...) to fn(...)
        #[arg(long)]
        simplify_calls: bool,

        /// Unsafely simplify method calls (e.g., obj.fn.call(obj, args) -> obj.fn(args))
        /// Warning: This transformation is not semantics-preserving in all cases
        #[arg(long)]
        unsafe_simplify_calls: bool,

        /// Inline parameter references to use original parameter names (this, arg0, arg1, etc.)
        #[arg(long)]
        inline_parameters: bool,

        /// Inline constructor calls (CreateThis/Construct/SelectObject pattern to new Constructor(...))
        #[arg(long)]
        inline_constructor_calls: bool,

        /// Inline object literals when safe (experimental)
        #[arg(long)]
        inline_object_literals: bool,
    },

    /// Analyze Metro/Metro-like package structure and module graph
    PackageAnalyze {
        /// Input HBC file
        input: PathBuf,
        /// Emit JSON report
        #[arg(long)]
        json: bool,
        /// Print only high-level summary in text mode
        #[arg(long)]
        summary: bool,
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
            optimize_safe,
            optimize_all,
            inline_constants,
            inline_all_constants,
            inline_property_access,
            inline_all_property_access,
            inline_global_this,
            simplify_calls,
            unsafe_simplify_calls,
            inline_parameters,
            inline_constructor_calls,
            inline_object_literals,
            format: _,
            minify: _,
            hbc_version: _,
        } => {
            // Apply optimization presets
            let (
                inline_constants,
                inline_all_constants,
                inline_property_access,
                inline_all_property_access,
                inline_global_this,
                simplify_calls,
                unsafe_simplify_calls,
                inline_parameters,
                inline_constructor_calls,
                inline_object_literals,
            ) = if optimize_all {
                // Enable ALL optimizations
                (true, true, true, true, true, true, true, true, true, true)
            } else if optimize_safe {
                // Enable safe optimizations only
                (true, true, true, true, true, true, false, true, true, true)
            } else {
                // Use individual flags
                (
                    inline_constants,
                    inline_all_constants,
                    inline_property_access,
                    inline_all_property_access,
                    inline_global_this,
                    simplify_calls,
                    unsafe_simplify_calls,
                    inline_parameters,
                    inline_constructor_calls,
                    inline_object_literals,
                )
            };

            let args = cli::decompile::DecompileArgs {
                input_path: input,
                function_index: function,
                output_path: output,
                comments,
                skip_validation,
                decompile_nested,
                inline_constants: Some(inline_constants),
                inline_all_constants: Some(inline_all_constants),
                inline_property_access: Some(inline_property_access),
                inline_all_property_access: Some(inline_all_property_access),
                inline_global_this: Some(inline_global_this),
                simplify_calls: Some(simplify_calls),
                unsafe_simplify_calls: Some(unsafe_simplify_calls),
                inline_parameters: Some(inline_parameters),
                inline_constructor_calls: Some(inline_constructor_calls),
                inline_object_literals: Some(inline_object_literals),
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
            optimize_safe,
            optimize_all,
            inline_constants,
            inline_all_constants,
            inline_property_access,
            inline_all_property_access,
            inline_global_this,
            simplify_calls,
            unsafe_simplify_calls,
            inline_parameters,
            inline_constructor_calls,
            inline_object_literals,
        } => {
            // Apply optimization presets
            let (
                _inline_constants, // Not used in analyze_cfg yet
                inline_all_constants,
                _inline_property_access, // Not used in analyze_cfg yet
                inline_all_property_access,
                inline_global_this,
                _simplify_calls, // Not used in analyze_cfg yet
                unsafe_simplify_calls,
                inline_parameters,
                inline_constructor_calls,
                inline_object_literals,
            ) = if optimize_all {
                // Enable ALL optimizations
                (true, true, true, true, true, true, true, true, true, true)
            } else if optimize_safe {
                // Enable safe optimizations only
                (true, true, true, true, true, true, false, true, true, true)
            } else {
                // Use individual flags
                (
                    inline_constants,
                    inline_all_constants,
                    inline_property_access,
                    inline_all_property_access,
                    inline_global_this,
                    simplify_calls,
                    unsafe_simplify_calls,
                    inline_parameters,
                    inline_constructor_calls,
                    inline_object_literals,
                )
            };

            cli::analyze_cfg::analyze_cfg(
                &input,
                function,
                verbose,
                inline_all_constants,
                inline_all_property_access,
                unsafe_simplify_calls,
                inline_global_this,
                inline_parameters,
                inline_constructor_calls,
                inline_object_literals,
            )
            .map_err(|e| miette!("{}", e))
        }
        Commands::PackageAnalyze {
            input,
            json,
            summary,
        } => cli::package::package_analyze(&input, json, summary).map_err(|e| miette!("{}", e)),
    }
}
