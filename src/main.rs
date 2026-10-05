use clap::{Parser, Subcommand};
use miette::{miette, Result};
use std::path::PathBuf;

use hermes_dec_rs::cli;

#[derive(Parser)]
#[command(name = "hermes-dec-rs")]
#[command(about = "Rust-based high-level decompiler for Hermes bytecode")]
#[command(
    after_help = concat!(
        "Agent workflow: inputs . finds hidden/Git-ignored header candidates, largest first.\n",
        "Then inspect INPUT, search INPUT QUERY --json, and show INPUT FUNCTION_IDS for complete JS.\n",
        "Search is OR by default; --all intersects queries and --word avoids substring noise.\n",
        "Huge initializers: sites INPUT FUNCTION --match NAME --depth 8 --compact filters local source dependencies.\n",
        "--from-pc/--to-pc narrow inclusive byte-PC ranges; --kind call separates callee/receiver/user arguments.\n",
        "Opaque captured slots: captures INPUT FUNCTION_IDS batches reads and candidate ancestor stores.\n",
        "Cross-block registers: origins INPUT FUNCTION PC reports candidate definitions on normal JS paths.\n",
        "Inspect a candidate: sites INPUT ANCESTOR --kind slot-write --slot N --depth 8 --compact.\n",
        "Use show --around-pc PC --context 250 for bounded JS, refs for static closures/calls, trace for one definition DAG.\n",
        "All matches/captures are syntactic navigation, not evaluated values or authoritative runtime bindings.\n",
        "Follow serializers and callers to verify units/indexing; a schema inventory is not a protocol description.\n",
        "workspace exports complete JS fragments and a bounded index for shell inspection.\n",
        "export-bundle emits runnable full-project JS; legacy decompile --function uses slower CFG/SSA analysis."
    )
)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Experimental bounded reaching-definition candidates over exporter JS control flow
    Origins {
        input: PathBuf,
        function: u32,
        pc: u32,
        /// Dependency depth (0..64); alternatives remain candidates, never runtime values
        #[arg(long, default_value_t = 8)]
        depth: usize,
        /// Maximum definition nodes (1..4096)
        #[arg(long, default_value_t = 64)]
        limit: usize,
        /// JSON byte budget; errors before stdout rather than partial documents
        #[arg(long, default_value_t = 100_000)]
        max_bytes: usize,
    },
    /// Batch captured-slot reads and same-function/ancestor store candidates with JS evidence
    Captures {
        input: PathBuf,
        #[arg(required = true, num_args = 1.., value_delimiter = ',')]
        functions: Vec<u32>,
        #[arg(long, default_value_t = 3)]
        depth: usize,
        #[arg(long, default_value_t = 5)]
        limit: usize,
        #[arg(long, default_value_t = 0)]
        offset: usize,
        #[arg(long, default_value_t = 100_000)]
        max_bytes: usize,
    },
    /// Discover header candidates, including ignored files; no full program validation
    Inputs {
        #[arg(default_value = ".")]
        root: PathBuf,
        #[arg(long, default_value_t = 20)]
        limit: usize,
        #[arg(long, default_value_t = 0)]
        offset: usize,
        #[arg(long, default_value_t = 100_000)]
        max_bytes: usize,
    },
    /// Catalog JS call/constructor arguments, writes and slot accesses with local provenance
    Sites {
        input: PathBuf,
        /// Explicit function IDs, comma-separated or batched (no automatic whole-project scan)
        #[arg(required = true, num_args = 1.., value_delimiter = ',')]
        functions: Vec<u32>,
        #[arg(long, default_value = "all", value_parser = ["all", "constructor", "call", "slot-write", "slot-read", "property-write"])]
        kind: String,
        /// Filter slot indices; excludes non-slot sites, including with --kind all
        #[arg(long, value_delimiter = ',')]
        slot: Vec<u32>,
        /// Local definition depth (0..8); candidates, not evaluated values
        #[arg(long, default_value_t = 3)]
        depth: usize,
        /// Bounded page size (1..1000); small default keeps provenance within byte budget
        #[arg(long, default_value_t = 5)]
        limit: usize,
        #[arg(long, default_value_t = 0)]
        offset: usize,
        /// JSON byte budget; errors before stdout, no partial document
        #[arg(long, default_value_t = 100_000)]
        max_bytes: usize,
        /// Deduplicate provenance definitions into a shared table (same syntactic evidence)
        #[arg(long)]
        compact: bool,
        /// OR source substrings in site expressions/local prior definitions (repeatable)
        #[arg(long = "match")]
        matches: Vec<String>,
        /// Inclusive function-local PC range, applied separately to each selected function
        #[arg(long)]
        from_pc: Option<u32>,
        #[arg(long)]
        to_pc: Option<u32>,
    },
    /// Trace bounded syntactic JS register definitions at an exact byte PC (not values)
    Trace {
        input: PathBuf,
        function: u32,
        pc: u32,
        /// Dependency edge depth (0..64); no cross-block or runtime evaluation
        #[arg(long, default_value_t = 8)]
        depth: usize,
        /// Maximum provenance nodes (1..4096); errors instead of silent omission
        #[arg(long, default_value_t = 64)]
        limit: usize,
        /// JSON output byte budget (1..16777216)
        #[arg(long, default_value_t = 100_000)]
        max_bytes: usize,
    },
    /// Find candidate captured-slot writes in closure ancestors, with bounded JS excerpts
    Slots {
        input: PathBuf,
        function: u32,
        slot: u32,
        #[arg(long, default_value_t = 3)]
        depth: usize,
        #[arg(long, default_value_t = 10)]
        limit: usize,
    },
    /// Generate a new directory of complete JS function files and a searchable index
    Workspace {
        input: PathBuf,
        /// New output directory (must not already exist)
        #[arg(short, long)]
        output: PathBuf,
    },
    /// Find names/literals with function IDs and byte PCs; bounded results, no huge dumps
    Search {
        input: PathBuf,
        #[arg(required = true, num_args = 1..)]
        query: Vec<String>,
        /// Treat queries as regular expressions (otherwise OR substrings)
        #[arg(long)]
        regex: bool,
        #[arg(long)]
        case_sensitive: bool,
        /// Match whole alphanumeric components (underscore/punctuation are boundaries)
        #[arg(long)]
        word: bool,
        /// Require every query to match within each function (default: any query)
        #[arg(long)]
        all: bool,
        #[arg(long, default_value_t = 20)]
        limit: usize,
        #[arg(long, default_value_t = 0)]
        offset: usize,
        /// Compact machine-readable JSON (default: pretty JSON)
        #[arg(long)]
        json: bool,
    },
    /// Read complete decompiled JS for multiple function IDs in one parse (not standalone)
    Show {
        input: PathBuf,
        #[arg(required = true, num_args = 1.., value_delimiter = ',')]
        functions: Vec<u32>,
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Include metadata and JS in machine-readable JSON
        #[arg(long)]
        json: bool,
        /// Output budget; errors instead of silently truncating JavaScript
        #[arg(long, default_value_t = 100_000)]
        max_bytes: usize,
        /// Return a bounded JSON excerpt around an exact byte PC (one function only)
        #[arg(long)]
        around_pc: Option<u32>,
        /// Number of surrounding HBC instructions for --around-pc (0..1000)
        #[arg(long, default_value_t = 8)]
        context: usize,
    },
    /// Follow static closure creators/children and direct calls (not dynamic call graph)
    Refs {
        input: PathBuf,
        #[arg(required = true, num_args = 1.., value_delimiter = ',')]
        functions: Vec<u32>,
        #[arg(long, default_value = "both", value_parser = ["in", "out", "both"])]
        direction: String,
        #[arg(long, default_value_t = 1)]
        depth: usize,
        #[arg(long, default_value_t = 100)]
        limit: usize,
    },
    /// Export every HBC function as executable JavaScript with opaque names
    ExportBundle {
        input: PathBuf,
        #[arg(short, long)]
        output: Option<PathBuf>,
        #[arg(long)]
        minify: bool,
        /// CommonJS module name or numeric module ID to execute
        #[arg(long)]
        entry_module: Option<String>,
    },
    /// Inspect HBC file header and tables
    Inspect {
        /// Input HBC file
        input: PathBuf,

        /// Output format (summary is bounded JSON; json dumps all tables)
        #[arg(short, long, default_value = "summary", value_parser = ["summary", "json", "text"])]
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

        /// Function index; omit to export the complete executable bundle
        #[arg(long)]
        function: Option<usize>,

        /// Output file (defaults to stdout)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Output format (JavaScript only)
        #[arg(short, long, default_value = "js", value_parser = ["js"])]
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
        Commands::Origins {
            input,
            function,
            pc,
            depth,
            limit,
            max_bytes,
        } => cli::origins::run(&input, function, pc, depth, limit, max_bytes)
            .map_err(|e| miette!("{e}")),
        Commands::Captures {
            input,
            functions,
            depth,
            limit,
            offset,
            max_bytes,
        } => cli::captures::run(&input, &functions, depth, limit, offset, max_bytes)
            .map_err(|e| miette!("{e}")),
        Commands::Inputs {
            root,
            limit,
            offset,
            max_bytes,
        } => cli::inputs::run(&root, limit, offset, max_bytes).map_err(|e| miette!("{e}")),
        Commands::Sites {
            input,
            functions,
            kind,
            slot,
            depth,
            limit,
            offset,
            max_bytes,
            compact,
            matches,
            from_pc,
            to_pc,
        } => cli::sites::run_filtered(
            &input,
            &functions,
            &kind,
            &slot,
            depth,
            limit,
            offset,
            max_bytes,
            compact,
            &cli::sites::SiteFilter {
                matches,
                from_pc,
                to_pc,
            },
        )
        .map_err(|e| miette!("{e}")),
        Commands::Trace {
            input,
            function,
            pc,
            depth,
            limit,
            max_bytes,
        } => cli::trace::run(&input, function, pc, depth, limit, max_bytes)
            .map_err(|e| miette!("{e}")),
        Commands::Slots {
            input,
            function,
            slot,
            depth,
            limit,
        } => cli::slots::run(&input, function, slot, depth, limit).map_err(|e| miette!("{e}")),
        Commands::Workspace { input, output } => {
            cli::workspace::workspace(&input, &output).map_err(|e| miette!("{e}"))?;
            println!(
                "{}",
                serde_json::json!({"schema_version":1,"workspace":output,"manifest":"manifest.json","index":"index.jsonl"})
            );
            Ok(())
        }
        Commands::Search {
            input,
            query,
            regex,
            case_sensitive,
            word,
            all,
            limit,
            offset,
            json,
        } => cli::explore::search(
            &input,
            &query,
            &cli::explore::SearchOptions {
                regex,
                case_sensitive,
                word,
                all,
                limit,
                offset,
                json,
            },
        )
        .map_err(|e| miette!("{e}")),
        Commands::Show {
            input,
            functions,
            output,
            json,
            max_bytes,
            around_pc,
            context,
        } => cli::explore::show(
            &input,
            &functions,
            output.as_deref(),
            json,
            max_bytes,
            around_pc,
            context,
        )
        .map_err(|e| miette!("{e}")),
        Commands::Refs {
            input,
            functions,
            direction,
            depth,
            limit,
        } => cli::explore::refs(&input, &functions, &direction, depth, limit)
            .map_err(|e| miette!("{e}")),
        Commands::ExportBundle {
            input,
            output,
            minify,
            entry_module,
        } => cli::bundle::export(&input, output.as_deref(), minify, entry_module)
            .map_err(|e| miette!("{}", e)),
        Commands::Inspect { input, format } => {
            cli::inspect::inspect_with_format(&input, &format).map_err(|e| miette!("{}", e))
        }
        Commands::Disasm {
            input,
            output,
            annotate_pc,
        } => cli::disasm::disasm_with_options(&input, output.as_deref(), annotate_pc)
            .map_err(|e| miette!("{}", e)),
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
            minify,
            hbc_version,
        } => {
            if function.is_none() {
                if comments != "none"
                    || skip_validation
                    || decompile_nested
                    || optimize_safe
                    || optimize_all
                    || inline_constants
                    || inline_all_constants
                    || inline_property_access
                    || inline_all_property_access
                    || inline_global_this
                    || simplify_calls
                    || unsafe_simplify_calls
                    || inline_parameters
                    || inline_constructor_calls
                    || inline_object_literals
                {
                    return Err(miette!("Whole-bundle export does not accept structured-function analysis/optimization flags; use --function for those options"));
                }
                if let Some(version) = hbc_version {
                    let data = std::fs::read(&input).map_err(|error| miette!("{error}"))?;
                    let hbc =
                        hermes_dec_rs::HbcFile::parse(&data).map_err(|error| miette!("{error}"))?;
                    if hbc.header.version() != version {
                        return Err(miette!(
                            "Expected HBC {version}, found {}",
                            hbc.header.version()
                        ));
                    }
                }
                return cli::bundle::export(&input, output.as_deref(), minify, None)
                    .map_err(|e| miette!("{e}"));
            }
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
                function_index: function.unwrap(),
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
                minify,
                hbc_version,
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
            dot.as_deref(),
            loops.as_deref(),
            analysis.as_deref(),
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
