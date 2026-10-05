# Hermes-dec-rs

A Rust-based high-level decompiler for Hermes bytecode (HBC) that converts Hermes HBC bytecode into readable JavaScript source code.

## Features

- **HBC Parsing**: Table-driven instruction support for registered versions through HBC 96; fixture coverage includes HBC 90 and 96
- **Control Flow Analysis**: Convert bytecode into control flow graphs with modular architecture
- **High-level Constructs**: Raise to high-level constructs (`if/else`, loops, `try/catch`, `switch`)
- **OXC Integration**: Emit well-structured OXC AST and pretty-printed JavaScript
- **Full-Bundle Export**: Executable physical-register lowering, with parallel function generation and syntax validation
- **Parallel Disassembly**: Generate and format disassembly in parallel
- **Package Analysis**: Report CommonJS/Metro modules, dependencies, entrypoints, and suggested layout
- **Rich Diagnostics**: Color-coded error reporting with span information
- **Modular CFG**: Extensible control flow graph with separate modules for analysis, visualization, and regions

## Installation

### From Source

```bash
git clone https://github.com/kroo/hermes-dec-rs.git
cd hermes-dec-rs
cargo build --release
```

### From Crates.io (when published)

```bash
cargo install hermes-dec-rs
```

## Usage

### Inspect HBC File

```bash
# Inspect header and tables
hermes-dec-rs inspect input.hbc
hermes-dec-rs inspect input.hbc --format text
```

### Disassemble HBC File

```bash
# Basic disassembly
hermes-dec-rs disasm input.hbc

# Writes input.hasm beside the input file
hermes-dec-rs disasm input.hbc -o output.hasm --annotate-pc
```

### Analyze Control Flow Graph

```bash
# Generate CFG visualization
hermes-dec-rs cfg input.hbc --function 0 --dot cfg.dot

# Export to DOT format
hermes-dec-rs cfg input.hbc --function 0 --loops loops.dot --analysis analysis.dot
```

### Decompile to JavaScript

```bash
# Complete executable bundle, including all closures and the original entrypoint
hermes-dec-rs export-bundle input.hbc -o bundle.js
# Equivalent default mode
hermes-dec-rs decompile input.hbc -o bundle.js

# Structured single-function output
hermes-dec-rs decompile input.hbc --function 0

# With options
hermes-dec-rs decompile input.hbc \
  --function 0 \
  --comments ssa,instructions \
  --optimize-safe \
  -o output.js
```

Bundle export preserves physical registers, closures, compiled exception/finally
paths, generators, and iterator cleanup. Function names may be opaque; the output
is intentionally not a reconstruction of original source files. CommonJS bundles
run their first listed module; `export-bundle --entry-module ID` chooses another.
Metro/native-host bundles still require their original host globals and services.

`--minify` compacts either output mode. `decompile --hbc-version N` checks the
detected version rather than overriding decoding. Only `--format js` is accepted.
Structured comments, SSA optimizations, nested expansion and validation bypass
require `--function`; bundle export rejects these rather than ignoring them.
`--decompile-nested` remains experimental in structured mode. Unsupported bundle
opcodes/builtin versions fail before replacing output, without placeholders.

Behavioral evidence covers HBC 90 and 96. Parser registry coverage through 96
does not imply verified bundle semantics for every older version or all programs.
See [bundle validation](docs/bundle-validation.md) for reproducible workloads,
runtime requirements and limits of this evidence.

### Agent Exploration

Start with `inputs .` to include ignored/hidden input candidates, largest first.
Use `search input.hbc QUERY --json` for bounded function/PC evidence, then
`show input.hbc ID...` for complete correctness-first JavaScript bodies.
Search is OR by default; `--all` intersects queries within each function and
`--word` avoids short-substring noise.
`show --around-pc PC` gives a bounded JSON excerpt; `refs` follows static
closure/direct-call links; `slots` shows candidate captured-slot writes.
`captures input.hbc FUNCTION...` batches captured reads and candidate stores,
ranked by shortest static closure witness, with unresolved scope clearly marked.
`trace input.hbc FUNCTION PC` follows bounded local JS register-definition
provenance, not evaluated values or resolved runtime environments.
`sites input.hbc FUNCTION...` batch-catalogs constructor arguments, slot accesses
and property writes with bounded local provenance for large initializer scans.
`--kind call` exposes ordered user arguments separately from callee/receiver;
`--compact` deduplicates definition sources without evaluating values.
`--match TEXT` filters by literal source substrings in the site and bounded
prior definitions; `--from-pc`/`--to-pc` restrict inclusive function-local ranges.
These filters do not resolve runtime values or prove absence of behavior.
`workspace input.hbc -o NEW_DIR` generates individual JS files and a searchable
index for repeated offline reads. Unlike `export-bundle`, these function files
are inspection fragments, not standalone programs. See
[agent CLI contracts and evaluation](docs/agent-cli.md).

### Analyze Structure and Packages

```bash
hermes-dec-rs analyze-cfg input.hbc --function 0 --verbose
hermes-dec-rs package-analyze input.hbc --json
```

Package layout and clustering are heuristic reports, not reconstructed source
filenames; executable export does not depend on these heuristics.

## Architecture

```
┌─────────────┐   scroll    ┌─────────────┐   SSA/CFG   ┌─────────────┐   structurer   ┌─────────────┐
│ HBC Reader  │ ──────────▶ │ Instr Vec   │ ──────────▶ │   CFG (PG)  │ ─────────────▶ │  AST (OXC)  │
└─────────────┘             └─────────────┘             └─────────────┘                └─────────────┘
       ▲                                                       │               comments           │
 .hbc  │                                                       ▼                               ▼
       │                                               Pass pipeline                 oxc_codegen
```

## Development

### Building

```bash
cargo build
```

### Testing

```bash
# Run all tests
cargo test

# Run specific test modules
cargo test cfg
cargo test ast
cargo test hbc

# Run with output
cargo test -- --nocapture
```

### Benchmarks

```bash
cargo bench

# Opt-in full-bundle API benchmarks for local large fixtures (no file I/O)
HERMES_BENCH_LARGE=1 cargo bench --bench decompilation_benchmark full_bundle

# Full-project latency, deterministic output, syntax and sampled memory checks
node scripts/benchmark_bundle.mjs target/release/hermes-dec-rs input.hbc
```

## Project Structure

```
src/
├── main.rs              # CLI entry point
├── lib.rs               # Library entry point
├── error.rs             # Error handling
├── hbc/                 # HBC parsing
│   ├── mod.rs
│   ├── header.rs        # File header parsing
│   ├── tables/          # Table parsing modules
│   │   ├── mod.rs
│   │   ├── string_table.rs
│   │   ├── function_table.rs
│   │   ├── bigint_table.rs
│   │   ├── regexp_table.rs
│   │   ├── commonjs_table.rs
│   │   └── ...
│   ├── instructions/    # Instruction parsing
│   │   ├── mod.rs
│   │   ├── versions.rs
│   │   ├── hermes_repo_parser.rs
│   │   └── ...
│   └── ...
├── cfg/                 # Control flow graph (modular)
│   ├── mod.rs          # Main CFG interface
│   ├── block.rs        # Block definitions and operations
│   ├── builder.rs      # CFG construction logic
│   ├── analysis.rs     # Advanced analysis algorithms
│   ├── visualization.rs # DOT export functionality
│   └── regions.rs      # Region detection and analysis
├── ast/                 # OXC AST building
│   └── mod.rs
├── decompiler.rs        # Main decompiler logic
├── generated/           # Generated instruction definitions
│   ├── mod.rs
│   ├── unified_instructions.rs
│   └── generated_traits.rs
└── cli/                 # CLI subcommands
    ├── mod.rs
    ├── inspect.rs
    ├── disasm.rs
    ├── cfg.rs
    ├── decompile.rs
    └── generate.rs
```

## CFG Module Architecture

The CFG module has been refactored into a modular structure to enable parallel development:

- **`block.rs`**: Basic block definitions and operations
- **`builder.rs`**: CFG construction from instructions
- **`analysis.rs`**: Dominator analysis, loop detection, and advanced algorithms
- **`visualization.rs`**: DOT format export for graph visualization
- **`regions.rs`**: Region detection for if/else and switch structures

## Implementation Status

See [docs/roadmap.md](docs/roadmap.md) for the current scope and next milestones.

### ✅ Completed
- **HBC Parser**: Header, tables, version registry, and instruction decoding
- **String/Function Tables**: Complete table parsing infrastructure
- **Instruction System**: Unified instruction handling across versions
- **CFG Foundation**: Basic control flow graph construction
- **AST Integration**: OXC AST builder integration
- **Structured Output**: Conditionals, switches, loops, and selected exception patterns
- **Package Reports**: CommonJS/Metro dependency and layout analysis
- **Test Suite**: Parser/CFG checks and fixture-based decompilation regressions
- **Bundle Export**: Complete function table, shared environments and CommonJS bootstrap
- **CLI Options**: Output paths, PC annotation, JSON/text inspection, minify and version checks

### 🚧 In Progress
- **Exception Structuring**: Complex exception loops use an instruction dispatch fallback that preserves handler ranges; output is less readable than structured try/catch
- **Semantic Coverage**: Broader runtime comparisons and native-host app execution

### 📋 Planned
- **Readable Module Export**: Recover source layout where evidence supports it
- **Performance Optimization**: Maintain a sub-second release target on large bundles
- **Parser Hardening**: Malformed-input tests and explicit version/opcode coverage

## Dependencies

- **scroll**: Zero-copy binary parsing
- **petgraph**: Graph algorithms for CFG
- **oxc_***: OXC ecosystem for AST and code generation
- **rayon**: Parallel processing
- **thiserror + miette**: Rich error handling and diagnostics
- **clap**: CLI argument parsing

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Development Issues

The project uses GitHub issues for tracking development work and parallel development:

- **CFG-01**: PC-to-block mapping
- **CFG-02**: Basic block identification  
- **CFG-03**: Edge creation and analysis
- **CFG-04**: Dominator analysis
- **CFG-05**: Natural loop detection
- **CFG-06**: Post-dominator analysis
- **CFG-07**: If/else region detection
- **CFG-08**: Switch region detection
- **CFG-09**: Control flow structuring
- **CFG-10**: AST generation

See [GitHub Issues](https://github.com/kroo/hermes-dec-rs/issues?q=is%3Aissue+is%3Aopen+label%3Acfg) for detailed specifications and progress tracking.

## License

This project is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Acknowledgments

- [Hermes Engine](https://hermesengine.dev/) - The JavaScript engine this decompiler targets
- [OXC](https://oxc-project.github.io/) - The JavaScript/TypeScript compiler infrastructure
- [React Native](https://reactnative.dev/) - The primary use case for this tool
