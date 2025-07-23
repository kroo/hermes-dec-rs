# Hermes-dec-rs

A Rust-based high-level decompiler for Hermes bytecode (HBC) that converts Hermes HBC bytecode into readable JavaScript source code.

## Features

- **HBC Parsing**: Parse all HBC versions ≥ 80 (React Native 0.72+)
- **Control Flow Analysis**: Convert bytecode into control flow graphs with modular architecture
- **High-level Constructs**: Raise to high-level constructs (`if/else`, loops, `try/catch`, `switch`)
- **OXC Integration**: Emit well-structured OXC AST and pretty-printed JavaScript
- **Parallel Processing**: Process functions in parallel for better performance
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
# Inspect header and tables as JSON
hermes-dec-rs inspect input.hbc --format json

# Inspect as text
hermes-dec-rs inspect input.hbc --format text
```

### Disassemble HBC File

```bash
# Basic disassembly
hermes-dec-rs disasm input.hbc

# With PC annotations
hermes-dec-rs disasm input.hbc --annotate-pc

# Output to file
hermes-dec-rs disasm input.hbc -o disassembly.txt
```

### Analyze Control Flow Graph

```bash
# Generate CFG visualization
hermes-dec-rs cfg input.hbc --function 0 --dot

# Export to DOT format
hermes-dec-rs cfg input.hbc --function 0 --dot -o cfg.dot
```

### Decompile to JavaScript

```bash
# Basic decompilation
hermes-dec-rs decompile input.hbc

# With options
hermes-dec-rs decompile input.hbc \
  --format js \
  --comments pc \
  --minify \
  -o output.js
```

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

### ✅ Completed
- **HBC Parser**: Full support for Hermes bytecode parsing
- **String/Function Tables**: Complete table parsing infrastructure
- **Instruction System**: Unified instruction handling across versions
- **CFG Foundation**: Basic control flow graph construction
- **AST Integration**: OXC AST builder integration
- **CLI Framework**: Complete command-line interface
- **Test Suite**: Comprehensive test coverage

### 🚧 In Progress
- **CFG Analysis**: Advanced control flow analysis algorithms
- **Region Detection**: If/else and switch structure detection
- **AST Generation**: Converting CFG to OXC AST nodes

### 📋 Planned
- **Control Flow Structuring**: High-level construct reconstruction
- **Code Generation**: JavaScript output from AST
- **Performance Optimization**: Parallel processing improvements
- **Advanced Features**: Type inference, dead code elimination

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