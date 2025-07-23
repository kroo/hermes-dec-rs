# Hermes-dec-rs

A Rust-based high-level decompiler for Hermes bytecode (HBC) that converts Hermes HBC bytecode into readable ES/TS source code.

## Features

- **HBC Parsing**: Parse all HBC versions ≥ 80 (React Native 0.72+)
- **Control Flow Analysis**: Convert bytecode into control flow graphs
- **High-level Constructs**: Raise to high-level constructs (`if/else`, loops, `try/catch`, `switch`)
- **SWC Integration**: Emit well-structured SWC AST and pretty-printed JavaScript
- **Parallel Processing**: Process functions in parallel for better performance
- **Rich Diagnostics**: Color-coded error reporting with span information

## Installation

### From Source

```bash
git clone https://github.com/yourusername/hermes-dec-rs.git
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
│ HBC Reader  │ ──────────▶ │ Instr Vec   │ ──────────▶ │   CFG (PG)  │ ─────────────▶ │  AST (SWC)  │
└─────────────┘             └─────────────┘             └─────────────┘                └─────────────┘
       ▲                                                       │               comments           │
 .hbc  │                                                       ▼                               ▼
       │                                               Pass pipeline                 swc_ecma_codegen
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
│   ├── tables.rs        # String/RegExp/BigInt tables
│   └── instructions.rs  # Instruction parsing
├── cfg/                 # Control flow graph
│   └── mod.rs
├── ast/                 # SWC AST building
│   └── mod.rs
├── decompiler.rs        # Main decompiler logic
└── cli/                 # CLI subcommands
    ├── mod.rs
    ├── inspect.rs
    ├── disasm.rs
    └── decompile.rs
```

## Dependencies

- **scroll**: Zero-copy binary parsing
- **petgraph**: Graph algorithms for CFG
- **swc_***: SWC ecosystem for AST and code generation
- **rayon**: Parallel processing
- **thiserror + miette**: Rich error handling and diagnostics
- **clap**: CLI argument parsing

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Acknowledgments

- [Hermes Engine](https://hermesengine.dev/) - The JavaScript engine this decompiler targets
- [SWC](https://swc.rs/) - The JavaScript/TypeScript compiler infrastructure
- [React Native](https://reactnative.dev/) - The primary use case for this tool 