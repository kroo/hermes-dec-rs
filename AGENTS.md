# Repository Guidelines

## Project Structure & Module Organization
- `src/`: core library and CLI (`main.rs`, `lib.rs`), modules in `analysis/`, `ast/`, `cfg/`, `cli/`, `hbc/`, and `generated/`.
- `tests/`: integration tests (one file per feature, e.g., `tests/cfg.rs`, `tests/decompiler.rs`).
- `benches/`: Criterion benchmarks (e.g., `decompilation_benchmark.rs`).
- `data/`: sample inputs and artifacts used by tests/docs.
- `scripts/`: helpers for CFG/visualization generation.
- `docs/`: design notes and user-facing docs.

## Build, Test, and Development Commands
- Build: `cargo build` (release: `cargo build --release`).
- Run CLI: `cargo run -- <subcommand>` (e.g., `inspect`, `disasm`, `cfg`, `decompile`, `analyze-cfg`).
- Test: `cargo test` (examples: `cargo test cfg`, `cargo test decompiler`; verbose: `cargo test -- --nocapture`).
- Bench: `cargo bench`.
- Format: `cargo fmt --all --check`.
- Lint: `cargo clippy --all-targets --all-features -- -W clippy::all`.
- Generate instruction definitions: `cargo run -- generate` (updates unified instruction tables).

## CLI: Decompile, Analyze-CFG, CFG DOT
- Inputs: `.hbc` files live in `data/`.
- Decompile one function to JS: `cargo run -- decompile data/sample.hbc --function 0 -o out.js --comments ssa,instructions --optimize-safe`.
  - Useful flags: `--decompile-nested`, `--skip-validation`; optimization presets `--optimize-safe` or `--optimize-all`, or individual inliners like `--inline-constants`, `--inline-property-access`, `--inline-global-this`.
- Analyze CFG (no JS output): `cargo run -- analyze-cfg data/sample.hbc --function 0 --verbose`.
  - Prints conditional chains, switch regions, loop info; with `--verbose` also dominance frontiers and live-in/out sets. Accepts the same optimization presets/inline flags to influence analysis.
- Export CFG visuals (DOT): `cargo run -- cfg data/sample.hbc --function 0 --dot cfg.dot --loops loops.dot --analysis analysis.dot`.
  - Omit `--function` to iterate all functions; DOT files can be rendered with Graphviz (`dot -Tpng cfg.dot -o cfg.png`).

## Coding Style & Naming Conventions
- Rust 2021, rustfmt defaults (4-space indent, no tabs); keep functions short and cohesive.
- Naming: modules/files `snake_case`, types/enums `CamelCase`, functions/vars `snake_case`.
- Prefer `Result` over panics; use `thiserror`/`miette` for diagnostics; log via `env_logger` (`RUST_LOG=info`).
- Address all Clippy warnings in PRs; keep public APIs stable unless justified.

## Testing Guidelines
- Add integration tests under `tests/` with descriptive filenames; keep unit tests near code when appropriate.
- Use realistic fixtures from `data/`; avoid committing large/proprietary binaries.
- For debugging, run `cargo test -- --nocapture` and target modules (e.g., `cargo test decompiler`).

## Commit & Pull Request Guidelines
- Commits: imperative mood with scope tag when helpful (e.g., `cfg: handle irreducible loops`, `AST-08: improve exception handling`), reference issues (`#123`).
- PRs: include clear description, linked issues, tests and docs updated, and sample CLI output or CFG diff when behavior changes.
- CI must pass: fmt, clippy, build, test; run `cargo audit` locally when touching dependencies.

## Security & Configuration Tips
- Avoid network access in tests unless mocked; prefer deterministic inputs.
- Configure logs locally with `RUST_LOG=debug` for deep traces; keep default at `info` in docs/examples.
- Opcode reference: `python3 docs/analyze_opcodes.py [OPCODE_NAME]` for quick instruction docs.
