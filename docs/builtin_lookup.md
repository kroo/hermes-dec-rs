# Builtin Lookup

Hermes bytecode encodes calls to runtime builtins using small numeric IDs.
The set of available builtins changes between Hermes versions which makes
interpreting these calls difficult without additional metadata.

During code generation a lookup table is produced for each supported Hermes
version. At runtime the disassembler reads the bytecode version from the HBC
header and uses this table to resolve builtin IDs in `CallBuiltin`,
`CallBuiltinLong` and `GetBuiltinClosure` instructions to human readable
names. Resolution is O(1) and falls back to the numeric ID when a mapping is
not available.

The generated lookup lives in `src/generated/builtins.rs`. To refresh the
table after updating supported Hermes versions run:

```bash
cargo run -- generate
```

This will parse builtin definition files from the Hermes repository and
regenerate the lookup tables.
