This is a repo that supports conversion from hermes bytecode back to javascript.  Here are a few important instructions:
 - hermes bytecode ends with .hbc, and lives in ./data/*.hbc
 - `cargo run disasm` will generate a .hasm file in the same directory as an hbc file (in the data directory, these all should exist already)
 - `cargo run decompile [hbcfile] --function [id]` will decompile a function by id.  Consider using `--comments [ssa],[instructions]` to annotate the source code.
 - `cargo run analyze-cfg [hbcfile] --function [id]` will analyze the control flow graph and instructions inside the function by id.  This will give you the assembly for the function, as well as the control flow plan -- but doesn't try to convert to javascript.
 - if you need to list all instruction types, `python docs/analyze_opcodes.py` (with an optional `opcode name` arg) will give you some short documentation on each opcode (or a single opcode if you specify the name).

When fixing compiler warnings:
 - Do NOT add #[allow(dead_code)] to unused functions or variables.
 - Do NOT add #[allow(unused_variables)] to unused variables, or prefix unused variables with an underscore.
