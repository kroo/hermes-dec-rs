//! Direct, local closure-to-property assignments, not runtime export resolution.

use std::collections::{HashMap, HashSet};

use crate::bundle::operands;
use crate::generated::generated_traits::is_jump_instruction;
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::function_table::HbcFunctionInstruction;
use crate::hbc::HbcFile;
use crate::DecompilerResult;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
    pub function_id: u32,
    pub source_function_id: u32,
    /// Function-relative byte offset of the property write.
    pub pc: u32,
    pub name: String,
    /// Syntactic assignment path; empty when the receiver is unknown.
    pub target: String,
}

#[derive(Clone)]
enum Value {
    Closure(u32),
    Path(String),
}

const MAX_PATH_BYTES: usize = 1024;

fn boundary(name: &str) -> bool {
    is_jump_instruction(name)
        || name.starts_with("Throw")
        || matches!(
            name,
            "Ret"
                | "Catch"
                | "SaveGenerator"
                | "SaveGeneratorLong"
                | "ResumeGenerator"
                | "StartGenerator"
                | "CompleteGenerator"
                | "Unreachable"
        )
}

fn relative_target(pc: u32, delta: i64) -> Option<u32> {
    u32::try_from(i64::from(pc).checked_add(delta)?).ok()
}

fn property_path(base: &str, name: &str) -> Option<String> {
    // Drop oversized paths rather than truncating a property or splitting UTF-8.
    // Each tracked base is bounded, so long chains cannot accumulate quadratic work.
    if base.len().saturating_add(name.len()).saturating_add(1) > MAX_PATH_BYTES {
        return None;
    }
    let mut chars = name.chars();
    let identifier = chars
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_' || c == '$')
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$');
    let path = if identifier {
        format!("{base}.{name}")
    } else {
        // JSON strings are also valid JavaScript property string literals.
        format!(
            "{base}[{}]",
            serde_json::to_string(name).unwrap_or_default()
        )
    };
    (path.len() <= MAX_PATH_BYTES).then_some(path)
}

/// Collect assignments proven by local closure creation and straight-line moves.
/// No environment slots, calls, computed properties, or control-flow joins are resolved.
pub fn collect(hbc: &HbcFile<'_>) -> DecompilerResult<Vec<Binding>> {
    let mut result = Vec::new();
    for source_function_id in 0..hbc.functions.count() {
        let instructions = hbc.functions.get_instructions_ref(source_function_id)?;
        // Most functions cannot contain a local closure-to-property assignment.
        // Avoid register/path tracking for those functions entirely.
        if !instructions.iter().any(|i| {
            matches!(
                i.instruction.name(),
                "CreateClosure"
                    | "CreateClosureLongIndex"
                    | "CreateGeneratorClosure"
                    | "CreateGeneratorClosureLongIndex"
                    | "CreateAsyncClosure"
                    | "CreateAsyncClosureLongIndex"
            )
        }) || !instructions.iter().any(|i| {
            matches!(
                i.instruction.name(),
                "PutById"
                    | "PutByIdLong"
                    | "TryPutById"
                    | "TryPutByIdLong"
                    | "PutNewOwnById"
                    | "PutNewOwnByIdShort"
                    | "PutNewOwnByIdLong"
                    | "PutNewOwnNEById"
                    | "PutNewOwnNEByIdLong"
            )
        }) {
            continue;
        }
        let mut targets = HashSet::new();
        if let Some(header) = hbc
            .functions
            .parsed_headers
            .get(source_function_id as usize)
        {
            targets.extend(header.exc_handlers.iter().map(|handler| handler.target));
        }
        // Pre-scan even unreachable branches: a backwards edge can target an
        // otherwise straight-line region before we encounter its source.
        for ins in instructions {
            let name = ins.instruction.name();
            let pc = ins.offset.0;
            if name == "SwitchImm" {
                let args = operands(&ins.instruction)?;
                if let Some(target) = args.get(2).and_then(|delta| relative_target(pc, *delta)) {
                    targets.insert(target);
                }
                if let Some(table) = hbc.switch_tables.get_switch_table_by_instruction(
                    source_function_id,
                    ins.instruction_index.0 as u32,
                ) {
                    targets.extend(
                        table
                            .cases
                            .iter()
                            .filter_map(|case| relative_target(pc, i64::from(case.target_offset))),
                    );
                } else {
                    // Without decoded case targets no region is safe to propagate.
                    targets.extend(instructions.iter().map(|instruction| instruction.offset.0));
                    break;
                }
            } else if is_jump_instruction(name)
                || matches!(name, "SaveGenerator" | "SaveGeneratorLong")
            {
                let args = operands(&ins.instruction)?;
                if let Some(target) = args.first().and_then(|delta| relative_target(pc, *delta)) {
                    targets.insert(target);
                }
            }
        }
        scan(hbc, source_function_id, instructions, &targets, &mut result)?;
    }
    Ok(result)
}

fn scan(
    hbc: &HbcFile<'_>,
    source_function_id: u32,
    instructions: &[HbcFunctionInstruction],
    targets: &HashSet<u32>,
    result: &mut Vec<Binding>,
) -> DecompilerResult<()> {
    let mut values: HashMap<i64, Value> = HashMap::new();
    for ins in instructions {
        let pc = ins.offset.0;
        if targets.contains(&pc) {
            values.clear();
        }
        let name = ins.instruction.name();
        if boundary(name) {
            values.clear();
            continue;
        }
        if let UnifiedInstruction::LoadConstDouble { operand_0, .. } = &ins.instruction {
            values.remove(&i64::from(*operand_0));
            continue;
        }
        let args = match operands(&ins.instruction) {
            Ok(args) => args,
            Err(_) => {
                // The instruction stream has already decoded successfully. Unsupported
                // operand representations are barriers, not navigation/decode failures.
                values.clear();
                continue;
            }
        };
        match name {
            "CreateClosure"
            | "CreateClosureLongIndex"
            | "CreateGeneratorClosure"
            | "CreateGeneratorClosureLongIndex"
            | "CreateAsyncClosure"
            | "CreateAsyncClosureLongIndex" => {
                values.remove(&args[0]);
                if let Ok(id) = u32::try_from(args[2]) {
                    if id < hbc.functions.count() {
                        values.insert(args[0], Value::Closure(id));
                    }
                }
            }
            "Mov" | "MovLong" => {
                let value = values.get(&args[1]).cloned();
                values.remove(&args[0]);
                if let Some(value) = value {
                    values.insert(args[0], value);
                }
            }
            "GetGlobalObject" => {
                values.insert(args[0], Value::Path("globalThis".into()));
            }
            "GetById" | "GetByIdShort" | "GetByIdLong" | "TryGetById" | "TryGetByIdLong" => {
                let base = values.get(&args[1]).cloned();
                values.remove(&args[0]);
                if let Some(Value::Path(base)) = base {
                    if let Ok(property) = hbc.strings.get(args[3] as u32) {
                        if let Some(path) = property_path(&base, &property) {
                            values.insert(args[0], Value::Path(path));
                        }
                    }
                }
            }
            "PutById"
            | "PutByIdLong"
            | "TryPutById"
            | "TryPutByIdLong"
            | "PutNewOwnById"
            | "PutNewOwnByIdShort"
            | "PutNewOwnByIdLong"
            | "PutNewOwnNEById"
            | "PutNewOwnNEByIdLong" => {
                let property_index = if name.starts_with("PutNewOwn") { 2 } else { 3 };
                if let Some(Value::Closure(function_id)) = values.get(&args[1]) {
                    if let Ok(property) = hbc.strings.get(args[property_index] as u32) {
                        let target = match values.get(&args[0]) {
                            Some(Value::Path(base)) => {
                                property_path(base, &property).unwrap_or_default()
                            }
                            _ => String::new(),
                        };
                        result.push(Binding {
                            function_id: *function_id,
                            source_function_id,
                            pc,
                            name: property,
                            target,
                        });
                    }
                }
            }
            _ => {
                // Conservatively forget every operand candidate, including secondary
                // outputs. Do not truncate Reg32 operands via u8 register analysis.
                for operand in args {
                    values.remove(&operand);
                }
                // Instructions with no operands cannot declare their clobbers.
                if ins.instruction.size() == 1 {
                    values.clear();
                }
            }
        }
    }
    Ok(())
}
