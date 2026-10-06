//! Correctness-first whole-file lowering using physical registers and byte PCs.

use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::serialized_literal_parser::{unpack_slp_array, SLPValue};
use crate::hbc::tables::function_table::HbcFunctionInstruction;
use crate::hbc::HbcFile;
use crate::{DecompilerError, DecompilerResult};
use rayon::prelude::*;
use std::collections::{BTreeMap, HashSet};
use std::fmt::Write;

#[derive(Debug, Default, Clone)]
pub struct BundleOptions {
    /// Compact generated JavaScript without changing instruction semantics.
    pub minify: bool,
    /// CommonJS module to run instead of the first module in the bytecode table.
    pub entry_module: Option<String>,
}

fn error(message: impl Into<String>) -> DecompilerError {
    DecompilerError::Internal {
        message: message.into(),
    }
}

fn quote(value: &str) -> String {
    serde_json::to_string(value)
        .expect("string serialization is infallible")
        .replace('\u{2028}', "\\u2028")
        .replace('\u{2029}', "\\u2029")
}

fn number(value: f64) -> String {
    if value.is_nan() {
        "(0/0)".into()
    } else if value == f64::INFINITY {
        "(1/0)".into()
    } else if value == f64::NEG_INFINITY {
        "(-1/0)".into()
    } else if value == 0.0 && value.is_sign_negative() {
        "(-0)".into()
    } else {
        value.to_string()
    }
}

fn encoded_size(instruction: &UnifiedInstruction, version: u32) -> u32 {
    if version >= 95 && matches!(instruction, UnifiedInstruction::DirectEval { .. }) {
        4
    } else {
        instruction.size() as u32
    }
}

pub(crate) fn operands(
    instruction: &UnifiedInstruction,
) -> DecompilerResult<smallvec::SmallVec<[i64; 6]>> {
    // Avoid a JSON tree and heap allocation for the dominant opcode families.
    macro_rules! one {
        ($($name:ident),*) => { match instruction {
            $(UnifiedInstruction::$name { operand_0 } => Some(smallvec::smallvec![*operand_0 as i64]),)*
            _ => None,
        }};
    }
    macro_rules! two {
        ($($name:ident),*) => { match instruction {
            $(UnifiedInstruction::$name { operand_0, operand_1 } => Some(smallvec::smallvec![*operand_0 as i64, *operand_1 as i64]),)*
            _ => None,
        }};
    }
    macro_rules! three {
        ($($name:ident),*) => { match instruction {
            $(UnifiedInstruction::$name { operand_0, operand_1, operand_2 } => Some(smallvec::smallvec![*operand_0 as i64, *operand_1 as i64, *operand_2 as i64]),)*
            _ => None,
        }};
    }
    macro_rules! four {
        ($($name:ident),*) => { match instruction {
            $(UnifiedInstruction::$name { operand_0, operand_1, operand_2, operand_3 } => Some(smallvec::smallvec![*operand_0 as i64, *operand_1 as i64, *operand_2 as i64, *operand_3 as i64]),)*
            _ => None,
        }};
    }
    macro_rules! five {
        ($($name:ident),*) => { match instruction {
            $(UnifiedInstruction::$name { operand_0, operand_1, operand_2, operand_3, operand_4 } => Some(smallvec::smallvec![*operand_0 as i64, *operand_1 as i64, *operand_2 as i64, *operand_3 as i64, *operand_4 as i64]),)*
            _ => None,
        }};
    }
    if matches!(
        instruction,
        UnifiedInstruction::StartGenerator {}
            | UnifiedInstruction::CompleteGenerator {}
            | UnifiedInstruction::Debugger {}
            | UnifiedInstruction::AsyncBreakCheck {}
            | UnifiedInstruction::Unreachable {}
    ) {
        return Ok(smallvec::SmallVec::new());
    }
    if let Some(values) = one!(
        LoadConstEmpty,
        LoadConstUndefined,
        LoadConstNull,
        LoadConstTrue,
        LoadConstFalse,
        LoadConstZero,
        GetGlobalObject,
        LoadThisNS,
        GetNewTarget,
        CreateEnvironment,
        NewObject,
        ReifyArguments,
        Catch,
        Throw,
        Ret,
        DeclareGlobalVar,
        ThrowIfHasRestrictedGlobalProperty,
        SaveGenerator,
        SaveGeneratorLong,
        Jmp,
        JmpLong,
        ProfilePoint
    ) {
        return Ok(values);
    }
    if let Some(values) = two!(
        Mov,
        MovLong,
        LoadParam,
        LoadParamLong,
        LoadConstUInt8,
        LoadConstInt,
        LoadConstString,
        LoadConstStringLongIndex,
        LoadConstBigInt,
        LoadConstBigIntLongIndex,
        GetEnvironment,
        NewArray,
        Negate,
        Not,
        TypeOf,
        BitNot,
        Inc,
        Dec,
        ToNumber,
        ToNumeric,
        ToInt32,
        AddEmptyString,
        CoerceThisNS,
        GetArgumentsLength,
        IteratorBegin,
        IteratorClose,
        ResumeGenerator,
        DirectEval,
        ThrowIfEmpty,
        NewObjectWithParent,
        GetBuiltinClosure,
        JmpTrue,
        JmpTrueLong,
        JmpFalse,
        JmpFalseLong,
        JmpUndefined,
        JmpUndefinedLong
    ) {
        return Ok(values);
    }
    if let Some(values) = three!(
        Add,
        AddN,
        Sub,
        SubN,
        Mul,
        MulN,
        Div,
        DivN,
        Mod,
        Eq,
        Neq,
        StrictEq,
        StrictNeq,
        Less,
        LessEq,
        Greater,
        GreaterEq,
        LShift,
        RShift,
        URshift,
        BitAnd,
        BitOr,
        BitXor,
        IsIn,
        InstanceOf,
        LoadFromEnvironment,
        LoadFromEnvironmentL,
        StoreToEnvironment,
        StoreToEnvironmentL,
        StoreNPToEnvironment,
        StoreNPToEnvironmentL,
        CreateClosure,
        CreateClosureLongIndex,
        CreateGeneratorClosure,
        CreateGeneratorClosureLongIndex,
        CreateGenerator,
        CreateGeneratorLongIndex,
        CreateAsyncClosure,
        CreateAsyncClosureLongIndex,
        PutOwnByIndex,
        PutOwnByIndexL,
        PutNewOwnById,
        PutNewOwnByIdShort,
        PutNewOwnByIdLong,
        PutNewOwnNEById,
        PutNewOwnNEByIdLong,
        GetByVal,
        PutByVal,
        DelByVal,
        CreateThis,
        SelectObject,
        Call,
        CallLong,
        Construct,
        ConstructLong,
        Call1,
        CallDirect,
        CallDirectLongIndex,
        GetArgumentsPropByVal,
        CallBuiltin,
        CallBuiltinLong,
        IteratorNext,
        CreateInnerEnvironment,
        DelById,
        DelByIdLong,
        JNotLessEqualN,
        JNotGreaterEqualLong,
        JGreaterN,
        JLess,
        JGreaterEqual,
        JNotGreaterN,
        JStrictNotEqual,
        JNotLessN,
        JStrictEqualLong,
        JNotEqualLong,
        JStrictNotEqualLong,
        JNotLessEqual,
        JNotGreaterLong,
        JNotGreaterEqual,
        JLessNLong,
        JNotEqual,
        JLessEqualN,
        JGreaterEqualN,
        JGreaterNLong,
        JEqualLong,
        JGreaterLong,
        JNotLessLong,
        JNotLessEqualLong,
        JNotGreaterEqualN,
        JNotGreaterEqualNLong,
        JEqual,
        JNotLessEqualNLong,
        JStrictEqual,
        JLessEqual,
        JGreaterEqualNLong,
        JLessLong,
        JLessEqualNLong,
        JLessN,
        JNotLessNLong,
        JLessEqualLong,
        JGreater,
        JNotGreater,
        JGreaterEqualLong,
        JNotLess,
        JNotGreaterNLong
    ) {
        return Ok(values);
    }
    if let Some(values) = four!(
        GetById,
        GetByIdShort,
        GetByIdLong,
        TryGetById,
        TryGetByIdLong,
        PutById,
        PutByIdLong,
        TryPutById,
        TryPutByIdLong,
        Call2,
        NewArrayWithBuffer,
        NewArrayWithBufferLong,
        CreateRegExp,
        PutOwnByVal,
        GetPNameList
    ) {
        return Ok(values);
    }
    if let Some(values) = five!(
        NewObjectWithBuffer,
        NewObjectWithBufferLong,
        PutOwnGetterSetterByVal,
        GetNextPName,
        Call3,
        SwitchImm
    ) {
        return Ok(values);
    }
    if let UnifiedInstruction::Call4 {
        operand_0,
        operand_1,
        operand_2,
        operand_3,
        operand_4,
        operand_5,
    } = instruction
    {
        return Ok(smallvec::smallvec![
            *operand_0 as i64,
            *operand_1 as i64,
            *operand_2 as i64,
            *operand_3 as i64,
            *operand_4 as i64,
            *operand_5 as i64
        ]);
    }
    let json = serde_json::to_value(instruction).map_err(|e| error(e.to_string()))?;
    let values = &json[instruction.name()];
    (0..6)
        .take_while(|i| values.get(format!("operand_{i}")).is_some())
        .map(|i| {
            values[format!("operand_{i}")]
                .as_i64()
                .ok_or_else(|| error("Invalid integer instruction operand"))
        })
        .collect()
}

struct Lowerer<'a, 'data, const BOUNDED: bool = false> {
    hbc: &'a HbcFile<'data>,
    strings: &'a [String],
    index: u32,
    frame_size: u32,
    env_size: u32,
    generator: bool,
    boundaries: Vec<u32>,
    source_limit: usize,
}

const MAX_FRAGMENT_BYTES: usize = 64 * 1024 * 1024;

fn source_budget_error() -> DecompilerError {
    error("Bounded fragment exceeds max_source_bytes")
}

// The unbounded instantiation keeps ordinary String reservation and writes.
struct FragmentSource<const BOUNDED: bool> {
    text: String,
    limit: usize,
    exceeded: bool,
}

impl<const BOUNDED: bool> FragmentSource<BOUNDED> {
    #[inline]
    fn reserve(&mut self, additional: usize) {
        if BOUNDED {
            self.text
                .reserve_exact(additional.min(self.limit.saturating_sub(self.text.len())));
        } else {
            self.text.reserve(additional);
        }
    }

    #[inline]
    fn push_str(&mut self, value: &str) {
        self.write_str(value).unwrap();
    }

    fn check(&self) -> DecompilerResult<()> {
        if BOUNDED && self.exceeded {
            Err(source_budget_error())
        } else {
            Ok(())
        }
    }
}

impl<const BOUNDED: bool> Write for FragmentSource<BOUNDED> {
    #[inline]
    fn write_str(&mut self, value: &str) -> std::fmt::Result {
        if BOUNDED {
            if self.exceeded || value.len() > self.limit.saturating_sub(self.text.len()) {
                self.exceeded = true;
                return Ok(());
            }
            let needed = self.text.len() + value.len();
            if needed > self.text.capacity() {
                let capacity = needed
                    .max(self.text.capacity().saturating_mul(2))
                    .min(self.limit);
                self.text.reserve_exact(capacity - self.text.len());
            }
        }
        self.text.push_str(value);
        Ok(())
    }
}

struct Register(i64);

fn javascript_string(hbc: &HbcFile<'_>, index: u32) -> DecompilerResult<String> {
    let entry = hbc.strings.get_entry(index).map_err(error)?;
    if !entry.is_utf16 {
        return hbc.strings.get(index).map(|s| quote(&s)).map_err(error);
    }
    // Rust strings cannot represent lone UTF-16 surrogates. Escape original
    // code units once per table entry, then borrow the literal during lowering.
    let mut literal = String::from("\"");
    for bytes in entry.bytes.chunks_exact(2) {
        let unit = u16::from_le_bytes([bytes[0], bytes[1]]);
        if (32..=126).contains(&unit) && unit != u16::from(b'"') && unit != u16::from(b'\\') {
            literal.push(char::from(unit as u8));
        } else {
            write!(literal, "\\u{unit:04x}").unwrap();
        }
    }
    literal.push('"');
    Ok(literal)
}

impl std::fmt::Display for Register {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "r[{}]", self.0)
    }
}

impl<const BOUNDED: bool> Lowerer<'_, '_, BOUNDED> {
    fn preflight(&self, bytes: usize) -> DecompilerResult<()> {
        if BOUNDED && bytes > self.source_limit {
            Err(source_budget_error())
        } else {
            Ok(())
        }
    }

    fn string(&self, index: i64) -> DecompilerResult<&str> {
        self.strings
            .get(index as usize)
            .map(String::as_str)
            .ok_or_else(|| error(format!("Invalid string reference {index}")))
    }

    fn target(&self, pc: u32, delta: i64) -> DecompilerResult<u32> {
        let target = i64::from(pc) + delta;
        if target < 0 || self.boundaries.binary_search(&(target as u32)).is_err() {
            return Err(error(format!("Invalid instruction target {target}")));
        }
        Ok(target as u32)
    }

    fn function_id(&self, id: i64) -> DecompilerResult<i64> {
        if id < 0 || id >= i64::from(self.hbc.functions.count()) {
            return Err(error(format!("Invalid function reference {id}")));
        }
        Ok(id)
    }

    fn arguments(&self, count: i64) -> DecompilerResult<Vec<String>> {
        // The last six frame slots are VM call metadata, not JS arguments.
        if count < 1 || count + 6 > i64::from(self.frame_size) {
            return Err(error(format!("Invalid call argument count {count}")));
        }
        if BOUNDED {
            self.preflight((count as usize).saturating_mul(24).saturating_add(256))?;
        }
        Ok((0..count)
            .map(|i| format!("r[{}]", i64::from(self.frame_size) - 7 - i))
            .collect())
    }

    fn literal(&self, data: &[u8], offset: i64, count: i64) -> DecompilerResult<Vec<String>> {
        if BOUNDED {
            if count < 0 {
                return Err(error("Invalid literal count"));
            }
            self.preflight((count as usize).saturating_mul(5).saturating_add(256))?;
        }
        let bytes = data
            .get(offset as usize..)
            .ok_or_else(|| error("Literal offset out of range"))?;
        let values = unpack_slp_array(bytes, Some(count as usize)).map_err(error)?;
        if values.items.len() != count as usize {
            return Err(error("Truncated literal buffer"));
        }
        if BOUNDED {
            let mut size = 256usize;
            for value in &values.items {
                let length = match value {
                    SLPValue::LongString(n) => self.string(i64::from(*n))?.len(),
                    SLPValue::ShortString(n) => self.string(i64::from(*n))?.len(),
                    SLPValue::ByteString(n) => self.string(i64::from(*n))?.len(),
                    SLPValue::Number(n) => number(*n).len(),
                    _ => 32,
                };
                size = size.saturating_add(length).saturating_add(1);
                self.preflight(size)?;
            }
        }
        values
            .items
            .iter()
            .map(|v| {
                Ok(match v {
                    SLPValue::Null => "null".into(),
                    SLPValue::True => "true".into(),
                    SLPValue::False => "false".into(),
                    SLPValue::Number(n) => number(*n),
                    SLPValue::Integer(n) => n.to_string(),
                    SLPValue::LongString(n) => self.string(i64::from(*n))?.to_owned(),
                    SLPValue::ShortString(n) => self.string(i64::from(*n))?.to_owned(),
                    SLPValue::ByteString(n) => self.string(i64::from(*n))?.to_owned(),
                })
            })
            .collect()
    }

    fn instruction(&self, ins: &HbcFunctionInstruction) -> DecompilerResult<String> {
        let instruction = &ins.instruction;
        if let UnifiedInstruction::LoadConstDouble {
            operand_0,
            operand_1,
        } = instruction
        {
            return Ok(format!("r[{operand_0}] = {};", number(*operand_1)));
        }
        let o = operands(instruction)?;
        let n = instruction.name();
        if BOUNDED {
            // At most two string operands occur in any one instruction.
            let ids: &[i64] = match n {
                "LoadConstString" | "LoadConstStringLongIndex" => &o[1..2],
                "GetById" | "GetByIdShort" | "GetByIdLong" | "TryGetById" | "TryGetByIdLong"
                | "PutById" | "PutByIdLong" | "TryPutById" | "TryPutByIdLong" => &o[3..4],
                "PutNewOwnById"
                | "PutNewOwnByIdShort"
                | "PutNewOwnByIdLong"
                | "PutNewOwnNEById"
                | "PutNewOwnNEByIdLong"
                | "DelById"
                | "DelByIdLong" => &o[2..3],
                "DeclareGlobalVar" | "ThrowIfHasRestrictedGlobalProperty" => &o[..1],
                "CreateRegExp" => &o[1..3],
                _ => &[],
            };
            if !ids.is_empty() {
                let mut size = 256usize;
                for &id in ids {
                    size = size.saturating_add(self.string(id)?.len());
                }
                self.preflight(size)?;
            }
        }
        let r = |i: usize| Register(o[i]);
        let assign = |v: String| format!("{} = {v};", r(0));
        let binary = |op: &str| assign(format!("{} {op} {}", r(1), r(2)));
        let pc = ins.offset.value();
        let next = pc + encoded_size(instruction, self.hbc.header.version());
        let result = match n {
            "Mov" | "MovLong" => assign(r(1).to_string()),
            "LoadConstUInt8" | "LoadConstInt" => assign(o[1].to_string()),
            "LoadConstString" | "LoadConstStringLongIndex" => assign(self.string(o[1])?.to_owned()),
            "LoadConstBigInt" | "LoadConstBigIntLongIndex" => {
                let start = o[1] as usize * 8;
                let entry = self.hbc.bigints.table.get(start..start + 8).ok_or_else(|| error("Invalid bigint index"))?;
                let offset = u32::from_le_bytes(entry[..4].try_into().unwrap()) as usize;
                let length = u32::from_le_bytes(entry[4..].try_into().unwrap()) as usize;
                let bytes = self.hbc.bigints.storage.get(offset..offset + length).ok_or_else(|| error("Truncated bigint"))?;
                if BOUNDED { self.preflight(length.saturating_mul(3).saturating_add(256))?; }
                assign(format!("{}n", num_bigint::BigInt::from_signed_bytes_le(bytes)))
            }
            "LoadConstEmpty" => assign("EMPTY".into()),
            "LoadConstUndefined" => assign("void 0".into()),
            "LoadConstNull" => assign("null".into()),
            "LoadConstTrue" => assign("true".into()),
            "LoadConstFalse" => assign("false".into()),
            "LoadConstZero" => assign("0".into()),
            "Add" | "AddN" => binary("+"), "Sub" | "SubN" => binary("-"),
            "Mul" | "MulN" => binary("*"), "Div" | "DivN" => binary("/"), "Mod" => binary("%"),
            "Eq" => binary("=="), "Neq" => binary("!="),
            "StrictEq" => binary("==="), "StrictNeq" => binary("!=="),
            "Less" => binary("<"), "LessEq" => binary("<="),
            "Greater" => binary(">"), "GreaterEq" => binary(">="),
            "LShift" => binary("<<"), "RShift" => binary(">>"), "URshift" => binary(">>>"),
            "BitAnd" => binary("&"), "BitOr" => binary("|"), "BitXor" => binary("^"),
            "IsIn" => binary("in"), "InstanceOf" => binary("instanceof"),
            "Negate" => assign(format!("-{}", r(1))), "Not" => assign(format!("!{}", r(1))),
            "BitNot" => assign(format!("~{}", r(1))), "TypeOf" => assign(format!("typeof {}", r(1))),
            "Inc" => assign(format!("increment({}, 1)", r(1))),
            "Dec" => assign(format!("increment({}, -1)", r(1))),
            "ToNumber" => assign(format!("+{}", r(1))),
            "ToNumeric" => assign(format!("numeric({})", r(1))),
            "ToInt32" => assign(format!("{} | 0", r(1))),
            "AddEmptyString" => assign(format!("\"\" + {}", r(1))),
            "GetGlobalObject" => assign("G".into()),
            "LoadParam" | "LoadParamLong" => assign(if o[1] == 0 { "self".into() } else { format!("(args.length > {} ? args[{}] : void 0)", o[1] - 1, o[1] - 1) }),
            "LoadThisNS" => assign("coerceThis(self)".into()),
            "CoerceThisNS" => assign(format!("coerceThis({})", r(1))),
            "GetNewTarget" => assign("newTarget".into()),
            "DirectEval" => {
                let strict = self.hbc.header.version() >= 95 && self.hbc.functions.get_parsed_header(self.index).unwrap().body[(pc + 3) as usize] != 0;
                assign(format!("directEval({}, {strict})", r(1)))
            }
            "CreateEnvironment" => assign(format!("environment(env, {})", self.env_size)),
            "CreateInnerEnvironment" => assign(format!("environment({}, {})", r(1), o[2])),
            "GetEnvironment" => assign(format!("parentEnvironment(env, {})", o[1])),
            "LoadFromEnvironment" | "LoadFromEnvironmentL" => assign(format!("{}.slots[{}]", r(1), o[2])),
            "StoreToEnvironment" | "StoreToEnvironmentL" | "StoreNPToEnvironment" | "StoreNPToEnvironmentL" => format!("{}.slots[{}] = {};", r(0), o[1], r(2)),
            "CreateClosure" | "CreateClosureLongIndex" => assign(format!("closure({}, {})", self.function_id(o[2])?, r(1))),
            "CreateGeneratorClosure" | "CreateGeneratorClosureLongIndex" => assign(format!("closure({}, {}, 1)", self.function_id(o[2])?, r(1))),
            "CreateAsyncClosure" | "CreateAsyncClosureLongIndex" => assign(format!("closure({}, {}, 2)", self.function_id(o[2])?, r(1))),
            "CreateGenerator" | "CreateGeneratorLongIndex" => assign(format!("generator({}, {}, self, args, callee)", self.function_id(o[2])?, r(1))),
            "StartGenerator" => String::new(),
            "ResumeGenerator" => format!("{} = state.value; {} = state.action === 'return'; if (state.action === 'throw') throw {};", r(0), r(1), r(0)),
            "SaveGenerator" | "SaveGeneratorLong" => format!("state.resume = {};", self.target(pc, o[0])?),
            "CompleteGenerator" => "state.done = true;".into(),
            "NewObject" => assign("{}".into()),
            "NewObjectWithParent" => assign(format!("createObject({})", r(1))),
            "NewArray" => assign(format!("new ArrayCtor({})", o[1])),
            "NewArrayWithBuffer" | "NewArrayWithBufferLong" => {
                let values = self.literal(self.hbc.serialized_literals.arrays_data, o[3], o[2])?;
                assign(format!("[{}]", values.join(",")))
            }
            "NewObjectWithBuffer" | "NewObjectWithBufferLong" => {
                let keys = self.literal(self.hbc.serialized_literals.object_keys_data, o[3], o[2])?;
                let values = self.literal(self.hbc.serialized_literals.object_values_data, o[4], o[2])?;
                if BOUNDED { self.preflight(keys.iter().chain(&values).fold(256usize, |size, value| size.saturating_add(value.len()).saturating_add(1)))?; }
                assign(format!("objectLiteral([{}], [{}])", keys.join(","), values.join(",")))
            }
            "GetById" | "GetByIdShort" | "GetByIdLong" => assign(format!("{}[{}]", r(1), self.string(o[3])?)),
            "TryGetById" | "TryGetByIdLong" => assign(format!("globalGet({}, {})", r(1), self.string(o[3])?)),
            "PutById" | "PutByIdLong" | "TryPutById" | "TryPutByIdLong" => format!("put({}, {}, {}, {}, {});", r(0), self.string(o[3])?, r(1), if n.starts_with("Try") { "true" } else { "false" }, "strict"),
            "PutNewOwnById" | "PutNewOwnByIdShort" | "PutNewOwnByIdLong" | "PutNewOwnNEById" | "PutNewOwnNEByIdLong" => format!("own({}, {}, {}, {});", r(0), self.string(o[2])?, r(1), !n.contains("NE")),
            "PutOwnByIndex" | "PutOwnByIndexL" => format!("own({}, {}, {}, true);", r(0), o[2], r(1)),
            "PutOwnByVal" => format!("own({}, {}, {}, {});", r(0), r(2), r(1), o[3] != 0),
            "PutOwnGetterSetterByVal" => format!("accessor({}, {}, {}, {}, {});", r(0), r(1), r(2), r(3), o[4] != 0),
            "GetByVal" => assign(format!("{}[{}]", r(1), r(2))),
            "PutByVal" => format!("put({}, {}, {}, false, strict);", r(0), r(1), r(2)),
            "DelById" | "DelByIdLong" => assign(format!("remove({}, {}, strict)", r(1), self.string(o[2])?)),
            "DelByVal" => assign(format!("remove({}, {}, strict)", r(1), r(2))),
            "DeclareGlobalVar" => format!("declareGlobal({});", self.string(o[0])?),
            "ThrowIfHasRestrictedGlobalProperty" => format!("checkGlobal({});", self.string(o[0])?),
            "GetPNameList" => format!("{} = propertyNames({}); {} = 0; {} = {} === void 0 ? 0 : {}.length;", r(0), r(1), r(2), r(3), r(0), r(0)),
            "GetNextPName" => format!("{} = void 0; while ({} < {}) {{ const key = {}[{}++]; if (key in ObjectCtor({})) {{ {} = key; break; }} }}", r(0), r(3), r(4), r(1), r(3), r(2), r(0)),
            "Call1" | "Call2" | "Call3" | "Call4" => assign(format!("apply({}, {}, [{}])", r(1), r(2), (3..o.len()).map(|i| r(i).to_string()).collect::<Vec<_>>().join(","))),
            "Call" | "CallLong" => {
                let args = self.arguments(o[2])?;
                assign(format!("apply({}, {}, [{}])", r(1), args[0], args[1..].join(",")))
            }
            "CallDirect" | "CallDirectLongIndex" => {
                let args = self.arguments(o[1])?;
                assign(format!("F[{}](null, {}, [{}], void 0)", self.function_id(o[2])?, args[0], args[1..].join(",")))
            }
            "CallBuiltin" | "CallBuiltinLong" => {
                check_builtin(self.hbc.header.version(), o[1])?;
                let values = self.arguments(o[2])?;
                assign(format!("builtin({}, [{}], args, {})", o[1], values[1..].join(","), if self.generator { "state" } else { "null" }))
            }
            "GetBuiltinClosure" => {
                check_builtin(self.hbc.header.version(), o[1])?;
                assign(format!("builtinClosure({})", o[1]))
            }
            "CreateThis" => assign(format!("createThis({}, {})", r(1), r(2))),
            "Construct" | "ConstructLong" => {
                let args = self.arguments(o[2])?;
                assign(format!("construct({}, {}, [{}])", r(1), args[0], args[1..].join(",")))
            }
            "SelectObject" => assign(format!("isObject({}) ? {} : {}", r(2), r(2), r(1))),
            "GetArgumentsLength" => assign(format!("({} === void 0 ? args : {}).length", r(1), r(1))),
            "GetArgumentsPropByVal" => format!("if ({} === void 0) {} = argumentsObject(args, callee, strict || {}); {}", r(2), r(2), self.generator, assign(format!("{}[{}]", r(2), r(1)))),
            "ReifyArguments" => format!("if ({} === void 0) {} = argumentsObject(args, callee, strict || {});", r(0), r(0), self.generator),
            "CreateRegExp" => assign(format!("new RegExpCtor({}, {})", self.string(o[1])?, self.string(o[2])?)),
            "Catch" => assign("caught".into()),
            "Throw" => format!("throw {};", r(0)),
            "ThrowIfEmpty" => format!("if ({} === EMPTY) throw new ReferenceErrorCtor('Uninitialized binding'); {}", r(1), assign(r(1).to_string())),
            "Ret" if self.generator => format!("state.pc = state.resume; state.caught = caught; if (state.delegated) {{ state.delegated = false; return {}; }} return {{value: {}, done: state.done}};", r(0), r(0)),
            "Ret" => format!("return {};", r(0)),
            "IteratorBegin" => format!("{{ const pair = iteratorBegin({}); {} = pair[0]; {} = pair[1]; }}", r(1), r(0), r(1)),
            "IteratorNext" => format!("{{ const pair = iteratorNext({}, {}); {} = pair[0]; {} = pair[1]; }}", r(1), r(2), r(0), r(1)),
            "IteratorClose" => format!("iteratorClose({}, {});", r(0), o[1] != 0),
            "Debugger" => "debugger;".into(),
            "AsyncBreakCheck" | "ProfilePoint" => String::new(),
            "Unreachable" => "throw new ErrorCtor('Unreachable bytecode executed');".into(),
            "SwitchImm" => {
                let table = self.hbc.switch_tables.get_switch_table_by_instruction(self.index, ins.instruction_index.value() as u32).ok_or_else(|| error("Missing switch table"))?;
                if BOUNDED { self.preflight(table.cases.len().saturating_mul(64).saturating_add(256))?; }
                let mut code = format!("switch ({}) {{", r(0));
                for case in &table.cases {
                    let target = self.target(pc, i64::from(case.target_offset))?;
                    write!(code, "case {}: pc = {target}; break;", case.value).unwrap();
                }
                write!(code, "default: pc = {}; }} continue;", self.target(pc, o[2])?).unwrap();
                code
            }
            _ if n.starts_with('J') => {
                let base = n.strip_suffix("Long").unwrap_or(n);
                let base = base.strip_suffix('N').unwrap_or(base);
                let target = self.target(pc, o[0])?;
                let condition = match base {
                    "Jmp" => return Ok(format!("pc = {target}; continue;")),
                    "JmpTrue" => r(1).to_string(), "JmpFalse" => format!("!{}", r(1)),
                    "JmpUndefined" => format!("{} === void 0", r(1)),
                    _ => {
                        let (op, negate) = match base {
                            "JLess" => ("<", false), "JNotLess" => ("<", true),
                            "JLessEqual" => ("<=", false), "JNotLessEqual" => ("<=", true),
                            "JGreater" => (">", false), "JNotGreater" => (">", true),
                            "JGreaterEqual" => (">=", false), "JNotGreaterEqual" => (">=", true),
                            "JEqual" => ("==", false), "JNotEqual" => ("!=", false),
                            "JStrictEqual" => ("===", false), "JStrictNotEqual" => ("!==", false),
                            _ => return Err(error(format!("Unsupported opcode {n}"))),
                        };
                        format!("{}({} {op} {})", if negate { "!" } else { "" }, r(1), r(2))
                    }
                };
                format!("pc = ({condition}) ? {target} : {next}; continue;")
            }
            _ => return Err(error(format!("Unsupported opcode {n}"))),
        };
        Ok(result)
    }
}

fn check_builtin(version: u32, id: i64) -> DecompilerResult<()> {
    // Builtin numbering is a bytecode-version contract, not an opcode invariant.
    if !matches!(version, 90 | 92..=96) || !(0..=if version >= 92 { 52 } else { 51 }).contains(&id)
    {
        return Err(error(format!("Unsupported builtin {id} for HBC {version}")));
    }
    Ok(())
}

/// Export all functions and execute the original global entrypoint. No SSA
/// transformations or speculative module splitting are used in this backend.
fn lower_function(
    hbc: &HbcFile<'_>,
    index: u32,
    allocator: &oxc_allocator::Allocator,
    strings: &[String],
) -> DecompilerResult<String> {
    lower_function_with_pcs(hbc, index, allocator, strings, false)
}

fn lower_function_with_pcs(
    hbc: &HbcFile<'_>,
    index: u32,
    allocator: &oxc_allocator::Allocator,
    strings: &[String],
    annotate_pc: bool,
) -> DecompilerResult<String> {
    lower_function_impl::<false>(hbc, index, allocator, strings, annotate_pc, usize::MAX)
}

fn lower_function_impl<const BOUNDED: bool>(
    hbc: &HbcFile<'_>,
    index: u32,
    allocator: &oxc_allocator::Allocator,
    strings: &[String],
    annotate_pc: bool,
    source_limit: usize,
) -> DecompilerResult<String> {
    let start = std::time::Instant::now();
    let mut output = FragmentSource::<BOUNDED> {
        text: String::new(),
        limit: source_limit,
        exceeded: false,
    };
    let mut failures = BTreeMap::<String, String>::new();
    {
        let header = hbc
            .functions
            .get_parsed_header(index)
            .ok_or_else(|| error("Missing function header"))?;
        let instructions = hbc.functions.get_instructions_ref(index)?;
        if instructions
            .windows(2)
            .any(|pair| pair[0].offset.value() >= pair[1].offset.value())
        {
            return Err(error(format!(
                "Unordered instruction offsets in function {index}"
            )));
        }
        output.reserve(instructions.len().saturating_mul(40).saturating_add(512));
        let frame_size = header
            .large_header
            .as_ref()
            .map_or(header.header.frame_size(), |h| h.frame_size);
        let env_size = header
            .large_header
            .as_ref()
            .map_or(u32::from(header.header.environment_size()), |h| {
                h.environment_size
            });
        let flags = header
            .large_header
            .as_ref()
            .map_or((header.header.flags() >> 24) as u8, |h| h.flags);
        let param_count = header
            .large_header
            .as_ref()
            .map_or(header.header.param_count(), |h| h.param_count)
            .saturating_sub(1);
        let generator = instructions
            .iter()
            .any(|i| i.instruction.name() == "StartGenerator");
        let lowerer = Lowerer::<BOUNDED> {
            hbc,
            strings,
            index,
            frame_size,
            env_size,
            generator,
            boundaries: instructions.iter().map(|i| i.offset.value()).collect(),
            source_limit,
        };
        let name = lowerer.string(i64::from(
            header
                .large_header
                .as_ref()
                .map_or(header.header.function_name(), |h| h.function_name),
        ))?;
        // Only branch/resume/handler destinations need switch labels. Straight-line
        // instructions share a case, but keep their exact PC for exception lookup.
        let mut dispatch = HashSet::from([0]);
        for ins in instructions {
            let n = ins.instruction.name();
            let pc = ins.offset.value();
            if n.starts_with('J') || n.starts_with("SaveGenerator") {
                let values = operands(&ins.instruction)?;
                dispatch.insert(lowerer.target(pc, values[0])?);
                if n.starts_with('J') {
                    dispatch.insert(pc + encoded_size(&ins.instruction, hbc.header.version()));
                }
            } else if n == "SwitchImm" {
                let values = operands(&ins.instruction)?;
                dispatch.insert(lowerer.target(pc, values[2])?);
                let table = hbc
                    .switch_tables
                    .get_switch_table_by_instruction(index, ins.instruction_index.value() as u32)
                    .ok_or_else(|| error("Missing switch table"))?;
                for case in &table.cases {
                    dispatch.insert(lowerer.target(pc, i64::from(case.target_offset))?);
                }
            }
        }
        for handler in &header.exc_handlers {
            dispatch.insert(handler.target);
        }
        let mut dispatch: Vec<u32> = dispatch.into_iter().collect();
        dispatch.sort_unstable();
        let mut dispatch = dispatch.into_iter().peekable();
        writeln!(
            output,
            "M[{index}] = [{}, {param_count}, {}];",
            name,
            flags & 3
        )
        .unwrap();
        writeln!(output, "F[{index}] = function function_{index}(env, self, args, newTarget, callee, state) {{\nconst strict = {};", flags & 4 != 0).unwrap();
        if generator {
            output.push_str("const r = state.r; let pc = state.pc, caught = state.caught;\n");
        } else {
            output.push_str("const r = objectCreate(null); let pc = 0, caught;\n");
        }
        output.push_str("for (;;) { try { switch (pc) {\n");
        if BOUNDED {
            output.check()?;
        }
        let mut case_open = false;
        for ins in instructions {
            let pc = ins.offset.value();
            match lowerer.instruction(ins) {
                Ok(code) => {
                    if dispatch.peek() == Some(&pc) {
                        dispatch.next();
                        if case_open {
                            output.push_str("}\n");
                        }
                        writeln!(output, "case {pc}: {{").unwrap();
                        case_open = true;
                    }
                    if annotate_pc {
                        writeln!(output, "// HBC function {index}, PC {pc}").unwrap();
                    }
                    if !header.exc_handlers.is_empty() {
                        writeln!(output, "pc = {pc};").unwrap();
                    }
                    writeln!(output, "{code}").unwrap();
                }
                Err(e) => {
                    if BOUNDED {
                        return Err(e);
                    }
                    failures
                        .entry(ins.instruction.name().into())
                        .or_insert_with(|| format!("function {index}, byte offset {pc}: {e}"));
                }
            }
            if BOUNDED {
                output.check()?;
            }
        }
        if case_open {
            output.push_str("}\n");
        }
        output.push_str(
            "default: throw new ErrorCtor('Invalid bytecode PC ' + pc);\n} } catch (error) {\n",
        );
        for handler in &header.exc_handlers {
            let (start, end, target) = (handler.start, handler.end, handler.target);
            if lowerer.boundaries.binary_search(&start).is_err()
                || lowerer.boundaries.binary_search(&target).is_err()
                || !(lowerer.boundaries.binary_search(&end).is_ok()
                    || end == header.body.len() as u32)
            {
                return Err(error(format!("Invalid exception range in function {index}: {start}..{end} -> {target}, body length {}", header.body.len())));
            }
            writeln!(
                output,
                "if (pc >= {start} && pc < {end}) {{ caught = error; pc = {target}; continue; }}"
            )
            .unwrap();
        }
        output.push_str("throw error;\n} } };\n");
    }
    if BOUNDED {
        output.check()?;
    }
    if !failures.is_empty() {
        return Err(error(format!(
            "Bundle export refused: {} unsupported or invalid opcode kinds:\n{}",
            failures.len(),
            failures.values().cloned().collect::<Vec<_>>().join("\n")
        )));
    }
    let output = output.text;
    validate_javascript_with_allocator(&format!("'use strict';\n{output}"), allocator)?;
    if output.len() > 100_000 {
        log::debug!(
            "bundle large function {index}: bytes={}, elapsed={:?}",
            output.len(),
            start.elapsed()
        );
    }
    Ok(output)
}

/// Export selected complete function bodies for inspection, not standalone execution.
/// References to the bundle runtime and other functions remain explicit.
pub fn export_functions(hbc: &HbcFile<'_>, indices: &[u32]) -> DecompilerResult<String> {
    let mut output = String::from("// Decompiled JS fragments, not a standalone bundle.\n// F = function bodies; M = function metadata; r = physical registers.\n// env = captured lexical environment; self = this; args = arguments.\n// Runtime helpers and referenced F entries are defined by export-bundle.\n");
    for (_, code) in export_function_fragments(hbc, indices)? {
        output.push_str(&code);
    }
    Ok(output)
}

/// Batch lowering with shared string conversion and bounded worker allocators.
pub fn export_function_fragments(
    hbc: &HbcFile<'_>,
    indices: &[u32],
) -> DecompilerResult<Vec<(u32, String)>> {
    for &index in indices {
        if index >= hbc.functions.count() {
            return Err(error(format!("Unknown function {index}")));
        }
    }
    let strings: Vec<String> = (0..hbc.strings.string_count)
        .map(|index| javascript_string(hbc, index))
        .collect::<DecompilerResult<_>>()?;
    indices
        .par_iter()
        .map_init(oxc_allocator::Allocator::default, |allocator, &index| {
            let result = lower_function_with_pcs(hbc, index, allocator, &strings, true)
                .map(|code| (index, code));
            allocator.reset();
            result
        })
        .collect()
}

fn escaped_char_size(c: char) -> usize {
    match c {
        '"' | '\\' | '\n' | '\r' | '\t' | '\u{8}' | '\u{c}' => 2,
        '\u{0}'..='\u{1f}' | '\u{2028}' | '\u{2029}' => 6,
        _ => c.len_utf8(),
    }
}

// Inspect borrowed bytes, not converted strings or cached values. Invalid UTF-8
// contributes one replacement character per lossy-decoding error sequence.
fn escaped_entry_size(bytes: &[u8], utf16: bool) -> usize {
    if utf16 {
        return bytes.chunks_exact(2).fold(2usize, |size, pair| {
            let unit = u16::from_le_bytes([pair[0], pair[1]]);
            size.saturating_add(if (32..=126).contains(&unit) && unit != 34 && unit != 92 {
                1
            } else {
                6
            })
        });
    }
    let mut remaining = bytes;
    let mut size = 2usize;
    loop {
        let (valid, invalid) = match std::str::from_utf8(remaining) {
            Ok(valid) => (valid, None),
            Err(e) => (
                std::str::from_utf8(&remaining[..e.valid_up_to()]).unwrap(),
                Some((e.valid_up_to(), e.error_len())),
            ),
        };
        for c in valid.chars() {
            size = size.saturating_add(escaped_char_size(c));
        }
        match invalid {
            None => break,
            Some((offset, length)) => {
                size = size.saturating_add(3);
                match length {
                    Some(length) => remaining = &remaining[offset + length..],
                    None => break,
                }
            }
        }
    }
    size
}

fn bounded_javascript_string(bytes: &[u8], utf16: bool, capacity: usize) -> String {
    let mut literal = String::with_capacity(capacity);
    literal.push('"');
    if utf16 {
        for pair in bytes.chunks_exact(2) {
            let unit = u16::from_le_bytes([pair[0], pair[1]]);
            if (32..=126).contains(&unit) && unit != 34 && unit != 92 {
                literal.push(char::from(unit as u8));
            } else {
                write!(literal, "\\u{unit:04x}").unwrap();
            }
        }
    } else {
        for c in String::from_utf8_lossy(bytes).chars() {
            match c {
                '"' => literal.push_str("\\\""),
                '\\' => literal.push_str("\\\\"),
                '\n' => literal.push_str("\\n"),
                '\r' => literal.push_str("\\r"),
                '\t' => literal.push_str("\\t"),
                '\u{8}' => literal.push_str("\\b"),
                '\u{c}' => literal.push_str("\\f"),
                '\u{0}'..='\u{1f}' | '\u{2028}' | '\u{2029}' => {
                    write!(literal, "\\u{:04x}", c as u32).unwrap();
                }
                _ => literal.push(c),
            }
        }
    }
    literal.push('"');
    literal
}

/// Export one PC-annotated function fragment, identical to the corresponding
/// `export_function_fragments` result on success (not standalone JavaScript).
///
/// Both limits must be in `1..=64 * 1024 * 1024`. `max_source_bytes` caps returned
/// UTF-8 source bytes; `max_string_bytes` caps cumulative escaped literal bytes
/// across ALL table entries, including duplicates and unused entries. The table
/// is preflighted before conversion and source growth is capped before syntax
/// validation. Variable-sized instruction temporaries use conservative source
/// preflights, so some fragments smaller than the source limit may be refused.
///
/// These are expansion limits, not a total-memory or execution-time sandbox:
/// caller-owned parsing/caches, collection overhead, allocator rounding, bounded
/// temporary copies, and the validator AST are not included in these budgets.
pub fn export_function_fragment_bounded(
    hbc: &HbcFile<'_>,
    index: u32,
    max_source_bytes: usize,
    max_string_bytes: usize,
) -> DecompilerResult<String> {
    for (name, limit) in [
        ("max_source_bytes", max_source_bytes),
        ("max_string_bytes", max_string_bytes),
    ] {
        if limit == 0 || limit > MAX_FRAGMENT_BYTES {
            return Err(error(format!(
                "{name} must be in 1..=67108864 bytes (64 MiB)"
            )));
        }
    }
    if index >= hbc.functions.count() {
        return Err(error(format!("Unknown function {index}")));
    }
    let mut total = 0usize;
    for id in 0..hbc.strings.string_count {
        let entry = hbc.strings.get_entry(id).map_err(error)?;
        let size = escaped_entry_size(entry.bytes, entry.is_utf16);
        if size > max_string_bytes.saturating_sub(total) {
            return Err(error(
                "Bounded fragment exceeds max_string_bytes (all table entries)",
            ));
        }
        total += size;
    }
    let strings = (0..hbc.strings.string_count)
        .map(|id| {
            let entry = hbc.strings.get_entry(id).map_err(error)?;
            Ok(bounded_javascript_string(
                entry.bytes,
                entry.is_utf16,
                escaped_entry_size(entry.bytes, entry.is_utf16),
            ))
        })
        .collect::<DecompilerResult<Vec<_>>>()?;
    lower_function_impl::<true>(
        hbc,
        index,
        &oxc_allocator::Allocator::default(),
        &strings,
        true,
        max_source_bytes,
    )
}

fn validate_javascript(code: &str) -> DecompilerResult<()> {
    let allocator = oxc_allocator::Allocator::default();
    validate_javascript_with_allocator(code, &allocator)
}

fn validate_javascript_with_allocator(
    code: &str,
    allocator: &oxc_allocator::Allocator,
) -> DecompilerResult<()> {
    let parsed = oxc_parser::Parser::new(allocator, code, oxc_span::SourceType::default()).parse();
    if !parsed.errors.is_empty() {
        return Err(error(format!(
            "Generated bundle failed syntax validation: {:?}",
            parsed.errors
        )));
    }
    Ok(())
}

/// Lower and validate independent functions in parallel, retaining original
/// function-table order. The wrapper is validated separately to bound AST memory.
pub fn export_bundle(hbc: &HbcFile<'_>, options: &BundleOptions) -> DecompilerResult<String> {
    let start = std::time::Instant::now();
    if let Some(entry) = &options.entry_module {
        let exists = hbc.cjs_modules.entries.iter().any(|module| {
            if hbc.cjs_modules.is_static {
                module.symbol_id.to_string() == *entry
            } else {
                hbc.strings
                    .get(module.symbol_id)
                    .is_ok_and(|id| id == *entry)
            }
        });
        if !exists {
            return Err(error(format!("Unknown CommonJS entry module {entry:?}")));
        }
    }
    let strings: Vec<String> = (0..hbc.strings.string_count)
        .into_par_iter()
        .map(|index| javascript_string(hbc, index))
        .collect::<DecompilerResult<_>>()?;
    // Start asset-heavy functions first rather than leaving a long serial tail
    // behind many small functions. Restore table order after parallel lowering.
    let mut indices: Vec<u32> = (0..hbc.functions.count()).collect();
    indices.sort_unstable_by_key(|index| {
        std::cmp::Reverse(hbc.functions.parsed_headers[*index as usize].body.len())
    });
    let job_count = rayon::current_num_threads().saturating_mul(2).max(1);
    let mut jobs = vec![Vec::new(); job_count];
    let mut costs = vec![0usize; job_count];
    for index in indices {
        let job = costs
            .iter()
            .enumerate()
            .min_by_key(|(_, cost)| *cost)
            .unwrap()
            .0;
        costs[job] =
            costs[job].saturating_add(hbc.functions.parsed_headers[index as usize].body.len());
        jobs[job].push(index);
    }
    let mut functions: Vec<(u32, String)> = jobs
        .into_par_iter()
        .with_max_len(1)
        .map(|job| {
            let mut allocator = oxc_allocator::Allocator::default();
            job.into_iter()
                .map(|index| {
                    let result =
                        lower_function(hbc, index, &allocator, &strings).map(|code| (index, code));
                    allocator.reset();
                    result
                })
                .collect::<DecompilerResult<Vec<_>>>()
        })
        .collect::<DecompilerResult<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect();
    functions.sort_unstable_by_key(|(index, _)| *index);
    log::info!(
        "bundle parallel lowering and function validation: {:?}",
        start.elapsed()
    );
    let mut output = String::from("// Hermes full-bundle export: physical registers, original handler order.\n(function(G) {\n'use strict';\n");
    output.push_str(include_str!("runtime.js"));
    let wrapper_prefix = output.clone();
    output.reserve(
        functions
            .iter()
            .map(|(_, function)| function.len())
            .sum::<usize>(),
    );
    for (_, function) in functions {
        output.push_str(&function);
    }
    let suffix_start = output.len();
    let entry = hbc.header.global_code_index();
    if entry >= hbc.functions.count() {
        return Err(error("Invalid global entrypoint"));
    }
    writeln!(output, "const bytecodeVersion = {};", hbc.header.version()).unwrap();
    let mut first_module = None;
    for module in &hbc.cjs_modules.entries {
        let id = if hbc.cjs_modules.is_static {
            module.symbol_id.to_string()
        } else {
            strings[module.symbol_id as usize].clone()
        };
        if first_module.is_none() {
            first_module = Some(id.clone());
        }
        writeln!(output, "cjsFunctions[{id}] = {};", module.offset).unwrap();
    }
    writeln!(
        output,
        "const globalResult = apply(closure({entry}, null), G, []);"
    )
    .unwrap();
    let entry_module = options
        .entry_module
        .as_ref()
        .map(|id| quote(id))
        .or(first_module);
    if let Some(id) = entry_module {
        writeln!(output, "return requireModule({id});").unwrap();
    } else {
        output.push_str("return globalResult;\n");
    }
    output.push_str("})(globalThis);\n");
    validate_javascript(&format!("{wrapper_prefix}{}", &output[suffix_start..]))?;
    if !options.minify {
        log::info!(
            "bundle generation total: {:?}, bytes={}",
            start.elapsed(),
            output.len()
        );
        return Ok(output);
    }
    log::info!(
        "bundle lowering: {:?}, bytes={}",
        start.elapsed(),
        output.len()
    );
    let validation_start = std::time::Instant::now();
    let allocator = oxc_allocator::Allocator::default();
    let parsed =
        oxc_parser::Parser::new(&allocator, &output, oxc_span::SourceType::default()).parse();
    if !parsed.errors.is_empty() {
        return Err(error(format!(
            "Generated bundle failed syntax validation: {:?}",
            parsed.errors
        )));
    }
    log::info!("bundle syntax validation: {:?}", validation_start.elapsed());
    if options.minify {
        Ok(oxc_codegen::Codegen::new()
            .with_options(oxc_codegen::CodegenOptions {
                minify: true,
                ..Default::default()
            })
            .build(&parsed.program)
            .code)
    } else {
        Ok(output)
    }
}
