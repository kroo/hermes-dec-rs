use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DecompiledRegExp {
    pub pattern: String,
    pub flags: String,
}

impl DecompiledRegExp {
    pub fn new(pattern: String, flags: String) -> Self {
        Self { pattern, flags }
    }

    pub fn to_string(&self) -> String {
        format!("/{}/{}", self.pattern, self.flags)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum RegexOpcode {
    Goal = 0x00,
    LeftAnchor = 0x01,
    RightAnchor = 0x02,
    MatchAny = 0x03,
    U16MatchAny = 0x04,
    MatchAnyButNewline = 0x05,
    U16MatchAnyButNewline = 0x06,
    MatchChar8 = 0x07,
    MatchChar16 = 0x08,
    U16MatchChar32 = 0x09,
    MatchNChar8 = 0x0a,
    MatchNCharICase8 = 0x0b,
    MatchCharICase8 = 0x0c,
    MatchCharICase16 = 0x0d,
    U16MatchCharICase32 = 0x0e,
    Alternation = 0x0f,
    Jump32 = 0x10,
    Bracket = 0x11,
    U16Bracket = 0x12,
    BeginMarkedSubexpression = 0x13,
    EndMarkedSubexpression = 0x14,
    BackRef = 0x15,
    WordBoundary = 0x16,
    Lookaround = 0x17,
    BeginLoop = 0x18,
    EndLoop = 0x19,
    BeginSimpleLoop = 0x1a,
    EndSimpleLoop = 0x1b,
    Width1Loop = 0x1c,
    // ... add more as needed
}

impl RegexOpcode {
    pub fn from_u8(byte: u8) -> Option<Self> {
        use RegexOpcode::*;
        Some(match byte {
            0x00 => Goal,
            0x01 => LeftAnchor,
            0x02 => RightAnchor,
            0x03 => MatchAny,
            0x04 => U16MatchAny,
            0x05 => MatchAnyButNewline,
            0x06 => U16MatchAnyButNewline,
            0x07 => MatchChar8,
            0x08 => MatchChar16,
            0x09 => U16MatchChar32,
            0x0a => MatchNChar8,
            0x0b => MatchNCharICase8,
            0x0c => MatchCharICase8,
            0x0d => MatchCharICase16,
            0x0e => U16MatchCharICase32,
            0x0f => Alternation,
            0x10 => Jump32,
            0x11 => Bracket,
            0x12 => U16Bracket,
            0x13 => BeginMarkedSubexpression,
            0x14 => EndMarkedSubexpression,
            0x15 => BackRef,
            0x16 => WordBoundary,
            0x17 => Lookaround,
            0x18 => BeginLoop,
            0x19 => EndLoop,
            0x1a => BeginSimpleLoop,
            0x1b => EndSimpleLoop,
            0x1c => Width1Loop,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RegexInstruction {
    Goal,
    LeftAnchor,
    RightAnchor,
    MatchAny,
    U16MatchAny,
    MatchAnyButNewline,
    U16MatchAnyButNewline,
    MatchChar8(u8),
    MatchChar16(u16),
    U16MatchChar32(u32),
    MatchNChar8(Vec<u8>),
    MatchNCharICase8(Vec<u8>),
    MatchCharICase8(u8),
    MatchCharICase16(u16),
    U16MatchCharICase32(u32),
    Alternation {
        secondary_branch: u32,
        primary_constraints: u8,
        secondary_constraints: u8,
    },
    Jump32(u32),
    Bracket {
        range_count: u32,
        negate: bool,
        positive_char_classes: u8,
        negative_char_classes: u8,
        brackets: Vec<(u32, u32)>,
    },
    U16Bracket {
        range_count: u32,
        negate: bool,
        positive_char_classes: u8,
        negative_char_classes: u8,
        brackets: Vec<(u32, u32)>,
    },
    BeginMarkedSubexpression(u16),
    EndMarkedSubexpression(u16),
    BackRef(u16),
    WordBoundary(bool),
    Lookaround {
        invert: bool,
        forwards: bool,
        constraints: u8,
        mexp_begin: u16,
        mexp_end: u16,
        continuation: u32,
    },
    BeginLoop {
        loop_id: u32,
        min: u32,
        max: u32,
        mexp_begin: u16,
        mexp_end: u16,
        greedy: bool,
        loopee_constraints: u8,
        not_taken_target: u32,
    },
    EndLoop(u32),
    BeginSimpleLoop {
        loopee_constraints: u8,
        not_taken_target: u32,
    },
    EndSimpleLoop(u32),
    Width1Loop {
        loop_id: u32,
        min: u32,
        max: u32,
        greedy: bool,
        not_taken_target: u32,
    },
    // ... add more as needed
    Unknown(u8),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RegexBytecodeHeader {
    pub marked_count: u16,
    pub loop_count: u16,
    pub syntax_flags: u8,
    pub constraints: u8,
}

impl RegexBytecodeHeader {
    pub fn parse(data: &[u8], offset: &mut usize) -> Option<Self> {
        if *offset + 6 > data.len() {
            return None;
        }
        let header = Self {
            marked_count: u16::from_le_bytes([data[*offset], data[*offset + 1]]),
            loop_count: u16::from_le_bytes([data[*offset + 2], data[*offset + 3]]),
            syntax_flags: data[*offset + 4],
            constraints: data[*offset + 5],
        };
        *offset += 6;
        Some(header)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegExpBytecodeParser<'a> {
    data: &'a [u8],
    offset: usize,
    pub header: Option<RegexBytecodeHeader>,
    pub instructions: Vec<RegexInstruction>,
    pub decompiled: Option<DecompiledRegExp>,
}

impl<'a> RegExpBytecodeParser<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self {
            data,
            offset: 0,
            header: None,
            instructions: Vec::new(),
            decompiled: None,
        }
    }

    pub fn parse(&mut self) -> Result<DecompiledRegExp, String> {
        self.header = RegexBytecodeHeader::parse(self.data, &mut self.offset);
        let data = self.data;
        let mut instructions = Vec::new();
        while self.offset < data.len() {
            let opcode_byte = data[self.offset];
            self.offset += 1;
            let opcode = RegexOpcode::from_u8(opcode_byte);
            let instr = match opcode {
                Some(RegexOpcode::Goal) => RegexInstruction::Goal,
                Some(RegexOpcode::LeftAnchor) => RegexInstruction::LeftAnchor,
                Some(RegexOpcode::RightAnchor) => RegexInstruction::RightAnchor,
                Some(RegexOpcode::MatchAny) => RegexInstruction::MatchAny,
                Some(RegexOpcode::U16MatchAny) => RegexInstruction::U16MatchAny,
                Some(RegexOpcode::MatchAnyButNewline) => RegexInstruction::MatchAnyButNewline,
                Some(RegexOpcode::U16MatchAnyButNewline) => RegexInstruction::U16MatchAnyButNewline,
                Some(RegexOpcode::MatchChar8) => {
                    let c = data.get(self.offset).copied().ok_or("EOF in MatchChar8")?;
                    self.offset += 1;
                    RegexInstruction::MatchChar8(c)
                }
                Some(RegexOpcode::MatchChar16) => {
                    let c = data
                        .get(self.offset..self.offset + 2)
                        .ok_or("EOF in MatchChar16")?;
                    let c = u16::from_le_bytes([c[0], c[1]]);
                    self.offset += 2;
                    RegexInstruction::MatchChar16(c)
                }
                Some(RegexOpcode::U16MatchChar32) => {
                    let c = data
                        .get(self.offset..self.offset + 4)
                        .ok_or("EOF in U16MatchChar32")?;
                    let c = u32::from_le_bytes([c[0], c[1], c[2], c[3]]);
                    self.offset += 4;
                    RegexInstruction::U16MatchChar32(c)
                }
                Some(RegexOpcode::MatchNChar8) => {
                    let count = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in MatchNChar8 count")?;
                    self.offset += 1;
                    let chars = data
                        .get(self.offset..self.offset + count as usize)
                        .ok_or("EOF in MatchNChar8 chars")?
                        .to_vec();
                    self.offset += count as usize;
                    RegexInstruction::MatchNChar8(chars)
                }
                Some(RegexOpcode::MatchNCharICase8) => {
                    let count = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in MatchNCharICase8 count")?;
                    self.offset += 1;
                    let chars = data
                        .get(self.offset..self.offset + count as usize)
                        .ok_or("EOF in MatchNCharICase8 chars")?
                        .to_vec();
                    self.offset += count as usize;
                    RegexInstruction::MatchNCharICase8(chars)
                }
                Some(RegexOpcode::MatchCharICase8) => {
                    let c = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in MatchCharICase8")?;
                    self.offset += 1;
                    RegexInstruction::MatchCharICase8(c)
                }
                Some(RegexOpcode::MatchCharICase16) => {
                    let c = data
                        .get(self.offset..self.offset + 2)
                        .ok_or("EOF in MatchCharICase16")?;
                    let c = u16::from_le_bytes([c[0], c[1]]);
                    self.offset += 2;
                    RegexInstruction::MatchCharICase16(c)
                }
                Some(RegexOpcode::U16MatchCharICase32) => {
                    let c = data
                        .get(self.offset..self.offset + 4)
                        .ok_or("EOF in U16MatchCharICase32")?;
                    let c = u32::from_le_bytes([c[0], c[1], c[2], c[3]]);
                    self.offset += 4;
                    RegexInstruction::U16MatchCharICase32(c)
                }
                Some(RegexOpcode::Alternation) => {
                    let sec = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in Alternation secondary_branch")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    let prim = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in Alternation primary_constraints")?;
                    self.offset += 1;
                    let sec_c = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in Alternation secondary_constraints")?;
                    self.offset += 1;
                    RegexInstruction::Alternation {
                        secondary_branch: sec,
                        primary_constraints: prim,
                        secondary_constraints: sec_c,
                    }
                }
                Some(RegexOpcode::Jump32) => {
                    let t = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in Jump32")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    RegexInstruction::Jump32(t)
                }
                Some(RegexOpcode::Bracket) | Some(RegexOpcode::U16Bracket) => {
                    let range_count = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in Bracket range_count")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    let flags = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in Bracket flags")?;
                    self.offset += 1;
                    let negate = (flags & 0x01) != 0;
                    let positive_char_classes = (flags >> 1) & 0x07;
                    let negative_char_classes = (flags >> 4) & 0x07;
                    let mut brackets = Vec::new();
                    for _ in 0..range_count {
                        let start = u32::from_le_bytes(
                            data.get(self.offset..self.offset + 4)
                                .ok_or("EOF in Bracket start")?
                                .try_into()
                                .unwrap(),
                        );
                        self.offset += 4;
                        let end = u32::from_le_bytes(
                            data.get(self.offset..self.offset + 4)
                                .ok_or("EOF in Bracket end")?
                                .try_into()
                                .unwrap(),
                        );
                        self.offset += 4;
                        brackets.push((start, end));
                    }
                    if opcode == Some(RegexOpcode::Bracket) {
                        RegexInstruction::Bracket {
                            range_count,
                            negate,
                            positive_char_classes,
                            negative_char_classes,
                            brackets,
                        }
                    } else {
                        RegexInstruction::U16Bracket {
                            range_count,
                            negate,
                            positive_char_classes,
                            negative_char_classes,
                            brackets,
                        }
                    }
                }
                Some(RegexOpcode::BeginMarkedSubexpression) => {
                    let mexp = u16::from_le_bytes(
                        data.get(self.offset..self.offset + 2)
                            .ok_or("EOF in BeginMarkedSubexpression")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 2;
                    RegexInstruction::BeginMarkedSubexpression(mexp)
                }
                Some(RegexOpcode::EndMarkedSubexpression) => {
                    let mexp = u16::from_le_bytes(
                        data.get(self.offset..self.offset + 2)
                            .ok_or("EOF in EndMarkedSubexpression")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 2;
                    RegexInstruction::EndMarkedSubexpression(mexp)
                }
                Some(RegexOpcode::BackRef) => {
                    let mexp = u16::from_le_bytes(
                        data.get(self.offset..self.offset + 2)
                            .ok_or("EOF in BackRef")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 2;
                    RegexInstruction::BackRef(mexp)
                }
                Some(RegexOpcode::WordBoundary) => {
                    let invert = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in WordBoundary")?
                        != 0;
                    self.offset += 1;
                    RegexInstruction::WordBoundary(invert)
                }
                Some(RegexOpcode::Lookaround) => {
                    let invert = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in Lookaround invert")?
                        != 0;
                    self.offset += 1;
                    let forwards = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in Lookaround forwards")?
                        != 0;
                    self.offset += 1;
                    let constraints = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in Lookaround constraints")?;
                    self.offset += 1;
                    let mexp_begin = u16::from_le_bytes(
                        data.get(self.offset..self.offset + 2)
                            .ok_or("EOF in Lookaround mexp_begin")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 2;
                    let mexp_end = u16::from_le_bytes(
                        data.get(self.offset..self.offset + 2)
                            .ok_or("EOF in Lookaround mexp_end")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 2;
                    let continuation = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in Lookaround continuation")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    RegexInstruction::Lookaround {
                        invert,
                        forwards,
                        constraints,
                        mexp_begin,
                        mexp_end,
                        continuation,
                    }
                }
                Some(RegexOpcode::BeginLoop) => {
                    let loop_id = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in BeginLoop loop_id")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    let min = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in BeginLoop min")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    let max = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in BeginLoop max")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    let mexp_begin = u16::from_le_bytes(
                        data.get(self.offset..self.offset + 2)
                            .ok_or("EOF in BeginLoop mexp_begin")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 2;
                    let mexp_end = u16::from_le_bytes(
                        data.get(self.offset..self.offset + 2)
                            .ok_or("EOF in BeginLoop mexp_end")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 2;
                    let greedy = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in BeginLoop greedy")?
                        != 0;
                    self.offset += 1;
                    let loopee_constraints = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in BeginLoop loopee_constraints")?;
                    self.offset += 1;
                    let not_taken_target = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in BeginLoop not_taken_target")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    RegexInstruction::BeginLoop {
                        loop_id,
                        min,
                        max,
                        mexp_begin,
                        mexp_end,
                        greedy,
                        loopee_constraints,
                        not_taken_target,
                    }
                }
                Some(RegexOpcode::EndLoop) => {
                    let target = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in EndLoop target")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    RegexInstruction::EndLoop(target)
                }
                Some(RegexOpcode::BeginSimpleLoop) => {
                    let loopee_constraints = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in BeginSimpleLoop loopee_constraints")?;
                    self.offset += 1;
                    let not_taken_target = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in BeginSimpleLoop not_taken_target")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    RegexInstruction::BeginSimpleLoop {
                        loopee_constraints,
                        not_taken_target,
                    }
                }
                Some(RegexOpcode::EndSimpleLoop) => {
                    let target = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in EndSimpleLoop target")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    RegexInstruction::EndSimpleLoop(target)
                }
                Some(RegexOpcode::Width1Loop) => {
                    let loop_id = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in Width1Loop loop_id")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    let min = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in Width1Loop min")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    let max = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in Width1Loop max")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    let greedy = data
                        .get(self.offset)
                        .copied()
                        .ok_or("EOF in Width1Loop greedy")?
                        != 0;
                    self.offset += 1;
                    let not_taken_target = u32::from_le_bytes(
                        data.get(self.offset..self.offset + 4)
                            .ok_or("EOF in Width1Loop not_taken_target")?
                            .try_into()
                            .unwrap(),
                    );
                    self.offset += 4;
                    RegexInstruction::Width1Loop {
                        loop_id,
                        min,
                        max,
                        greedy,
                        not_taken_target,
                    }
                }
                None => RegexInstruction::Unknown(opcode_byte),
            };
            instructions.push(instr);
        }
        self.instructions = instructions.clone();
        // Decompile
        let (pattern, flags) = decompile_regex(&self.header.as_ref().unwrap(), &instructions);
        self.decompiled = Some(DecompiledRegExp { pattern, flags });
        Ok(self.decompiled.as_ref().unwrap().clone())
    }
}

// --- Decompiler ---

fn escape_re_char(c: char) -> String {
    match c {
        '(' | ')' | '[' | ']' | '{' | '}' | '?' | '*' | '+' | '|' | '^' | '$' | '/' | '.' => {
            format!("\\{}", c)
        }
        _ if c.is_control() || !c.is_ascii() => format!("\\x{:02x}", c as u32),
        _ => c.to_string(),
    }
}

fn decompile_regex(
    header: &RegexBytecodeHeader,
    instructions: &[RegexInstruction],
) -> (String, String) {
    use RegexInstruction::*;
    let mut out = String::new();
    let mut flags = String::new();
    for instr in instructions {
        match instr {
            LeftAnchor => out.push('^'),
            RightAnchor => out.push('$'),
            MatchAny | U16MatchAny | MatchAnyButNewline | U16MatchAnyButNewline => out.push('.'),
            MatchChar8(c) | MatchCharICase8(c) => out.push_str(&escape_re_char(*c as char)),
            MatchChar16(c) | MatchCharICase16(c) => {
                out.push_str(&escape_re_char(char::from_u32(*c as u32).unwrap_or('?')))
            }
            U16MatchChar32(c) | U16MatchCharICase32(c) => {
                out.push_str(&escape_re_char(char::from_u32(*c).unwrap_or('?')))
            }
            MatchNChar8(chars) | MatchNCharICase8(chars) => {
                for &b in chars {
                    out.push_str(&escape_re_char(b as char));
                }
            }
            BeginMarkedSubexpression(_) => out.push('('),
            EndMarkedSubexpression(_) => out.push(')'),
            Bracket {
                brackets, negate, ..
            }
            | U16Bracket {
                brackets, negate, ..
            } => {
                out.push('[');
                if *negate {
                    out.push('^');
                }
                for &(start, end) in brackets {
                    if start == end {
                        out.push_str(&escape_re_char(char::from_u32(start).unwrap_or('?')));
                    } else {
                        out.push_str(&escape_re_char(char::from_u32(start).unwrap_or('?')));
                        out.push('-');
                        out.push_str(&escape_re_char(char::from_u32(end).unwrap_or('?')));
                    }
                }
                out.push(']');
            }
            WordBoundary(invert) => out.push_str(if *invert { "\\B" } else { "\\b" }),
            BackRef(mexp) => out.push_str(&format!("\\{}", mexp)),
            // Loops, alternations, lookarounds, etc. can be added here for more completeness
            // TODO: Add more instructions to the decompiler
            _ => {}
        }
    }
    // Flags
    if header.syntax_flags & 0x01 != 0 {
        flags.push('i');
    }
    if header.syntax_flags & 0x02 != 0 {
        flags.push('g');
    }
    if header.syntax_flags & 0x04 != 0 {
        flags.push('m');
    }
    if header.syntax_flags & 0x08 != 0 {
        flags.push('u');
    }
    if header.syntax_flags & 0x10 != 0 {
        flags.push('s');
    }
    if header.syntax_flags & 0x20 != 0 {
        flags.push('y');
    }
    (out, flags)
}

impl fmt::Display for DecompiledRegExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "/{}/{}", self.pattern, self.flags)
    }
}
