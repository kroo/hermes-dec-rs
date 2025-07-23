use super::super::header::HbcHeader;
use super::super::regexp_bytecode::{DecompiledRegExp, RegExpBytecodeParser};
use serde::ser::{SerializeStruct, Serializer};
use serde::Serialize;

#[derive(Debug)]
pub struct RegExpTable<'a> {
    pub table: &'a [u8],
    pub storage: &'a [u8],
    pub count: usize,
    pub regexps: Vec<RegExpBytecodeParser<'a>>,
}

impl<'a> RegExpTable<'a> {
    pub fn parse(data: &'a [u8], header: &HbcHeader, offset: &mut usize) -> Result<Self, String> {
        let count = header.reg_exp_count() as usize;
        let storage_size = header.reg_exp_storage_size() as usize;

        // Parse RegExp table
        let table_start = *offset;
        let table_size = count * 8; // 8 bytes per entry (4 bytes offset + 4 bytes length)

        if table_start + table_size > data.len() {
            return Err(format!(
                "RegExp table would exceed data bounds: start=0x{:x}, size={}, data_len={}",
                table_start,
                table_size,
                data.len()
            ));
        }

        let table = &data[table_start..table_start + table_size];
        *offset += table_size;

        // Align to 4-byte boundary
        Self::align_to_padding(offset, 4);

        // Parse RegExp storage
        let storage_start = *offset;

        if storage_start + storage_size > data.len() {
            return Err(format!(
                "RegExp storage would exceed data bounds: start=0x{:x}, size={}, data_len={}",
                storage_start,
                storage_size,
                data.len()
            ));
        }

        let storage = &data[storage_start..storage_start + storage_size];
        *offset += storage_size;

        // Parse RegExp entries
        let mut regexps = Vec::new();
        if storage_size > 0 {
            for i in 0..count {
                let offset = u32::from_le_bytes(table[i * 8..i * 8 + 4].try_into().unwrap());
                let length = u32::from_le_bytes(table[i * 8 + 4..i * 8 + 8].try_into().unwrap());
                let bytecode = &storage[offset as usize..offset as usize + length as usize];
                let mut parser = super::super::regexp_bytecode::RegExpBytecodeParser::new(bytecode);
                parser.parse()?;
                regexps.push(parser);
            }
        }

        Ok(RegExpTable {
            table,
            storage,
            count,
            regexps,
        })
    }

    fn align_to_padding(offset: &mut usize, padding: usize) {
        let remainder = *offset % padding;
        if remainder != 0 {
            *offset += padding - remainder;
        }
    }
    pub fn count(&self) -> usize {
        self.count
    }

    pub fn get(&self, index: u32) -> Option<&DecompiledRegExp> {
        if index < self.count as u32 {
            self.regexps[index as usize].decompiled.as_ref()
        } else {
            None
        }
    }

    pub fn get_placeholder(&self, index: usize) -> Option<String> {
        if index < self.count {
            Some(format!("regexp_{}", index))
        } else {
            None
        }
    }
}

impl<'a> Serialize for RegExpTable<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("RegExpTable", 2)?;
        state.serialize_field("count", &self.count())?;
        let mut regexps = Vec::new();
        for i in 0..self.count() {
            let regexp = self.get(i as u32);
            if let Some(regexp) = regexp {
                regexps.push(regexp.to_string());
            } else {
                let placeholder = self.get_placeholder(i);
                regexps.push(placeholder.unwrap_or("".to_string()));
            }
        }
        state.serialize_field("regexps", &regexps)?;
        state.end()
    }
}
