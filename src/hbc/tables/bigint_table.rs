use super::super::header::HbcHeader;
use num_bigint;
use serde::ser::{Serialize, SerializeStruct, Serializer};

#[derive(Debug)]
pub struct BigIntTable<'a> {
    pub table: &'a [u8],
    pub storage: &'a [u8],
    pub count: usize,
}

impl<'a> BigIntTable<'a> {
    pub fn parse(data: &'a [u8], header: &HbcHeader, offset: &mut usize) -> Result<Self, String> {
        // Check if BigInt support is available for this version
        let big_int_count = match header.big_int_count() {
            Some(count) => count as usize,
            None => {
                // No BigInt support for this version, return empty table
                return Ok(BigIntTable {
                    table: &[],
                    storage: &[],
                    count: 0,
                });
            }
        };

        let big_int_storage_size = match header.big_int_storage_size() {
            Some(size) => size as usize,
            None => {
                return Ok(BigIntTable {
                    table: &[],
                    storage: &[],
                    count: 0,
                });
            }
        };

        // Parse BigInt table
        let table_start = *offset;
        let table_size = big_int_count * 8; // 8 bytes per entry (4 bytes offset + 4 bytes length)
        let table = &data[table_start..table_start + table_size];
        *offset += table_size;

        // Align to 4-byte boundary
        Self::align_to_padding(offset, 4);

        // Parse BigInt storage
        let storage_start = *offset;
        let storage = &data[storage_start..storage_start + big_int_storage_size];
        *offset += big_int_storage_size;

        Ok(BigIntTable {
            table,
            storage,
            count: big_int_count,
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

    pub fn get_placeholder(&self, index: usize) -> Option<String> {
        if index < self.count {
            Some(format!("bigint_{}", index))
        } else {
            None
        }
    }

    pub fn get_string(&self, index: u32) -> Result<String, String> {
        self.get(index).map(|value| value.to_string())
    }

    pub fn get(&self, index: u32) -> Result<num_bigint::BigUint, String> {
        let entry_offset = index * 8; // 8 bytes per entry (4 bytes offset + 4 bytes length)
        if entry_offset + 8 > self.table.len() as u32 {
            return Err(format!(
                "BigInt table entry {} would exceed table bounds",
                index
            ));
        }

        let entry_bytes = &self.table[entry_offset as usize..entry_offset as usize + 8];
        let offset = u32::from_le_bytes(entry_bytes[0..4].try_into().unwrap());
        let length = u32::from_le_bytes(entry_bytes[4..8].try_into().unwrap());

        // Extract BigInt data from storage
        let storage_offset = offset as usize;
        let storage_end = storage_offset + length as usize;

        if storage_end > self.storage.len() {
            return Err(format!(
                "BigInt {} storage would exceed bounds: offset={}, length={}, storage_size={}",
                index,
                offset,
                length,
                self.storage.len()
            ));
        }

        let bigint_data = &self.storage[storage_offset..storage_end];
        // Decode as little-endian integer
        let value = num_bigint::BigUint::from_bytes_le(bigint_data);
        Ok(value)
    }

    pub fn extract_bigints(&self) -> Result<Vec<String>, String> {
        let mut bigints = Vec::new();

        // Parse BigInt table entries
        for i in 0..self.count as u32 {
            let bigint = self.get_string(i)?;
            bigints.push(bigint);
        }

        Ok(bigints)
    }
}

impl<'a> Serialize for BigIntTable<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("BigIntTable", 2)?;
        state.serialize_field("count", &self.count())?;

        // Extract actual BigInt values
        let bigints = match self.extract_bigints() {
            Ok(values) => values,
            Err(_) => {
                // Fallback to placeholders if parsing fails
                let mut seq = Vec::with_capacity(self.count());
                for i in 0..self.count() {
                    seq.push(
                        self.get_placeholder(i)
                            .unwrap_or_else(|| format!("bigint_{}", i)),
                    );
                }
                seq
            }
        };

        state.serialize_field("bigints", &bigints)?;
        state.end()
    }
}
