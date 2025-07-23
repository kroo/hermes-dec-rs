use super::super::header::HbcHeader;
use serde::ser::{Serialize, SerializeSeq};
use serde::{Deserialize, Serialize as SerializeTrait};
use serde_json;

#[derive(Debug)]
pub struct StringTable<'a> {
    pub string_kinds_data: &'a [u8],
    pub identifier_hashes_data: &'a [u8],
    pub small_string_table_data: &'a [u8],
    pub overflow_string_table_data: &'a [u8],
    pub storage: &'a [u8],
    pub string_count: u32,
    pub overflow_string_count: u32,
    pub version: u32,
    // Cached parsed entries for O(1) lookups
    pub small_entries: Vec<SmallStringTableEntry>,
    pub overflow_entries: Vec<OverflowStringTableEntry>,
    pub string_kinds: Vec<StringKind>,
    pub identifier_hashes: Vec<u32>,
    pub index_to_identifier_map: Vec<Option<u32>>,
    // Cached converted strings for fast lookups
    pub string_cache: Vec<Option<String>>,
}

#[derive(Debug)]
pub struct SmallStringTableEntry {
    pub is_utf16: bool,
    pub is_identifier: Option<bool>,
    pub offset: u32,
    pub length: u8,
}

#[derive(Debug)]
pub struct OverflowStringTableEntry {
    pub offset: u32,
    pub length: u32,
}

#[derive(Debug)]
pub struct StringTableEntry<'a> {
    pub is_utf16: bool,
    pub is_identifier: Option<bool>,
    pub offset: u32,
    pub length: u32,
    pub string_kind: StringKind,
    pub identifier_hash: Option<u32>,
    pub bytes: &'a [u8],
}

impl<'a> StringTableEntry<'a> {
    pub fn hex_string(&self) -> String {
        if self.is_utf16 {
            // Convert UTF-16 string to hex bytes
            let hex_string = self
                .bytes
                .chunks_exact(2)
                .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
                .map(|c| format!("\\x{:04X}", c))
                .collect::<String>();
            hex_string
        } else {
            // Convert ASCII string to hex bytes
            let hex_string = self
                .bytes
                .iter()
                .map(|b| format!("\\x{:02X}", b))
                .collect::<String>();
            hex_string
        }
    }

    pub fn string_value(&self) -> String {
        if self.is_utf16 {
            let utf16_bytes = self
                .bytes
                .chunks_exact(2)
                .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
                .collect::<Vec<u16>>();
            String::from_utf16_lossy(&utf16_bytes)
        } else {
            String::from_utf8_lossy(self.bytes).to_string()
        }
    }
}

#[derive(Debug, Clone, SerializeTrait, Deserialize, PartialEq, Eq)]
pub enum StringKind {
    String,
    Identifier,
}

impl StringKind {
    fn from_u32(value: u32) -> Result<Self, String> {
        match value {
            0 => Ok(StringKind::String),
            1 => Ok(StringKind::Identifier),
            _ => Err(format!("Invalid string kind: {}", value)),
        }
    }
}

impl<'a> StringTable<'a> {
    fn align_to_padding(offset: &mut usize, padding: usize) {
        let remainder = *offset % padding;
        if remainder != 0 {
            *offset += padding - remainder;
        }
    }

    pub fn parse(data: &'a [u8], header: &HbcHeader, offset: &mut usize) -> Result<Self, String> {
        // Align to 4-byte boundary before starting
        Self::align_to_padding(offset, 4);

        // Parse string kinds (run-length encoded)
        let string_kinds_start = *offset;
        let string_kinds_size = header.string_kind_count() as usize * 4;
        let string_kinds_data = &data[string_kinds_start..string_kinds_start + string_kinds_size];
        *offset += string_kinds_size;

        // Align to 4-byte boundary
        Self::align_to_padding(offset, 4);

        // Parse identifier hashes
        let identifier_hashes_start = *offset;
        let identifier_hashes_size = header.identifier_count() as usize * 4;
        let identifier_hashes_data =
            &data[identifier_hashes_start..identifier_hashes_start + identifier_hashes_size];
        *offset += identifier_hashes_size;

        // Align to 4-byte boundary
        Self::align_to_padding(offset, 4);

        // Parse small string table
        let small_string_table_start = *offset;
        let small_string_table_size = header.string_count() as usize * 4;
        let small_string_table =
            &data[small_string_table_start..small_string_table_start + small_string_table_size];
        *offset += small_string_table_size;

        // Align to 4-byte boundary
        Self::align_to_padding(offset, 4);

        // Parse overflow string table
        let overflow_string_table_start = *offset;
        let overflow_string_table_size = header.overflow_string_count() as usize * 8;
        let overflow_string_table = &data
            [overflow_string_table_start..overflow_string_table_start + overflow_string_table_size];
        *offset += overflow_string_table_size;

        // Align to 4-byte boundary before reading string storage
        Self::align_to_padding(offset, 4);

        // Parse string storage
        let storage_start = *offset;
        let storage_size = header.string_storage_size() as usize;
        let storage = &data[storage_start..storage_start + storage_size];
        *offset += storage_size;

        // Parse the small and overflow string tables once during initialization
        let small_entries = Self::parse_small_string_table_internal(
            small_string_table,
            header.string_count(),
            header.version(),
        )?;
        let overflow_entries = Self::parse_overflow_string_table_internal(overflow_string_table)?;
        let (string_kinds, index_to_identifier_map) = Self::parse_string_kinds(string_kinds_data)?;
        let identifier_hashes = Self::parse_identifier_hashes(identifier_hashes_data)?;

        Ok(StringTable {
            string_kinds_data,
            identifier_hashes_data,
            small_string_table_data: small_string_table,
            overflow_string_table_data: overflow_string_table,
            storage,
            string_count: header.string_count(),
            overflow_string_count: header.overflow_string_count(),
            version: header.version(),
            small_entries,
            overflow_entries,
            string_kinds,
            identifier_hashes,
            index_to_identifier_map,
            string_cache: vec![None; header.string_count() as usize],
        })
    }

    /// Populate the string cache with all converted strings
    pub fn populate_cache(&mut self) -> Result<(), String> {
        for i in 0..self.string_count {
            let string = self.get_uncached(i)?;
            self.string_cache[i as usize] = Some(string);
        }
        Ok(())
    }

    /// Check if cache is populated (for debugging)
    pub fn cache_stats(&self) -> (usize, usize) {
        let total = self.string_cache.len();
        let populated = self.string_cache.iter().filter(|s| s.is_some()).count();
        (total, populated)
    }

    pub fn get_entry(&self, string_id: u32) -> Result<StringTableEntry, String> {
        if string_id >= self.string_count {
            return Err(format!(
                "String ID {} out of range (max: {})",
                string_id,
                self.string_count - 1
            ));
        }

        let entry = &self.small_entries[string_id as usize];
        let offset_bytes = entry.offset;
        let length_raw = entry.length;
        let is_utf16 = entry.is_utf16;
        let uses_overflow = length_raw == 255;

        let bytes = if uses_overflow {
            // For the overflow case, look up the associated overflow entry
            if offset_bytes < self.overflow_entries.len() as u32 {
                let overflow_entry = &self.overflow_entries[offset_bytes as usize];
                let actual_offset = overflow_entry.offset;
                let actual_length = overflow_entry.length;
                let byte_length = if is_utf16 {
                    actual_length * 2
                } else {
                    actual_length
                };
                let end = actual_offset as usize + byte_length as usize;

                if end > self.storage.len() {
                    return Err(format!(
                        "String storage access out of bounds: offset={}, length={}, storage_size={}",
                        actual_offset, byte_length, self.storage.len()
                    ));
                }

                &self.storage[actual_offset as usize..end]
            } else {
                return Err(format!("String ID {}: OVERFLOW offset out of range (raw offset={} overflow_entries.len()={})", 
                                 string_id, offset_bytes, self.overflow_entries.len()));
            }
        } else {
            // for the simpler case, we just need to handle the utf16 case
            let length = length_raw as u32;
            let byte_length = if is_utf16 { length * 2 } else { length };
            let end = offset_bytes as usize + byte_length as usize;

            if end > self.storage.len() {
                return Err(format!(
                    "String storage access out of bounds: offset={}, length={}, storage_size={}",
                    offset_bytes,
                    byte_length,
                    self.storage.len()
                ));
            }

            &self.storage[offset_bytes as usize..end]
        };

        let string_kind = self
            .string_kinds
            .get(string_id as usize)
            .ok_or(format!(
                "String ID {} out of range (max: {})",
                string_id,
                self.string_count - 1
            ))?
            .clone();
        let identifier_hash = self.identifier_hashes.get(string_id as usize).cloned();

        Ok(StringTableEntry {
            is_utf16,
            is_identifier: entry.is_identifier,
            offset: offset_bytes,
            length: length_raw as u32,
            string_kind,
            identifier_hash,
            bytes,
        })
    }

    /// Get a string by ID in O(1) time
    pub fn get(&self, string_id: u32) -> Result<String, String> {
        // Check cache first
        if string_id < self.string_count {
            if let Some(ref cached_string) = self.string_cache[string_id as usize] {
                return Ok(cached_string.clone());
            }
        }

        // Fallback to original implementation if not cached
        self.get_uncached(string_id)
    }

    /// Get a string by ID without using cache (for cache population)
    fn get_uncached(&self, string_id: u32) -> Result<String, String> {
        let entry = self.get_entry(string_id)?;

        if entry.is_utf16 {
            let utf16_bytes = entry
                .bytes
                .chunks_exact(2)
                .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
                .collect::<Vec<u16>>();
            Ok(String::from_utf16_lossy(&utf16_bytes))
        } else {
            Ok(String::from_utf8_lossy(entry.bytes).to_string())
        }
    }

    pub fn count(&self) -> usize {
        self.string_count as usize
    }

    pub fn extract_strings(&self) -> Result<Vec<String>, String> {
        let mut strings = Vec::new();

        for i in 0..self.count() {
            strings.push(self.get(i as u32)?);
        }

        Ok(strings)
    }

    fn parse_small_string_table_internal(
        small_string_table: &[u8],
        string_count: u32,
        version: u32,
    ) -> Result<Vec<SmallStringTableEntry>, String> {
        let mut entries = Vec::new();

        for i in 0..string_count {
            let entry_offset = i as usize * 4;
            let entry_bytes: [u8; 4] = small_string_table[entry_offset..entry_offset + 4]
                .try_into()
                .unwrap();

            // Extract fields directly from the byte array, matching Hermes spec
            let length = entry_bytes[3];

            let flags_length = if version < 56 { 2 } else { 1 };
            let first_byte_mask = if version < 56 { 0xFC } else { 0xFE };
            let offset = ((entry_bytes[0] as u32 & first_byte_mask) >> flags_length)
                | ((entry_bytes[1] as u32) << (8 - flags_length))
                | ((entry_bytes[2] as u32) << (16 - flags_length));

            let is_utf16 = entry_bytes[0] & 0x01 != 0;

            let is_identifier = if version < 56 {
                // Extract isIdentifier bit for older versions (bit 6 of entry_bytes[3])
                Some((entry_bytes[0] & 0x40) != 0)
            } else {
                None
            };

            entries.push(SmallStringTableEntry {
                is_utf16,
                is_identifier,
                offset,
                length,
            });
        }

        Ok(entries)
    }

    fn parse_overflow_string_table_internal(
        overflow_string_table: &[u8],
    ) -> Result<Vec<OverflowStringTableEntry>, String> {
        let mut entries = Vec::new();

        // Each entry is 8 bytes - 4 bytes offset, 4 bytes length
        for chunk in overflow_string_table.chunks_exact(8) {
            let offset = u32::from_le_bytes(chunk[0..4].try_into().unwrap());
            let length = u32::from_le_bytes(chunk[4..8].try_into().unwrap());

            entries.push(OverflowStringTableEntry { offset, length });
        }

        Ok(entries)
    }

    fn parse_small_string_table(&self, version: u32) -> Result<Vec<SmallStringTableEntry>, String> {
        Self::parse_small_string_table_internal(
            self.small_string_table_data,
            self.string_count,
            version,
        )
    }

    fn parse_overflow_string_table(&self) -> Result<Vec<OverflowStringTableEntry>, String> {
        Self::parse_overflow_string_table_internal(self.overflow_string_table_data)
    }

    pub fn parse_string_kinds(
        string_kinds_data: &'a [u8],
    ) -> Result<(Vec<StringKind>, Vec<Option<u32>>), String> {
        let mut string_kinds = Vec::new();
        let mut offset = 0;

        // store an array of offsets to the identifier hashes
        let mut index_to_identifier_map = Vec::new();

        let mut count_indentfiers = 0;
        while offset < string_kinds_data.len() {
            let raw_bytes = &string_kinds_data[offset..offset + 4];
            let raw_entry = u32::from_le_bytes(raw_bytes.try_into().unwrap());
            // Correct bitfield logic: count = raw_entry & 0x7FFFFFFF, kind = (raw_entry & 0x80000000) >> 31
            let count = raw_entry & 0x7FFFFFFF;
            let kind = (raw_entry & 0x80000000) >> 31;
            let kind_enum = StringKind::from_u32(kind).map_err(|e| e.to_string())?;
            for _ in 0..count {
                string_kinds.push(kind_enum.clone());
                if kind_enum == StringKind::Identifier {
                    index_to_identifier_map.push(Some(count_indentfiers));
                    count_indentfiers += 1;
                } else {
                    index_to_identifier_map.push(None);
                }
            }
            offset += 4;
        }

        Ok((string_kinds, index_to_identifier_map))
    }

    pub fn parse_identifier_hashes(identifier_hashes_data: &[u8]) -> Result<Vec<u32>, String> {
        let mut hashes = Vec::new();

        for i in 0..identifier_hashes_data.len() / 4 {
            let offset = i * 4;
            let hash = u32::from_le_bytes(
                identifier_hashes_data[offset..offset + 4]
                    .try_into()
                    .unwrap(),
            );
            hashes.push(hash);
        }

        Ok(hashes)
    }
}

impl<'a> Serialize for StringTable<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let entries = self
            .parse_small_string_table(self.version)
            .map_err(serde::ser::Error::custom)?;
        let kinds = self.string_kinds.clone();
        let identifier_hashes = self.identifier_hashes.clone();
        let strings = self.extract_strings().unwrap_or_default();
        let mut seq = serializer.serialize_seq(Some(entries.len()))?;
        let mut id_hash_idx = 0;
        for (i, entry) in entries.iter().enumerate() {
            let string_type = match kinds.get(i) {
                Some(StringKind::Identifier) => "Identifier",
                _ => "String",
            };
            let is_utf16 = entry.is_utf16;
            let value = strings.get(i).cloned().unwrap_or_default();
            let mut obj = serde_json::Map::new();
            obj.insert("string_id".to_string(), serde_json::json!(i));
            obj.insert("string_type".to_string(), serde_json::json!(string_type));
            obj.insert("is_utf16".to_string(), serde_json::json!(is_utf16));
            obj.insert("value".to_string(), serde_json::json!(value));
            if let Some(StringKind::Identifier) = kinds.get(i) {
                if let Some(hash) = identifier_hashes.get(id_hash_idx) {
                    obj.insert("identifier_hash".to_string(), serde_json::json!(hash));
                }
                id_hash_idx += 1;
            }
            seq.serialize_element(&obj)?;
        }
        seq.end()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_string_kind_parsing() {
        // Entry 1: 0x11 0x00 0x00 0x00 (little-endian) => 0x00000011
        // Entry 2: 0x11 0x00 0x00 0x80 (little-endian) => 0x80000011
        let data = [0x11, 0x00, 0x00, 0x00, 0x11, 0x00, 0x00, 0x80];
        let mut kinds = Vec::new();
        let mut offset = 0;
        for _i in 0..2 {
            let raw_entry = u32::from_le_bytes(data[offset..offset + 4].try_into().unwrap());
            let count = raw_entry & 0x7FFFFFFF;
            let kind = (raw_entry & 0x80000000) >> 31;
            kinds.push((count, kind));
            offset += 4;
        }
        assert_eq!(kinds[0], (17, 0));
        assert_eq!(kinds[1], (17, 1));
    }
}
