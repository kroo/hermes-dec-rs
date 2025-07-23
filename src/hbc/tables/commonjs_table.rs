use super::super::header::HbcHeader;
use serde::Serialize;

/// Represents a CommonJS module entry with symbol ID and function offset
#[derive(Debug, Clone, Copy, Serialize, PartialEq)]
pub struct CjsModuleEntry {
    /// Symbol ID (filename ID for non-static, global module ID for static)
    pub symbol_id: u32,
    /// Function index where the module is defined
    pub offset: u32,
}

impl CjsModuleEntry {
    pub fn new(symbol_id: u32, offset: u32) -> Self {
        Self { symbol_id, offset }
    }
}

/// CommonJS module table that can handle both static and non-static modules
#[derive(Debug)]
pub struct CommonJsTable {
    /// Vector of CJS module entries
    pub entries: Vec<CjsModuleEntry>,
    /// Whether modules are statically resolved
    pub is_static: bool,
}

impl CommonJsTable {
    /// Parse CommonJS modules from the given data
    pub fn parse(data: &[u8], header: &HbcHeader, offset: &mut usize) -> Result<Self, String> {
        let count = header.cjs_module_count() as usize;
        let is_static = header.cjs_modules_statically_resolved();
        let version = header.version();

        // Align to 4-byte boundary
        if *offset % 4 != 0 {
            *offset += 4 - (*offset % 4);
        }

        let mut entries = Vec::with_capacity(count);

        if is_static && version < 77 {
            // Static modules for version < 77: just read function indices
            for i in 0..count {
                if *offset + 4 > data.len() {
                    return Err(format!("Not enough bytes for CJS module entry {}", i));
                }

                let function_index =
                    u32::from_le_bytes(data[*offset..*offset + 4].try_into().unwrap());
                *offset += 4;

                // For static modules in old versions, symbol_id is the index
                entries.push(CjsModuleEntry::new(i as u32, function_index));
            }
        } else {
            // Non-static modules or static modules for version >= 77: read symbol_id and offset pairs
            for i in 0..count {
                if *offset + 8 > data.len() {
                    return Err(format!("Not enough bytes for CJS module entry {}", i));
                }

                let symbol_id = u32::from_le_bytes(data[*offset..*offset + 4].try_into().unwrap());
                let function_offset =
                    u32::from_le_bytes(data[*offset + 4..*offset + 8].try_into().unwrap());
                *offset += 8;

                entries.push(CjsModuleEntry::new(symbol_id, function_offset));
            }
        }

        Ok(CommonJsTable { entries, is_static })
    }

    /// Get the number of CJS modules
    pub fn count(&self) -> usize {
        self.entries.len()
    }

    /// Get a specific CJS module entry
    pub fn get_entry(&self, index: usize) -> Option<&CjsModuleEntry> {
        self.entries.get(index)
    }

    /// Get all entries as a vector of tuples (symbol_id, offset)
    pub fn get_entries(&self) -> Vec<(u32, u32)> {
        self.entries
            .iter()
            .map(|entry| (entry.symbol_id, entry.offset))
            .collect()
    }
}

impl Serialize for CommonJsTable {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("CommonJsTable", 3)?;

        state.serialize_field("is_static", &self.is_static)?;
        state.serialize_field("entries", &self.entries)?;

        state.end()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cjs_module_entry() {
        let entry = CjsModuleEntry::new(42, 100);
        assert_eq!(entry.symbol_id, 42);
        assert_eq!(entry.offset, 100);
    }

    #[test]
    fn test_commonjs_table_methods() {
        let entries = vec![CjsModuleEntry::new(1, 10), CjsModuleEntry::new(2, 20)];

        let table = CommonJsTable {
            entries: entries.clone(),
            is_static: true,
        };

        assert_eq!(table.count(), 2);
        assert!(table.is_static);

        assert_eq!(table.get_entry(0), Some(&entries[0]));
        assert_eq!(table.get_entry(1), Some(&entries[1]));
        assert_eq!(table.get_entry(2), None);

        let entries_tuples = table.get_entries();
        assert_eq!(entries_tuples, vec![(1, 10), (2, 20)]);
    }
}
