use super::super::header::HbcHeader;
use super::string_table::StringTable;
use serde::Serialize;

/// Represents a function source entry mapping function ID to string ID
#[derive(Debug, Clone, Copy, Serialize, PartialEq)]
pub struct FunctionSourceEntry {
    /// Function ID (index into function table)
    pub function_id: u32,
    /// String ID (index into string table for source filename)
    pub string_id: u32,
}

impl FunctionSourceEntry {
    pub fn new(function_id: u32, string_id: u32) -> Self {
        Self {
            function_id,
            string_id,
        }
    }
}

/// Function source table that maps function IDs to their source filenames
#[derive(Debug)]
pub struct FunctionSourceTable {
    /// Vector of function source entries
    pub entries: Vec<FunctionSourceEntry>,
}

impl FunctionSourceTable {
    /// Parse function sources from the given data
    pub fn parse(data: &[u8], header: &HbcHeader, offset: &mut usize) -> Result<Self, String> {
        let count = match header.function_source_count() {
            Some(count) => count as usize,
            None => {
                // No function source support for this version, return empty table
                return Ok(FunctionSourceTable {
                    entries: Vec::new(),
                });
            }
        };

        // Align to 4-byte boundary
        if *offset % 4 != 0 {
            *offset += 4 - (*offset % 4);
        }

        let mut entries = Vec::with_capacity(count);

        // Each entry is 8 bytes: 4 bytes function_id + 4 bytes string_id
        for i in 0..count {
            if *offset + 8 > data.len() {
                return Err(format!("Not enough bytes for function source entry {}", i));
            }

            let function_id = u32::from_le_bytes(data[*offset..*offset + 4].try_into().unwrap());
            let string_id = u32::from_le_bytes(data[*offset + 4..*offset + 8].try_into().unwrap());
            *offset += 8;

            entries.push(FunctionSourceEntry::new(function_id, string_id));
        }

        Ok(FunctionSourceTable { entries })
    }

    /// Get the number of function source entries
    pub fn count(&self) -> usize {
        self.entries.len()
    }

    /// Get a specific function source entry
    pub fn get_entry(&self, index: usize) -> Option<&FunctionSourceEntry> {
        self.entries.get(index)
    }

    /// Get all entries as a vector of tuples (function_id, string_id)
    pub fn get_entries(&self) -> Vec<(u32, u32)> {
        self.entries
            .iter()
            .map(|entry| (entry.function_id, entry.string_id))
            .collect()
    }

    /// Get the source filename for a function ID
    pub fn get_source_filename(
        &self,
        function_id: u32,
        string_table: &StringTable,
    ) -> Option<String> {
        // Find the entry for this function_id
        let entry = self.entries.iter().find(|e| e.function_id == function_id)?;

        // Get the string from the string table
        match string_table.extract_strings() {
            Ok(strings) => {
                if (entry.string_id as usize) < strings.len() {
                    Some(strings[entry.string_id as usize].clone())
                } else {
                    None
                }
            }
            Err(_) => None,
        }
    }
}

impl Serialize for FunctionSourceTable {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("FunctionSourceTable", 1)?;

        state.serialize_field("entries", &self.entries)?;

        state.end()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_source_entry() {
        let entry = FunctionSourceEntry::new(42, 100);
        assert_eq!(entry.function_id, 42);
        assert_eq!(entry.string_id, 100);
    }

    #[test]
    fn test_function_source_table_methods() {
        let entries = vec![
            FunctionSourceEntry::new(1, 10),
            FunctionSourceEntry::new(2, 20),
        ];

        let table = FunctionSourceTable {
            entries: entries.clone(),
        };

        assert_eq!(table.count(), 2);

        assert_eq!(table.get_entry(0), Some(&entries[0]));
        assert_eq!(table.get_entry(1), Some(&entries[1]));
        assert_eq!(table.get_entry(2), None);

        let entries_tuples = table.get_entries();
        assert_eq!(entries_tuples, vec![(1, 10), (2, 20)]);
    }
}
