use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Represents a single case in a switch statement
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SwitchCase {
    /// The case value (e.g., 0, 1, 2, etc.)
    pub value: u32,
    /// The relative offset to the case target
    pub target_offset: i32,
    /// The instruction index where this case target is located
    pub target_instruction_index: Option<u32>,
}

/// Represents a parsed jump table with its entries
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct JumpTableData {
    /// The address where this jump table is located in the bytecode
    pub address: u32,
    /// The minimum value for this jump table
    pub min_value: u32,
    /// The maximum value for this jump table
    pub max_value: u32,
    /// The parsed jump table entries (case value -> target offset)
    pub entries: HashMap<u32, i32>,
    /// The number of switch tables that reference this jump table
    pub reference_count: u32,
}

impl JumpTableData {
    /// Create a new jump table data structure
    pub fn new(address: u32, min_value: u32, max_value: u32) -> Self {
        Self {
            address,
            min_value,
            max_value,
            entries: HashMap::new(),
            reference_count: 0,
        }
    }

    /// Add a jump table entry
    pub fn add_entry(&mut self, case_value: u32, target_offset: i32) {
        self.entries.insert(case_value, target_offset);
    }

    /// Get a jump table entry
    pub fn get_entry(&self, case_value: u32) -> Option<&i32> {
        self.entries.get(&case_value)
    }

    /// Check if this jump table has all expected entries
    pub fn is_complete(&self) -> bool {
        self.entries.len() == (self.max_value - self.min_value + 1) as usize
    }

    /// Get all case values in sorted order
    pub fn get_case_values(&self) -> Vec<u32> {
        let mut values: Vec<u32> = self.entries.keys().copied().collect();
        values.sort();
        values
    }

    /// Increment the reference count
    pub fn increment_reference_count(&mut self) {
        self.reference_count += 1;
    }
}

/// Cache for parsed jump tables to avoid re-parsing the same jump table multiple times
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JumpTableCache {
    /// Maps jump table address to parsed jump table data
    pub jump_tables: HashMap<u32, JumpTableData>,
}

impl JumpTableCache {
    /// Create a new empty jump table cache
    pub fn new() -> Self {
        Self {
            jump_tables: HashMap::new(),
        }
    }

    /// Get or create a jump table at the specified address
    pub fn get_or_create_jump_table(
        &mut self,
        address: u32,
        min_value: u32,
        max_value: u32,
    ) -> &mut JumpTableData {
        self.jump_tables
            .entry(address)
            .or_insert_with(|| JumpTableData::new(address, min_value, max_value))
    }

    /// Get a jump table by address
    pub fn get_jump_table(&self, address: u32) -> Option<&JumpTableData> {
        self.jump_tables.get(&address)
    }

    /// Check if a jump table exists at the specified address
    pub fn has_jump_table(&self, address: u32) -> bool {
        self.jump_tables.contains_key(&address)
    }

    /// Get the number of cached jump tables
    pub fn len(&self) -> usize {
        self.jump_tables.len()
    }

    /// Check if the cache is empty
    pub fn is_empty(&self) -> bool {
        self.jump_tables.is_empty()
    }

    /// Get all jump table addresses
    pub fn get_addresses(&self) -> Vec<u32> {
        self.jump_tables.keys().copied().collect()
    }

    /// Get jump tables that are referenced by multiple switch tables
    pub fn get_shared_jump_tables(&self) -> Vec<&JumpTableData> {
        self.jump_tables
            .values()
            .filter(|jt| jt.reference_count > 1)
            .collect()
    }
}

impl Default for JumpTableCache {
    fn default() -> Self {
        Self::new()
    }
}

/// Represents a complete switch table for a SwitchImm instruction
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SwitchTable {
    /// The minimum value (inclusive) for the switch range
    pub min_value: u32,
    /// The maximum value (inclusive) for the switch range
    pub max_value: u32,
    /// The relative offset for the default case
    pub default_offset: i32,
    /// The instruction index for the default case
    pub default_instruction_index: Option<u32>,
    /// The relative offset to the jump table
    pub jump_table_offset: u32,
    /// The absolute address of the jump table
    pub jump_table_address: u32,
    /// All cases in this switch table
    pub cases: Vec<SwitchCase>,
    /// The instruction index where this switch instruction is located
    pub switch_instruction_index: u32,
    /// The function index this switch table belongs to
    pub function_index: u32,
}

impl SwitchTable {
    /// Create a new switch table
    pub fn new(
        min_value: u32,
        max_value: u32,
        default_offset: i32,
        jump_table_offset: u32,
        jump_table_address: u32,
        switch_instruction_index: u32,
        function_index: u32,
    ) -> Self {
        Self {
            min_value,
            max_value,
            default_offset,
            default_instruction_index: None,
            jump_table_offset,
            jump_table_address,
            cases: Vec::new(),
            switch_instruction_index,
            function_index,
        }
    }

    /// Add a case to the switch table
    pub fn add_case(&mut self, value: u32, target_offset: i32) {
        self.cases.push(SwitchCase {
            value,
            target_offset,
            target_instruction_index: None,
        });
    }

    /// Get the number of cases in this switch table
    pub fn case_count(&self) -> usize {
        self.cases.len()
    }

    /// Get a case by its value
    pub fn get_case_by_value(&self, value: u32) -> Option<&SwitchCase> {
        self.cases.iter().find(|case| case.value == value)
    }

    /// Check if a value is within the switch range
    pub fn is_value_in_range(&self, value: u32) -> bool {
        value >= self.min_value && value <= self.max_value
    }

    /// Get the case for a given value (returns None if out of range)
    pub fn get_case_for_value(&self, value: u32) -> Option<&SwitchCase> {
        if self.is_value_in_range(value) {
            self.get_case_by_value(value)
        } else {
            None
        }
    }

    /// Get all case values in sorted order
    pub fn get_case_values(&self) -> Vec<u32> {
        let mut values: Vec<u32> = self.cases.iter().map(|case| case.value).collect();
        values.sort();
        values
    }

    /// Get the expected number of cases based on the range
    pub fn expected_case_count(&self) -> usize {
        (self.max_value - self.min_value + 1) as usize
    }

    /// Check if the switch table is complete (has all expected cases)
    pub fn is_complete(&self) -> bool {
        self.cases.len() == self.expected_case_count()
    }

    /// Check if this switch table shares its jump table with other switch tables
    pub fn shares_jump_table(&self, other: &SwitchTable) -> bool {
        self.jump_table_address == other.jump_table_address
    }
}

/// Collection of switch tables organized by function
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SwitchTableCollection {
    /// Maps function index to switch tables in that function
    switch_tables_by_function: HashMap<u32, Vec<SwitchTable>>,
    /// Maps function index to {instruction index -> switch table index}
    instruction_to_switch_map: HashMap<u32, HashMap<u32, usize>>,
    /// Jump table cache for shared jump tables
    jump_table_cache: JumpTableCache,
}

impl SwitchTableCollection {
    /// Create a new empty switch table collection
    pub fn new() -> Self {
        Self {
            switch_tables_by_function: HashMap::new(),
            instruction_to_switch_map: HashMap::new(),
            jump_table_cache: JumpTableCache::new(),
        }
    }

    /// Add a switch table to the collection
    pub fn add_switch_table(&mut self, switch_table: SwitchTable) {
        let function_index = switch_table.function_index;
        let instruction_index = switch_table.switch_instruction_index;

        // Add to function's switch tables
        let function_tables = self
            .switch_tables_by_function
            .entry(function_index)
            .or_insert_with(Vec::new);
        let table_index = function_tables.len();
        function_tables.push(switch_table);

        // Add to instruction mapping
        let instruction_map = self
            .instruction_to_switch_map
            .entry(function_index)
            .or_insert_with(HashMap::new);
        instruction_map.insert(instruction_index, table_index);
    }

    /// Get all switch tables for a function
    pub fn get_switch_tables_for_function(&self, function_index: u32) -> Option<&Vec<SwitchTable>> {
        self.switch_tables_by_function.get(&function_index)
    }

    /// Get a switch table by function and instruction index
    pub fn get_switch_table_by_instruction(
        &self,
        function_index: u32,
        instruction_index: u32,
    ) -> Option<&SwitchTable> {
        self.instruction_to_switch_map
            .get(&function_index)
            .and_then(|instruction_map| instruction_map.get(&instruction_index))
            .and_then(|&table_index| {
                self.switch_tables_by_function
                    .get(&function_index)
                    .and_then(|tables| tables.get(table_index))
            })
    }

    /// Get the number of switch tables for a function
    pub fn get_switch_table_count(&self, function_index: u32) -> usize {
        self.switch_tables_by_function
            .get(&function_index)
            .map(|tables| tables.len())
            .unwrap_or(0)
    }

    /// Check if a function has any switch tables
    pub fn has_switch_tables(&self, function_index: u32) -> bool {
        self.get_switch_table_count(function_index) > 0
    }

    /// Get the jump table cache
    pub fn get_jump_table_cache(&self) -> &JumpTableCache {
        &self.jump_table_cache
    }

    /// Get the jump table cache (mutable)
    pub fn get_jump_table_cache_mut(&mut self) -> &mut JumpTableCache {
        &mut self.jump_table_cache
    }

    /// Get switch tables that share jump tables
    pub fn get_switch_tables_with_shared_jump_tables(&self) -> Vec<(&SwitchTable, &SwitchTable)> {
        let mut shared_pairs = Vec::new();

        for function_tables in self.switch_tables_by_function.values() {
            for (i, table1) in function_tables.iter().enumerate() {
                for table2 in function_tables.iter().skip(i + 1) {
                    if table1.shares_jump_table(table2) {
                        shared_pairs.push((table1, table2));
                    }
                }
            }
        }

        shared_pairs
    }
}

impl Default for SwitchTableCollection {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_switch_case_creation() {
        let case = SwitchCase {
            value: 5,
            target_offset: 100,
            target_instruction_index: Some(10),
        };

        assert_eq!(case.value, 5);
        assert_eq!(case.target_offset, 100);
        assert_eq!(case.target_instruction_index, Some(10));
    }

    #[test]
    fn test_jump_table_data_creation() {
        let jump_table = JumpTableData::new(1000, 0, 5);

        assert_eq!(jump_table.address, 1000);
        assert_eq!(jump_table.min_value, 0);
        assert_eq!(jump_table.max_value, 5);
        assert_eq!(jump_table.reference_count, 0);
        assert!(jump_table.entries.is_empty());
    }

    #[test]
    fn test_jump_table_data_entries() {
        let mut jump_table = JumpTableData::new(1000, 0, 2);

        jump_table.add_entry(0, 10);
        jump_table.add_entry(1, 20);
        jump_table.add_entry(2, 30);

        assert_eq!(jump_table.get_entry(0), Some(&10));
        assert_eq!(jump_table.get_entry(1), Some(&20));
        assert_eq!(jump_table.get_entry(2), Some(&30));
        assert_eq!(jump_table.get_entry(3), None);

        assert!(jump_table.is_complete());

        let case_values = jump_table.get_case_values();
        assert_eq!(case_values, vec![0, 1, 2]);
    }

    #[test]
    fn test_jump_table_cache() {
        let mut cache = JumpTableCache::new();

        // Create a jump table
        {
            let jump_table = cache.get_or_create_jump_table(1000, 0, 5);
            jump_table.add_entry(0, 10);
            jump_table.add_entry(1, 20);
            jump_table.increment_reference_count();
        }

        // Get the same jump table again (should be cached)
        {
            let jump_table2 = cache.get_or_create_jump_table(1000, 0, 5);
            jump_table2.increment_reference_count();
        }

        assert_eq!(cache.len(), 1);

        // Check shared jump tables
        let shared = cache.get_shared_jump_tables();
        assert_eq!(shared.len(), 1);
        assert_eq!(shared[0].reference_count, 2);
    }

    #[test]
    fn test_switch_table_creation() {
        let switch_table = SwitchTable::new(0, 20, 144, 150, 1000, 1, 0);

        assert_eq!(switch_table.min_value, 0);
        assert_eq!(switch_table.max_value, 20);
        assert_eq!(switch_table.default_offset, 144);
        assert_eq!(switch_table.jump_table_offset, 150);
        assert_eq!(switch_table.jump_table_address, 1000);
        assert_eq!(switch_table.switch_instruction_index, 1);
        assert_eq!(switch_table.function_index, 0);
        assert_eq!(switch_table.case_count(), 0);
        assert_eq!(switch_table.expected_case_count(), 21);
        assert!(!switch_table.is_complete());
    }

    #[test]
    fn test_switch_table_add_cases() {
        let mut switch_table = SwitchTable::new(0, 2, 100, 150, 1000, 1, 0);

        switch_table.add_case(0, 10);
        switch_table.add_case(1, 20);
        switch_table.add_case(2, 30);

        assert_eq!(switch_table.case_count(), 3);
        assert!(switch_table.is_complete());

        assert_eq!(switch_table.get_case_by_value(0).unwrap().target_offset, 10);
        assert_eq!(switch_table.get_case_by_value(1).unwrap().target_offset, 20);
        assert_eq!(switch_table.get_case_by_value(2).unwrap().target_offset, 30);
        assert!(switch_table.get_case_by_value(3).is_none());
    }

    #[test]
    fn test_switch_table_value_range() {
        let mut switch_table = SwitchTable::new(5, 10, 100, 150, 1000, 1, 0);

        switch_table.add_case(5, 10);
        switch_table.add_case(7, 20);
        switch_table.add_case(10, 30);

        assert!(switch_table.is_value_in_range(5));
        assert!(switch_table.is_value_in_range(7));
        assert!(switch_table.is_value_in_range(10));
        assert!(!switch_table.is_value_in_range(4));
        assert!(!switch_table.is_value_in_range(11));

        assert!(switch_table.get_case_for_value(5).is_some());
        assert!(switch_table.get_case_for_value(7).is_some());
        assert!(switch_table.get_case_for_value(10).is_some());
        assert!(switch_table.get_case_for_value(4).is_none());
        assert!(switch_table.get_case_for_value(11).is_none());
    }

    #[test]
    fn test_switch_table_sharing() {
        let switch_table1 = SwitchTable::new(0, 5, 100, 150, 1000, 1, 0);
        let switch_table2 = SwitchTable::new(0, 5, 100, 150, 1000, 2, 0);
        let switch_table3 = SwitchTable::new(0, 5, 100, 150, 2000, 3, 0);

        assert!(switch_table1.shares_jump_table(&switch_table2));
        assert!(!switch_table1.shares_jump_table(&switch_table3));
    }

    #[test]
    fn test_switch_table_collection() {
        let mut collection = SwitchTableCollection::new();

        let mut switch_table1 = SwitchTable::new(0, 5, 100, 150, 1000, 1, 0);
        switch_table1.add_case(0, 10);
        switch_table1.add_case(1, 20);

        let mut switch_table2 = SwitchTable::new(10, 15, 200, 250, 2000, 5, 0);
        switch_table2.add_case(10, 30);
        switch_table2.add_case(15, 40);

        collection.add_switch_table(switch_table1);
        collection.add_switch_table(switch_table2);

        assert_eq!(collection.get_switch_table_count(0), 2);
        assert_eq!(collection.get_switch_table_count(1), 0);
        assert!(collection.has_switch_tables(0));
        assert!(!collection.has_switch_tables(1));

        let tables = collection.get_switch_tables_for_function(0).unwrap();
        assert_eq!(tables.len(), 2);
        assert_eq!(tables[0].case_count(), 2);
        assert_eq!(tables[1].case_count(), 2);

        let table = collection.get_switch_table_by_instruction(0, 1).unwrap();
        assert_eq!(table.min_value, 0);
        assert_eq!(table.max_value, 5);

        let table = collection.get_switch_table_by_instruction(0, 5).unwrap();
        assert_eq!(table.min_value, 10);
        assert_eq!(table.max_value, 15);

        assert!(collection.get_switch_table_by_instruction(0, 99).is_none());
    }

    #[test]
    fn test_switch_table_collection_with_shared_jump_tables() {
        let mut collection = SwitchTableCollection::new();

        // Create two switch tables that share the same jump table
        let mut switch_table1 = SwitchTable::new(0, 5, 100, 150, 1000, 1, 0);
        let mut switch_table2 = SwitchTable::new(0, 5, 100, 150, 1000, 2, 0);

        switch_table1.add_case(0, 10);
        switch_table1.add_case(1, 20);
        switch_table2.add_case(0, 10);
        switch_table2.add_case(1, 20);

        collection.add_switch_table(switch_table1);
        collection.add_switch_table(switch_table2);

        let shared_pairs = collection.get_switch_tables_with_shared_jump_tables();
        assert_eq!(shared_pairs.len(), 1);

        let (table1, table2) = shared_pairs[0];
        assert!(table1.shares_jump_table(table2));
    }
}
