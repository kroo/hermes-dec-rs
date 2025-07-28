pub mod bigint_table;
pub mod commonjs_table;
pub mod function_source_table;
pub mod function_table;
pub mod jump_table;
pub mod regexp_table;
pub mod serialized_literal_tables;
pub mod string_table;
pub mod switch_table;

pub use bigint_table::BigIntTable;
pub use commonjs_table::CommonJsTable;
pub use function_source_table::FunctionSourceTable;
pub use function_table::FunctionTable;
pub use jump_table::JumpTable;
pub use regexp_table::RegExpTable;
pub use serialized_literal_tables::SerializedLiteralTables;
pub use string_table::StringTable;
pub use switch_table::{SwitchCase, SwitchTable, SwitchTableCollection};
