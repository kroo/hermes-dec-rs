//! Instruction-to-statement conversion system
//!
//! This module provides the InstructionToStatementConverter that transforms
//! individual Hermes bytecode instructions into JavaScript statements using a 1:1 mapping.
//! This is the first phase of decompilation - direct translation without optimization.

use crate::analysis::ssa_usage_tracker::{DeclarationStrategy, VariableKind};
use crate::analysis::value_tracker::ConstantValue;
use crate::ast::{
    context::{ExpressionContext, ExpressionContextError},
    variables::RegisterManager,
};
use crate::cfg::ssa::DuplicatedSSAValue;
use crate::cfg::ssa::DuplicationContext;
use crate::cfg::switch_analysis::switch_info::CaseGroup;
use crate::hbc::InstructionIndex;
use crate::{analysis::GlobalAnalysisResult, generated::unified_instructions::UnifiedInstruction};
use oxc_ast::{
    ast::{Expression, Statement},
    AstBuilder as OxcAstBuilder,
};
use std::collections::HashSet;
use std::rc::Rc;
use std::sync::Arc;

mod arithmetic;
mod constants;
mod functions;
mod iterators;
mod jump;
mod misc;
mod objects;
mod variables;

/// Check if a property name is a standard global that doesn't need globalThis prefix
fn is_standard_global(property: &str) -> bool {
    matches!(
        property,
        "console"
            | "Math"
            | "Object"
            | "Array"
            | "String"
            | "Number"
            | "Boolean"
            | "Date"
            | "RegExp"
            | "Error"
            | "JSON"
            | "Promise"
            | "Map"
            | "Set"
            | "WeakMap"
            | "WeakSet"
            | "Symbol"
            | "Proxy"
            | "Reflect"
            | "BigInt"
            | "Int8Array"
            | "Uint8Array"
            | "Uint8ClampedArray"
            | "Int16Array"
            | "Uint16Array"
            | "Int32Array"
            | "Uint32Array"
            | "Float32Array"
            | "Float64Array"
            | "BigInt64Array"
            | "BigUint64Array"
            | "ArrayBuffer"
            | "SharedArrayBuffer"
            | "DataView"
            | "Atomics"
            | "encodeURI"
            | "encodeURIComponent"
            | "decodeURI"
            | "decodeURIComponent"
            | "eval"
            | "isFinite"
            | "isNaN"
            | "parseFloat"
            | "parseInt"
            | "URL"
            | "URLSearchParams"
            | "TextEncoder"
            | "TextDecoder"
            | "WebAssembly"
            | "Intl"
            | "Buffer"
            | "process"
            | "global"
            | "window"
            | "document"
            | "setTimeout"
            | "clearTimeout"
            | "setInterval"
            | "clearInterval"
            | "setImmediate"
            | "clearImmediate"
            | "requestAnimationFrame"
            | "cancelAnimationFrame"
            | "fetch"
            | "crypto"
            | "performance"
    )
}

use arithmetic::ArithmeticHelpers;
use constants::ConstantHelpers;
use functions::FunctionHelpers;
use iterators::IteratorHelpers;
use jump::JumpHelpers;
use misc::MiscHelpers;
use objects::ObjectHelpers;
use variables::VariableHelpers;

/// Error types for instruction-to-statement conversion
#[derive(Debug, thiserror::Error)]
pub enum StatementConversionError {
    #[error("Unsupported instruction: {0}")]
    UnsupportedInstruction(String),
    #[error("Invalid operand for instruction {instruction}: {message}")]
    InvalidOperand {
        instruction: String,
        message: String,
    },
    #[error("Register {0} not found")]
    RegisterNotFound(u8),
    #[error("No current PC available")]
    NoCurrentPC,
    #[error("No current block available")]
    NoCurrentBlock,
    #[error("Expression context error: {0}")]
    ContextError(#[from] ExpressionContextError),
    #[error("Operand extraction failed: {0}")]
    OperandExtraction(String),
    #[error("BigInt lookup failed: {0}")]
    BigIntLookupFailed(String),
}

/// Information about jump conditions for cross-block analysis
#[derive(Debug)]
pub struct JumpCondition<'a> {
    pub condition_expression: Option<oxc_ast::ast::Expression<'a>>,
    pub jump_type: JumpType,
    pub target_offset: Option<i32>,
}

#[derive(Debug, Clone)]
pub enum JumpType {
    Conditional,
    Unconditional,
    True,
    False,
    Undefined,
}

/// Result of instruction conversion - either a statement or jump condition info
#[derive(Debug)]
pub enum InstructionResult<'a> {
    /// Regular statement to be added to the block
    Statement(Statement<'a>),
    /// Jump condition information for block converter to use
    JumpCondition(JumpCondition<'a>),
    /// Instruction that doesn't produce output (like nops)
    None,
}

/// Converts individual instructions to JavaScript statements
///
/// This converter maintains a 1:1 mapping between instructions and statements,
/// handling register assignments, arithmetic operations, function calls, etc.
/// Cross-block control flow is handled by BlockToStatementConverter.
pub struct InstructionToStatementConverter<'a> {
    /// OXC AST builder for creating nodes
    ast_builder: &'a OxcAstBuilder<'a>,
    /// Expression context for string/constant lookups
    expression_context: ExpressionContext<'a>,
    /// Register manager for variable naming and lifetime tracking
    register_manager: RegisterManager,
    /// HBC analysis containing global analysis and other data
    hbc_analysis: &'a crate::analysis::HbcAnalysis<'a>,
    /// Whether to decompile nested function bodies
    decompile_nested: bool,
    /// Track variables that were used but not declared
    undeclared_variables: HashSet<String>,
    /// Current duplication context for switch case processing
    current_duplication_context: Option<DuplicationContext>,
    /// The control flow plan (shared ownership)
    pub control_flow_plan: Rc<crate::analysis::control_flow_plan::ControlFlowPlan>,
    /// Inline configuration settings
    pub inline_config: crate::decompiler::InlineConfig,
    /// Variable mapper for consistent naming
    variable_mapper: crate::ast::variables::VariableMapper,
}

impl<'a> InstructionToStatementConverter<'a> {
    /// Create a new instruction-to-statement converter with analysis
    pub fn new(
        ast_builder: &'a OxcAstBuilder<'a>,
        expression_context: ExpressionContext<'a>,
        hbc_analysis: &'a crate::analysis::HbcAnalysis<'a>,
        control_flow_plan: crate::analysis::control_flow_plan::ControlFlowPlan,
    ) -> Self {
        let control_flow_plan_rc = Rc::new(control_flow_plan);
        Self {
            ast_builder,
            expression_context,
            register_manager: RegisterManager::new(control_flow_plan_rc.clone()),
            hbc_analysis,
            decompile_nested: false,
            undeclared_variables: HashSet::new(),
            current_duplication_context: None,
            control_flow_plan: control_flow_plan_rc,
            inline_config: crate::decompiler::InlineConfig::default(),
            variable_mapper: crate::ast::variables::VariableMapper::new(),
        }
    }

    /// Set the inline configuration
    pub fn set_inline_config(&mut self, config: crate::decompiler::InlineConfig) {
        self.inline_config = config;
    }

    /// Set the variable mapper
    pub fn set_variable_mapper(&mut self, mapper: crate::ast::variables::VariableMapper) {
        self.variable_mapper = mapper;
    }

    /// Set the current program counter for context-aware operations
    pub fn set_current_pc(&mut self, pc: u32) {
        let idx = InstructionIndex(pc as usize);
        self.expression_context.set_current_pc(idx);
        self.register_manager.set_current_pc(idx);
    }

    /// Set whether to decompile nested function bodies
    pub fn set_decompile_nested(&mut self, decompile_nested: bool) {
        self.decompile_nested = decompile_nested;
    }

    /// Set the current duplication context
    pub fn set_duplication_context(&mut self, context: Option<DuplicationContext>) {
        self.current_duplication_context = context.clone();
        // Also set it in the register manager so it can use duplicated names
        self.register_manager.set_duplication_context(context);
    }

    /// Set the current duplication context for switch case processing
    pub fn set_duplication_context_for_case_group(&mut self, case_group: &CaseGroup) {
        self.set_duplication_context(Some(DuplicationContext::SwitchBlockDuplication {
            case_group_keys: case_group.keys.clone(),
        }));
    }

    /// Clear the current duplication context
    pub fn clear_duplication_context(&mut self) {
        self.current_duplication_context = None;
        // Also clear it in the register manager
        self.register_manager.set_duplication_context(None);
    }

    /// Get the set of undeclared variables
    pub fn get_undeclared_variables(&self) -> &HashSet<String> {
        &self.undeclared_variables
    }

    /// Get mutable reference to register manager
    pub fn register_manager_mut(&mut self) -> &mut RegisterManager {
        &mut self.register_manager
    }

    /// Get reference to register manager
    pub fn register_manager(&self) -> &RegisterManager {
        &self.register_manager
    }

    /// Check if the current instruction's result should be executed for side effects only
    fn should_be_side_effect_only(&self, dest_reg: u8) -> bool {
        // Get the SSA value for this register at the current PC
        if let Some(ssa_value) = self.register_manager.get_current_ssa_value(dest_reg) {
            let dup_ssa = DuplicatedSSAValue {
                original: ssa_value,
                duplication_context: self.current_duplication_context.clone(),
            };

            // Check the declaration strategy
            if let Some(strategy) = self.control_flow_plan.get_declaration_strategy(&dup_ssa) {
                return matches!(strategy, DeclarationStrategy::SideEffectOnly);
            }
        }
        false
    }

    /// Get reference to AST builder
    pub fn ast_builder(&self) -> &'a OxcAstBuilder<'a> {
        self.ast_builder
    }

    /// Get reference to expression context
    pub fn get_expression_context(&self) -> &ExpressionContext<'a> {
        &self.expression_context
    }

    /// Get mutable reference to expression context
    pub fn expression_context_mut(&mut self) -> &mut ExpressionContext<'a> {
        &mut self.expression_context
    }

    /// Check if a declaration should be skipped for the given destination register
    pub fn should_skip_declaration(&self, dest_reg: u8) -> bool {
        if let Some(ssa_value) = self.register_manager.get_current_ssa_value(dest_reg) {
            let dup_ssa = if let Some(context) = self.register_manager.current_duplication_context()
            {
                DuplicatedSSAValue {
                    original: ssa_value,
                    duplication_context: Some(context.clone()),
                }
            } else {
                DuplicatedSSAValue::original(ssa_value)
            };

            // Check if this declaration should be skipped
            if let Some(DeclarationStrategy::Skip) =
                self.control_flow_plan.get_declaration_strategy(&dup_ssa)
            {
                return true;
            }
        }
        false
    }

    /// Get the declaration strategy for the given destination register
    pub fn get_declaration_strategy_for_register(
        &self,
        dest_reg: u8,
    ) -> Option<DeclarationStrategy> {
        if let Some(ssa_value) = self.register_manager.get_current_ssa_value(dest_reg) {
            let dup_ssa = if let Some(context) = self.register_manager.current_duplication_context()
            {
                DuplicatedSSAValue {
                    original: ssa_value,
                    duplication_context: Some(context.clone()),
                }
            } else {
                DuplicatedSSAValue::original(ssa_value)
            };

            self.control_flow_plan
                .get_declaration_strategy(&dup_ssa)
                .cloned()
        } else {
            None
        }
    }

    /// Get the global analyzer from HBC analysis
    pub fn global_analyzer(&self) -> &Arc<GlobalAnalysisResult> {
        &self.hbc_analysis.global_analysis
    }

    /// Get the variable mapping from the register manager
    pub fn get_variable_mapping(&self) -> Option<&crate::ast::variables::VariableMapping> {
        self.register_manager.variable_mapping()
    }

    /// Convert a register operand to an expression, respecting use strategies
    /// This handles both variable references and inline constant values based on SSA analysis
    pub fn register_to_expression(
        &mut self,
        register: u8,
    ) -> Result<oxc_ast::ast::Expression<'a>, StatementConversionError> {
        // Try to get SSA value for the register
        if let Some(ssa_value) = self.register_manager.get_current_ssa_for_register(register) {
            // Use the common implementation
            self.ssa_value_to_expression_internal(&ssa_value, None)
        } else {
            // No SSA info, just use variable name
            let span = oxc_span::Span::default();
            let var_name = self.register_manager.get_variable_name(register);
            let var_atom = self.ast_builder.allocator.alloc_str(&var_name);
            Ok(self.ast_builder.expression_identifier(span, var_atom))
        }
    }

    /// Convert a source register operand to an expression (for reading values)
    /// This is a convenience wrapper that handles SSA tracking for source operands
    pub fn source_register_to_expression(
        &mut self,
        register: u8,
    ) -> Result<oxc_ast::ast::Expression<'a>, StatementConversionError> {
        // Get the source variable name to ensure proper SSA tracking
        let _ = self.register_manager.get_source_variable_name(register);
        self.register_to_expression(register)
    }

    /// Convert a unified instruction to a JavaScript statement or jump condition
    pub fn convert_instruction(
        &mut self,
        instruction: &UnifiedInstruction,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        use UnifiedInstruction::*;

        match instruction {
            // Jump instructions provide condition info but don't generate statements here
            JGreater {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                // JGreater compares operand_1 > operand_2, jumps to operand_0 if true
                self.build_comparison_jump(*operand_1, *operand_2, "Greater", *operand_0 as i32)
            }
            JmpTrue {
                operand_0,
                operand_1,
                ..
            } => self.build_truthiness_jump(*operand_1, JumpType::True, *operand_0 as i32),
            JmpFalse {
                operand_0,
                operand_1,
                ..
            } => self.build_truthiness_jump(*operand_1, JumpType::False, *operand_0 as i32),
            Jmp { operand_0, .. } => self.build_unconditional_jump(*operand_0 as i32),

            // Variable operations
            Mov {
                operand_0,
                operand_1,
                ..
            } => self.create_register_assignment(*operand_0, *operand_1),

            MovLong {
                operand_0,
                operand_1,
                ..
            } => self.create_register_assignment(*operand_0 as u8, *operand_1 as u8),
            LoadParam {
                operand_0,
                operand_1,
                ..
            } => self.create_parameter_load(*operand_0, *operand_1),

            // Arithmetic operations
            Add {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "Add"),
            Sub {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "Sub"),

            SubN {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "SubN"),

            Sub32 {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "Sub32"),
            Mul {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "Mul"),
            Div {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "Div"),

            MulN {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "MulN"),

            Mul32 {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "Mul32"),

            // Constant loading - generate assignment statements
            LoadConstInt {
                operand_0,
                operand_1,
                ..
            } => self.create_constant_assignment(*operand_0, *operand_1 as f64),

            LoadConstTrue { operand_0, .. } => self.create_boolean_assignment(*operand_0, true),

            LoadConstFalse { operand_0, .. } => self.create_boolean_assignment(*operand_0, false),

            LoadConstNull { operand_0, .. } => self.create_null_assignment(*operand_0),

            // Property access - generate expression statements
            PutById {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                ..
            } => self.create_property_assignment_by_id(
                *operand_0,
                *operand_1,
                *operand_2,
                *operand_3 as u32,
            ),

            PutByVal {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_property_assignment_by_value(*operand_0, *operand_1, *operand_2),

            // Control flow that generates statements
            Ret { operand_0, .. } => self.create_return_statement(*operand_0),

            Throw { operand_0, .. } => self.create_throw_statement(*operand_0),

            // === THROW OPERATIONS ===
            ThrowIfUndefinedInst { operand_0, .. } => self.create_throw_if_undefined(*operand_0),

            ThrowIfEmpty {
                operand_0,
                operand_1,
                ..
            } => self.create_throw_if_empty(*operand_0, *operand_1),

            ThrowIfHasRestrictedGlobalProperty { operand_0, .. } => {
                self.create_throw_if_has_restricted_global_property(*operand_0 as u8)
            }

            // For-in loop enumeration
            GetPNameList {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_get_pname_list_statement(*operand_0, *operand_1, *operand_2),

            // Unary expressions - generate assignment statements
            Not {
                operand_0,
                operand_1,
                ..
            } => self.create_unary_operation(*operand_0, *operand_1, "Not"),

            BitNot {
                operand_0,
                operand_1,
                ..
            } => self.create_unary_operation(*operand_0, *operand_1, "BitNot"),

            Negate {
                operand_0,
                operand_1,
                ..
            } => self.create_unary_operation(*operand_0, *operand_1, "Negate"),

            TypeOf {
                operand_0,
                operand_1,
                ..
            } => self.create_unary_operation(*operand_0, *operand_1, "TypeOf"),

            // Update expressions - generate assignment statements
            Inc {
                operand_0,
                operand_1,
                ..
            } => self.create_update_operation(*operand_0, *operand_1, "Inc"),

            Dec {
                operand_0,
                operand_1,
                ..
            } => self.create_update_operation(*operand_0, *operand_1, "Dec"),

            // String operations
            LoadConstString {
                operand_0,
                operand_1,
                ..
            } => self.create_string_assignment(*operand_0, *operand_1 as u32),

            LoadConstBigInt {
                operand_0,
                operand_1,
                ..
            } => self.create_bigint_assignment(*operand_0, *operand_1 as u32),

            // Type conversion operations
            ToInt32 {
                operand_0,
                operand_1,
                ..
            } => self.create_type_conversion(*operand_0, *operand_1, "ToInt32"),

            ToNumber {
                operand_0,
                operand_1,
                ..
            } => self.create_type_conversion(*operand_0, *operand_1, "ToNumber"),

            ToNumeric {
                operand_0,
                operand_1,
                ..
            } => self.create_type_conversion(*operand_0, *operand_1, "ToNumeric"),

            // Object property operations
            PutNewOwnById {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                self.create_property_assignment_by_id(*operand_0, *operand_1, 0, *operand_2 as u32)
            }

            PutOwnByVal {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_property_assignment_by_value(*operand_0, *operand_1, *operand_2),

            GetById {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                ..
            } => self.create_property_access_by_id(
                *operand_0,
                *operand_1,
                *operand_2,
                *operand_3 as u32,
            ),

            GetByVal {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_property_access_by_value(*operand_0, *operand_1, *operand_2),

            // This and target operations
            CreateThis {
                operand_0,
                operand_1,
                ..
            } => self.create_this_statement(*operand_0, *operand_1),

            GetNewTarget { operand_0, .. } => self.create_new_target_statement(*operand_0),

            LoadThisNS { operand_0, .. } => self.create_load_this_statement(*operand_0),

            // Iterator operations for for-of loops
            IteratorBegin {
                operand_0,
                operand_1,
                ..
            } => self.create_iterator_begin(*operand_0, *operand_1),

            IteratorNext {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_iterator_next(*operand_0, *operand_1, *operand_2),

            IteratorClose {
                operand_0,
                operand_1,
                ..
            } => self.create_iterator_close(*operand_0, *operand_1),

            // Specialized operations
            AddEmptyString {
                operand_0,
                operand_1,
                ..
            } => self.create_add_empty_string(*operand_0, *operand_1),

            CoerceThisNS {
                operand_0,
                operand_1,
                ..
            } => self.create_coerce_this(*operand_0, *operand_1),

            SelectObject {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_select_object(*operand_0, *operand_1, *operand_2),

            // Debug operations
            ProfilePoint { operand_0, .. } => self.create_profile_point(*operand_0),

            // === CONTROL FLOW OPERATIONS ===
            SwitchImm {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                operand_4,
                ..
            } => self.create_switch_imm(
                *operand_0,
                *operand_1,
                *operand_2 as u16,
                *operand_3,
                *operand_4 as u32,
            ),

            Unreachable { .. } => self.create_unreachable(),

            // Generator operations
            CreateGenerator {
                operand_0,
                operand_1,
                ..
            } => self.create_generator(*operand_0, *operand_1),

            SaveGenerator { operand_0, .. } => self.create_save_generator(*operand_0 as u32),

            SaveGeneratorLong { operand_0 } => self.create_save_generator(*operand_0 as u32),

            StartGenerator { .. } => self.create_start_generator(),

            ResumeGenerator {
                operand_0,
                operand_1,
                ..
            } => self.create_resume_generator(*operand_0, *operand_1),

            // Function operations
            Call {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_function_call(*operand_0, *operand_1, *operand_2 as u8),

            CallDirect {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_function_call(*operand_0, *operand_1, *operand_2 as u8),

            NewObject { operand_0, .. } => self.create_new_object(*operand_0),

            NewArray {
                operand_0,
                operand_1,
                ..
            } => self.create_new_array(*operand_0, *operand_1 as u8),

            // Heap storage operations
            Loadi16 {
                operand_0,
                operand_1,
                ..
            } => self.create_heap_load(*operand_0, *operand_1 as u32, "Int16"),

            Store32 {
                operand_0,
                operand_1,
                ..
            } => self.create_heap_store(*operand_0, *operand_1 as u32, "Int32"),

            // Comparison operations
            Less {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_comparison(*operand_0, *operand_1, *operand_2, "Less"),

            Greater {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_comparison(*operand_0, *operand_1, *operand_2, "Greater"),

            Eq {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_comparison(*operand_0, *operand_1, *operand_2, "Eq"),

            StrictEq {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_comparison(*operand_0, *operand_1, *operand_2, "StrictEq"),

            // Bitwise operations
            BitOr {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "BitOr"),

            BitAnd {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "BitAnd"),

            BitXor {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "BitXor"),

            LShift {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "LShift"),

            RShift {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "RShift"),

            // Remaining constant loading operations
            LoadConstUInt8 {
                operand_0,
                operand_1,
                ..
            } => self.create_constant_assignment(*operand_0, *operand_1 as f64),

            LoadConstZero { operand_0, .. } => self.create_constant_assignment(*operand_0, 0.0),

            LoadConstUndefined { operand_0, .. } => self.create_undefined_assignment(*operand_0),

            // === ADDITIONAL COMPARISON OPERATIONS ===
            LessEq {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_comparison(*operand_0, *operand_1, *operand_2, "LessEq"),

            GreaterEq {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_comparison(*operand_0, *operand_1, *operand_2, "GreaterEq"),

            Neq {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_comparison(*operand_0, *operand_1, *operand_2, "Neq"),

            StrictNeq {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_comparison(*operand_0, *operand_1, *operand_2, "StrictNeq"),

            // === ADDITIONAL ARITHMETIC OPERATIONS ===
            Mod {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "Mod"),

            Add32 {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "Add32"),

            AddN {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "AddN"),

            DivN {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "DivN"),

            Divi32 {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "Divi32"),

            Divu32 {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "Divu32"),

            URshift {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_binary_operation(*operand_0, *operand_1, *operand_2, "URShift"),

            // === ADDITIONAL CONSTANT LOADING ===
            LoadConstDouble {
                operand_0,
                operand_1,
                ..
            } => self.create_constant_assignment(*operand_0, *operand_1 as f64),

            LoadConstEmpty { operand_0, .. } => self.create_empty_assignment(*operand_0),

            LoadConstStringLongIndex {
                operand_0,
                operand_1,
                ..
            } => self.create_string_assignment(*operand_0, *operand_1),

            LoadConstBigIntLongIndex {
                operand_0,
                operand_1,
                ..
            } => self.create_bigint_assignment(*operand_0, *operand_1),

            // === FUNCTION OPERATIONS ===
            Call1 {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_function_call_1(*operand_0, *operand_1, *operand_2),

            Call2 {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                ..
            } => self.create_function_call_2(*operand_0, *operand_1, *operand_2, *operand_3),

            Call3 {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                operand_4,
                ..
            } => self
                .create_function_call_3(*operand_0, *operand_1, *operand_2, *operand_3, *operand_4),

            Call4 {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                operand_4,
                operand_5,
                ..
            } => self.create_function_call_4(
                *operand_0, *operand_1, *operand_2, *operand_3, *operand_4, *operand_5,
            ),

            CallLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_function_call(*operand_0, *operand_1, *operand_2 as u8),

            CallBuiltin {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_builtin_call(*operand_0, *operand_1, *operand_2),

            CallBuiltinLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_builtin_call(*operand_0, *operand_1 as u8, *operand_2 as u8),

            CallDirectLongIndex {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_function_call(*operand_0, *operand_1, *operand_2 as u8),

            Construct {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_constructor_call(*operand_0, *operand_1, *operand_2),

            ConstructLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_constructor_call(*operand_0, *operand_1, *operand_2 as u8),

            DirectEval {
                operand_0,
                operand_1,
                ..
            } => self.create_direct_eval(*operand_0, *operand_1),

            // === CLOSURE AND ENVIRONMENT OPERATIONS ===
            CreateClosure {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_closure(*operand_0, *operand_1, *operand_2),

            CreateClosureLongIndex {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_closure_long_index(*operand_0, *operand_1, *operand_2),

            CreateAsyncClosure {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_async_closure(*operand_0, *operand_1, *operand_2),

            CreateAsyncClosureLongIndex {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_async_closure_long_index(*operand_0, *operand_1, *operand_2),

            CreateGeneratorClosure {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_generator_closure(*operand_0, *operand_1, *operand_2),

            CreateGeneratorClosureLongIndex {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_generator_closure_long_index(*operand_0, *operand_1, *operand_2),

            CreateGeneratorLongIndex {
                operand_0,
                operand_1,
                ..
            } => self.create_generator(*operand_0, *operand_1),

            CreateEnvironment { operand_0, .. } => self.create_environment(*operand_0),

            CreateInnerEnvironment {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_inner_environment(*operand_0, *operand_1, *operand_2 as u32),

            GetEnvironment {
                operand_0,
                operand_1,
                ..
            } => self.create_get_environment(*operand_0, *operand_1),

            LoadFromEnvironment {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_load_from_environment(*operand_0, *operand_1, *operand_2),

            LoadFromEnvironmentL {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_load_from_environment_long(*operand_0, *operand_1, *operand_2 as u32),

            StoreToEnvironment {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_store_to_environment(*operand_0, *operand_1, *operand_2),

            StoreToEnvironmentL {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_store_to_environment_long(*operand_0, *operand_1 as u32, *operand_2),

            StoreNPToEnvironment {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_store_np_to_environment(*operand_0, *operand_1, *operand_2),

            StoreNPToEnvironmentL {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                self.create_store_np_to_environment_long(*operand_0, *operand_1 as u32, *operand_2)
            }

            // === PROPERTY OPERATIONS ===
            GetByIdShort {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                ..
            } => self.create_property_access_by_id(
                *operand_0,
                *operand_1,
                *operand_2,
                *operand_3 as u32,
            ),

            GetByIdLong {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                ..
            } => self.create_property_access_by_id(*operand_0, *operand_1, *operand_2, *operand_3),

            PutByIdLong {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                ..
            } => self
                .create_property_assignment_by_id(*operand_0, *operand_1, *operand_2, *operand_3),

            TryGetById {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                ..
            } => self.create_property_access_by_id(
                *operand_0,
                *operand_1,
                *operand_2,
                *operand_3 as u32,
            ),

            DelById {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_property_deletion_by_id(*operand_0, *operand_1, *operand_2 as u32),

            DelByIdLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_property_deletion_by_id(*operand_0, *operand_1, *operand_2),

            DelByVal {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_property_deletion_by_value(*operand_0, *operand_1, *operand_2),

            TryGetByIdLong {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                ..
            } => self.create_try_property_access_by_id_long(
                *operand_0,
                *operand_1,
                *operand_2 as u32,
                *operand_3 as u8,
            ),

            TryPutById {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                ..
            } => self.create_try_property_assignment_by_id(
                *operand_0,
                *operand_1,
                *operand_2,
                *operand_3 as u8,
            ),

            TryPutByIdLong {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                ..
            } => self.create_try_property_assignment_by_id_long(
                *operand_0,
                *operand_1,
                *operand_2 as u32,
                *operand_3 as u8,
            ),

            PutNewOwnByIdLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_put_new_own_by_id_long(*operand_0, *operand_1, *operand_2 as u32),

            PutNewOwnByIdShort {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_put_new_own_by_id_short(*operand_0, *operand_1, *operand_2 as u16),

            PutNewOwnNEById {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_put_new_own_ne_by_id(*operand_0, *operand_1, *operand_2 as u8),

            PutNewOwnNEByIdLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_put_new_own_ne_by_id_long(*operand_0, *operand_1, *operand_2 as u32),

            PutOwnByIndex {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_put_own_by_index(*operand_0, *operand_1, *operand_2),

            PutOwnByIndexL {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_put_own_by_index_long(*operand_0, *operand_1, *operand_2 as u32),

            PutOwnGetterSetterByVal {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                operand_4,
                ..
            } => self.create_put_own_getter_setter_by_val(
                *operand_0, *operand_1, *operand_2, *operand_3, *operand_4,
            ),

            // === PARAMETER OPERATIONS ===
            LoadParamLong {
                operand_0,
                operand_1,
                ..
            } => self.create_parameter_load(*operand_0, *operand_1 as u8),

            GetArgumentsLength { operand_0, .. } => self.create_get_arguments_length(*operand_0),

            GetArgumentsPropByVal {
                operand_0,
                operand_1,
                ..
            } => self.create_get_arguments_prop_by_val(*operand_0, *operand_1),

            ReifyArguments { operand_0, .. } => self.create_reify_arguments(*operand_0),

            // === OBJECT CREATION ===
            NewArrayWithBuffer {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                ..
            } => self.create_new_array_with_buffer(*operand_0, *operand_1, *operand_2, *operand_3),

            NewArrayWithBufferLong {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                ..
            } => self
                .create_new_array_with_buffer_long(*operand_0, *operand_1, *operand_2, *operand_3),

            NewObjectWithBuffer {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                operand_4,
                ..
            } => self.create_new_object_with_buffer(
                *operand_0, *operand_1, *operand_2, *operand_3, *operand_4,
            ),

            NewObjectWithBufferLong {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                operand_4,
                ..
            } => self.create_new_object_with_buffer_long(
                *operand_0, *operand_1, *operand_2, *operand_3, *operand_4,
            ),

            NewObjectWithParent {
                operand_0,
                operand_1,
                ..
            } => self.create_new_object_with_parent(*operand_0, *operand_1),

            // === REGEX OPERATIONS ===
            CreateRegExp {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_regexp(*operand_0, *operand_1 as u32, *operand_2 as u32),

            // === ADDITIONAL HEAP OPERATIONS ===
            Loadi8 {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_loadi8(*operand_0, *operand_1, *operand_2),

            Loadi32 {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_loadi32(*operand_0, *operand_1, *operand_2),

            Loadu8 {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_loadu8(*operand_0, *operand_1, *operand_2),

            Loadu16 {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_loadu16(*operand_0, *operand_1, *operand_2),

            Loadu32 {
                operand_0,
                operand_1,
                ..
            } => self.create_heap_load(*operand_0, *operand_1 as u32, "UInt32"),

            Store8 {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_store8(*operand_0, *operand_1, *operand_2),

            Store16 {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_store16(*operand_0, *operand_1, *operand_2),

            // Storef32 and Storef64 don't exist in the generated instruction set

            // === RELATIONAL OPERATIONS ===
            InstanceOf {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_instanceof(*operand_0, *operand_1, *operand_2),

            IsIn {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.create_is_in(*operand_0, *operand_1, *operand_2),

            // === ADDITIONAL ITERATOR OPERATIONS ===
            GetNextPName {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                operand_4,
                ..
            } => self
                .create_get_next_pname(*operand_0, *operand_1, *operand_2, *operand_3, *operand_4),

            // === GLOBAL OPERATIONS ===
            GetGlobalObject { operand_0, .. } => self.create_get_global_object(*operand_0),

            DeclareGlobalVar { operand_0, .. } => self.create_declare_global_var(*operand_0),

            GetBuiltinClosure {
                operand_0,
                operand_1,
                ..
            } => self.create_get_builtin_closure(*operand_0, *operand_1),

            // === CATCH OPERATIONS ===
            UnifiedInstruction::Catch { operand_0, .. } => self.create_catch(*operand_0),

            // === GENERATOR OPERATIONS (additional) ===
            UnifiedInstruction::CompleteGenerator { .. } => self.create_complete_generator(),

            // === ALL JUMP INSTRUCTIONS (provide jump condition info) ===
            JEqual {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "Equal", *operand_0 as i32),

            JEqualLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "Equal", *operand_0 as i32),

            JGreaterEqual {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "GreaterEqual",
                *operand_0 as i32,
            ),

            JGreaterEqualLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "GreaterEqual",
                *operand_0 as i32,
            ),

            JGreaterEqualN {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "GreaterEqual",
                *operand_0 as i32,
            ),

            JGreaterEqualNLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "GreaterEqual",
                *operand_0 as i32,
            ),

            JGreaterLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "Greater", *operand_0 as i32),

            JGreaterN {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "Greater", *operand_0 as i32),

            JGreaterNLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "Greater", *operand_0 as i32),

            JLess {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "Less", *operand_0 as i32),

            JLessEqual {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "LessEqual", *operand_0 as i32),

            JLessEqualLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "LessEqual", *operand_0 as i32),

            JLessEqualN {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "LessEqual", *operand_0 as i32),

            JLessEqualNLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "LessEqual", *operand_0 as i32),

            JLessLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "Less", *operand_0 as i32),

            JLessN {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "Less", *operand_0 as i32),

            JLessNLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "Less", *operand_0 as i32),

            JNotEqual {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "NotEqual", *operand_0 as i32),

            JNotEqualLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "NotEqual", *operand_0 as i32),

            JNotGreater {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                self.build_comparison_jump(*operand_1, *operand_2, "NotGreater", *operand_0 as i32)
            }

            JNotGreaterEqual {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "NotGreaterEqual",
                *operand_0 as i32,
            ),

            JNotGreaterEqualLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "NotGreaterEqual",
                *operand_0 as i32,
            ),

            JNotGreaterEqualN {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "NotGreaterEqual",
                *operand_0 as i32,
            ),

            JNotGreaterEqualNLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "NotGreaterEqual",
                *operand_0 as i32,
            ),

            JNotGreaterLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                self.build_comparison_jump(*operand_1, *operand_2, "NotGreater", *operand_0 as i32)
            }

            JNotGreaterN {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                self.build_comparison_jump(*operand_1, *operand_2, "NotGreater", *operand_0 as i32)
            }

            JNotGreaterNLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                self.build_comparison_jump(*operand_1, *operand_2, "NotGreater", *operand_0 as i32)
            }

            JNotLess {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "NotLess", *operand_0 as i32),

            JNotLessEqual {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "NotLessEqual",
                *operand_0 as i32,
            ),

            JNotLessEqualLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "NotLessEqual",
                *operand_0 as i32,
            ),

            JNotLessEqualN {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "NotLessEqual",
                *operand_0 as i32,
            ),

            JNotLessEqualNLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "NotLessEqual",
                *operand_0 as i32,
            ),

            JNotLessLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "NotLess", *operand_0 as i32),

            JNotLessN {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "NotLess", *operand_0 as i32),

            JNotLessNLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(*operand_1, *operand_2, "NotLess", *operand_0 as i32),

            JStrictEqual {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                self.build_comparison_jump(*operand_1, *operand_2, "StrictEqual", *operand_0 as i32)
            }

            JStrictEqualLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                self.build_comparison_jump(*operand_1, *operand_2, "StrictEqual", *operand_0 as i32)
            }

            JStrictNotEqual {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "StrictNotEqual",
                *operand_0 as i32,
            ),

            JStrictNotEqualLong {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => self.build_comparison_jump(
                *operand_1,
                *operand_2,
                "StrictNotEqual",
                *operand_0 as i32,
            ),

            JmpLong { operand_0, .. } => self.build_unconditional_jump(*operand_0 as i32),

            JmpTrueLong {
                operand_0,
                operand_1,
                ..
            } => self.build_truthiness_jump(*operand_1, JumpType::True, *operand_0 as i32),

            JmpFalseLong {
                operand_0,
                operand_1,
                ..
            } => self.build_truthiness_jump(*operand_1, JumpType::False, *operand_0 as i32),

            JmpUndefined {
                operand_0,
                operand_1,
                ..
            } => self.build_undefined_jump(*operand_1, *operand_0 as i32),

            JmpUndefinedLong {
                operand_0,
                operand_1,
                ..
            } => self.build_undefined_jump(*operand_1, *operand_0 as i32),

            // === DEBUG OPERATIONS (unit variants) ===
            UnifiedInstruction::Debugger { .. } => self.create_debugger_statement(),

            UnifiedInstruction::AsyncBreakCheck { .. } => self.create_async_break_check(),

            DebuggerCheckBreak { .. } => self.create_debugger_check_break(),
        }
    }

    // ===== Helper Methods =====

    /// Create a variable declaration statement: `let variable_name = init_expression;`
    pub fn create_variable_declaration(
        &self,
        variable_name: &str,
        init_expression: Option<oxc_ast::ast::Expression<'a>>,
        kind: oxc_ast::ast::VariableDeclarationKind,
    ) -> Result<Statement<'a>, StatementConversionError> {
        use oxc_ast::ast::{
            BindingIdentifier, BindingPattern, BindingPatternKind, VariableDeclaration,
            VariableDeclarator,
        };
        use oxc_span::{Atom, Span};
        use std::cell::Cell;

        let span = Span::default();

        // Create the binding identifier
        let name_atom = self.ast_builder.allocator.alloc_str(variable_name);
        let binding_identifier = BindingIdentifier {
            span,
            name: Atom::from(name_atom),
            symbol_id: Cell::new(None),
        };

        // Create the binding pattern
        let binding_pattern = BindingPattern {
            kind: BindingPatternKind::BindingIdentifier(self.ast_builder.alloc(binding_identifier)),
            type_annotation: None,
            optional: false,
        };

        // Create the variable declarator
        let declarator = VariableDeclarator {
            span,
            kind: kind.clone(),
            id: binding_pattern,
            init: init_expression,
            definite: false,
        };

        // Create the declarations vector
        let mut declarations = self.ast_builder.vec();
        declarations.push(declarator);

        // Create the variable declaration
        let var_decl = VariableDeclaration {
            span,
            kind,
            declarations,
            declare: false,
        };

        Ok(Statement::VariableDeclaration(
            self.ast_builder.alloc(var_decl),
        ))
    }

    /// Create a statement for a register assignment based on its declaration strategy
    pub fn create_register_assignment_statement(
        &mut self,
        dest_reg: u8,
        init_expression: oxc_ast::ast::Expression<'a>,
    ) -> Result<Statement<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        // Check the declaration strategy for this register
        match self.get_declaration_strategy_for_register(dest_reg) {
            Some(crate::analysis::ssa_usage_tracker::DeclarationStrategy::AssignOnly) => {
                // Create assignment only (no declaration)
                let span = oxc_span::Span::default();
                let var_atom = self.ast_builder.allocator.alloc_str(&dest_var);
                let assign_expr = self.ast_builder.expression_assignment(
                    span,
                    oxc_ast::ast::AssignmentOperator::Assign,
                    oxc_ast::ast::AssignmentTarget::AssignmentTargetIdentifier(self.ast_builder.alloc(
                        oxc_ast::ast::IdentifierReference {
                            span,
                            name: oxc_span::Atom::from(var_atom),
                            reference_id: std::cell::Cell::new(None),
                        },
                    )),
                    init_expression,
                );
                Ok(self.ast_builder.statement_expression(span, assign_expr))
            }
            Some(crate::analysis::ssa_usage_tracker::DeclarationStrategy::DeclareAndInitialize { kind }) => {
                // Create declaration with the specified kind
                let declaration_kind = match kind {
                    VariableKind::Const => oxc_ast::ast::VariableDeclarationKind::Const,
                    VariableKind::Let => oxc_ast::ast::VariableDeclarationKind::Let,
                };
                self.create_variable_declaration(&dest_var, Some(init_expression), declaration_kind)
            }
            Some(crate::analysis::ssa_usage_tracker::DeclarationStrategy::Skip) => {
                // Skip - the value is fully inlined, no statement needed
                Ok(self.ast_builder.statement_empty(oxc_span::Span::default()))
            }
            Some(crate::analysis::ssa_usage_tracker::DeclarationStrategy::SideEffectOnly) => {
                // Execute for side effects only, create an expression statement
                let span = oxc_span::Span::default();
                Ok(self.ast_builder.statement_expression(span, init_expression))
            }
            Some(crate::analysis::ssa_usage_tracker::DeclarationStrategy::DeclareAtDominator { kind: _, .. }) => {
                // The variable declaration (with 'kind' as const/let) is already handled
                // in ControlFlowPlanConverter::convert_basic_block() at the dominator block.
                // Here we only generate the assignment statement
                let span = oxc_span::Span::default();
                let var_atom = self.ast_builder.allocator.alloc_str(&dest_var);
                let assign_expr = self.ast_builder.expression_assignment(
                    span,
                    oxc_ast::ast::AssignmentOperator::Assign,
                    oxc_ast::ast::AssignmentTarget::AssignmentTargetIdentifier(self.ast_builder.alloc(
                        oxc_ast::ast::IdentifierReference {
                            span,
                            name: oxc_span::Atom::from(var_atom),
                            reference_id: std::cell::Cell::new(None),
                        },
                    )),
                    init_expression,
                );
                Ok(self.ast_builder.statement_expression(span, assign_expr))
            }
            None => {
                // No strategy - shouldn't happen, fall back to assignment
                let span = oxc_span::Span::default();
                let var_atom = self.ast_builder.allocator.alloc_str(&dest_var);
                let assign_expr = self.ast_builder.expression_assignment(
                    span,
                    oxc_ast::ast::AssignmentOperator::Assign,
                    oxc_ast::ast::AssignmentTarget::AssignmentTargetIdentifier(self.ast_builder.alloc(
                        oxc_ast::ast::IdentifierReference {
                            span,
                            name: oxc_span::Atom::from(var_atom),
                            reference_id: std::cell::Cell::new(None),
                        },
                    )),
                    init_expression,
                );
                Ok(self.ast_builder.statement_expression(span, assign_expr))
            }
        }
    }

    /// Create a variable declaration or assignment based on whether the variable was already declared
    pub fn create_variable_declaration_or_assignment(
        &mut self,
        variable_name: &str,
        init_expression: Option<oxc_ast::ast::Expression<'a>>,
    ) -> Result<Statement<'a>, StatementConversionError> {
        // Check if this is the first definition of the variable
        let is_first_definition = self.register_manager.is_first_definition(variable_name);

        if is_first_definition {
            // Check if variable should be const - first check declaration strategy, then fallback to usage analysis
            let declaration_kind =
                if let Some(kind) = self.get_declaration_kind_from_plan(variable_name) {
                    // Use declaration strategy from control flow plan
                    match kind {
                        VariableKind::Const => oxc_ast::ast::VariableDeclarationKind::Const,
                        VariableKind::Let => oxc_ast::ast::VariableDeclarationKind::Let,
                    }
                } else if self.register_manager.should_be_const(variable_name) {
                    // Fallback to variable usage analysis
                    oxc_ast::ast::VariableDeclarationKind::Const
                } else {
                    oxc_ast::ast::VariableDeclarationKind::Let
                };
            // Create declaration: let/const variable_name = init_expression
            self.create_variable_declaration(variable_name, init_expression, declaration_kind)
        } else {
            // Track that this variable was used without declaration
            self.undeclared_variables.insert(variable_name.to_string());

            // Create assignment: variable_name = init_expression
            if let Some(init_expr) = init_expression {
                let span = oxc_span::Span::default();
                let var_atom = self.ast_builder.allocator.alloc_str(variable_name);
                let assign_expr = self.ast_builder.expression_assignment(
                    span,
                    oxc_ast::ast::AssignmentOperator::Assign,
                    oxc_ast::ast::AssignmentTarget::AssignmentTargetIdentifier(
                        self.ast_builder.alloc(oxc_ast::ast::IdentifierReference {
                            span,
                            name: oxc_span::Atom::from(var_atom),
                            reference_id: std::cell::Cell::new(None),
                        }),
                    ),
                    init_expr,
                );
                Ok(self.ast_builder.statement_expression(span, assign_expr))
            } else {
                // No init expression, return empty statement
                Ok(self.ast_builder.statement_empty(oxc_span::Span::default()))
            }
        }
    }

    /// Create an expression from an SSA value, respecting use strategies
    pub fn ssa_value_to_expression(
        &mut self,
        ssa_value: &crate::cfg::ssa::SSAValue,
    ) -> Result<oxc_ast::ast::Expression<'a>, StatementConversionError> {
        // Use the common implementation
        self.ssa_value_to_expression_internal(ssa_value, None)
    }
    
    /// Create an expression from an SSA value at a specific use site
    pub fn ssa_value_to_expression_at_use_site(
        &mut self,
        ssa_value: &crate::cfg::ssa::SSAValue,
        use_site: crate::cfg::ssa::RegisterUse,
    ) -> Result<oxc_ast::ast::Expression<'a>, StatementConversionError> {
        // Use the common implementation with the specified use site
        self.ssa_value_to_expression_internal(ssa_value, Some(use_site))
    }
    
    /// Internal method for creating expression from SSA value with optional use site override
    fn ssa_value_to_expression_internal(
        &mut self,
        ssa_value: &crate::cfg::ssa::SSAValue,
        use_site_override: Option<crate::cfg::ssa::RegisterUse>,
    ) -> Result<oxc_ast::ast::Expression<'a>, StatementConversionError> {
        let span = oxc_span::Span::default();
        
        // Get current PC and block for use site lookup, or use override
        let use_site = if let Some(override_site) = use_site_override {
            override_site
        } else if let (Some(current_pc), Some(current_block)) = (
            self.register_manager.current_pc(),
            self.register_manager.current_block(),
        ) {
            crate::cfg::ssa::RegisterUse {
                register: ssa_value.register,
                block_id: current_block,
                instruction_idx: current_pc,
            }
        } else {
            // No PC/block info, just use variable name
            let dup_value = crate::cfg::ssa::types::DuplicatedSSAValue::original(ssa_value.clone());
            let var_name = self.register_manager.get_variable_name_for_duplicated(&dup_value);
            let var_atom = self.ast_builder.allocator.alloc_str(&var_name);
            return Ok(self.ast_builder.expression_identifier(span, var_atom));
        };
        
        // Create duplicated SSA value with current context if any
        let dup_value = if let Some(context) = self.register_manager.current_duplication_context() {
            crate::cfg::ssa::DuplicatedSSAValue {
                original: ssa_value.clone(),
                duplication_context: Some(context.clone()),
            }
        } else {
            crate::cfg::ssa::DuplicatedSSAValue::original(ssa_value.clone())
        };
        
        log::debug!(
            "Looking up use strategy for {} at {:?} with context: {}",
            ssa_value,
            use_site,
            dup_value.context_description()
        );
        
        // Look up use strategy
        let key = (dup_value.clone(), use_site.clone());
        if let Some(strategy) = self.control_flow_plan.use_strategies.get(&key) {
            log::debug!("Found use strategy for key {:?}: {:?}", key, strategy);
            match strategy {
                crate::analysis::ssa_usage_tracker::UseStrategy::InlineValue(constant) => {
                    log::debug!("Inlining constant: {:?}", constant);
                    return self.create_constant_expression(constant);
                }
                crate::analysis::ssa_usage_tracker::UseStrategy::InlinePropertyAccess(tracked_value) => {
                    log::debug!("Inlining property access: {:?}", tracked_value);
                    return self.create_property_access_expression(tracked_value);
                }
                crate::analysis::ssa_usage_tracker::UseStrategy::InlineGlobalThis => {
                    log::debug!("Inlining globalThis");
                    let global_atom = self.ast_builder.allocator.alloc_str("globalThis");
                    return Ok(self.ast_builder.expression_identifier(span, global_atom));
                }
                crate::analysis::ssa_usage_tracker::UseStrategy::InlineParameter { param_index } => {
                    let param_name = self.variable_mapper.get_parameter_name(*param_index);
                    log::debug!("Inlining parameter: {}", param_name);
                    let param_atom = self.ast_builder.allocator.alloc_str(&param_name);
                    return Ok(self.ast_builder.expression_identifier(span, param_atom));
                }
                crate::analysis::ssa_usage_tracker::UseStrategy::SimplifyCall { .. } |
                crate::analysis::ssa_usage_tracker::UseStrategy::UseVariable => {
                    // Use the variable reference
                    let var_name = self.register_manager.get_variable_name_for_duplicated(&dup_value);
                    log::debug!("Using variable: {}", var_name);
                    let var_atom = self.ast_builder.allocator.alloc_str(&var_name);
                    return Ok(self.ast_builder.expression_identifier(span, var_atom));
                }
            }
        }
        
        // No use strategy found, use variable name
        let var_name = self.register_manager.get_variable_name_for_duplicated(&dup_value);
        let var_atom = self.ast_builder.allocator.alloc_str(&var_name);
        Ok(self.ast_builder.expression_identifier(span, var_atom))
    }

    /// Create a constant expression from a ConstantValue
    pub fn create_constant_expression(
        &self,
        value: &ConstantValue,
    ) -> Result<oxc_ast::ast::Expression<'a>, StatementConversionError> {
        let span = oxc_span::Span::default();
        let expr = match value {
            ConstantValue::Number(n) => self.ast_builder.expression_numeric_literal(
                span,
                *n,
                None,
                oxc_ast::ast::NumberBase::Decimal,
            ),
            ConstantValue::String(s) => {
                let string_atom = self.ast_builder.allocator.alloc_str(s);
                self.ast_builder
                    .expression_string_literal(span, string_atom, None)
            }
            ConstantValue::Boolean(b) => self.ast_builder.expression_boolean_literal(span, *b),
            ConstantValue::Null => self.ast_builder.expression_null_literal(span),
            ConstantValue::Undefined => {
                let undefined_atom = self.ast_builder.allocator.alloc_str("undefined");
                self.ast_builder.expression_identifier(span, undefined_atom)
            }
            ConstantValue::ArrayLiteral(elements) => {
                let mut array_elements = self.ast_builder.vec();
                for element in elements {
                    let element_expr = self.create_constant_expression(element)?;
                    array_elements.push(oxc_ast::ast::ArrayExpressionElement::from(element_expr));
                }
                self.ast_builder.expression_array(span, array_elements)
            }
            ConstantValue::ObjectLiteral(properties) => {
                let mut object_properties = self.ast_builder.vec();
                for (key, value) in properties {
                    let key_atom = self.ast_builder.allocator.alloc_str(key);
                    let key_ident = self.ast_builder.identifier_name(span, key_atom);
                    let property_key = oxc_ast::ast::PropertyKey::StaticIdentifier(
                        self.ast_builder.alloc(key_ident),
                    );
                    let value_expr = self.create_constant_expression(value)?;
                    let property = self.ast_builder.object_property(
                        span,
                        oxc_ast::ast::PropertyKind::Init,
                        property_key,
                        value_expr,
                        false,
                        false,
                        false,
                    );
                    object_properties.push(oxc_ast::ast::ObjectPropertyKind::ObjectProperty(
                        self.ast_builder.alloc(property),
                    ));
                }
                self.ast_builder.expression_object(span, object_properties)
            }
        };
        Ok(expr)
    }

    /// Create a property access expression from a TrackedValue
    pub fn create_property_access_expression(
        &self,
        tracked_value: &crate::analysis::value_tracker::TrackedValue,
    ) -> Result<oxc_ast::ast::Expression<'a>, StatementConversionError> {
        use crate::analysis::value_tracker::TrackedValue;

        let span = oxc_span::Span::default();

        match tracked_value {
            TrackedValue::PropertyAccess { object, property } => {
                // Recursively build the object expression
                let object_expr = self.create_property_access_expression(object)?;

                // Check if this is a global property access that can be simplified
                // For example, globalThis.console can be simplified to just console
                if let TrackedValue::GlobalObject = &**object {
                    // Check if this is a standard global property
                    if is_standard_global(property) {
                        // Just use the property name directly
                        let prop_atom = self.ast_builder.allocator.alloc_str(property);
                        return Ok(self.ast_builder.expression_identifier(span, prop_atom));
                    }
                }

                // Create the member expression
                let prop_atom = self.ast_builder.allocator.alloc_str(property);
                let property_ident = self.ast_builder.identifier_name(span, prop_atom);
                let member = self.ast_builder.member_expression_static(
                    span,
                    object_expr,
                    property_ident,
                    false, // optional
                );
                Ok(Expression::from(member))
            }
            TrackedValue::GlobalObject => {
                // Use globalThis as the base
                let global_atom = self.ast_builder.allocator.alloc_str("globalThis");
                Ok(self.ast_builder.expression_identifier(span, global_atom))
            }
            TrackedValue::Constant(value) => {
                // If for some reason we have a constant in the chain, inline it
                self.create_constant_expression(value)
            }
            TrackedValue::Parameter { .. } 
            | TrackedValue::Phi { .. } 
            | TrackedValue::Unknown 
            | TrackedValue::MutableObject { .. }
            | TrackedValue::MergedObject { .. } => {
                // These shouldn't appear in property access chains normally
                // But if they do, we have a problem - we can't inline them properly
                // This is likely a bug in the tracking logic
                log::warn!(
                    "Unexpected TrackedValue in property access chain: {:?}",
                    tracked_value
                );
                let undefined_atom = self.ast_builder.allocator.alloc_str("undefined");
                Ok(self.ast_builder.expression_identifier(span, undefined_atom))
            }
        }
    }

    /// Create a return statement: `return expression;`
    pub fn create_return_statement(
        &self,
        expression: Option<oxc_ast::ast::Expression<'a>>,
    ) -> Statement<'a> {
        let span = oxc_span::Span::default();
        self.ast_builder.statement_return(span, expression)
    }

    /// Helper function to create formal parameters for a function
    pub fn create_function_parameters(
        &mut self,
        param_count: u32,
        _func_idx: u32,
    ) -> oxc_ast::ast::FormalParameters<'a> {
        let span = oxc_span::Span::default();
        let mut params = self.ast_builder.vec();

        // In Hermes bytecode, the first parameter is always the implicit 'this' value
        // We skip it to match the original JavaScript function signature
        // So if bytecode says 3 params, the actual function has 2 user-defined params
        let actual_param_count = if param_count > 0 { param_count - 1 } else { 0 };

        for i in 0..actual_param_count {
            // Parameters are named using the variable mapper
            // param_index starts at 1 since 0 is 'this'
            let param_name = self.variable_mapper.get_parameter_name((i + 1) as u8);
            let param_atom = self.ast_builder.allocator.alloc_str(&param_name);

            // Create binding identifier for the parameter
            let binding_identifier = oxc_ast::ast::BindingIdentifier {
                span,
                name: oxc_span::Atom::from(param_atom),
                symbol_id: std::cell::Cell::new(None),
            };

            // Create binding pattern
            let binding_pattern = oxc_ast::ast::BindingPattern {
                kind: oxc_ast::ast::BindingPatternKind::BindingIdentifier(
                    self.ast_builder.alloc(binding_identifier),
                ),
                type_annotation: None,
                optional: false,
            };

            // Create formal parameter
            let formal_param = oxc_ast::ast::FormalParameter {
                span,
                decorators: self.ast_builder.vec(),
                pattern: binding_pattern,
                accessibility: None,
                readonly: false,
                r#override: false,
            };

            params.push(formal_param);
        }

        self.ast_builder.formal_parameters(
            span,
            oxc_ast::ast::FormalParameterKind::FormalParameter,
            params,
            None::<oxc_ast::ast::BindingRestElement>,
        )
    }

    /// Get the declaration kind (const vs let) from the control flow plan
    fn get_declaration_kind_from_plan(&self, variable_name: &str) -> Option<VariableKind> {
        let plan = &self.control_flow_plan;

        // Look through all declaration strategies to find one that matches this variable name
        for (dup_ssa, strategy) in &plan.declaration_strategies {
            // Check if this strategy applies to a variable with the given name
            let strategy_var_name = self
                .register_manager
                .get_variable_name_for_duplicated(dup_ssa);
            if strategy_var_name == variable_name {
                return match strategy {
                    DeclarationStrategy::DeclareAndInitialize { kind } => Some(*kind),
                    DeclarationStrategy::DeclareAtDominator { kind, .. } => Some(*kind),
                    _ => None,
                };
            }
        }
        None
    }
}
