//! Value tracking and analysis
//!
//! This module provides functionality to track and analyze values through SSA form,
//! including constant folding, parameter detection, and static value analysis.

use crate::cfg::{
    ssa::{
        types::{RegisterDef, SSAValue},
        SSAAnalysis,
    },
    Cfg,
};
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::{HbcFile, InstructionIndex};
use petgraph::graph::NodeIndex;

// Re-export types for backward compatibility
pub use crate::analysis::value_tracking::{
    ConstantValue, MutationKind, ObjectBaseType, ObjectMutation, TrackedValue,
};

/// Value tracker for analyzing SSA values
pub struct ValueTracker<'a> {
    cfg: &'a Cfg<'a>,
    ssa: &'a SSAAnalysis,
    hbc_file: &'a HbcFile<'a>,
    /// PHI deconstructions from control flow plan (if available)
    /// Maps (block_id, context) to PHI replacement info
    phi_deconstructions: Option<
        &'a std::collections::HashMap<
            (
                petgraph::graph::NodeIndex,
                Option<crate::cfg::ssa::DuplicationContext>,
            ),
            crate::analysis::control_flow_plan::PhiDeconstructionInfo,
        >,
    >,
    /// Current duplication context for value resolution
    current_context: Option<crate::cfg::ssa::DuplicationContext>,
}

impl<'a> ValueTracker<'a> {
    /// Create a new value tracker
    pub fn new(cfg: &'a Cfg<'a>, ssa: &'a SSAAnalysis, hbc_file: &'a HbcFile<'a>) -> Self {
        let tracker = Self {
            cfg,
            ssa,
            hbc_file,
            phi_deconstructions: None,
            current_context: None,
        };

        // Note: Mutations are collected lazily when analyzing values
        tracker
    }

    /// Create a value tracker with PHI deconstruction information
    pub fn with_phi_deconstructions(
        cfg: &'a Cfg<'a>,
        ssa: &'a SSAAnalysis,
        hbc_file: &'a HbcFile<'a>,
        phi_deconstructions: &'a std::collections::HashMap<
            (
                petgraph::graph::NodeIndex,
                Option<crate::cfg::ssa::DuplicationContext>,
            ),
            crate::analysis::control_flow_plan::PhiDeconstructionInfo,
        >,
    ) -> Self {
        Self {
            cfg,
            ssa,
            hbc_file,
            phi_deconstructions: Some(phi_deconstructions),
            current_context: None,
        }
    }

    /// Set the current duplication context for value resolution
    pub fn with_context(mut self, context: Option<crate::cfg::ssa::DuplicationContext>) -> Self {
        self.current_context = context;
        self
    }

    /// Find mutations for a given SSA value by analyzing its uses
    fn find_mutations_for_ssa_value(&self, ssa_value: &SSAValue) -> Vec<ObjectMutation> {
        let mut mutations = Vec::new();

        // Find the definition for this SSA value
        let def = RegisterDef {
            register: ssa_value.register,
            block_id: ssa_value.def_site.block_id,
            instruction_idx: ssa_value.def_site.instruction_idx,
        };

        // Get all uses of this definition
        if let Some(uses) = self.ssa.def_use_chains.get(&def) {
            for use_site in uses {
                // Get the instruction at this use site
                let block_data = &self.cfg.graph()[use_site.block_id];
                if let Some(instr_info) = block_data
                    .instructions
                    .iter()
                    .find(|i| i.instruction_index == use_site.instruction_idx)
                {
                    // Check if this is a mutation instruction
                    if let Some(mutation) = self.extract_mutation_from_instruction(
                        &instr_info.instruction,
                        use_site.instruction_idx.value(),
                        ssa_value.register,
                    ) {
                        mutations.push(mutation);
                    }
                }
            }
        }

        mutations
    }

    /// Extract a mutation from an instruction if it mutates the given object register
    fn extract_mutation_from_instruction(
        &self,
        instruction: &UnifiedInstruction,
        pc: usize,
        object_reg: u8,
    ) -> Option<ObjectMutation> {
        use crate::generated::unified_instructions::UnifiedInstruction;

        match instruction {
            // Property mutations by ID
            UnifiedInstruction::PutById {
                operand_0,
                operand_1: value_reg,
                operand_3: prop_id,
                ..
            }
            | UnifiedInstruction::PutNewOwnById {
                operand_0,
                operand_1: value_reg,
                operand_2: prop_id,
            }
            | UnifiedInstruction::PutNewOwnNEById {
                operand_0,
                operand_1: value_reg,
                operand_2: prop_id,
            } if *operand_0 == object_reg => {
                if let Ok(prop_name) = self.hbc_file.strings.get((*prop_id).into()) {
                    let value = self.get_register_value_at_pc(*value_reg, pc);
                    Some(ObjectMutation {
                        pc,
                        kind: MutationKind::PropertySet {
                            key: Box::new(TrackedValue::Constant(ConstantValue::String(
                                prop_name.to_string(),
                            ))),
                            value: Box::new(value),
                        },
                    })
                } else {
                    None
                }
            }

            UnifiedInstruction::PutByIdLong {
                operand_0,
                operand_1: value_reg,
                operand_3: prop_id,
                ..
            }
            | UnifiedInstruction::PutNewOwnByIdLong {
                operand_0,
                operand_1: value_reg,
                operand_2: prop_id,
            }
            | UnifiedInstruction::PutNewOwnNEByIdLong {
                operand_0,
                operand_1: value_reg,
                operand_2: prop_id,
            } if *operand_0 == object_reg => {
                if let Ok(prop_name) = self.hbc_file.strings.get((*prop_id).into()) {
                    let value = self.get_register_value_at_pc(*value_reg, pc);
                    Some(ObjectMutation {
                        pc,
                        kind: MutationKind::PropertySet {
                            key: Box::new(TrackedValue::Constant(ConstantValue::String(
                                prop_name.to_string(),
                            ))),
                            value: Box::new(value),
                        },
                    })
                } else {
                    None
                }
            }

            UnifiedInstruction::PutNewOwnByIdShort {
                operand_0,
                operand_1: value_reg,
                operand_2: prop_id,
            } if *operand_0 == object_reg => {
                if let Ok(prop_name) = self.hbc_file.strings.get((*prop_id as u32).into()) {
                    let value = self.get_register_value_at_pc(*value_reg, pc);
                    Some(ObjectMutation {
                        pc,
                        kind: MutationKind::PropertySet {
                            key: Box::new(TrackedValue::Constant(ConstantValue::String(
                                prop_name.to_string(),
                            ))),
                            value: Box::new(value),
                        },
                    })
                } else {
                    None
                }
            }

            // Array mutations by index
            UnifiedInstruction::PutOwnByIndex {
                operand_0,
                operand_1: value_reg,
                operand_2: index,
            } if *operand_0 == object_reg => {
                let value = self.get_register_value_at_pc(*value_reg, pc);
                Some(ObjectMutation {
                    pc,
                    kind: MutationKind::ArraySet {
                        index: Box::new(TrackedValue::Constant(ConstantValue::Number(
                            *index as f64,
                        ))),
                        value: Box::new(value),
                    },
                })
            }

            UnifiedInstruction::PutOwnByIndexL {
                operand_0,
                operand_1: value_reg,
                operand_2: index,
            } if *operand_0 == object_reg => {
                let value = self.get_register_value_at_pc(*value_reg, pc);
                Some(ObjectMutation {
                    pc,
                    kind: MutationKind::ArraySet {
                        index: Box::new(TrackedValue::Constant(ConstantValue::Number(
                            *index as f64,
                        ))),
                        value: Box::new(value),
                    },
                })
            }

            // Dynamic property mutations
            UnifiedInstruction::PutByVal {
                operand_0,
                operand_1: key_reg,
                operand_2: value_reg,
            } if *operand_0 == object_reg => {
                let key = self.get_register_value_at_pc(*key_reg, pc);
                let value = self.get_register_value_at_pc(*value_reg, pc);
                Some(ObjectMutation {
                    pc,
                    kind: MutationKind::PropertySet {
                        key: Box::new(key),
                        value: Box::new(value),
                    },
                })
            }

            UnifiedInstruction::PutOwnByVal {
                operand_0,
                operand_1: key_reg,
                operand_2: value_reg,
                ..
            } if *operand_0 == object_reg => {
                let key = self.get_register_value_at_pc(*key_reg, pc);
                let value = self.get_register_value_at_pc(*value_reg, pc);
                Some(ObjectMutation {
                    pc,
                    kind: MutationKind::PropertySet {
                        key: Box::new(key),
                        value: Box::new(value),
                    },
                })
            }

            _ => None,
        }
    }

    /// Check if an object escapes the current function scope
    pub fn check_object_escape(&self, ssa_value: &SSAValue) -> bool {
        use crate::analysis::value_tracking::EscapeAnalyzer;

        let analyzer = EscapeAnalyzer::new(self.ssa, self.cfg);
        let def = RegisterDef {
            register: ssa_value.register,
            block_id: ssa_value.def_site.block_id,
            instruction_idx: ssa_value.def_site.instruction_idx,
        };

        let result = analyzer.analyze_object_escape(&def);
        result.escapes
    }

    /// Get the value of a register at a specific PC
    fn get_register_value_at_pc(&self, reg: u8, pc: usize) -> TrackedValue {
        // Try to find the SSA value for this register at this PC
        if let Some(ssa_value) = self
            .ssa
            .get_value_after_instruction(reg, InstructionIndex::new(pc))
        {
            self.get_value(ssa_value)
        } else {
            TrackedValue::Unknown
        }
    }

    /// Get the tracked value of an SSA value
    pub fn get_value(&self, ssa_value: &SSAValue) -> TrackedValue {
        // First, check if we have PHI deconstructions and if this SSA value is a PHI result
        // that has been replaced in the current context
        if let (Some(phi_decons), Some(ref context)) =
            (&self.phi_deconstructions, &self.current_context)
        {
            // Check if there's a PHI replacement for this value in the current context
            // We need to check all blocks to find where this PHI result might be replaced
            for ((_block_id, ctx), phi_info) in phi_decons.iter() {
                if ctx.as_ref() == Some(context) {
                    // Check if this SSA value is one of the PHI results that got replaced
                    if let Some(replacement) = phi_info.replacements.get(ssa_value) {
                        log::debug!(
                            "Found PHI replacement for {} -> {} in context {:?}",
                            ssa_value,
                            replacement,
                            context
                        );
                        // Recursively get the value of the replacement
                        return self.get_value(replacement);
                    }
                }
            }
        }

        // Check if this SSA value is produced by a phi function
        if let Some(phi_functions) = self.ssa.phi_functions.get(&ssa_value.def_site.block_id) {
            for phi in phi_functions {
                if phi.result == *ssa_value {
                    return self.analyze_phi_function(phi);
                }
            }
        }

        // Look up the instruction that defined this SSA value
        let block = &self.cfg.graph()[ssa_value.def_site.block_id];
        let instructions = block.instructions();

        // Find the instruction at the definition site
        let instruction_offset =
            ssa_value.def_site.instruction_idx.value() - block.start_pc().value();
        if let Some(instr) = instructions.get(instruction_offset) {
            return self.analyze_instruction(&instr.instruction, ssa_value);
        }

        TrackedValue::Unknown
    }

    /// Analyze a phi function to see if all inputs resolve to the same constant
    fn analyze_phi_function(&self, phi: &crate::cfg::ssa::types::PhiFunction) -> TrackedValue {
        let mut constant_value = None;

        // Check all operands of the phi function
        for (_pred_block, operand_ssa) in &phi.operands {
            match self.get_value(operand_ssa) {
                TrackedValue::Constant(c) => {
                    log::trace!("PHI operand {} evaluates to constant: {:?}", operand_ssa, c);
                    if let Some(ref existing) = constant_value {
                        if existing != &c {
                            // Different constants - this phi doesn't resolve to a single constant
                            log::trace!("PHI has different constants, returning Phi");
                            return TrackedValue::Phi {
                                ssa_value: phi.result.clone(),
                            };
                        }
                    } else {
                        constant_value = Some(c);
                    }
                }
                _ => {
                    // Non-constant operand - this phi doesn't resolve to a constant
                    log::trace!("PHI operand {} is not constant, returning Phi", operand_ssa);
                    return TrackedValue::Phi {
                        ssa_value: phi.result.clone(),
                    };
                }
            }
        }

        // All operands are the same constant
        if let Some(c) = constant_value {
            log::trace!("All PHI operands are the same constant: {:?}", c);
            TrackedValue::Constant(c)
        } else {
            TrackedValue::Phi {
                ssa_value: phi.result.clone(),
            }
        }
    }

    /// Analyze an instruction, potentially walking the def tree for complex cases
    fn analyze_instruction(
        &self,
        instruction: &UnifiedInstruction,
        ssa_value: &SSAValue,
    ) -> TrackedValue {
        match instruction {
            // Direct constant loads
            UnifiedInstruction::LoadConstUInt8 { operand_1, .. } => {
                TrackedValue::Constant(ConstantValue::Number(*operand_1 as f64))
            }
            UnifiedInstruction::LoadConstInt { operand_1, .. } => {
                TrackedValue::Constant(ConstantValue::Number(*operand_1 as f64))
            }
            UnifiedInstruction::LoadConstDouble { operand_1, .. } => {
                TrackedValue::Constant(ConstantValue::Number(*operand_1))
            }
            UnifiedInstruction::LoadConstString { operand_1, .. } => {
                if let Ok(string) = self.hbc_file.strings.get((*operand_1).into()) {
                    TrackedValue::Constant(ConstantValue::String(string.clone()))
                } else {
                    TrackedValue::Unknown
                }
            }
            UnifiedInstruction::LoadConstStringLongIndex { operand_1, .. } => {
                if let Ok(string) = self.hbc_file.strings.get((*operand_1).into()) {
                    TrackedValue::Constant(ConstantValue::String(string.clone()))
                } else {
                    TrackedValue::Unknown
                }
            }
            UnifiedInstruction::LoadConstNull { .. } => TrackedValue::Constant(ConstantValue::Null),
            UnifiedInstruction::LoadConstUndefined { .. } => {
                TrackedValue::Constant(ConstantValue::Undefined)
            }
            UnifiedInstruction::LoadConstTrue { .. } => {
                TrackedValue::Constant(ConstantValue::Boolean(true))
            }
            UnifiedInstruction::LoadConstFalse { .. } => {
                TrackedValue::Constant(ConstantValue::Boolean(false))
            }
            UnifiedInstruction::LoadConstZero { .. } => {
                TrackedValue::Constant(ConstantValue::Number(0.0))
            }

            // Parameter loads
            UnifiedInstruction::LoadParam { operand_1, .. } => TrackedValue::Parameter {
                index: *operand_1 as u32,
                ssa_value: ssa_value.clone(),
            },

            // Mov instruction - follow the source
            UnifiedInstruction::Mov { operand_1, .. } => {
                // Find the SSA value for the source register
                if let Some(source_ssa) = self
                    .ssa
                    .get_value_before_instruction(*operand_1, ssa_value.def_site.instruction_idx)
                {
                    self.get_value(source_ssa)
                } else {
                    TrackedValue::Unknown
                }
            }

            // Arithmetic operations - perform constant folding
            UnifiedInstruction::Add {
                operand_1,
                operand_2,
                ..
            } => self.fold_binary_operation(
                *operand_1,
                *operand_2,
                ssa_value.def_site.instruction_idx,
                |a, b| match (a, b) {
                    (ConstantValue::Number(n1), ConstantValue::Number(n2)) => {
                        Some(ConstantValue::Number(n1 + n2))
                    }
                    (ConstantValue::String(s1), ConstantValue::String(s2)) => {
                        Some(ConstantValue::String(s1.clone() + s2))
                    }
                    _ => None,
                },
            ),

            UnifiedInstruction::Sub {
                operand_1,
                operand_2,
                ..
            } => self.fold_binary_operation(
                *operand_1,
                *operand_2,
                ssa_value.def_site.instruction_idx,
                |a, b| match (a, b) {
                    (ConstantValue::Number(n1), ConstantValue::Number(n2)) => {
                        Some(ConstantValue::Number(n1 - n2))
                    }
                    _ => None,
                },
            ),

            UnifiedInstruction::Mul {
                operand_1,
                operand_2,
                ..
            } => self.fold_binary_operation(
                *operand_1,
                *operand_2,
                ssa_value.def_site.instruction_idx,
                |a, b| match (a, b) {
                    (ConstantValue::Number(n1), ConstantValue::Number(n2)) => {
                        Some(ConstantValue::Number(n1 * n2))
                    }
                    _ => None,
                },
            ),

            UnifiedInstruction::Div {
                operand_1,
                operand_2,
                ..
            } => self.fold_binary_operation(
                *operand_1,
                *operand_2,
                ssa_value.def_site.instruction_idx,
                |a, b| match (a, b) {
                    (ConstantValue::Number(n1), ConstantValue::Number(n2)) if *n2 != 0.0 => {
                        Some(ConstantValue::Number(n1 / n2))
                    }
                    _ => None,
                },
            ),

            // Array creation with buffer
            UnifiedInstruction::NewArrayWithBuffer {
                operand_1: _size_hint,
                operand_2: num_elements,
                ..
            } => {
                // For now, track as mutable array
                // TODO: Parse the buffer and add initial mutations
                let creation_pc = ssa_value.def_site.instruction_idx.value();
                TrackedValue::MutableObject {
                    creation_pc,
                    version: 0,
                    base_type: ObjectBaseType::Array {
                        initial_length: Some(*num_elements as usize),
                    },
                    mutations: Vec::new(),
                }
            }

            UnifiedInstruction::NewArrayWithBufferLong {
                operand_1: _size_hint,
                operand_2: num_elements,
                ..
            } => {
                // For now, track as mutable array
                // TODO: Parse the buffer and add initial mutations
                let creation_pc = ssa_value.def_site.instruction_idx.value();
                TrackedValue::MutableObject {
                    creation_pc,
                    version: 0,
                    base_type: ObjectBaseType::Array {
                        initial_length: Some(*num_elements as usize),
                    },
                    mutations: Vec::new(),
                }
            }

            // Object creation with buffer
            UnifiedInstruction::NewObjectWithBuffer {
                operand_1: _size_hint,
                operand_2: _num_elements,
                ..
            } => {
                // Track as mutable object with buffer
                // TODO: Parse the buffer and add initial mutations
                let creation_pc = ssa_value.def_site.instruction_idx.value();

                // Find mutations by looking at uses of this SSA value
                let mutations = self.find_mutations_for_ssa_value(ssa_value);

                TrackedValue::MutableObject {
                    creation_pc,
                    version: mutations.len(),
                    base_type: ObjectBaseType::ObjectWithBuffer,
                    mutations,
                }
            }

            UnifiedInstruction::NewObjectWithBufferLong {
                operand_1: _size_hint,
                operand_2: _num_elements,
                ..
            } => {
                // Track as mutable object with buffer
                // TODO: Parse the buffer and add initial mutations
                let creation_pc = ssa_value.def_site.instruction_idx.value();

                // Find mutations by looking at uses of this SSA value
                let mutations = self.find_mutations_for_ssa_value(ssa_value);

                TrackedValue::MutableObject {
                    creation_pc,
                    version: mutations.len(),
                    base_type: ObjectBaseType::ObjectWithBuffer,
                    mutations,
                }
            }

            // Array creation with specified size
            UnifiedInstruction::NewArray { operand_1, .. } => {
                // Track as a mutable array with versioning
                let creation_pc = ssa_value.def_site.instruction_idx.value();

                // Find mutations by looking at uses of this SSA value
                let mutations = self.find_mutations_for_ssa_value(ssa_value);

                TrackedValue::MutableObject {
                    creation_pc,
                    version: mutations.len(),
                    base_type: ObjectBaseType::Array {
                        initial_length: Some(*operand_1 as usize),
                    },
                    mutations,
                }
            }

            // Empty object creation
            UnifiedInstruction::NewObject { .. } => {
                // Track as a mutable object with versioning
                let creation_pc = ssa_value.def_site.instruction_idx.value();

                // Find mutations by looking at uses of this SSA value
                let mutations = self.find_mutations_for_ssa_value(ssa_value);

                TrackedValue::MutableObject {
                    creation_pc,
                    version: mutations.len(),
                    base_type: ObjectBaseType::Object,
                    mutations,
                }
            }

            // Object with parent
            UnifiedInstruction::NewObjectWithParent { .. } => {
                // Track as a mutable object with versioning
                let creation_pc = ssa_value.def_site.instruction_idx.value();

                // Find mutations by looking at uses of this SSA value
                let mutations = self.find_mutations_for_ssa_value(ssa_value);

                TrackedValue::MutableObject {
                    creation_pc,
                    version: mutations.len(),
                    base_type: ObjectBaseType::Object,
                    mutations,
                }
            }

            // Global object access
            UnifiedInstruction::GetGlobalObject { .. } => TrackedValue::GlobalObject,

            // Property access instructions
            UnifiedInstruction::GetById {
                operand_1,
                operand_2,
                ..
            } => {
                // Get the object being accessed
                if let Some(object_ssa) = self
                    .ssa
                    .get_value_before_instruction(*operand_1, ssa_value.def_site.instruction_idx)
                {
                    let object_value = self.get_value(object_ssa);

                    // Get the property name
                    if let Ok(property_name) = self.hbc_file.strings.get((*operand_2).into()) {
                        log::trace!(
                            "GetById for {}: object reg {}, property '{}', object_value: {:?}",
                            ssa_value,
                            *operand_1,
                            property_name,
                            object_value
                        );
                        // Avoid infinite recursion with a depth check
                        if let Some(depth) = self.get_property_chain_depth(&object_value) {
                            if depth < 10 {
                                // Reasonable depth limit
                                let result = TrackedValue::PropertyAccess {
                                    object: Box::new(object_value),
                                    property: property_name.clone(),
                                };
                                log::trace!("Returning PropertyAccess: {:?}", result);
                                return result;
                            }
                        }
                    }
                }
                TrackedValue::Unknown
            }

            UnifiedInstruction::GetByIdShort {
                operand_1,
                operand_3,
                ..
            } => {
                // Get the object being accessed (operand_1 is object register)
                if let Some(object_ssa) = self
                    .ssa
                    .get_value_before_instruction(*operand_1, ssa_value.def_site.instruction_idx)
                {
                    let object_value = self.get_value(object_ssa);

                    // Get the property name (operand_3 is string ID for GetByIdShort)
                    if let Ok(property_name) = self.hbc_file.strings.get((*operand_3 as u32).into())
                    {
                        log::trace!(
                            "GetByIdShort for {}: object reg {}, property '{}', object_value: {:?}",
                            ssa_value,
                            *operand_1,
                            property_name,
                            object_value
                        );
                        // Avoid infinite recursion with a depth check
                        if let Some(depth) = self.get_property_chain_depth(&object_value) {
                            if depth < 10 {
                                // Reasonable depth limit
                                let result = TrackedValue::PropertyAccess {
                                    object: Box::new(object_value),
                                    property: property_name.clone(),
                                };
                                log::trace!("Returning PropertyAccess: {:?}", result);
                                return result;
                            }
                        }
                    }
                }
                TrackedValue::Unknown
            }

            UnifiedInstruction::TryGetById {
                operand_1,
                operand_3,
                ..
            } => {
                // Get the object being accessed (operand_1 is object register)
                if let Some(object_ssa) = self
                    .ssa
                    .get_value_before_instruction(*operand_1, ssa_value.def_site.instruction_idx)
                {
                    let object_value = self.get_value(object_ssa);

                    // Get the property name (operand_3 is string ID for TryGetById)
                    if let Ok(property_name) = self.hbc_file.strings.get((*operand_3 as u32).into())
                    {
                        log::trace!(
                            "TryGetById for {}: object reg {}, property '{}', object_value: {:?}",
                            ssa_value,
                            *operand_1,
                            property_name,
                            object_value
                        );
                        // Avoid infinite recursion with a depth check
                        if let Some(depth) = self.get_property_chain_depth(&object_value) {
                            if depth < 10 {
                                // Reasonable depth limit
                                let result = TrackedValue::PropertyAccess {
                                    object: Box::new(object_value),
                                    property: property_name.clone(),
                                };
                                log::trace!("Returning PropertyAccess: {:?}", result);
                                return result;
                            }
                        }
                    }
                }
                TrackedValue::Unknown
            }

            UnifiedInstruction::GetByIdLong {
                operand_1,
                operand_2,
                ..
            } => {
                // Get the object being accessed
                if let Some(object_ssa) = self
                    .ssa
                    .get_value_before_instruction(*operand_1, ssa_value.def_site.instruction_idx)
                {
                    let object_value = self.get_value(object_ssa);

                    // Get the property name
                    if let Ok(property_name) = self.hbc_file.strings.get((*operand_2).into()) {
                        // Avoid infinite recursion with a depth check
                        if let Some(depth) = self.get_property_chain_depth(&object_value) {
                            if depth < 10 {
                                // Reasonable depth limit
                                return TrackedValue::PropertyAccess {
                                    object: Box::new(object_value),
                                    property: property_name,
                                };
                            }
                        }
                    }
                }
                TrackedValue::Unknown
            }

            UnifiedInstruction::TryGetByIdLong {
                operand_1,
                operand_3,
                ..
            } => {
                // Get the object being accessed (operand_1 is object register)
                if let Some(object_ssa) = self
                    .ssa
                    .get_value_before_instruction(*operand_1, ssa_value.def_site.instruction_idx)
                {
                    let object_value = self.get_value(object_ssa);

                    // Get the property name (operand_3 is string ID for TryGetByIdLong)
                    if let Ok(property_name) = self.hbc_file.strings.get((*operand_3).into()) {
                        // Avoid infinite recursion with a depth check
                        if let Some(depth) = self.get_property_chain_depth(&object_value) {
                            if depth < 10 {
                                // Reasonable depth limit
                                return TrackedValue::PropertyAccess {
                                    object: Box::new(object_value),
                                    property: property_name,
                                };
                            }
                        }
                    }
                }
                TrackedValue::Unknown
            }

            // TODO: Handle more operations like:
            // - Negate, Not, BitNot
            // - Mod, BitAnd, BitOr, BitXor, LShift, RShift, URshift
            // - StrictEq, StrictNotEq, Eq, NotEq, Less, LessEq, Greater, GreaterEq
            // - TypeOf, InstanceOf, IsIn
            // - ToNumber, ToString
            _ => TrackedValue::Unknown,
        }
    }

    /// Helper to fold binary operations
    fn fold_binary_operation<F>(
        &self,
        operand1: u8,
        operand2: u8,
        pc: InstructionIndex,
        folder: F,
    ) -> TrackedValue
    where
        F: Fn(&ConstantValue, &ConstantValue) -> Option<ConstantValue>,
    {
        // Get SSA values for both operands
        let val1 = if let Some(ssa1) = self.ssa.get_value_before_instruction(operand1, pc) {
            let v = self.get_value(ssa1);
            log::trace!(
                "Binary op at PC {} operand1 (r{}) = {} = {:?}",
                pc,
                operand1,
                ssa1,
                v
            );
            v
        } else {
            log::trace!("Binary op operand1 (r{}) = Unknown (no SSA)", operand1);
            TrackedValue::Unknown
        };

        let val2 = if let Some(ssa2) = self.ssa.get_value_before_instruction(operand2, pc) {
            let v = self.get_value(ssa2);
            log::trace!("Binary op operand2 (r{}) = {:?}", operand2, v);
            v
        } else {
            log::trace!("Binary op operand2 (r{}) = Unknown (no SSA)", operand2);
            TrackedValue::Unknown
        };

        // Try to fold if both are constants
        match (val1, val2) {
            (TrackedValue::Constant(c1), TrackedValue::Constant(c2)) => {
                if let Some(result) = folder(&c1, &c2) {
                    log::trace!("Folding constants: {:?} + {:?} = {:?}", c1, c2, result);
                    TrackedValue::Constant(result)
                } else {
                    TrackedValue::Unknown
                }
            }
            _ => {
                log::trace!("Cannot fold non-constants");
                TrackedValue::Unknown
            }
        }
    }

    /// Convert a register lookup at a specific point to an SSA value lookup
    pub fn get_value_at_point(
        &self,
        register: u8,
        _block_id: NodeIndex,
        instruction_idx: InstructionIndex,
    ) -> TrackedValue {
        // Find the SSA value for this register at this point
        if let Some(ssa_value) = self
            .ssa
            .get_value_before_instruction(register, instruction_idx)
        {
            self.get_value(ssa_value)
        } else {
            TrackedValue::Unknown
        }
    }

    /// Get the depth of a property chain to avoid infinite recursion
    fn get_property_chain_depth(&self, value: &TrackedValue) -> Option<usize> {
        match value {
            TrackedValue::PropertyAccess { object, .. } => {
                self.get_property_chain_depth(object).map(|d| d + 1)
            }
            TrackedValue::GlobalObject
            | TrackedValue::Constant(_)
            | TrackedValue::Parameter { .. }
            | TrackedValue::Phi { .. }
            | TrackedValue::MutableObject { .. }
            | TrackedValue::MergedObject { .. } => Some(0),
            TrackedValue::Unknown => None,
        }
    }
}
