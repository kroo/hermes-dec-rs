//! Exception handler analysis for CFG
//!
//! This module analyzes exception handling structures (try-catch-finally blocks)
//! in the control flow graph.

use super::Cfg;
use crate::hbc::instruction_types::InstructionIndex;
use petgraph::graph::NodeIndex;
use std::collections::HashSet;

/// Information about exception handlers in a function
#[derive(Debug, Clone)]
pub struct ExceptionAnalysis {
    /// List of exception handler regions
    pub regions: Vec<ExceptionRegion>,
}

/// A single exception handler region (try-catch-finally)
#[derive(Debug, Clone)]
pub struct ExceptionRegion {
    /// Blocks that form the try body
    pub try_blocks: Vec<NodeIndex>,
    /// The catch handler block (if any)
    pub catch_handler: Option<CatchHandler>,
    /// The finally block (if any)
    pub finally_block: Option<NodeIndex>,
    /// Start instruction index of the try region
    pub try_start_idx: u32,
    /// End instruction index of the try region (exclusive)
    pub try_end_idx: u32,
}

/// Information about a catch handler
#[derive(Debug, Clone)]
pub struct CatchHandler {
    /// The block where the catch handler starts
    pub catch_block: NodeIndex,
    /// The register that receives the caught exception
    pub error_register: u8,
    /// The instruction index where the catch handler starts
    pub catch_target_idx: u32,
}

impl<'a> Cfg<'a> {
    /// Analyze exception handlers in the function
    pub fn analyze_exception_handlers(&self) -> Option<ExceptionAnalysis> {
        // Get the parsed header for this function
        let parsed_header = self
            .hbc_file()
            .functions
            .get_parsed_header(self.function_index())?;

        // If there are no exception handlers, return None
        if parsed_header.exc_handlers.is_empty() {
            return None;
        }

        let mut regions = Vec::new();

        // Process each exception handler
        log::debug!(
            "Function {} has {} exception handlers",
            self.function_index(),
            parsed_header.exc_handlers.len()
        );
        for (i, handler) in parsed_header.exc_handlers.iter().enumerate() {
            // Convert byte offsets to instruction indices
            let try_start = self
                .hbc_file()
                .jump_table
                .byte_offset_to_instruction_index(self.function_index(), handler.start)?;
            let try_end = self
                .hbc_file()
                .jump_table
                .byte_offset_to_instruction_index(self.function_index(), handler.end)?;
            let catch_target = self
                .hbc_file()
                .jump_table
                .byte_offset_to_instruction_index(self.function_index(), handler.target)?;

            log::debug!(
                "Handler {}: try range [{}..{}), catch target: {}",
                i,
                try_start,
                try_end,
                catch_target
            );

            // Find all blocks in the try range
            let try_blocks = self.find_blocks_in_range(try_start, try_end);

            // Find the catch block
            let catch_block = self.find_block_for_instruction(catch_target)?;

            // Extract the error register from the Catch instruction (first instruction)
            let error_register = self.extract_catch_info(catch_block)?;

            // Create the catch handler
            let catch_handler = Some(CatchHandler {
                catch_block,
                error_register,
                catch_target_idx: catch_target,
            });

            // TODO: Detect finally blocks
            // Finally blocks are typically identified by:
            // 1. Being reachable from both try and catch blocks
            // 2. Having a specific structure (often with cleanup code)
            let finally_block = None;

            regions.push(ExceptionRegion {
                try_blocks,
                catch_handler,
                finally_block,
                try_start_idx: try_start,
                try_end_idx: try_end,
            });
        }

        // Merge overlapping regions if needed
        regions = self.merge_exception_regions(regions);

        Some(ExceptionAnalysis { regions })
    }

    /// Find all blocks within the given instruction range
    fn find_blocks_in_range(&self, start_idx: u32, end_idx: u32) -> Vec<NodeIndex> {
        let mut blocks = Vec::new();
        let start = InstructionIndex::from(start_idx);
        let end = InstructionIndex::from(end_idx);

        for node in self.graph().node_indices() {
            let block = &self.graph()[node];
            // Check if this block overlaps with the range
            if block.start_pc() < end && block.end_pc() > start {
                blocks.push(node);
            }
        }

        blocks
    }

    /// Find the block containing the given instruction index
    fn find_block_for_instruction(&self, instruction_idx: u32) -> Option<NodeIndex> {
        let target_idx = InstructionIndex::from(instruction_idx);

        for node in self.graph().node_indices() {
            let block = &self.graph()[node];
            if block.start_pc() <= target_idx && target_idx < block.end_pc() {
                return Some(node);
            }
        }
        None
    }

    /// Extract info from the Catch instruction (which is always the first instruction)
    fn extract_catch_info(&self, catch_block: NodeIndex) -> Option<u8> {
        use crate::generated::unified_instructions::UnifiedInstruction;

        let block = &self.graph()[catch_block];

        // The Catch instruction is always the first instruction in an exception handler
        let first_instr = block.instructions().first()?;

        if let UnifiedInstruction::Catch { operand_0 } = &first_instr.instruction {
            return Some(*operand_0);
        }

        None
    }

    /// Merge overlapping or nested exception regions and detect finally blocks
    fn merge_exception_regions(&self, mut regions: Vec<ExceptionRegion>) -> Vec<ExceptionRegion> {
        // Sort regions by start index, then by catch target
        regions.sort_by(|a, b| {
            a.try_start_idx.cmp(&b.try_start_idx).then_with(|| {
                // Sort by catch target to ensure deterministic ordering
                let a_target = a
                    .catch_handler
                    .as_ref()
                    .map(|h| h.catch_target_idx)
                    .unwrap_or(u32::MAX);
                let b_target = b
                    .catch_handler
                    .as_ref()
                    .map(|h| h.catch_target_idx)
                    .unwrap_or(u32::MAX);
                a_target.cmp(&b_target)
            })
        });

        log::debug!("Merging {} exception regions", regions.len());

        // Pattern detection for try-catch-finally:
        // When we have multiple handlers with the same try range, they typically represent:
        // 1. First handler: try → catch
        // 2. Second handler: try → finally
        // 3. Third handler (if exists): catch → finally

        if regions.len() == 3 {
            // Check for try-catch-finally pattern
            let (r0, r1, r2) = (&regions[0], &regions[1], &regions[2]);

            // Check if r0 and r1 have the same try range (both handle the try block)
            if r0.try_start_idx == r1.try_start_idx && r0.try_end_idx == r1.try_end_idx {
                // r0 is try→catch, r1 is try→finally
                // r2 should be catch→finally
                if let (Some(catch0), Some(finally1), Some(finally2)) =
                    (&r0.catch_handler, &r1.catch_handler, &r2.catch_handler)
                {
                    // Verify r2 covers the catch block
                    if r2.try_start_idx == catch0.catch_target_idx
                        && finally1.catch_block == finally2.catch_block
                    {
                        log::debug!("Detected try-catch-finally pattern:");
                        log::debug!("  Try: [{}, {})", r0.try_start_idx, r0.try_end_idx);
                        log::debug!("  Catch: block at {}", catch0.catch_target_idx);
                        log::debug!("  Finally: block at {}", finally1.catch_target_idx);

                        // Create merged region with try, catch, and finally
                        return vec![ExceptionRegion {
                            try_blocks: r0.try_blocks.clone(),
                            catch_handler: r0.catch_handler.clone(),
                            finally_block: Some(finally1.catch_block),
                            try_start_idx: r0.try_start_idx,
                            try_end_idx: r0.try_end_idx,
                        }];
                    }
                }
            }
        } else if regions.len() == 2 {
            // Check for try-finally (no catch) or try-catch patterns
            let (r0, r1) = (&regions[0], &regions[1]);

            if r0.try_start_idx == r1.try_start_idx && r0.try_end_idx == r1.try_end_idx {
                // Both handlers cover the same try range
                // This could be try-catch with implicit finally or try-finally
                if let (Some(_handler0), Some(handler1)) = (&r0.catch_handler, &r1.catch_handler) {
                    // Check if the second handler looks like a finally block
                    if self.has_finally_pattern(handler1.catch_block) {
                        log::debug!("Detected try-finally pattern (2 handlers, same range)");
                        return vec![ExceptionRegion {
                            try_blocks: r0.try_blocks.clone(),
                            catch_handler: None,
                            finally_block: Some(handler1.catch_block),
                            try_start_idx: r0.try_start_idx,
                            try_end_idx: r0.try_end_idx,
                        }];
                    }
                }
            }
        } else if regions.len() == 1 {
            // Single handler - check if it's actually a finally block
            let mut region = regions[0].clone();
            if let Some(ref handler) = region.catch_handler {
                if self.has_finally_pattern(handler.catch_block) {
                    log::debug!(
                        "Detected finally block (single handler) at {:?}",
                        handler.catch_block
                    );
                    // Convert catch handler to finally block
                    region.finally_block = Some(handler.catch_block);
                    region.catch_handler = None;
                }
            }
            return vec![region];
        }

        // Default: return regions as-is if no pattern matched
        regions
    }

    /// Check if a block has the finally pattern (Catch followed by code and Throw of same register)
    fn has_finally_pattern(&self, block: NodeIndex) -> bool {
        use crate::generated::unified_instructions::UnifiedInstruction;

        let block_data = &self.graph()[block];
        let instructions = block_data.instructions();

        if instructions.len() < 3 {
            return false;
        }

        // First instruction should be Catch
        let catch_register = match instructions.first().map(|i| &i.instruction) {
            Some(UnifiedInstruction::Catch { operand_0 }) => *operand_0,
            _ => return false,
        };

        // Last instruction should be Throw of the same register
        let throws_same_register = match instructions.last().map(|i| &i.instruction) {
            Some(UnifiedInstruction::Throw { operand_0 }) => *operand_0 == catch_register,
            _ => false,
        };

        // Must throw the same register that was caught (finally pattern)
        // If it throws a different register, it's a catch block that creates a new exception
        throws_same_register
    }
}

/// Check if a block is within an exception handler
pub fn is_exception_handler_block(
    analysis: &ExceptionAnalysis,
    block: NodeIndex,
) -> Option<&ExceptionRegion> {
    for region in &analysis.regions {
        if region.try_blocks.contains(&block) {
            return Some(region);
        }
        if let Some(ref catch) = region.catch_handler {
            if catch.catch_block == block {
                return Some(region);
            }
        }
        if let Some(finally) = region.finally_block {
            if finally == block {
                return Some(region);
            }
        }
    }
    None
}

/// Check if a block is a catch block
pub fn is_catch_block(analysis: &ExceptionAnalysis, block: NodeIndex) -> bool {
    for region in &analysis.regions {
        if let Some(ref catch) = region.catch_handler {
            if catch.catch_block == block {
                return true;
            }
        }
    }
    false
}

/// Get all catch blocks from the analysis
pub fn get_catch_blocks(analysis: &ExceptionAnalysis) -> HashSet<NodeIndex> {
    let mut catch_blocks = HashSet::new();
    for region in &analysis.regions {
        if let Some(ref catch) = region.catch_handler {
            catch_blocks.insert(catch.catch_block);
        }
    }
    catch_blocks
}
