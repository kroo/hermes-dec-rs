use hermes_dec_rs::generated::generated_traits::{get_category_from_name, InstructionCategory};

#[test]
fn test_instruction_category_duplication() {
    // Test that the generated version has the expected InstructionCategory variants
    let generated_categories = vec![
        InstructionCategory::Debug,
        InstructionCategory::Arithmetic,
        InstructionCategory::TypeConversion,
        InstructionCategory::Logical,
        InstructionCategory::Call,
        InstructionCategory::ConstantLoad,
        InstructionCategory::Return,
        InstructionCategory::Variable,
        InstructionCategory::Other,
        InstructionCategory::Exception,
        InstructionCategory::PropertyAccess,
        InstructionCategory::Environment,
        InstructionCategory::Jump,
        InstructionCategory::Comparison,
        InstructionCategory::ObjectCreation,
        InstructionCategory::Bitwise,
    ];
    assert_eq!(generated_categories.len(), 16);

    // Test that the generated version can be used for category mapping
    let test_instruction = "LoadConstString";
    let generated_category = get_category_from_name(test_instruction);
    println!("LoadConstString category: {:?}", generated_category);
    assert!(matches!(
        generated_category,
        InstructionCategory::ConstantLoad
    ));
}

#[test]
fn test_generated_instruction_parsing() {
    // Test that the generated UnifiedInstruction can parse instructions
    // This demonstrates that the generated version is more complete
    // (This is a placeholder; actual parsing would require valid bytecode)
}

#[test]
fn test_category_mapping_consistency() {
    // Test that the generated version maps instruction names to categories consistently
    let test_instructions = vec![
        "LoadConstString",
        "Call",
        "Ret",
        "Add",
        "GetById",
        "NewObject",
    ];
    for instruction in test_instructions {
        let generated_category = get_category_from_name(instruction);
        match instruction {
            "LoadConstString" => assert!(matches!(
                generated_category,
                InstructionCategory::ConstantLoad
            )),
            "Call" => assert!(matches!(generated_category, InstructionCategory::Call)),
            "Ret" => assert!(matches!(generated_category, InstructionCategory::Return)),
            "Add" => assert!(matches!(
                generated_category,
                InstructionCategory::Arithmetic
            )),
            "GetById" => assert!(matches!(
                generated_category,
                InstructionCategory::PropertyAccess
            )),
            "NewObject" => assert!(matches!(
                generated_category,
                InstructionCategory::ObjectCreation
            )),
            _ => assert!(matches!(generated_category, InstructionCategory::Other)),
        }
    }
}

#[test]
fn test_generated_version_registry() {
    use hermes_dec_rs::generated::generated_traits::get_version_registry;
    let registry = get_version_registry();
    assert!(registry.is_supported(90));
    assert!(registry.is_supported(95));
    assert!(registry.is_supported(96));
}
