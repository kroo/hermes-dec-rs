# DuplicatedSSAValue Design

## Problem Statement

When decompiling switch statements, we often need to duplicate blocks of code into multiple case groups. This creates a fundamental issue where the same SSA value from the original bytecode needs to be handled differently in each duplication context:

1. **Variable declarations**: The first occurrence should be `const var1_d = "value"`, subsequent occurrences should be assignments `var1_d = "value"`
2. **Variable naming**: Different duplications may need different variable names to avoid conflicts
3. **Elimination tracking**: An SSA value might be eliminated in one context but not another

## Core Design

### Types

```rust
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct DuplicatedSSAValue {
    pub original: SSAValue,
    pub duplication_context: Option<DuplicationContext>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DuplicationContext {
    SwitchBlockDuplication {
        case_group_keys: Vec<CaseKey>,
    },
    // Future: could add LoopUnrolling, InlineExpansion, etc.
}
```

### Constructors

```rust
impl DuplicatedSSAValue {
    /// Create an original (non-duplicated) SSA value
    pub fn original(ssa_value: SSAValue) -> Self {
        Self {
            original: ssa_value,
            duplication_context: None,
        }
    }
    
    /// Create a duplicated SSA value for switch block duplication
    /// The case group uniquely identifies this duplication context
    pub fn switch_duplicated(ssa_value: SSAValue, case_group: &CaseGroup) -> Self {
        Self {
            original: ssa_value,
            duplication_context: Some(DuplicationContext::SwitchBlockDuplication {
                case_group_keys: case_group.keys.clone(),
            }),
        }
    }
}

impl From<SSAValue> for DuplicatedSSAValue {
    fn from(ssa_value: SSAValue) -> Self {
        Self::original(ssa_value)
    }
}
```

### Helper Methods

```rust
impl DuplicatedSSAValue {
    /// Check if this is a duplicated value
    pub fn is_duplicated(&self) -> bool {
        self.duplication_context.is_some()
    }
    
    /// Get the original SSA value
    pub fn original(&self) -> &SSAValue {
        &self.original
    }
    
    /// Get duplication context if this is duplicated
    pub fn duplication_context(&self) -> Option<&DuplicationContext> {
        self.duplication_context.as_ref()
    }
    
    /// Get a unique identifier based on case group
    pub fn unique_id(&self) -> String {
        match &self.duplication_context {
            None => format!("r{}_{}", self.original.register, self.original.version),
            Some(DuplicationContext::SwitchBlockDuplication { case_group_keys }) => {
                let case_id = Self::case_group_to_id(case_group_keys);
                format!("r{}_{}_{}", self.original.register, self.original.version, case_id)
            }
        }
    }
    
    /// Convert case group keys to a stable, short identifier
    fn case_group_to_id(case_keys: &[CaseKey]) -> String {
        case_keys.iter()
            .map(|k| match k {
                CaseKey::Number(n) => (n.0 as i32).to_string(),
                CaseKey::String(s) => format!("s{}", s.len()), // "s" + length to avoid long names
                CaseKey::Boolean(true) => "t".to_string(),
                CaseKey::Boolean(false) => "f".to_string(),
                CaseKey::Null => "n".to_string(),
                CaseKey::Undefined => "u".to_string(),
            })
            .collect::<Vec<_>>()
            .join("_")
    }
    
    /// Get a descriptive name for debugging
    pub fn context_description(&self) -> String {
        match &self.duplication_context {
            None => "original".to_string(),
            Some(DuplicationContext::SwitchBlockDuplication { case_group_keys }) => {
                let keys_str = case_group_keys.iter()
                    .map(|k| match k {
                        CaseKey::Number(n) => n.0.to_string(),
                        CaseKey::String(s) => format!("\"{}\"", s),
                        CaseKey::Boolean(b) => b.to_string(),
                        CaseKey::Null => "null".to_string(),
                        CaseKey::Undefined => "undefined".to_string(),
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                format!("switch_cases[{}]", keys_str)
            }
        }
    }
    
    /// Check if this represents the same duplication context as another
    pub fn same_duplication_context(&self, other: &DuplicatedSSAValue) -> bool {
        self.duplication_context == other.duplication_context
    }
}
```

## Integration Points

### SSAUsageTracker Updates

The `SSAUsageTracker` needs to be updated to work with `DuplicatedSSAValue` instead of `SSAValue`:

```rust
pub struct SSAUsageTracker {
    // Track elimination per duplicated value
    eliminated_values: HashSet<DuplicatedSSAValue>,
    
    // Track consumption per duplicated value
    consumed_uses: HashMap<DuplicatedSSAValue, HashSet<SSAUse>>,
    
    // Track function-level declarations to avoid duplicate const/let
    declared_in_function: HashSet<String>, // variable names, not SSA values
}

impl SSAUsageTracker {
    pub fn is_eliminated(&self, dup_ssa: &DuplicatedSSAValue) -> bool;
    pub fn mark_eliminated(&mut self, dup_ssa: DuplicatedSSAValue);
    pub fn should_declare_variable(&self, dup_ssa: &DuplicatedSSAValue, var_name: &str) -> bool;
    pub fn mark_variable_declared(&mut self, var_name: String);
    // ... other methods updated to use DuplicatedSSAValue
}
```

### Variable Naming Updates

Update `VariableManager` to handle duplicated values:

```rust
impl VariableManager {
    /// Get variable name for a duplicated SSA value
    pub fn get_variable_name_for_duplicated(&self, dup_ssa: &DuplicatedSSAValue) -> String {
        // Get base name from original SSA value
        let base_name = self.ssa_to_var.get(&dup_ssa.original)
            .cloned()
            .unwrap_or_else(|| format!("var{}", dup_ssa.original.register));
        
        match &dup_ssa.duplication_context {
            None => base_name,
            Some(DuplicationContext::SwitchBlockDuplication { case_group_keys }) => {
                // Create suffix from case group to ensure unique names
                let suffix = DuplicatedSSAValue::case_group_to_id(case_group_keys);
                format!("{}_{}", base_name, suffix)
            }
        }
    }
    
    /// Store mapping for a duplicated SSA value (if we need explicit tracking)
    pub fn set_duplicated_variable_name(&mut self, dup_ssa: DuplicatedSSAValue, name: String) {
        // Implementation depends on whether we need explicit storage
    }
}
```

### Switch Converter Updates

The switch converter will create `DuplicatedSSAValue` instances when generating case bodies:

```rust
impl SwitchConverter {
    fn generate_setup_instructions_for_case_group(
        &mut self,
        case_group: &CaseGroup,
        // ...
    ) -> Result<(), SwitchConversionError> {
        for setup_instr in &case_group.setup {
            // Create duplicated SSA value for this case group
            let dup_ssa = DuplicatedSSAValue::switch_duplicated(
                setup_instr.ssa_value.clone(),
                case_group
            );
            
            // Use dup_ssa for all subsequent operations:
            // - Check if eliminated
            // - Get variable name  
            // - Determine declaration vs assignment
            // - Track consumption
        }
    }
}
```

## Usage Examples

### Variable Naming Examples

For original SSA value `r1_5` mapped to base name `var1_d`:

```rust
// Case group [0, 1, 2]
let dup_1 = DuplicatedSSAValue::switch_duplicated(r1_5, &case_group_0_1_2);
// Variable name: "var1_d_0_1_2"

// Case group [3, 4] 
let dup_2 = DuplicatedSSAValue::switch_duplicated(r1_5, &case_group_3_4);
// Variable name: "var1_d_3_4"

// Original (non-duplicated)
let orig = DuplicatedSSAValue::original(r1_5);
// Variable name: "var1_d"
```

### Declaration vs Assignment Logic

```rust
// First time we see var1_d_0_1_2 in the function
if ssa_tracker.should_declare_variable(&dup_ssa, &var_name) {
    // Generate: const var1_d_0_1_2 = "value";
    ssa_tracker.mark_variable_declared(var_name.clone());
} else {
    // Generate: var1_d_0_1_2 = "value";
}
```

### Independent Elimination

```rust
// Same original SSA value, different elimination status
let dup_1 = DuplicatedSSAValue::switch_duplicated(r1_5, &case_group_0_1_2);
let dup_2 = DuplicatedSSAValue::switch_duplicated(r1_5, &case_group_3_4);

ssa_tracker.mark_eliminated(dup_1); // Only this duplication is eliminated
assert!(ssa_tracker.is_eliminated(&dup_1));
assert!(!ssa_tracker.is_eliminated(&dup_2)); // This one is still needed
```

## Migration Plan

1. **Add types**: Add `DuplicatedSSAValue` and `DuplicationContext` to the codebase
2. **Update SSAUsageTracker**: Modify to work with `DuplicatedSSAValue`
3. **Update VariableManager**: Add support for duplicated variable naming
4. **Update switch converter**: Use `DuplicatedSSAValue` when generating case bodies
5. **Test**: Verify that function 4 generates correct variable declarations and names

## Benefits

1. **Correct variable declarations**: No more duplicate `const` declarations in the same scope
2. **Clear variable names**: `var1_d_3_4` clearly indicates this is for cases 3-4
3. **Independent tracking**: Each duplication can have different elimination status
4. **Type safety**: Can't accidentally mix original and duplicated values
5. **Debuggability**: Clear context descriptions for logging
6. **Extensibility**: Easy to add other duplication types in the future
7. **SSA analysis stays pure**: Original SSA analysis remains unchanged, duplication is a code generation concern

## Expected Results

For function 4, instead of:
```javascript
case 3:
case 4:
    let var2 = "";           // ❌ Redeclaration error
    const var1_d = "mid ";   // ❌ Duplicate const name
    var0_e = var2 + var1_d;
    break;
```

We should generate:
```javascript
case 3:
case 4:
    var2_3_4 = "";           // ✅ Unique variable name, assignment not declaration
    const var1_d_3_4 = "mid "; // ✅ Unique const name
    var0_e = var2_3_4 + var1_d_3_4;
    break;
```