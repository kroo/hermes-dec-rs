#!/usr/bin/env python3
"""
Generate a comprehensive JavaScript test file that forces Hermes to create
serialized literals with all tag types and edge cases.
"""

import json
import struct

def generate_js_file():
    lines = []
    
    # Generate many unique strings to force different string tag types
    strings = []
    for i in range(500):  # Generate 500 unique strings to push string IDs beyond 255
        strings.append(f"unique_string_{i}_with_content_{i*7}_{chr(65 + (i % 26))}")
    
    # Test 1: Array literals with all basic tag types
    lines.append("// Test 1: Array with all basic tag types")
    lines.append("let basicTagTypes = [")
    lines.append("    null,")           # NullTag
    lines.append("    true,")           # TrueTag  
    lines.append("    false,")          # FalseTag
    lines.append("    3.14159,")        # NumberTag (double)
    lines.append("    42,")             # IntegerTag
    lines.append("    -1,")             # IntegerTag (negative)
    lines.append("    2147483647,")     # IntegerTag (max int32)
    lines.append("    -2147483648,")    # IntegerTag (min int32)
    lines.append("    2147483648,")     # NumberTag (beyond int32)
    lines.append("    Infinity,")       # NumberTag (special)
    lines.append("    -Infinity,")      # NumberTag (special)
    lines.append("    NaN,")            # NumberTag (special)
    
    # Add strings that will use different string tag types
    for i in range(10):
        lines.append(f'    "str_{i}",')  # ByteStringTag initially
    
    lines.append("];")
    lines.append("")
    
    # Test 2: Arrays that test sequence boundaries
    lines.append("// Test 2: Sequence boundary tests")
    
    # Exactly 15 elements (short format boundary)
    lines.append("let exactly15Nulls = [")
    lines.append("    " + ", ".join(["null"] * 15))
    lines.append("];")
    lines.append("")
    
    # Exactly 16 elements (forces long format)
    lines.append("let exactly16Nulls = [")
    lines.append("    " + ", ".join(["null"] * 16))
    lines.append("];")
    lines.append("")
    
    # 4095 elements (max sequence length)
    lines.append("let max4095Integers = [")
    for i in range(0, 4095, 50):  # Generate in chunks for readability
        chunk = ", ".join([str(j) for j in range(i, min(i + 50, 4095))])
        if i + 50 >= 4095:
            lines.append(f"    {chunk}")
        else:
            lines.append(f"    {chunk},")
    lines.append("];")
    lines.append("")
    
    # 4096 elements (requires multiple sequences)
    lines.append("let over4095Integers = [")
    for i in range(0, 4096, 50):
        chunk = ", ".join([str(j) for j in range(i, min(i + 50, 4096))])
        if i + 50 >= 4096:
            lines.append(f"    {chunk}")
        else:
            lines.append(f"    {chunk},")
    lines.append("];")
    lines.append("")
    
    # Test 3: Mixed type arrays that create fragmented sequences
    lines.append("// Test 3: Mixed types creating fragmented sequences")
    lines.append("let fragmentedArray = [")
    for i in range(100):
        lines.append(f"    {i},")              # IntegerTag
        lines.append(f"    null,")             # NullTag
        lines.append(f"    {i + 0.5},")        # NumberTag
        lines.append(f'    "frag_{i}",')       # StringTag
        if i % 10 == 0:
            lines.append(f"    true,")          # TrueTag
        else:
            lines.append(f"    false,")         # FalseTag
    lines.append("];")
    lines.append("")
    
    # Test 4: Object literals to test key/value buffers
    lines.append("// Test 4: Object literals with many properties")
    lines.append("let largeObject = {")
    for i in range(200):
        key = f"property_{i}_with_long_name_to_test_string_table"
        lines.append(f'    "{key}": {i},')
    lines.append("};")
    lines.append("")
    
    # Test 5: Objects with mixed value types
    lines.append("// Test 5: Object with mixed value types")
    lines.append("let mixedValueObject = {")
    for i in range(50):
        lines.append(f'    "null_prop_{i}": null,')
        lines.append(f'    "bool_prop_{i}": {str(i % 2 == 0).lower()},')
        lines.append(f'    "int_prop_{i}": {i},')
        lines.append(f'    "double_prop_{i}": {i + 0.333},')
        lines.append(f'    "string_prop_{i}": "value_{i}_content",')
    lines.append("};")
    lines.append("")
    
    # Test 6: Many unique strings to force different string tag types
    lines.append("// Test 6: Many unique strings to test string table growth")
    lines.append("let manyUniqueStrings = [")
    for i, s in enumerate(strings):
        comma = "," if i < len(strings) - 1 else ""
        lines.append(f'    "{s}"{comma}')
    lines.append("];")
    lines.append("")
    
    # Test 7: Nested structures
    lines.append("// Test 7: Nested arrays and objects")
    lines.append("let nestedStructure = {")
    lines.append("    level1: {")
    lines.append("        level2: {")
    lines.append("            arrays: [")
    for i in range(20):
        lines.append(f"                [{i}, {i+1}, {i+2}],")
    lines.append("            ]")
    lines.append("        }")
    lines.append("    }")
    lines.append("};")
    lines.append("")
    
    # Test 8: Edge case numbers
    lines.append("// Test 8: Edge case numbers")
    lines.append("let edgeNumbers = [")
    lines.append("    0,")               # Zero
    lines.append("    -0,")              # Negative zero
    lines.append("    1,")               # One
    lines.append("    -1,")              # Negative one
    lines.append("    255,")             # Byte boundary
    lines.append("    256,")             # Beyond byte
    lines.append("    65535,")           # Short boundary
    lines.append("    65536,")           # Beyond short
    lines.append("    2147483647,")      # Max int32
    lines.append("    -2147483648,")     # Min int32
    lines.append("    2147483648,")      # Beyond int32
    lines.append("    4294967295,")      # Max uint32
    lines.append("    4294967296,")      # Beyond uint32
    lines.append("    Number.MAX_SAFE_INTEGER,")
    lines.append("    Number.MIN_SAFE_INTEGER,")
    lines.append("    Number.EPSILON,")
    lines.append("    Math.PI,")
    lines.append("    Math.E,")
    lines.append("];")
    lines.append("")
    
    # Make sure everything is actually used so it doesn't get optimized away
    lines.append("// Force usage to prevent optimization")
    lines.append("function useEverything() {")
    lines.append("    console.log(basicTagTypes.length);")
    lines.append("    console.log(exactly15Nulls.length);")
    lines.append("    console.log(exactly16Nulls.length);")
    lines.append("    console.log(max4095Integers.length);")
    lines.append("    console.log(over4095Integers.length);")
    lines.append("    console.log(fragmentedArray.length);")
    lines.append("    console.log(Object.keys(largeObject).length);")
    lines.append("    console.log(Object.keys(mixedValueObject).length);")
    lines.append("    console.log(manyUniqueStrings.length);")
    lines.append("    console.log(nestedStructure.level1.level2.arrays.length);")
    lines.append("    console.log(edgeNumbers.length);")
    lines.append("    return 'all used';")
    lines.append("}")
    lines.append("")
    lines.append("// Call the function to ensure usage")
    lines.append("useEverything();")
    
    return "\n".join(lines)

def main():
    js_content = generate_js_file()
    
    output_file = "comprehensive_literals.js"
    with open(output_file, 'w') as f:
        f.write(js_content)
    
    print(f"Generated {output_file}")
    print(f"File size: {len(js_content)} bytes")
    print("Lines of code:", len(js_content.split('\n')))
    print("\nTo compile and analyze:")
    print(f"bin/hermesc -emit-binary -out hbc_testcases/comprehensive_literals.hbc hbc_testcases/{output_file}")
    print(f"bin/hbcdump -pretty-disassemble -c 'disassemble;quit' hbc_testcases/comprehensive_literals.hbc > hbc_testcases/comprehensive_literals.hasm")

if __name__ == "__main__":
    main()