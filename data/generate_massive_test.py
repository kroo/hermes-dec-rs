#!/usr/bin/env python3
"""
Generate a massive JavaScript test file that forces buffer indices beyond 65535
to test UInt16 vs UInt32 buffer index variants.
"""

def generate_massive_js_file():
    lines = []
    
    # Generate a HUGE number of unique strings to force large string tables
    # and large buffer sizes
    num_unique_strings = 10000  # This should definitely push us past limits
    
    lines.append("// Massive test to force buffer index overflow")
    lines.append("// This creates arrays with many unique strings to grow buffers")
    lines.append("")
    
    # Create many large arrays with unique strings
    for array_idx in range(100):  # 100 arrays
        lines.append(f"let massiveArray{array_idx} = [")
        
        # Each array has 200 unique strings
        for str_idx in range(200):
            global_str_idx = array_idx * 200 + str_idx
            string_content = f"unique_string_{global_str_idx}_with_very_long_content_to_increase_buffer_size_significantly_and_force_overflow_beyond_65535_bytes_{chr(65 + (global_str_idx % 26))}{chr(97 + (global_str_idx % 26))}{global_str_idx}"
            
            comma = "," if str_idx < 199 else ""
            lines.append(f'    "{string_content}"{comma}')
        
        lines.append("];")
        lines.append("")
    
    # Create arrays with many integers to create large integer sequences
    lines.append("// Large integer arrays to test sequence limits")
    for i in range(20):
        lines.append(f"let largeIntArray{i} = [")
        
        # Create arrays larger than 4095 to force sequence fragmentation
        for j in range(5000):  # 5000 integers per array
            comma = "," if j < 4999 else ""
            lines.append(f"    {i * 5000 + j}{comma}")
        
        lines.append("];")
        lines.append("")
    
    # Create objects with many properties using unique string keys
    lines.append("// Large objects to test key/value buffer limits")
    for obj_idx in range(50):
        lines.append(f"let massiveObject{obj_idx} = {{")
        
        for prop_idx in range(500):  # 500 properties per object
            global_prop_idx = obj_idx * 500 + prop_idx
            key = f"property_{global_prop_idx}_with_extremely_long_name_to_maximize_buffer_usage_and_force_index_overflow_{chr(65 + (global_prop_idx % 26))}{global_prop_idx}"
            
            comma = "," if prop_idx < 499 else ""
            lines.append(f'    "{key}": {global_prop_idx}{comma}')
        
        lines.append("};")
        lines.append("")
    
    # Mix all tag types in one massive array
    lines.append("// Mixed tag types in one massive array")
    lines.append("let mixedMassiveArray = [")
    
    for i in range(20000):  # 20,000 elements
        element_type = i % 8
        
        if element_type == 0:  # null
            element = "null"
        elif element_type == 1:  # true
            element = "true"
        elif element_type == 2:  # false
            element = "false"
        elif element_type == 3:  # number (double)
            element = f"{i + 0.123456}"
        elif element_type == 4:  # integer
            element = str(i)
        elif element_type == 5:  # string (will become various string tags)
            element = f'"mixed_string_{i}_content_{chr(65 + (i % 26))}"'
        elif element_type == 6:  # large integer
            element = str(i * 1000000)
        else:  # special numbers
            specials = ["Infinity", "-Infinity", "NaN", "Math.PI", "Math.E"]
            element = specials[i % len(specials)]
        
        comma = "," if i < 19999 else ""
        lines.append(f"    {element}{comma}")
    
    lines.append("];")
    lines.append("")
    
    # Force usage so nothing gets optimized away
    lines.append("// Force usage to prevent optimization")
    lines.append("function useMassiveData() {")
    lines.append("    let total = 0;")
    
    for i in range(100):
        lines.append(f"    total += massiveArray{i}.length;")
    
    for i in range(20):
        lines.append(f"    total += largeIntArray{i}.length;")
    
    for i in range(50):
        lines.append(f"    total += Object.keys(massiveObject{i}).length;")
    
    lines.append("    total += mixedMassiveArray.length;")
    lines.append("    console.log('Total elements:', total);")
    lines.append("    return total;")
    lines.append("}")
    lines.append("")
    lines.append("// Call to ensure usage")
    lines.append("useMassiveData();")
    
    return "\n".join(lines)

def main():
    js_content = generate_massive_js_file()
    
    output_file = "massive_literals.js"
    with open(output_file, 'w') as f:
        f.write(js_content)
    
    print(f"Generated {output_file}")
    print(f"File size: {len(js_content):,} bytes")
    print("Lines of code:", len(js_content.split('\n')))
    print("Estimated string count: ~20,000+ unique strings")
    print("This should definitely exceed UInt16 buffer limits!")
    print("\nTo compile and analyze:")
    print(f"bin/hermesc -emit-binary -out hbc_testcases/massive_literals.hbc hbc_testcases/{output_file}")
    print(f"bin/hbcdump -pretty-disassemble -c 'disassemble;quit' hbc_testcases/massive_literals.hbc > hbc_testcases/massive_literals.hasm")

if __name__ == "__main__":
    main()