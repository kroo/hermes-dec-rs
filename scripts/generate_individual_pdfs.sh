#!/bin/bash

# Script to generate individual PDFs for each function from DOT files
# This extracts each subgraph and creates a separate PDF for each function

DATA_DIR="data"
OUTPUT_DIR="data/individual_pdfs"

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

# Process each DOT file
for dot_file in "$DATA_DIR"/*.dot; do
    if [ ! -f "$dot_file" ]; then
        echo "No DOT files found in $DATA_DIR"
        exit 1
    fi
    
    base_name=$(basename "$dot_file" .dot)
    echo "Processing $base_name..."
    
    # Find all function numbers in the file
    function_numbers=$(grep -o 'cluster_function_[0-9]*' "$dot_file" | sed 's/cluster_function_//' | sort -u)
    
    for func_num in $function_numbers; do
        echo "  Extracting function $func_num..."
        
        # Create individual DOT file for this function
        output_dot="$OUTPUT_DIR/${base_name}_function_${func_num}.dot"
        
        # Write header
        cat > "$output_dot" << EOF
// Function $func_num from $base_name
// Generated from $dot_file

digraph {
  rankdir=TB;
  node [shape=box, fontname="monospace"];

EOF
        
        # Extract the subgraph content (without the subgraph wrapper)
        sed -n "/subgraph cluster_function_${func_num}/,/^  }$/p" "$dot_file" | \
        sed '1d;$d' | \
        sed 's/^    /  /' | \
        sed '/^  subgraph cluster_function_/,$d' | \
        sed '/^  }$/d' >> "$output_dot"
        
        # Close the digraph
        echo "}" >> "$output_dot"
        
        # Generate PDF
        output_pdf="$OUTPUT_DIR/${base_name}_function_${func_num}.pdf"
        dot -Tpdf "$output_dot" -o "$output_pdf"
        
        echo "    Generated: ${base_name}_function_${func_num}.pdf"
    done
    
    echo "✓ Completed $base_name"
done

echo "All individual PDFs generated in $OUTPUT_DIR" 