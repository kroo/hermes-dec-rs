#!/bin/bash

# Script to combine individual PDFs into single large PDFs for each HBC file
# This creates one PDF per HBC file with each function on its own page

DATA_DIR="data"
INDIVIDUAL_DIR="data/individual_pdfs"
OUTPUT_DIR="data/combined_pdfs"

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

# Check if pdftk is available
if ! command -v pdftk &> /dev/null; then
    echo "Error: pdftk is required but not installed."
    echo "Install with: brew install pdftk-java"
    exit 1
fi

# Process each HBC file
for dot_file in "$DATA_DIR"/*.dot; do
    if [ ! -f "$dot_file" ]; then
        echo "No DOT files found in $DATA_DIR"
        exit 1
    fi
    
    base_name=$(basename "$dot_file" .dot)
    echo "Processing $base_name..."
    
    # Find all individual PDFs for this HBC file
    individual_pdfs=$(ls "$INDIVIDUAL_DIR/${base_name}_function_"*.pdf 2>/dev/null | sort -V)
    
    if [ -z "$individual_pdfs" ]; then
        echo "  No individual PDFs found for $base_name"
        continue
    fi
    
    # Count functions
    function_count=$(echo "$individual_pdfs" | wc -l)
    echo "  Found $function_count functions"
    
    # Create combined PDF
    output_pdf="$OUTPUT_DIR/${base_name}_all_functions.pdf"
    
    # Use pdftk to combine PDFs
    pdftk $individual_pdfs cat output "$output_pdf"
    
    if [ $? -eq 0 ]; then
        echo "  ✓ Generated: ${base_name}_all_functions.pdf"
    else
        echo "  ✗ Failed to generate combined PDF for $base_name"
    fi
done

echo
echo "=== Summary ==="
echo "Combined PDFs generated in $OUTPUT_DIR:"
ls -la "$OUTPUT_DIR"/*.pdf 2>/dev/null | sed 's/^/  /' || echo "  No combined PDFs found"

echo
echo "📊 File breakdown:"
for pdf in "$OUTPUT_DIR"/*.pdf; do
    if [ -f "$pdf" ]; then
        base_name=$(basename "$pdf" _all_functions.pdf)
        function_count=$(ls "$INDIVIDUAL_DIR/${base_name}_function_"*.pdf 2>/dev/null | wc -l)
        file_size=$(ls -lh "$pdf" | awk '{print $5}')
        echo "  $base_name: $function_count functions, $file_size"
    fi
done 