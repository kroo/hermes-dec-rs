#!/bin/bash

# Master script to generate all types of CFG visualizations
# This script runs all the visualization generation steps in sequence

echo "🚀 Generating all CFG visualizations..."
echo

# Step 1: Run the tests to generate DOT files
echo "📝 Step 1: Running CFG tests to generate DOT files..."
cargo test test_cfg_integration_with_hbc_files -- --nocapture
if [ $? -ne 0 ]; then
    echo "❌ Tests failed, stopping visualization generation"
    exit 1
fi
echo "✅ DOT files generated"
echo

# Step 2: Generate combined visualizations (PDF, PNG, SVG)
echo "🖼️  Step 2: Generating combined visualizations..."
for dot_file in data/*.dot; do
    if [ -f "$dot_file" ]; then
        base_name=$(basename "$dot_file" .dot)
        echo "  Processing $base_name..."
        
        # Generate PDF
        dot -Tpdf "$dot_file" -o "data/${base_name}.pdf" 2>/dev/null
        echo "    ✓ PDF generated"
        
        # Generate PNG
        dot -Tpng "$dot_file" -o "data/${base_name}.png" 2>/dev/null
        echo "    ✓ PNG generated"
        
        # Generate SVG
        dot -Tsvg "$dot_file" -o "data/${base_name}.svg" 2>/dev/null
        echo "    ✓ SVG generated"
    fi
done
echo "✅ Combined visualizations generated"
echo

# Step 3: Generate individual PDFs
echo "📋 Step 3: Generating individual PDFs..."
./scripts/generate_individual_pdfs.sh
if [ $? -ne 0 ]; then
    echo "❌ Individual PDF generation failed"
    exit 1
fi
echo "✅ Individual PDFs generated"
echo

# Step 4: Combine individual PDFs
echo "📚 Step 4: Combining individual PDFs..."
./scripts/combine_individual_pdfs.sh
if [ $? -ne 0 ]; then
    echo "❌ PDF combination failed"
    exit 1
fi
echo "✅ Combined individual PDFs generated"
echo

# Step 5: Reorganize files into subdirectories
echo "🗂️  Step 5: Reorganizing files into subdirectories..."
./scripts/reorganize_cfg_visualizations.sh
if [ $? -ne 0 ]; then
    echo "❌ File reorganization failed"
    exit 1
fi
echo "✅ Files reorganized"
echo

# Step 6: Show summary
echo "📊 Step 6: Generating summary..."
./scripts/list_generated_files.sh

echo
echo "🎉 All visualizations generated successfully!"
echo
echo "📁 Generated files:"
echo "  - data/cfg_visualizations/*/: Organized CFG visualizations by file"
echo "    Each subdirectory contains:"
echo "    - *.dot: Combined DOT file"
echo "    - *.pdf: Combined PDF file"
echo "    - *.png: Combined PNG file"
echo "    - *.svg: Combined SVG file"
echo "    - *_all_functions.pdf: Combined individual PDFs"
echo
echo "📖 Usage:"
echo "  - Use combined DOT/PDF/PNG/SVG for overview"
echo "  - Use combined individual PDFs for complete file analysis" 