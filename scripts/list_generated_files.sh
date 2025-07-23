#!/bin/bash

# Script to list all generated visualization files

echo "=== CFG Visualization Files Generated ==="
echo

echo "📁 Combined DOT files (all functions in one file):"
ls -la data/*.dot 2>/dev/null | wc -l | xargs echo "  Total DOT files:"
ls data/*.dot 2>/dev/null | sed 's/^/  /' || echo "  No DOT files found"

echo
echo "📄 Combined PDF files (all functions in one file):"
ls -la data/*.pdf 2>/dev/null | wc -l | xargs echo "  Total PDF files:"
ls data/*.pdf 2>/dev/null | sed 's/^/  /' || echo "  No PDF files found"

echo
echo "🖼️  Combined PNG files (all functions in one file):"
ls -la data/*.png 2>/dev/null | wc -l | xargs echo "  Total PNG files:"
ls data/*.png 2>/dev/null | sed 's/^/  /' || echo "  No PNG files found"

echo
echo "🎨 Combined SVG files (all functions in one file):"
ls -la data/*.svg 2>/dev/null | wc -l | xargs echo "  Total SVG files:"
ls data/*.svg 2>/dev/null | sed 's/^/  /' || echo "  No SVG files found"

echo
echo "📋 Individual PDF files (one function per file):"
if [ -d "data/individual_pdfs" ]; then
    ls -la data/individual_pdfs/*.pdf 2>/dev/null | wc -l | xargs echo "  Total individual PDF files:"
    echo "  Files by HBC file:"
    for dot_file in data/*.dot; do
        if [ -f "$dot_file" ]; then
            base_name=$(basename "$dot_file" .dot)
            count=$(ls data/individual_pdfs/${base_name}_function_*.pdf 2>/dev/null | wc -l)
            echo "    $base_name: $count functions"
        fi
    done
else
    echo "  No individual PDFs directory found"
fi

echo
echo "📚 Combined individual PDFs (all functions in one file, one per page):"
if [ -d "data/combined_pdfs" ]; then
    ls -la data/combined_pdfs/*.pdf 2>/dev/null | wc -l | xargs echo "  Total combined PDF files:"
    ls data/combined_pdfs/*.pdf 2>/dev/null | sed 's/^/  /' || echo "  No combined PDF files found"
else
    echo "  No combined PDFs directory found"
fi

echo
echo "📊 Summary:"
echo "  - Combined DOT files show all functions in a single visualization"
echo "  - Individual PDFs show each function on its own page"
echo "  - Combined individual PDFs show all functions in one file, one per page"
echo "  - Use combined DOT files for overview, individual PDFs for detailed analysis"
echo "  - Use combined individual PDFs for complete file analysis" 