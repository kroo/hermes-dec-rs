#!/bin/bash

# Script to list all generated visualization files

echo "=== CFG Visualization Files Generated ==="
echo

if [ -d "data/cfg_visualizations" ]; then
    echo "📁 Organized CFG visualizations:"
    echo "  Total directories: $(ls -d data/cfg_visualizations/*/ 2>/dev/null | wc -l)"
    echo "  Directory structure:"
    for dir in data/cfg_visualizations/*/; do
        if [ -d "$dir" ]; then
            base=$(basename "$dir")
            echo "    ${base}/"
            ls -la "$dir" | grep -E '\.(dot|pdf|png|svg)$' | awk '{print "      " $9}' || echo "      (no files)"
        fi
    done
else
    echo "📁 CFG visualizations directory not found"
fi

echo
echo "📊 Summary:"
echo "  - Each subdirectory contains all visualizations for one HBC file"
echo "  - *.dot: Combined DOT file with all functions"
echo "  - *.pdf: Combined PDF visualization"
echo "  - *.png: Combined PNG visualization"
echo "  - *.svg: Combined SVG visualization"
echo "  - *_all_functions.pdf: Combined individual PDFs (one function per page)"
echo "  - Use combined files for overview"
echo "  - Use combined individual PDFs for detailed analysis" 