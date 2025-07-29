#!/bin/bash

# Script to reorganize CFG visualization files
# Moves visualization files from data/ to data/cfg_visualizations/[filename]/
# Also moves combined pdfs and removes intermediate folders

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if we're in the right directory
if [ ! -d "data" ]; then
    print_error "This script must be run from the project root directory"
    exit 1
fi

# Create cfg_visualizations directory if it doesn't exist
if [ ! -d "data/cfg_visualizations" ]; then
    print_info "Creating data/cfg_visualizations directory"
    mkdir -p data/cfg_visualizations
fi

# Function to move visualization files for a given base name
move_visualization_files() {
    local base_name=$1
    local target_dir="data/cfg_visualizations/$base_name"
    
    # Create target directory if it doesn't exist
    if [ ! -d "$target_dir" ]; then
        print_info "Creating directory: $target_dir"
        mkdir -p "$target_dir"
    fi
    
    # Move visualization files (dot, pdf, png, svg) - both regular and analysis
    local moved_count=0
    
    # Move regular files
    for ext in dot pdf png svg; do
        local source_file="data/$base_name.$ext"
        local target_file="$target_dir/$base_name.$ext"
        
        if [ -f "$source_file" ]; then
            print_info "Moving $source_file to $target_file"
            mv "$source_file" "$target_file"
            ((moved_count++))
        fi
    done
    
    # Move analysis files
    for ext in dot pdf png svg; do
        local source_file="data/${base_name}_analysis.$ext"
        local target_file="$target_dir/${base_name}_analysis.$ext"
        
        if [ -f "$source_file" ]; then
            print_info "Moving $source_file to $target_file"
            mv "$source_file" "$target_file"
            ((moved_count++))
        fi
    done
    
    if [ $moved_count -gt 0 ]; then
        print_info "Moved $moved_count files for $base_name"
    else
        print_warning "No visualization files found for $base_name"
    fi
}

# Function to move combined PDFs
move_combined_pdfs() {
    local base_name=$1
    local target_dir="data/cfg_visualizations/$base_name"
    
    # Create target directory if it doesn't exist
    if [ ! -d "$target_dir" ]; then
        print_info "Creating directory: $target_dir"
        mkdir -p "$target_dir"
    fi
    
    local source_file="data/combined_pdfs/${base_name}_all_functions.pdf"
    local target_file="$target_dir/${base_name}_all_functions.pdf"
    
    if [ -f "$source_file" ]; then
        print_info "Moving combined PDF: $source_file to $target_file"
        mv "$source_file" "$target_file"
        return 0
    else
        print_warning "Combined PDF not found: $source_file"
    fi
    return 1
}

# List of base names to process (extracted from the data directory)
base_names=(
    "array_constants_v90"
    "array_constants_v96"
    "bigints_v96"
    "cjs_v96"
    "cjs-show-source"
    "complex_control_flow"
    "dense_switch_test"
    "flow_control"
    "hermes_dec_sample"
    "regex_test"
)

print_info "Starting reorganization of CFG visualization files..."

# Process each base name
for base_name in "${base_names[@]}"; do
    print_info "Processing: $base_name"
    move_visualization_files "$base_name"
    move_combined_pdfs "$base_name"
done

# Remove intermediate directories if they're empty
print_info "Cleaning up intermediate directories..."

if [ -d "data/combined_pdfs" ]; then
    if [ -z "$(ls -A data/combined_pdfs)" ]; then
        print_info "Removing empty combined_pdfs directory"
        rmdir data/combined_pdfs
    else
        print_warning "combined_pdfs directory is not empty, skipping removal"
        print_warning "Remaining files in combined_pdfs:"
        ls -la data/combined_pdfs/
    fi
fi

if [ -d "data/individual_pdfs" ]; then
    rm -rf data/individual_pdfs
    print_info "Removed individual_pdfs directory"
fi

print_info "Reorganization complete!"

# Show summary of what was moved
print_info "Summary of directories in data/cfg_visualizations/:"
ls -la data/cfg_visualizations/

print_info "Remaining files in data/ directory:"
ls -la data/*.{dot,pdf,png,svg} 2>/dev/null || print_warning "No remaining visualization files in data/"
