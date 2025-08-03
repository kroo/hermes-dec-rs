#!/usr/bin/env python3
"""
Script to analyze the Hermes opcodes table and identify instructions with destination registers.
"""

import re
import sys
import argparse
from html.parser import HTMLParser
from pathlib import Path

class TableParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.in_table = False
        self.in_row = False
        self.in_cell = False
        self.current_row = []
        self.rows = []
        self.cell_content = ''
        
    def handle_starttag(self, tag, attrs):
        if tag == 'table':
            self.in_table = True
        elif tag == 'tr' and self.in_table:
            self.in_row = True
            self.current_row = []
        elif tag in ['td', 'th'] and self.in_row:
            self.in_cell = True
            self.cell_content = ''
            
    def handle_endtag(self, tag):
        if tag == 'table':
            self.in_table = False
        elif tag == 'tr' and self.in_row:
            self.in_row = False
            if self.current_row:
                self.rows.append(self.current_row[:])
        elif tag in ['td', 'th'] and self.in_cell:
            self.in_cell = False
            self.current_row.append(self.cell_content.strip())
            
    def handle_data(self, data):
        if self.in_cell:
            self.cell_content += data.strip() + ' '

def analyze_opcodes():
    """Analyze the opcodes table to find instructions with destination registers."""
    html_file = Path(__file__).parent / "opcodes_table.html"
    
    with open(html_file, 'r') as f:
        content = f.read()
    
    parser = TableParser()
    parser.feed(content)
    
    dest_register_instructions = []
    no_dest_register_instructions = []
    
    for row in parser.rows:
        if len(row) >= 3:
            instruction_name = row[0].strip()
            doc = row[-1].strip()  # Last column is documentation
            
            # Skip header row and empty rows
            if not instruction_name or instruction_name == 'Instruction' or instruction_name.isdigit():
                continue
                
            # Check if instruction has Reg8 as first parameter (destination register)
            if re.match(r'^Reg8,', doc):
                dest_register_instructions.append(instruction_name)
            elif 'total size' in doc:  # Has documentation but no Reg8 destination
                no_dest_register_instructions.append(instruction_name)
    
    return sorted(set(dest_register_instructions)), sorted(set(no_dest_register_instructions))

def generate_rust_code(dest_instructions):
    """Generate Rust code for the get_target_register method."""
    print("// Instructions with destination registers (Reg8 as first parameter):")
    for instr in dest_instructions:
        rust_name = instr  # Assume the instruction name matches the Rust enum variant
        print(f"UnifiedInstruction::{rust_name} {{ operand_0, .. }} => Some(*operand_0),")

def get_instruction_details(instruction_name):
    """Get details for a specific instruction."""
    html_file = Path(__file__).parent / "opcodes_table.html"
    with open(html_file, 'r') as f:
        content = f.read()
    
    parser = TableParser()
    parser.feed(content)
    
    for row in parser.rows:
        if len(row) >= 3 and row[0].strip() == instruction_name:
            return row[-1]
    return None

def list_all_instructions():
    """List all instructions with their documentation."""
    html_file = Path(__file__).parent / "opcodes_table.html"
    with open(html_file, 'r') as f:
        content = f.read()
    
    parser = TableParser()
    parser.feed(content)
    
    instructions = []
    for row in parser.rows:
        if len(row) >= 3:
            instruction_name = row[0].strip()
            doc = row[-1].strip()
            
            # Skip header row and empty rows
            if not instruction_name or instruction_name == 'Instruction' or instruction_name.isdigit():
                continue
            
            # Only include rows that have documentation with operand info
            if 'total size' in doc:
                instructions.append((instruction_name, doc))
    
    return sorted(instructions)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Analyze Hermes opcodes table')
    parser.add_argument('instruction', nargs='?', help='Get details for a specific instruction')
    parser.add_argument('--list-all', action='store_true', help='List all instructions with documentation')
    parser.add_argument('--rust-code', action='store_true', help='Generate Rust code for get_target_register method')
    
    args = parser.parse_args()
    
    if args.list_all:
        instructions = list_all_instructions()
        for name, doc in instructions:
            print(f"{name}: {doc}")
    elif args.instruction:
        doc = get_instruction_details(args.instruction)
        if doc:
            print(f"{args.instruction}: {doc}")
        else:
            print(f"Instruction '{args.instruction}' not found")
    elif args.rust_code:
        dest_instructions, no_dest_instructions = analyze_opcodes()
        print("=== Rust code for get_target_register method ===")
        generate_rust_code(dest_instructions)
    else:
        # Default behavior: show human readable list
        instructions = list_all_instructions()
        print("=== Hermes Bytecode Instructions ===")
        for name, doc in instructions:
            print(f"{name}: {doc}")
            print()