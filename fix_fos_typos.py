#!/usr/bin/env python3
"""
Fix os.path typos to os.path
"""
import os
import re

def fix_fos_typos(file_path):
    """Fix os.path typos in a single file"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    original_content = content
    
    # Replace os.path with os.path
    content = content.replace('os.path', 'os.path')
    
    # Write back if changed
    if content != original_content:
        with open(file_path, 'w') as f:
            f.write(content)
        print(f"Fixed: {file_path}")
        return True
    else:
        return False

def main():
    # Get all Python files with os.path typos
    import subprocess
    result = subprocess.run([
        'find', '.', '-name', '*.py', '-exec', 'grep', '-l', 'fos\.path', '{}', ';'
    ], capture_output=True, text=True, cwd='/Users/syverjohansen/blog/daehl-e')
    
    files_to_fix = result.stdout.strip().split('\n')
    files_to_fix = [f for f in files_to_fix if f]  # Remove empty strings
    
    print(f"Found {len(files_to_fix)} files with os.path typos")
    
    fixed_count = 0
    for file_path in files_to_fix:
        full_path = os.path.join('/Users/syverjohansen/blog/daehl-e', file_path.lstrip('./'))
        if os.path.exists(full_path):
            if fix_fos_typos(full_path):
                fixed_count += 1
    
    print(f"\nSummary: Fixed {fixed_count} out of {len(files_to_fix)} files")

if __name__ == "__main__":
    main()