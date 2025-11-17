#!/usr/bin/env python3
"""
Fix remaining hard-coded blog paths in Python files
"""
import os
import re

def fix_file_paths(file_path):
    """Fix hard-coded paths in a single file"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    original_content = content
    
    # Replace hard-coded Mac blog paths with expanduser paths
    # Pattern: os.path.expanduser('~/blog/...') -> os.path.expanduser('~/blog/...')
    content = re.sub(
        r"'/Users/syverjohansen(/blog/[^']*)'",
        r"os.path.expanduser('~\1')",
        content
    )
    
    # Pattern: os.path.expanduser("~/blog/...") -> os.path.expanduser("~/blog/...")
    content = re.sub(
        r'"/Users/syverjohansen(/blog/[^"]*)"',
        r'os.path.expanduser("~\1")',
        content
    )
    
    # If we made changes, ensure os is imported
    if content != original_content:
        lines = content.split('\n')
        has_os_import = any('import os' in line for line in lines[:10])
        
        if not has_os_import:
            # Find a good place to add the import
            insert_pos = 0
            for i, line in enumerate(lines):
                if line.startswith('import ') or line.startswith('from '):
                    insert_pos = i + 1
                elif line.strip() == '':
                    continue
                else:
                    break
            
            lines.insert(insert_pos, 'import os')
            content = '\n'.join(lines)
    
    # Write back if changed
    if content != original_content:
        with open(file_path, 'w') as f:
            f.write(content)
        print(f"Fixed: {file_path}")
        return True
    else:
        print(f"No changes needed: {file_path}")
        return False

def main():
    # Get all Python files with the hard-coded blog paths
    import subprocess
    result = subprocess.run([
        'find', '.', '-name', '*.py', '-exec', 'grep', '-l', '/Users/syverjohansen/blog', '{}', ';'
    ], capture_output=True, text=True, cwd=os.path.expanduser('~/blog/daehl-e'))
    
    files_to_fix = result.stdout.strip().split('\n')
    files_to_fix = [f for f in files_to_fix if f]  # Remove empty strings
    
    print(f"Found {len(files_to_fix)} files to potentially fix")
    
    fixed_count = 0
    for file_path in files_to_fix:
        full_path = os.path.join(os.path.expanduser('~/blog/daehl-e'), file_path.lstrip('./'))
        if os.path.exists(full_path):
            if fix_file_paths(full_path):
                fixed_count += 1
    
    print(f"\nSummary: Fixed {fixed_count} out of {len(files_to_fix)} files")

if __name__ == "__main__":
    main()