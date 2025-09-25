#!/usr/bin/env python3
"""
LLM Code Grapher - Main Entry Point

This is the main entry point that delegates to the appropriate module:
- codebase-analysis: For code analysis functionality
- product-management: For product management functionality
"""

import sys
import os
from pathlib import Path

# Add the current directory to Python path
current_dir = Path(__file__).parent
sys.path.insert(0, str(current_dir))

def main():
    """Main entry point that delegates to appropriate module"""
    if len(sys.argv) < 2:
        print("LLM Code Grapher")
        print("Usage:")
        print("  python main.py codebase-analysis [args...]  # Run codebase analysis")
        print("  python main.py product-management [args...] # Run product management")
        print("  python main.py --help                       # Show this help")
        return
    
    module = sys.argv[1]
    
    if module == "codebase-analysis":
        # Delegate to codebase analysis module
        sys.path.insert(0, str(current_dir / "codebase-analysis"))
        # Remove the module name from sys.argv and pass the rest
        original_argv = sys.argv[:]
        sys.argv = [original_argv[0]] + original_argv[2:]
        from main import main as codebase_main
        codebase_main()
    elif module == "product-management":
        # Delegate to product management module
        sys.path.insert(0, str(current_dir / "product-management"))
        # Remove the module name from sys.argv and pass the rest
        original_argv = sys.argv[:]
        sys.argv = [original_argv[0]] + original_argv[2:]
        from main import main as product_main
        product_main()
    elif module == "--help" or module == "-h":
        print("LLM Code Grapher")
        print("Usage:")
        print("  python main.py codebase-analysis [args...]  # Run codebase analysis")
        print("  python main.py product-management [args...] # Run product management")
        print("  python main.py --help                       # Show this help")
    else:
        print(f"Unknown module: {module}")
        print("Available modules: codebase-analysis, product-management")
        sys.exit(1)

if __name__ == "__main__":
    main()
