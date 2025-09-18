#!/usr/bin/env python3
"""
COBOL Code Grapher - Main Entry Point

A tool for analyzing COBOL code structure using LLMs and generating
graph representations of code relationships.
"""

import sys
import os
from pathlib import Path

# Add src directory to Python path
project_root = Path(__file__).parent
src_path = project_root / "src"
sys.path.insert(0, str(src_path))

from src.cli import main

if __name__ == "__main__":
    main()
