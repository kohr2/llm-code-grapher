#!/usr/bin/env python3
"""
Code Analysis Comparator Launcher

Easy access to the code analysis comparator tool.
"""

import sys
import os
from pathlib import Path

# Add the tools directory to Python path
tools_dir = Path(__file__).parent / "tools" / "graph_analyzer"
sys.path.insert(0, str(tools_dir))

# Import and run the comparator
from code_analysis_comparator import main

if __name__ == "__main__":
    main()

