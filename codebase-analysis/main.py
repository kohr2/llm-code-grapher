#!/usr/bin/env python3
"""
LLM Code Grapher - Codebase Analysis Main Entry Point

A tool for analyzing code structure using LLMs and generating
graph representations of code relationships.
"""

import sys
import os
from pathlib import Path
from typing import Optional, Dict, Any

# Add current directory to Python path for imports
project_root = Path(__file__).parent
sys.path.insert(0, str(project_root))

from src.cli import main as cli_main
from src.utils import detect_language_from_extension


def get_parser_for_language(language: str, config: Optional[Dict[str, Any]] = None):
    """Get appropriate parser for the given language"""
    if language.lower() == 'cobol':
        from lang.cobol.parser.cobol_parser import COBOLParser
        return COBOLParser(config)
    elif language.lower() == 'python':
        # For now, return a mock parser for Python
        # This will be implemented when Python support is added
        from unittest.mock import Mock
        return Mock()
    elif language.lower() == 'java':
        # For now, return a mock parser for Java
        # This will be implemented when Java support is added
        from unittest.mock import Mock
        return Mock()
    elif language.lower() == 'generic':
        # For now, return a mock parser for generic
        # This will be implemented when generic support is added
        from unittest.mock import Mock
        return Mock()
    else:
        raise ValueError(f"Unsupported language: {language}")


def get_validator_for_language(language: str):
    """Get appropriate validator for the given language"""
    if language.lower() == 'cobol':
        from lang.cobol.ontology.cobol_ontology_validator import COBOLOntologyValidator
        return COBOLOntologyValidator()
    elif language.lower() == 'python':
        # For now, return a mock validator for Python
        # This will be implemented when Python support is added
        from unittest.mock import Mock
        return Mock()
    elif language.lower() == 'java':
        # For now, return a mock validator for Java
        # This will be implemented when Java support is added
        from unittest.mock import Mock
        return Mock()
    elif language.lower() == 'generic':
        # For now, return a mock validator for generic
        # This will be implemented when generic support is added
        from unittest.mock import Mock
        return Mock()
    else:
        raise ValueError(f"Unsupported language: {language}")


def get_analyzer_for_language(language: str, provider_config=None):
    """Get appropriate analyzer for the given language"""
    if language.lower() == 'cobol':
        from lang.cobol.parser.llm_analyzer import COBOLAnalyzer
        return COBOLAnalyzer(provider_config)
    elif language.lower() == 'python':
        # For now, return a mock analyzer for Python
        # This will be implemented when Python support is added
        from unittest.mock import Mock
        return Mock()
    elif language.lower() == 'java':
        # For now, return a mock analyzer for Java
        # This will be implemented when Java support is added
        from unittest.mock import Mock
        return Mock()
    elif language.lower() == 'generic':
        # For now, return a mock analyzer for generic
        # This will be implemented when generic support is added
        from unittest.mock import Mock
        return Mock()
    else:
        raise ValueError(f"Unsupported language: {language}")


def setup_logging(verbose: bool = False):
    """Setup logging configuration"""
    import logging
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )


def parse_arguments():
    """Parse command line arguments"""
    import argparse
    parser = argparse.ArgumentParser(description='LLM Code Grapher')
    parser.add_argument('input_file', help='Input file to analyze')
    parser.add_argument('--verbose', '-v', action='store_true', help='Enable verbose output')
    parser.add_argument('--output-dir', '-o', help='Output directory')
    parser.add_argument('--output-format', '--format', choices=['json', 'text', 'yaml', 'all'], default='all', help='Output format')
    parser.add_argument('--confidence-threshold', type=float, default=0.7, help='Confidence threshold')
    return parser.parse_args()


def main():
    """Main entry point for the codebase analysis module"""
    # Delegate to the CLI module
    cli_main()


def validate_input_file(input_file: str) -> None:
    """Validate input file exists and has valid extension"""
    if not os.path.exists(input_file):
        raise FileNotFoundError(f"Input file not found: {input_file}")
    
    # Check file extension
    valid_extensions = ['.cbl', '.cob', '.py', '.java', '.js', '.ts', '.txt']
    file_ext = Path(input_file).suffix.lower()
    if file_ext not in valid_extensions:
        raise ValueError(f"Invalid file extension: {file_ext}. Supported extensions: {valid_extensions}")


if __name__ == "__main__":
    main()
