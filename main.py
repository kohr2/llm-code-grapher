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
from src.utils import detect_language_from_extension


def get_parser_for_language(language: str):
    """Get appropriate parser for the given language"""
    if language.lower() == 'cobol':
        from lang.cobol.parser.cobol_parser import COBOLParser
        return COBOLParser()
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


def get_analyzer_for_language(language: str):
    """Get appropriate analyzer for the given language"""
    if language.lower() == 'cobol':
        from lang.cobol.parser.llm_analyzer import COBOLAnalyzer
        return COBOLAnalyzer()
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
    """Main entry point for the application"""
    try:
        # Parse arguments
        args = parse_arguments()
        
        # Setup logging
        setup_logging(args.verbose)
        
        # Validate input file
        validate_input_file(args.input_file)
        
        # Detect language
        language = detect_language_from_extension(args.input_file)
        
        # Get components
        parser = get_parser_for_language(language)
        analyzer = get_analyzer_for_language(language)
        validator = get_validator_for_language(language)
        
        # Parse the file
        result = parser.parse(args.input_file)
        
        # Analyze sections
        for section in result.sections:
            analysis = analyzer.analyze_section(section)
            section.business_logic = analysis.business_logic
            section.confidence = analysis.confidence
        
        # Validate result
        validation = validator.validate_analysis_result(result)
        if not validation.is_valid:
            raise ValueError(f"Analysis validation failed: {validation.errors}")
        
        # Generate output
        from src.output_generator import OutputGenerator
        output_dir = getattr(args, 'output_dir', None)
        if output_dir is None or not isinstance(output_dir, str):
            output_dir = "./data/output"
        output_gen = OutputGenerator(
            output_dir=output_dir
        )
        input_filename = Path(args.input_file).stem
        output_gen.generate_all_outputs(result, input_filename)
        
        print(f"Analysis complete. Results saved to {output_gen.output_dir}")
        
    except SystemExit:
        # Re-raise SystemExit for help/version commands
        raise
    except FileNotFoundError:
        # Re-raise FileNotFoundError for file validation
        raise
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)


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
