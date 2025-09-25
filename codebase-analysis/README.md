# Codebase Analysis Module

This directory contains all the codebase analysis functionality for the LLM Code Grapher project.

## Structure

- `lang/` - Language-specific parsers and ontologies
- `src/` - Core analysis engine and utilities
- `tools/` - Analysis tools and utilities
- `examples/` - Usage examples and demos
- `tests/` - Test suites
- `data/` - Sample data and fixtures
- `output/` - Analysis output files
- `logs/` - Analysis logs
- `scripts/` - Utility scripts

## Quick Start

```bash
# Run the main analysis
python main.py

# Run the graph analyzer
python analyze_graph.py --database llmcodegrapher

# Run tests
python -m pytest tests/
```

## Features

- Multi-language code parsing (COBOL, etc.)
- Neo4j graph database integration
- LLM-powered analysis
- Comprehensive test coverage
- Rich output formats (JSON, text, visualizations)
