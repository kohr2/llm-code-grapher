# LLM Code Grapher Tools

This directory contains specialized tools and utilities for the LLM Code Grapher project.

## Available Tools

### Graph Analyzer (`graph_analyzer/`)

A comprehensive Neo4j graph analysis tool that provides:

- **Hierarchical Structure Analysis**: Analyze graph structure level by level
- **Database Management**: Switch between different Neo4j databases
- **Relationship Mapping**: Detailed analysis of relationship patterns
- **Path Analysis**: Find most common paths through code graphs
- **Export Capabilities**: Save analysis results for further processing

**Quick Start:**
```bash
# From project root
python analyze_graph.py --uri bolt://localhost:7687 --user neo4j --password password

# Or directly
python tools/graph_analyzer/graph_analyzer.py --uri bolt://localhost:7687 --user neo4j --password password
```

**Documentation:** See `graph_analyzer/README.md` for detailed usage instructions.

## Directory Structure

```
tools/
├── README.md                    # This file
└── graph_analyzer/             # Neo4j Graph Analysis Tool
    ├── __init__.py             # Package initialization
    ├── README.md               # Tool documentation
    ├── graph_analyzer.py       # Main analyzer script
    ├── graph_analyzer_requirements.txt  # Dependencies
    ├── graph_analyzer_config.yaml       # Configuration examples
    └── graph_analyzer_examples.md       # Usage examples
```

## Adding New Tools

When adding new tools to this directory:

1. Create a subdirectory for your tool
2. Include a `README.md` with documentation
3. Add an `__init__.py` file for Python packages
4. Update this README to document the new tool
5. Consider adding a launcher script in the main project directory

## Integration

Tools in this directory are designed to work with the main LLM Code Grapher project and can be:

- Run independently for specific analysis tasks
- Integrated into the main CLI for extended functionality
- Used as standalone utilities for graph analysis and processing

## Requirements

Each tool has its own requirements file. Install dependencies as needed:

```bash
# For graph analyzer
pip install -r tools/graph_analyzer/graph_analyzer_requirements.txt
```
