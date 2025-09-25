# LLM Code Grapher

A comprehensive tool for analyzing code structure using LLMs to create hierarchical mappings, business logic extraction, and relationship mapping across multiple programming languages.

## Architecture

This project is organized into two main modules:

### 🔍 Codebase Analysis Module
- **Language Parsers**: COBOL, Python, Java, and more
- **Ontology Definitions**: Language-specific code structures
- **Neo4j Integration**: Graph database for code relationships
- **LLM Analysis**: AI-powered code understanding
- **Analysis Tools**: Graph analyzers, visualizations, and reports

### 📊 Product Management Module
- **Project Planning**: Roadmaps and feature management
- **Release Planning**: Version control and deployment
- **Stakeholder Communication**: Reports and dashboards
- **Metrics & Reporting**: Performance and usage analytics

## Quick Start

### Prerequisites
- Python 3.8+
- Neo4j database
- One of the following:
  - OpenAI API key (for OpenAI provider)
  - Ollama installed locally (for Ollama provider)

### Installation
```bash
# Clone the repository
git clone <repository-url>
cd llm-code-grapher

# Install dependencies
pip install -r requirements.txt

# Set up environment
cp .env.example .env
# Edit .env with your configuration
```

### Usage

#### Codebase Analysis
```bash
# Analyze code using the codebase analysis module
python main.py codebase-analysis analyze data/fixtures/vasu_fraud_management_cobol_reformatted.cbl

# Run graph analyzer
python main.py codebase-analysis analyze_graph --database llmcodegrapher

# Get help
python main.py codebase-analysis --help
```

#### Product Management
```bash
# Access product management features
python main.py product-management

# Get help
python main.py product-management --help
```

#### Direct Module Access
```bash
# Access codebase analysis directly
cd codebase-analysis
python main.py analyze file.cbl

# Run graph analyzer directly
cd codebase-analysis
python analyze_graph.py --database llmcodegrapher
```

## Project Structure

```
llm-code-grapher/
├── codebase-analysis/       # Code analysis functionality
│   ├── lang/               # Language parsers and ontologies
│   ├── src/                # Core analysis engine
│   ├── tools/              # Analysis tools and utilities
│   ├── examples/           # Usage examples
│   ├── tests/              # Test suites
│   ├── data/               # Sample data and fixtures
│   ├── output/             # Analysis outputs
│   └── logs/               # Analysis logs
├── product-management/      # Product management functionality
│   ├── src/                # Product management core
│   ├── tests/              # Product management tests
│   └── docs/               # Product management docs
├── docs/                   # Project documentation
└── main.py                 # Main entry point
```

## Development

### Running Tests
```bash
python -m pytest tests/
```

### Code Quality
```bash
# Format code
black src/ tests/

# Lint code
flake8 src/ tests/
```

## Configuration

Edit `config.yaml` to customize:
- LLM provider and model settings
- Processing parameters
- Output formats
- COBOL pattern matching

## License

MIT License
