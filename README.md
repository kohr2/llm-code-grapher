# COBOL Code Grapher

A Python tool that analyzes COBOL code structure using LLMs to create hierarchical mappings of sections and subsections, including business logic extraction and relationship mapping.

## Features

- **Hierarchical Structure Mapping**: Identifies COBOL sections and subsections
- **Business Logic Extraction**: Uses LLMs to understand what each section does
- **Relationship Mapping**: Finds connections between sections
- **Confidence Scoring**: Provides confidence levels for all extractions
- **Multiple Output Formats**: JSON, text summaries, and visualizations

## Quick Start

### Prerequisites
- Python 3.8+
- OpenAI API key

### Installation
```bash
# Clone the repository
git clone <repository-url>
cd llm-code-grapher

# Install dependencies
pip install -r requirements.txt

# Set up environment
cp .env.example .env
# Edit .env with your OpenAI API key
```

### Usage
```bash
# Analyze a COBOL file
python main.py analyze data/fixtures/vasu_fraud_management_cobol_reformatted.cbl

# Get help
python main.py --help
```

## Project Structure

```
llm-code-grapher/
├── src/                    # Source code
├── tests/                  # Test files
├── data/                   # Data and fixtures
│   ├── fixtures/           # Input COBOL files
│   └── output/             # Generated outputs
├── docs/                   # Documentation
├── scripts/                # Utility scripts
└── logs/                   # Application logs
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
