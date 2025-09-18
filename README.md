# COBOL Code Grapher

A Python tool that analyzes COBOL code structure using LLMs to create hierarchical mappings of sections and subsections, including business logic extraction and relationship mapping.

## Features

- **Hierarchical Structure Mapping**: Identifies COBOL sections and subsections
- **Business Logic Extraction**: Uses LLMs to understand what each section does
- **Relationship Mapping**: Finds connections between sections
- **Confidence Scoring**: Provides confidence levels for all extractions
- **Multiple Output Formats**: JSON, text summaries, and visualizations
- **Multiple LLM Providers**: Support for OpenAI and Ollama (local) providers

## Quick Start

### Prerequisites
- Python 3.8+
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
# Edit .env with your OpenAI API key
```

### Usage

#### Using OpenAI (default)
```bash
# Set your OpenAI API key
export OPENAI_API_KEY="your-api-key-here"

# Analyze a COBOL file
python -m src.cli analyze data/fixtures/vasu_fraud_management_cobol_reformatted.cbl

# Or explicitly specify OpenAI
python -m src.cli analyze file.cbl --provider openai
```

#### Using Ollama (local)
```bash
# Start Ollama (if not already running)
ollama serve

# Pull a model (e.g., llama2)
ollama pull llama2

# Analyze with Ollama
python -m src.cli analyze file.cbl --provider ollama --model llama2

# Or use custom Ollama URL
python -m src.cli analyze file.cbl --provider ollama --base-url http://localhost:11434
```

#### Configuration
You can also configure the provider in `config.yaml`:
```yaml
llm:
  provider: "ollama"  # or "openai"
  model: "llama2"     # or "gpt-4"
  base_url: "http://localhost:11434"  # for Ollama
  api_key: null       # for OpenAI
```

#### Get help
```bash
python -m src.cli --help
python -m src.cli analyze --help
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
