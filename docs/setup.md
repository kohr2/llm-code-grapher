# Setup Instructions

## Prerequisites

- Python 3.8 or higher
- OpenAI API key (get one from https://platform.openai.com/api-keys)

## Installation

### 1. Clone the Repository
```bash
git clone <repository-url>
cd llm-code-grapher
```

### 2. Create Virtual Environment
```bash
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
```

### 3. Install Dependencies
```bash
pip install -r requirements.txt
```

### 4. Set Up Environment Variables
```bash
cp .env.example .env
# Edit .env and add your OpenAI API key
```

### 5. Verify Installation
```bash
python main.py --help
```

## Configuration

Edit `config.yaml` to customize:
- LLM provider and model settings
- Processing parameters
- Output formats
- COBOL pattern matching

## First Run

```bash
# Analyze the sample COBOL file
python main.py analyze data/fixtures/vasu_fraud_management_cobol_reformatted.cbl
```

## Troubleshooting

### Common Issues

1. **API Key Error**: Make sure your OpenAI API key is set in `.env`
2. **Import Error**: Ensure you're in the virtual environment
3. **File Not Found**: Check that the COBOL file path is correct

### Getting Help

- Check the logs in `logs/` directory
- Run with verbose output: `python main.py --verbose`
- See usage: `python main.py --help`
