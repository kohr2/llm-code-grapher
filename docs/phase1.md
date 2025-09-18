# Phase 1 MVP - COBOL Code Grapher

## MVP Goals
- **Primary**: Accurately identify and map COBOL sections and subsections
- **Secondary**: Extract meaningful business logic descriptions
- **Constraint**: Keep code simple, focus on accuracy over features

## Core MVP Features

### 1. Basic Structure Detection
- **Regex-based section identification** (PROCEDURE, DATA, FILE, WORKING-STORAGE)
- **Simple subsection detection** (paragraphs within sections)
- **Line range and count tracking**
- **Basic validation of detected structures**

### 2. LLM Integration (Single Provider)
- **OpenAI GPT-4 only** (most reliable for accuracy)
- **Simple prompt engineering** with clear, focused prompts
- **Business logic extraction** for each section/subsection
- **Confidence scoring** for all extractions

### 3. Simple Data Model
```python
@dataclass
class COBOLSection:
    name: str
    type: str
    line_range: tuple[int, int]
    line_count: int
    business_logic: str
    confidence: float

@dataclass
class COBOLSubsection:
    name: str
    parent_section: str
    line_range: tuple[int, int]
    line_count: int
    business_logic: str
    confidence: float
```

### 4. Basic Output
- **JSON output** with all extracted data
- **Simple text summary** of findings
- **Confidence indicators** for manual review

## Implementation Steps

### Step 1: Project Setup (Day 1)
```bash
# Minimal dependencies
pip install openai pydantic rich click pyyaml
```

**Files to create:**
- `main.py` - CLI entry point
- `config.yaml` - Simple configuration
- `models.py` - Data classes
- `cobol_parser.py` - Regex-based parsing
- `llm_analyzer.py` - LLM integration

### Step 2: COBOL Structure Detection (Day 2)
**Focus: Accuracy over complexity**

```python
# Simple, reliable regex patterns
SECTION_PATTERNS = [
    r'^\s*([A-Z0-9-]+)\s+SECTION\s*\.',
    r'^\s*([A-Z0-9-]+)\s+PARAGRAPH\s*\.'
]

SUBSECTION_PATTERNS = [
    r'^\s*([A-Z0-9-]+)\s*\.'
]
```

**Validation rules:**
- Section names must be valid COBOL identifiers
- Line ranges must be sequential
- No overlapping sections
- Basic COBOL syntax validation

### Step 3: LLM Integration (Day 3)
**Focus: Simple, effective prompts**

```python
# Single, focused prompt for business logic
PROMPT_TEMPLATE = """
Analyze this COBOL section and provide:
1. What this section does in 1-2 sentences
2. Confidence level (0-1) for your analysis

COBOL Section:
{section_code}

Response format:
Description: [your description]
Confidence: [0.0-1.0]
"""
```

**Error handling:**
- Retry failed requests (max 3 attempts)
- Fallback to "Unable to analyze" for failures
- Log all LLM interactions

### Step 4: Context Window Management (Day 4)
**Focus: Simple chunking strategy**

```python
# Simple sliding window approach
def chunk_cobol_code(code: str, chunk_size: int = 2000, overlap: int = 200):
    lines = code.split('\n')
    chunks = []
    
    for i in range(0, len(lines), chunk_size - overlap):
        chunk_lines = lines[i:i + chunk_size]
        chunks.append('\n'.join(chunk_lines))
    
    return chunks
```

**Strategy:**
- Process each chunk independently
- Merge results at section boundaries
- Flag sections that span multiple chunks

### Step 5: Basic Relationship Detection (Day 5)
**Focus: Simple, high-confidence relationships**

```python
# Look for obvious patterns
def find_relationships(sections: List[COBOLSection]) -> List[Relationship]:
    relationships = []
    
    for section in sections:
        # Look for PERFORM statements
        if 'PERFORM' in section.code:
            # Extract target section names
            targets = extract_perform_targets(section.code)
            for target in targets:
                relationships.append(Relationship(
                    source=section.name,
                    target=target,
                    type='CALLS'
                ))
    
    return relationships
```

### Step 6: Output Generation (Day 6)
**Focus: Clear, actionable output**

```python
# Simple JSON output with confidence indicators
{
    "program_name": "fraud_management",
    "total_sections": 5,
    "total_subsections": 23,
    "sections": [
        {
            "name": "MAIN-PROCESSING",
            "type": "PROCEDURE",
            "line_range": [45, 120],
            "line_count": 76,
            "business_logic": "Main processing loop that handles transaction validation",
            "confidence": 0.92,
            "subsections": [...]
        }
    ],
    "relationships": [...],
    "analysis_metadata": {
        "processing_time": "2.3s",
        "llm_tokens_used": 15420,
        "confidence_threshold": 0.7
    }
}
```

## Accuracy Optimization Strategies

### 1. Prompt Engineering
- **Single, focused prompts** (avoid complex multi-step prompts)
- **Few-shot examples** with known COBOL patterns
- **Clear output format** requirements
- **Confidence scoring** for every response

### 2. Validation Rules
- **Cross-reference validation** between regex and LLM results
- **Consistency checks** (section boundaries, naming conventions)
- **Confidence thresholds** (flag low-confidence results)
- **Manual review flags** for uncertain cases

### 3. Error Handling
- **Graceful degradation** (continue processing if some sections fail)
- **Detailed logging** of all processing steps
- **Retry mechanisms** for transient failures
- **Fallback strategies** for LLM failures

## Testing Strategy

### 1. Unit Tests
- Regex pattern matching accuracy
- Data model validation
- LLM response parsing
- Error handling scenarios

### 2. Integration Tests
- End-to-end processing with known COBOL files
- Accuracy validation against manual analysis
- Performance testing with different file sizes

### 3. Validation Dataset
- Use the provided `vasu_fraud_management_cobol_reformatted.cbl`
- Manually annotate expected sections and business logic
- Measure accuracy against ground truth

## Success Criteria

### Functional Requirements
- ✅ Identify 90%+ of sections correctly
- ✅ Extract meaningful business logic for 80%+ of sections
- ✅ Process files up to 10,000 lines
- ✅ Generate clear, actionable output

### Performance Requirements
- ✅ Process 1000 lines in < 30 seconds
- ✅ Memory usage < 500MB for 10K line files
- ✅ 95%+ uptime for LLM calls
- ✅ Clear error messages and logging

### Quality Requirements
- ✅ Confidence scoring for all extractions
- ✅ Manual review flags for uncertain cases
- ✅ Consistent output format
- ✅ Comprehensive logging

## Directory Structure

### Root Level
```
llm-code-grapher/
├── README.md                           # Project overview and setup
├── requirements.txt                    # Python dependencies
├── config.yaml                        # Configuration file
├── .env.example                       # Environment variables template
├── .gitignore                         # Git ignore rules
├── main.py                           # CLI entry point
└── setup.py                          # Package installation
```

### Source Code (`src/` directory)
```
src/
├── __init__.py                        # Package initialization
├── models.py                          # Data classes (COBOLSection, COBOLSubsection)
├── cobol_parser.py                    # Regex-based COBOL parsing
├── llm_analyzer.py                    # LLM integration and analysis
├── output_generator.py                # JSON/text output generation
├── config_manager.py                  # Configuration loading and validation
├── utils.py                           # Utility functions
└── cli.py                            # Command-line interface logic
```

### Tests (`tests/` directory)
```
tests/
├── __init__.py                        # Test package initialization
├── conftest.py                        # Pytest configuration and fixtures
├── test_models.py                     # Data model tests
├── test_cobol_parser.py               # Parser functionality tests
├── test_llm_analyzer.py               # LLM integration tests
├── test_output_generator.py           # Output generation tests
├── test_integration.py                # End-to-end integration tests
└── test_fixtures/                     # Test-specific fixtures
    ├── sample_small.cbl               # Small test COBOL file
    └── expected_output.json           # Expected test results
```

### Data and Fixtures (`data/` directory)
```
data/
├── fixtures/                          # Input COBOL files
│   └── vasu_fraud_management_cobol_reformatted.cbl
├── output/                            # Generated analysis outputs
│   ├── json/                          # JSON format outputs
│   ├── text/                          # Text summary outputs
│   └── logs/                          # Processing logs
└── validation/                        # Manual validation data
    ├── ground_truth.json              # Manually annotated sections
    └── accuracy_reports/              # Accuracy measurement results
```

### Configuration and Documentation
```
docs/                                  # Documentation
├── setup.md                          # Setup instructions
├── usage.md                          # Usage examples
└── api.md                            # API documentation

scripts/                               # Utility scripts
├── setup_env.sh                      # Environment setup
├── run_tests.sh                      # Test runner
└── validate_accuracy.py              # Accuracy validation script

logs/                                  # Application logs
├── app.log                           # Main application log
├── llm_calls.log                     # LLM API call logs
└── errors.log                        # Error logs
```

### Complete Structure
```
llm-code-grapher/
├── README.md
├── requirements.txt
├── config.yaml
├── .env.example
├── .gitignore
├── main.py
├── setup.py
├── src/
│   ├── __init__.py
│   ├── models.py
│   ├── cobol_parser.py
│   ├── llm_analyzer.py
│   ├── output_generator.py
│   ├── config_manager.py
│   ├── utils.py
│   └── cli.py
├── tests/
│   ├── __init__.py
│   ├── conftest.py
│   ├── test_models.py
│   ├── test_cobol_parser.py
│   ├── test_llm_analyzer.py
│   ├── test_output_generator.py
│   ├── test_integration.py
│   └── test_fixtures/
│       ├── sample_small.cbl
│       └── expected_output.json
├── data/
│   ├── fixtures/
│   │   └── vasu_fraud_management_cobol_reformatted.cbl
│   ├── output/
│   │   ├── json/
│   │   ├── text/
│   │   └── logs/
│   └── validation/
│       ├── ground_truth.json
│       └── accuracy_reports/
├── docs/
│   ├── setup.md
│   ├── usage.md
│   └── api.md
├── scripts/
│   ├── setup_env.sh
│   ├── run_tests.sh
│   └── validate_accuracy.py
└── logs/
    ├── app.log
    ├── llm_calls.log
    └── errors.log
```

## Directory Setup Commands

### Initial Setup
```bash
# Create main directories
mkdir -p src tests data/{fixtures,output/{json,text,logs},validation/accuracy_reports}
mkdir -p docs scripts logs

# Create empty __init__.py files
touch src/__init__.py tests/__init__.py

# Create placeholder files
touch README.md .env.example .gitignore setup.py
```

### File Creation Order (Following Implementation Steps)
```bash
# Day 1: Project Setup
touch main.py src/models.py src/config_manager.py

# Day 2: COBOL Parser
touch src/cobol_parser.py tests/test_cobol_parser.py

# Day 3: LLM Integration
touch src/llm_analyzer.py tests/test_llm_analyzer.py

# Day 4: Context Management
touch src/utils.py

# Day 5: Relationships
touch src/output_generator.py tests/test_output_generator.py

# Day 6: Output Generation
touch tests/test_integration.py scripts/validate_accuracy.py
```

## Key Benefits of This Structure

### 1. **Separation of Concerns**
- `src/` contains all business logic
- `tests/` is organized by functionality
- `data/` separates input, output, and validation data

### 2. **Scalability**
- Easy to add new modules in `src/`
- Test organization matches source structure
- Clear separation between different types of data

### 3. **Maintainability**
- Clear naming conventions
- Logical grouping of related files
- Easy to find and modify specific functionality

### 4. **Development Workflow**
- `scripts/` for automation and utilities
- `logs/` for debugging and monitoring
- `docs/` for documentation and examples

### 5. **Testing Strategy**
- Unit tests for each module
- Integration tests for end-to-end validation
- Test fixtures for consistent testing data

## Next Steps After MVP
1. **Add relationship mapping** (Phase 1.5)
2. **Support multiple LLM providers** (Phase 2)
3. **Add visualization** (Phase 2)
4. **Improve context window management** (Phase 2)
5. **Add more COBOL patterns** (Phase 3)

## Risk Mitigation
- **LLM API failures**: Implement retry logic and fallbacks
- **Context window limits**: Use simple chunking strategy
- **Accuracy issues**: Focus on high-confidence extractions only
- **Performance problems**: Process sections in parallel
- **Complex COBOL patterns**: Start with common patterns only

This MVP focuses on getting the core functionality working reliably with high accuracy, setting the foundation for more advanced features in later phases.
