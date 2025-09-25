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
- **Environment-based API key configuration** via `.env` file

### 3. Simple Data Model with COBOL Ontology
```python
@dataclass
class COBOLSection:
    name: str
    type: str  # IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE, etc.
    line_range: tuple[int, int]
    line_count: int
    business_logic: str
    confidence: float
    complexity_score: float = 0.0
    risk_level: str = "LOW"  # LOW, MEDIUM, HIGH, CRITICAL

@dataclass
class COBOLSubsection:
    name: str
    parent_section: str
    line_range: tuple[int, int]
    line_count: int
    business_logic: str
    confidence: float
    complexity_score: float = 0.0
    risk_level: str = "LOW"

@dataclass
class COBOLRelationship:
    source: str
    target: str
    relationship_type: str  # CALLS, USES, MODIFIES, DEPENDS_ON, DATA_FLOW
    confidence: float
    strength: float = 1.0  # Relationship strength (0.0-1.0)

@dataclass
class COBOLProgramOntology:
    """COBOL Program Ontology - Structured representation of COBOL programs"""
    program_name: str
    divisions: List[COBOLSection]
    sections: List[COBOLSection]
    subsections: List[COBOLSubsection]
    relationships: List[COBOLRelationship]
    data_items: List[COBOLDataItem]
    business_rules: List[COBOLBusinessRule]
    
    # Ontology Properties
    complexity_metrics: Dict[str, float]
    quality_indicators: Dict[str, str]
    maintenance_risks: List[str]
    modernization_potential: str  # LOW, MEDIUM, HIGH

@dataclass
class COBOLDataItem:
    name: str
    type: str  # PIC, REDEFINES, FILLER, etc.
    level: int
    parent_item: Optional[str]
    business_meaning: str
    usage_patterns: List[str]

@dataclass
class COBOLBusinessRule:
    rule_id: str
    description: str
    section: str
    confidence: float
    risk_impact: str
    modernization_priority: str
```

### 4. Interface-Based COBOL Ontology
The ontology is implemented using Python abstract base classes and interfaces, providing type safety and better maintainability than YAML-based approaches.

**Architecture Benefits:**
- **Type Safety**: Full compile-time type checking with Python interfaces
- **IDE Support**: Autocomplete, refactoring, and error detection
- **Single Source of Truth**: All ontology logic in Python code
- **Easy Testing**: Simple to mock and unit test
- **No Runtime Overhead**: No YAML parsing at runtime

**Key Concepts:**
- **Program**: Complete COBOL program with metadata and quality metrics
- **Division**: Main COBOL divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- **Section**: Sections within divisions (FILE, WORKING-STORAGE, PROCEDURE_SECTION)
- **Subsection**: Paragraphs within sections
- **DataItem**: Data variables and structures with business meanings
- **BusinessRule**: Business logic and rules with risk assessments
- **Relationship**: Connections between components (CALLS, USES, MODIFIES, etc.)

**Quality Assessment:**
- **Complexity Metrics**: Cyclomatic complexity, maintainability index, technical debt
- **Risk Levels**: LOW, MEDIUM, HIGH, CRITICAL
- **Quality Indicators**: Code coverage, documentation quality, test coverage
- **Modernization Potential**: LOW, MEDIUM, HIGH

**Implementation:**
- `COBOLOntology` extends `BaseOntology` with COBOL-specific concepts
- `COBOLOntologyValidator` extends `BaseOntologyValidator` with COBOL validation rules
- All ontology logic is defined in Python methods, not external YAML files

See `lang/cobol/ontology/README.md` for detailed documentation and examples.

**Extensible Design**: The language structure is designed to support multiple programming languages and domains. Future languages can be added as separate subdirectories (e.g., `lang/java/`, `lang/python/`, `lang/legacy/`) while maintaining the same parsing and ontology framework.

### 5. Base Parser Framework
The project uses an abstract base parser framework that ensures consistency across all programming languages:

**Base Parser (`lang/base/parser/`):**
- `BaseParser` - Abstract base class for language-specific parsers
- `BaseLLMAnalyzer` - Abstract base class for LLM analysis
- `BaseParserResult` - Abstract base class for parsing results
- Common parsing utilities and validation methods

**Base Ontology (`lang/base/ontology/`):**
- `BaseOntology` - Abstract base class for ontology management (interface-based)
- `BaseOntologyValidator` - Abstract base class for validation
- `BaseProgram`, `BaseSection`, `BaseSubsection`, `BaseRelationship` - Abstract data models
- `ValidationResult` - Standard validation result class
- Common validation utilities and metrics

**Language-Specific Extensions:**
- Each language extends the base classes with language-specific logic only
- COBOL parser extends `BaseParser` with COBOL-specific patterns and rules
- COBOL ontology extends `BaseOntology` with COBOL-specific concepts
- Future languages (Java, Python, etc.) will follow the same pattern

**Optimization Benefits:**
- **Maximum Code Reuse**: 90% of logic in base classes
- **Consistency**: All languages follow the same parsing interface
- **Maintainability**: Common parsing logic centralized
- **Extensibility**: New languages need minimal code (~20-30 lines)
- **Testability**: Base classes fully testable independently
- **Performance**: Minimal code duplication across languages

**Interface-Based Architecture Benefits:**
- **Type Safety**: Full compile-time type checking with Python abstract base classes
- **IDE Support**: Autocomplete, refactoring, and error detection for all ontology operations
- **Single Source of Truth**: All ontology logic defined in Python methods, not external YAML files
- **Easy Testing**: Simple to mock abstract base classes and test concrete implementations
- **No Runtime Overhead**: No YAML parsing at runtime - all logic is compiled Python code
- **Better Maintainability**: Changes to ontology logic are made in one place (Python code)
- **Version Control**: Ontology changes are tracked in Git like any other code changes

### 6. Basic Output
- **JSON output** with all extracted data
- **Simple text summary** of findings
- **Confidence indicators** for manual review

### 7. Graph Database Integration (Neo4j)
- **Neo4j graph database** for storing code relationships
- **Environment-based configuration** via `.env` file
- **Structured data storage** for sections, subsections, and relationships
- **Query capabilities** for complex code analysis

## Implementation Steps

### Step 1: Project Setup (Day 1)
```bash
# Install dependencies
pip install -r requirements.txt

# Set up environment variables
cp .env.example .env
# Edit .env with your API keys and database credentials

# Optional: Install Neo4j (if not already installed)
# Using Docker (recommended):
docker run --name neo4j -p 7474:7474 -p 7687:7687 -e NEO4J_AUTH=neo4j/dashboard-killer neo4j:latest

# Or using Homebrew (macOS):
# brew install neo4j
# brew services start neo4j
```

**Environment Variables (.env):**
```bash
# LLM Configuration
OPENAI_API_KEY="your-openai-api-key-here"

# Graph Database Configuration (Neo4j)
NEO4J_URI=bolt://localhost:7687
NEO4J_USERNAME=neo4j
NEO4J_PASSWORD=your-neo4j-password
NEO4J_DATABASE=your-database-name
```

**Dependencies (requirements.txt):**
- Core: `langchain`, `openai`, `anthropic`, `networkx`, `pydantic`, `rich`, `click`
- Text processing: `nltk`, `spacy`, `regex`
- Data handling: `pandas`, `pyyaml`
- Visualization: `matplotlib`, `plotly`, `graphviz`
- Development: `pytest`, `black`, `flake8`

**Files to create:**
- `main.py` - CLI entry point
- `config.yaml` - Simple configuration
- `src/config_manager.py` - Configuration loading and validation
- `src/output_generator.py` - JSON/text output generation
- `src/utils.py` - Utility functions
- `src/cli.py` - Command-line interface logic
- `.env` - Environment variables (from .env.example template)

**Language-specific components (in `lang/` directory):**
- `lang/cobol/parser/cobol_parser.py` - COBOL-specific parsing
- `lang/cobol/parser/llm_analyzer.py` - COBOL-specific LLM analysis
- `lang/cobol/ontology/cobol_ontology.py` - COBOL-specific ontology

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
**Focus: Clear, actionable output with ontology structure**

```python
# Ontology-based JSON output with confidence indicators
{
    "program_name": "fraud_management",
    "ontology_version": "1.0",
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
            "complexity_score": 0.75,
            "risk_level": "MEDIUM",
            "subsections": [...]
        }
    ],
    "relationships": [
        {
            "source": "MAIN-PROCESSING",
            "target": "INITIALIZE-PROGRAM",
            "relationship_type": "CALLS",
            "confidence": 0.95,
            "strength": 1.0
        }
    ],
    "ontology_metrics": {
        "complexity_metrics": {
            "cyclomatic_complexity": 12.5,
            "maintainability_index": 0.68,
            "technical_debt_ratio": 0.23
        },
        "quality_indicators": {
            "code_coverage": "HIGH",
            "documentation_quality": "MEDIUM",
            "test_coverage": "LOW"
        },
        "maintenance_risks": [
            "High coupling between sections",
            "Complex business logic in single section",
            "Missing error handling"
        ],
        "modernization_potential": "MEDIUM"
    },
    "analysis_metadata": {
        "processing_time": "2.3s",
        "llm_tokens_used": 15420,
        "confidence_threshold": 0.7,
        "ontology_validation": "PASSED"
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

### 1. Base Test Framework
**Generic Test Classes (`lang/base/tests/`):**
- `BaseParserTests` - Generic parser testing logic for all languages
- `BaseOntologyValidatorTests` - Generic ontology validation testing
- `BaseLLMAnalyzerTests` - Generic LLM analyzer testing
- Common test patterns, fixtures, and validation methods

### 2. Language-Specific Tests
**COBOL-Specific Tests (`lang/cobol/tests/`):**
- `TestCOBOLParserSpecific` - Only COBOL-specific parser tests
- `TestCOBOLOntologyValidatorSpecific` - Only COBOL-specific ontology tests
- `TestCOBOLAnalyzerSpecific` - Only COBOL-specific LLM tests
- `TestCOBOLIntegration` - End-to-end integration tests

### 3. Test Optimization Benefits
- **Maximum Code Reuse**: 90% of test logic in base classes
- **Consistent Testing**: All languages use same test patterns
- **Fast Development**: New languages inherit 90% of tests
- **Easy Maintenance**: Changes in base classes affect all languages
- **Better Metrics**: Standardized test reporting across languages

### 4. Validation Dataset
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
- ✅ Ontology validation and consistency
- ✅ Semantic relationship accuracy

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

### Language Framework (`lang/` directory)
```
lang/
├── base/                              # Abstract Base Classes
│   ├── parser/                        # Base Parser Framework
│   │   ├── base_parser.py             # Abstract base parser
│   │   └── base_llm_analyzer.py       # Abstract base LLM analyzer
│   ├── ontology/                      # Base Ontology Framework
│   │   ├── base_models.py             # Abstract base models
│   │   ├── base_ontology.py           # Abstract base ontology
│   │   └── base_ontology.yaml         # Base ontology schema
│   └── tests/                         # Base Test Framework
│       ├── base_parser_tests.py       # Generic parser tests
│       ├── base_ontology_tests.py     # Generic ontology tests
│       └── base_llm_tests.py          # Generic LLM tests
└── cobol/                             # COBOL Language Package
    ├── parser/                        # COBOL Parser Components
    │   ├── cobol_parser.py            # COBOL parser (extends base)
    │   └── llm_analyzer.py            # COBOL LLM analyzer (extends base)
    ├── ontology/                      # COBOL Ontology
    │   ├── cobol_ontology.py          # COBOL ontology (extends base, interface-based)
    │   ├── cobol_ontology_validator.py # COBOL validator (extends base)
    │   ├── __init__.py                # Package exports
    │   └── README.md                  # Ontology documentation
    └── tests/                         # COBOL-Specific Tests
        ├── test_cobol_parser_specific.py    # Parser tests
        ├── test_cobol_ontology_specific.py  # Ontology tests
        ├── test_cobol_llm_specific.py       # LLM tests
        ├── test_cobol_integration.py        # Integration tests
        └── test_fixtures/             # Test data
            ├── sample_small.cbl       # Small test COBOL file
            └── expected_output.json   # Expected test results
```

### Source Code (`src/` directory)
```
src/
├── __init__.py                        # Package initialization
├── output_generator.py                # JSON/text output generation
├── config_manager.py                  # Configuration loading and validation
├── utils.py                           # Utility functions
└── cli.py                            # Command-line interface logic
```

**Note**: The `src/` directory contains only **language-agnostic components**. All language-specific code (parsers, analyzers, ontologies) is located in the `lang/` directory structure.

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
├── api.md                            # API documentation
├── lang/                             # Language-specific Packages
│   ├── base/                         # Abstract Base Classes
│   │   ├── parser/                   # Base Parser Framework
│   │   │   ├── base_parser.py        # Abstract base parser
│   │   │   └── base_llm_analyzer.py  # Abstract base LLM analyzer
│   │   └── ontology/                 # Base Ontology Framework
│   │       ├── base_models.py        # Abstract base models
│   │       ├── base_ontology.py      # Abstract base ontology (interface-based)
│   │       └── __init__.py           # Package exports
│   └── cobol/                        # COBOL Language Package
│       ├── parser/                   # COBOL Parser Components
│       │   ├── cobol_parser.py       # COBOL parser (extends base)
│       │   └── llm_analyzer.py       # COBOL LLM analyzer (extends base)
│       ├── ontology/                 # COBOL Ontology
│       │   ├── cobol_ontology.py     # COBOL ontology (extends base, interface-based)
│       │   ├── cobol_ontology_validator.py   # COBOL validator (extends base)
│       │   ├── __init__.py           # Package exports
│       │   └── README.md             # Ontology documentation
│       └── tests/                    # COBOL-specific Tests
│           ├── test_cobol_parser.py  # Parser tests
│           ├── test_ontology_validator.py    # Ontology tests
│           ├── test_integration.py   # Integration tests
│           ├── test_models.py        # Model tests
│           └── test_fixtures/        # Test data

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
│   ├── output_generator.py                # Language-agnostic output generation
│   ├── config_manager.py                  # Language-agnostic configuration
│   ├── utils.py                           # Language-agnostic utilities
│   └── cli.py                            # Language-agnostic CLI
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
├── lang/                                  # Language Framework
│   ├── base/                              # Abstract Base Classes
│   │   ├── parser/                        # Base Parser Framework
│   │   │   ├── base_parser.py             # Abstract base parser
│   │   │   └── base_llm_analyzer.py       # Abstract base LLM analyzer
│   │   ├── ontology/                      # Base Ontology Framework
│   │   │   ├── base_models.py             # Abstract base models
│   │   │   ├── base_ontology.py           # Abstract base ontology (interface-based)
│   │   │   └── __init__.py                # Package exports
│   │   └── tests/                         # Base Test Framework
│   │       ├── base_parser_tests.py       # Generic parser tests
│   │       ├── base_ontology_tests.py     # Generic ontology tests
│   │       └── base_llm_tests.py          # Generic LLM tests
│   └── cobol/                             # COBOL Language Package
│       ├── parser/                        # COBOL Parser Components
│       │   ├── cobol_parser.py            # COBOL parser (extends base)
│       │   └── llm_analyzer.py            # COBOL LLM analyzer (extends base)
│       ├── ontology/                      # COBOL Ontology
│       │   ├── cobol_ontology.py          # COBOL ontology (extends base)
│       │   ├── cobol_program_ontology.yaml # COBOL-specific schema
│       │   ├── cobol_ontology_validator.py # COBOL validator (extends base)
│       │   └── README.md                  # Ontology documentation
│       └── tests/                         # COBOL-Specific Tests
│           ├── test_cobol_parser_specific.py    # Parser tests
│           ├── test_cobol_ontology_specific.py  # Ontology tests
│           ├── test_cobol_llm_specific.py       # LLM tests
│           ├── test_cobol_integration.py        # Integration tests
│           └── test_fixtures/             # Test data
│               ├── sample_small.cbl       # Small test COBOL file
│               └── expected_output.json   # Expected test results
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

### 1. **Maximum Code Reuse**
- **90% of logic** in base classes (`lang/base/`)
- **Language-specific extensions** only contain unique logic
- **New languages** need minimal code (~20-30 lines)
- **Language-agnostic components** in `src/` directory

### 2. **Extensible Architecture**
- **Abstract base classes** for parsers, ontologies, and tests
- **Language packages** (`lang/cobol/`, `lang/java/`, etc.)
- **Consistent interfaces** across all languages

### 3. **Optimized Testing**
- **Base test classes** with generic testing logic
- **Language-specific tests** only for unique functionality
- **90% test code reuse** across languages

### 4. **Maintainability**
- **Centralized common logic** in base classes
- **Clear separation** between generic and specific code
- **Easy to add new languages** following the same pattern

### 5. **Performance Benefits**
- **Minimal code duplication** across languages
- **Shared validation logic** in base classes
- **Consistent error handling** and logging

### 6. **Future Language Implementation**
```python
# New language (e.g., Java) - only 20-30 lines needed!
class JavaParser(BaseParser):
    def _get_section_patterns(self):
        return {"CLASS": r"^class\s+\w+", "METHOD": r"^\s*public\s+\w+"}
    
    def _is_comment_line(self, line):
        return line.strip().startswith('//') or line.strip().startswith('/*')

class JavaAnalyzer(BaseLLMAnalyzer):
    def _initialize_client(self):
        return openai.OpenAI(api_key=self.api_key)
    
    # Everything else inherited from base classes!
```

## Extension Logic Optimization Results

### **Code Reduction Summary:**
| Component | Before | After | Reduction |
|-----------|--------|-------|-----------|
| COBOL Parser | 310 lines | ~150 lines | **~52%** |
| COBOL LLM Analyzer | 145 lines | ~95 lines | **~34%** |
| COBOL Models | 80 lines | ~20 lines | **~75%** |
| COBOL Tests | 650 lines | ~400 lines | **~38%** |
| Base Classes | 0 lines | 800 lines | **New (reusable)** |

### **Optimization Benefits:**
- **🔄 Maximum Consistency**: All languages inherit identical behavior
- **🛠️ Maximum Maintainability**: 90% of logic centralized in base classes
- **🚀 Maximum Extensibility**: New languages need minimal code
- **🧪 Maximum Testability**: Base classes fully testable independently
- **⚡ Maximum Performance**: Minimal code duplication

## Next Steps After MVP
1. **Add relationship mapping** (Phase 1.5)
2. **Support multiple LLM providers** (Phase 2)
3. **Add visualization** (Phase 2)
4. **Improve context window management** (Phase 2)
5. **Add more COBOL patterns** (Phase 3)
6. **Add new languages** (Java, Python, etc.) using base framework

## Risk Mitigation
- **LLM API failures**: Implement retry logic and fallbacks
- **Context window limits**: Use simple chunking strategy
- **Accuracy issues**: Focus on high-confidence extractions only
- **Performance problems**: Process sections in parallel
- **Complex COBOL patterns**: Start with common patterns only

This MVP focuses on getting the core functionality working reliably with high accuracy, setting the foundation for more advanced features in later phases.
