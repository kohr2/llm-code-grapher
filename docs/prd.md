# LLM Code Grapher - Product Requirements Document

## Overview
A Python program that analyzes code structure using LLMs to create hierarchical mappings of sections and subsections, including business logic extraction and relationship mapping. Initially focused on COBOL but designed to support multiple programming languages through a language-agnostic architecture.

## Core Requirements

### 1. Hierarchical Structure Mapping
- **Sections**: Identify and map main code sections (language-specific)
  - Description, line range, line count, business logic
  - Section type classification (language-specific patterns)
  - Complexity scoring and metrics
- **Subsections**: Map subsections within each section
  - Same metadata as sections
  - Hierarchical relationships (parent-child)
- **Relationship Mapping**: 
  - Section-to-section relationships (calls, data flow, control flow)
  - Section-to-subsection relationships
  - Subsection-to-subsection relationships
  - Data dependencies and control flow

### 2. Business Logic Extraction
- Natural language descriptions of what each section/subsection does
- Data flow analysis (inputs, outputs, transformations)
- Control flow patterns (loops, conditions, branches)
- Business rules and logic patterns
- Confidence scoring for extracted information

## Technical Strategy

### Hybrid Approach
- **Lightweight language parsing**: Use regex patterns for structure detection (language-specific)
- **LLM semantic analysis**: Extract business logic and relationships
- **Multi-pass processing**: Structure → Semantics → Relationships → Validation
- **Language-agnostic core**: Common processing pipeline with language-specific extensions

### Language Extension Architecture
- **Base Framework**: Abstract classes for parsers, analyzers, and ontologies
- **Language Extensions**: Language-specific implementations (COBOL, Java, Python, etc.)
- **Language-Agnostic Core**: Common processing logic in `src/` directory
- **Extensible Design**: Easy addition of new languages with minimal code

### Base Parser Framework
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

### LLM Integration
- Support multiple LLM providers (OpenAI, Anthropic, Ollama)
- Configurable models and parameters
- Fallback strategies for different model capabilities
- Cost optimization through model selection

## Context Window Management

### Chunking Strategies
- **Sliding window**: Process 2000-3000 lines with 200-line overlap
- **Hierarchical chunking**: High-level structure first, then drill down
- **Summary chains**: Use smaller models for summaries, larger for details
- **Vector embeddings**: Store section embeddings for similarity analysis

### Processing Pipeline
1. **Pass 1**: Structure Detection (language-specific regex patterns)
2. **Pass 2**: Semantic Analysis (LLM for business logic extraction)
3. **Pass 3**: Relationship Mapping (LLM for connection analysis)
4. **Pass 4**: Validation & Refinement (cross-checking and improvement)

### Language Extension Architecture
- **Base Framework**: Abstract classes for parsers, analyzers, and ontologies
- **Language Extensions**: Language-specific implementations (COBOL, Java, Python, etc.)
- **Language-Agnostic Core**: Common processing logic in `src/` directory
- **Extensible Design**: Easy addition of new languages with minimal code

### Directory Structure
```
llm-code-grapher/
├── src/                          # Language-agnostic core components
│   ├── config_manager.py         # Configuration management
│   ├── output_generator.py       # Output generation
│   ├── utils.py                  # Utility functions
│   └── cli.py                   # Command-line interface
├── lang/                         # Language-specific extensions
│   ├── base/                     # Abstract base classes
│   │   ├── parser/               # Base parser framework
│   │   ├── ontology/             # Base ontology framework
│   │   └── tests/                # Base test framework
│   └── cobol/                    # COBOL language package
│       ├── parser/               # COBOL parser implementation
│       ├── ontology/             # COBOL ontology implementation
│       └── tests/                # COBOL-specific tests
└── main.py                       # Application entry point
```

**Architecture Principles:**
- **Separation of Concerns**: Language-specific code isolated in `lang/` directory
- **Language-Agnostic Core**: `src/` contains only reusable, language-independent components
- **Extensibility**: New languages added as subdirectories under `lang/`
- **Consistency**: All languages follow the same interface patterns

## Data Model

### Interface-Based COBOL Ontology
The ontology is implemented using Python abstract base classes and interfaces, providing type safety and better maintainability than YAML-based approaches.

**Architecture Benefits:**
- **Type Safety**: Full compile-time type checking with Python interfaces
- **IDE Support**: Autocomplete, refactoring, and error detection
- **Single Source of Truth**: All ontology logic in Python code
- **Easy Testing**: Simple to mock and unit test
- **No Runtime Overhead**: No YAML parsing at runtime

### Base Models (Language-Agnostic)
```python
class BaseSection:
    name: str
    type: str  # Language-specific section types
    line_range: tuple[int, int]
    line_count: int
    business_logic: str
    complexity_score: float
    confidence: float
    dependencies: List[str]
    data_flow: Dict[str, List[str]]
    control_flow: List[str]
    subsections: List[BaseSubsection]

class BaseSubsection:
    name: str
    parent_section: str
    line_range: tuple[int, int]
    line_count: int
    business_logic: str
    confidence: float
    relationships: List[BaseRelationship]
```

### COBOL Data Models
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

### Key Concepts
- **Program**: Complete COBOL program with metadata and quality metrics
- **Division**: Main COBOL divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- **Section**: Sections within divisions (FILE, WORKING-STORAGE, PROCEDURE_SECTION)
- **Subsection**: Paragraphs within sections
- **DataItem**: Data variables and structures with business meanings
- **BusinessRule**: Business logic and rules with risk assessments
- **Relationship**: Connections between components (CALLS, USES, MODIFIES, etc.)

### Quality Assessment
- **Complexity Metrics**: Cyclomatic complexity, maintainability index, technical debt
- **Risk Levels**: LOW, MEDIUM, HIGH, CRITICAL
- **Quality Indicators**: Code coverage, documentation quality, test coverage
- **Modernization Potential**: LOW, MEDIUM, HIGH

### Language-Specific Extensions
- **COBOLSection**: Extends BaseSection with COBOL-specific patterns
- **JavaClass**: Extends BaseSection with Java-specific patterns
- **PythonModule**: Extends BaseSection with Python-specific patterns

## Output Formats

### Primary Outputs
- **JSON/YAML**: Structured data with full metadata
- **Graph Database**: Neo4j format for complex relationship queries
- **Interactive Visualization**: Network graphs showing relationships
- **Documentation**: Auto-generated technical documentation

### Visualization Features
- Section hierarchy trees
- Relationship network graphs
- Data flow diagrams
- Complexity heatmaps
- Interactive exploration tools

## Configuration

### LLM Settings
- Provider selection (OpenAI, Anthropic, Ollama)
- Model configuration (GPT-4, Claude, local models)
- Token limits and temperature settings
- API key management

### Processing Settings
- Chunk size and overlap configuration
- Parallel processing options
- Retry mechanisms and error handling
- Confidence thresholds

### Language-Specific Patterns
- **COBOL**: Section identification patterns, subsection detection rules, data structure patterns
- **Java**: Class identification patterns, method detection rules, package structure patterns
- **Python**: Module identification patterns, function detection rules, import structure patterns
- **Custom pattern definitions** for each supported language

## Quality Assurance

### Validation Strategies
- Cross-reference validation of relationships
- Confidence scoring for all extractions
- Human-in-the-loop flagging for uncertain sections
- Test case validation with known COBOL programs

### Error Handling
- Graceful degradation for failed sections
- Comprehensive logging and error tracking
- Retry mechanisms with exponential backoff
- Fallback strategies for different failure modes

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

## Performance Requirements

### Scalability
- Handle code files up to 100,000+ lines (language-agnostic)
- Parallel processing of multiple sections
- Incremental updates for changed sections
- Memory-efficient streaming for large files

### Accuracy Targets
- >90% accuracy for section identification
- >80% accuracy for business logic extraction
- >85% accuracy for relationship mapping
- Confidence scoring for all extractions

## Library Recommendations

### Core Libraries
- **langchain**: LLM orchestration and prompt management
- **networkx**: Graph relationship modeling
- **pydantic**: Data validation and serialization
- **rich**: Progress bars and enhanced output
- **click**: Command-line interface

### LLM Providers
- **openai**: GPT models integration
- **anthropic**: Claude models integration
- **ollama**: Local model support

### Text Processing
- **regex**: COBOL pattern matching
- **nltk**: Natural language processing
- **spacy**: Advanced NLP capabilities

### Data & Visualization
- **pandas**: Data manipulation and analysis
- **matplotlib/plotly**: Visualization and graphs
- **graphviz**: Network diagram generation
- **pyyaml**: Configuration management

## Success Metrics

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

### Functional Metrics
- Successful analysis of 95%+ of COBOL programs
- Accurate identification of 90%+ of sections and subsections
- Meaningful business logic extraction for 80%+ of sections
- Valid relationship mapping for 85%+ of connections

### Performance Metrics
- Processing time < 1 minute per 1000 lines
- Memory usage < 2GB for 50,000 line programs
- API cost optimization through smart model selection
- User satisfaction with output quality and usability

## Architecture Optimization Benefits

### Code Reduction Summary
| Component | Before | After | Reduction |
|-----------|--------|-------|-----------|
| COBOL Parser | 310 lines | ~150 lines | **~52%** |
| COBOL LLM Analyzer | 145 lines | ~95 lines | **~34%** |
| COBOL Models | 80 lines | ~20 lines | **~75%** |
| COBOL Tests | 650 lines | ~400 lines | **~38%** |
| Base Classes | 0 lines | 800 lines | **New (reusable)** |

### Optimization Benefits
- **🔄 Maximum Consistency**: All languages inherit identical behavior
- **🛠️ Maximum Maintainability**: 90% of logic centralized in base classes
- **🚀 Maximum Extensibility**: New languages need minimal code
- **🧪 Maximum Testability**: Base classes fully testable independently
- **⚡ Maximum Performance**: Minimal code duplication

### Future Language Implementation
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

## Future Enhancements

### Phase 2 Features
- Real-time analysis and monitoring
- Integration with version control systems
- Automated refactoring suggestions
- Code quality metrics and recommendations

### Phase 3 Features
- Additional language support (PL/I, RPG, C++, etc.)
- Cloud deployment and scaling
- Team collaboration features
- Integration with development workflows
