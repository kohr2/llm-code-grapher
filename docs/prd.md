# COBOL Code Grapher - Product Requirements Document

## Overview
A Python program that analyzes COBOL code structure using LLMs to create hierarchical mappings of sections and subsections, including business logic extraction and relationship mapping.

## Core Requirements

### 1. Hierarchical Structure Mapping
- **Sections**: Identify and map main COBOL sections (PROCEDURE, DATA, FILE, etc.)
  - Description, line range, line count, business logic
  - Section type classification (PROCEDURE, DATA, FILE, WORKING-STORAGE, etc.)
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
- **Lightweight COBOL parsing**: Use regex patterns for structure detection
- **LLM semantic analysis**: Extract business logic and relationships
- **Multi-pass processing**: Structure → Semantics → Relationships → Validation

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
1. **Pass 1**: Structure Detection (regex-based section identification)
2. **Pass 2**: Semantic Analysis (LLM for business logic extraction)
3. **Pass 3**: Relationship Mapping (LLM for connection analysis)
4. **Pass 4**: Validation & Refinement (cross-checking and improvement)

## Data Model

### COBOLSection
```python
class COBOLSection:
    name: str
    type: str  # PROCEDURE, DATA, FILE, WORKING-STORAGE, etc.
    line_range: tuple[int, int]
    line_count: int
    business_logic: str
    complexity_score: float
    confidence: float
    dependencies: List[str]
    data_flow: Dict[str, List[str]]
    control_flow: List[str]
    subsections: List[COBOLSubsection]
```

### COBOLSubsection
```python
class COBOLSubsection:
    name: str
    parent_section: str
    line_range: tuple[int, int]
    line_count: int
    business_logic: str
    confidence: float
    relationships: List[Relationship]
```

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

### COBOL Patterns
- Section identification patterns
- Subsection detection rules
- Data structure patterns
- Custom pattern definitions

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

## Performance Requirements

### Scalability
- Handle COBOL programs up to 100,000+ lines
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

## Future Enhancements

### Phase 2 Features
- Real-time analysis and monitoring
- Integration with version control systems
- Automated refactoring suggestions
- Code quality metrics and recommendations

### Phase 3 Features
- Multi-language support (PL/I, RPG, etc.)
- Cloud deployment and scaling
- Team collaboration features
- Integration with development workflows
