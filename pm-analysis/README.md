# Product Management Integration Guide

This directory contains documentation and implementation guidance for integrating the LLM Code Grapher with product management systems.

## Overview

The goal is to automatically map code structure and business logic to product requirements through deep analysis of:

1. **Code Business Logic** (extracted via LLM analysis)
2. **Git History** (commit messages and file edits)
3. **Requirement Content** (descriptions, acceptance criteria)

## Integration Strategy

### Multi-Source Evidence Approach

Instead of simple keyword matching, we use **triangulated evidence** from multiple sources:

```
Code Analysis → Business Logic Extraction → Semantic Matching → Requirements
     ↓                    ↓                       ↓              ↓
Git History → File Edit Analysis → Change Pattern Matching → Requirements
     ↓                    ↓                       ↓              ↓
Requirement Content → Business Capability Extraction → Similarity Scoring → Code
```

## Implementation Phases

### Phase 1: Enhanced Business Logic Extraction

**Goal**: Extract structured business information from code analysis

**Implementation**:
- Extend existing LLM analysis to extract:
  - Business domains (fraud_detection, customer_management)
  - Operations (validates, calculates, processes)
  - Data entities (customer_account, transaction)
  - Business rules (validation_rules, approval_criteria)

**Files to create**:
```
src/business_logic_analyzer.py
src/business_capability_extractor.py
```

### Phase 2: Git History Analysis

**Goal**: Analyze git history to find concrete evidence of code-requirement relationships

**Implementation**:
- Parse commit messages for requirement identifiers
- Analyze actual file edits/patches
- Extract business logic from code changes
- Build evidence database of code-requirement links

**Files to create**:
```
src/git_requirement_analyzer.py
src/file_edit_analyzer.py
src/patch_content_analyzer.py
```

### Phase 3: Requirement Content Analysis

**Goal**: Analyze requirements to understand their business context

**Implementation**:
- Extract business capabilities from requirement descriptions
- Parse acceptance criteria for business rules
- Build business capability taxonomy
- Create requirement pattern library

**Files to create**:
```
src/requirement_business_analyzer.py
src/requirement_pattern_extractor.py
```

### Phase 4: Semantic Matching Engine

**Goal**: Create intelligent matching between code and requirements

**Implementation**:
- Use embeddings for semantic similarity
- Implement multi-factor confidence scoring
- Cross-validate evidence from different sources
- Generate mapping suggestions with confidence scores

**Files to create**:
```
src/semantic_matcher.py
src/multi_source_mapper.py
src/confidence_calculator.py
```

### Phase 5: Automated Mapping System

**Goal**: Implement end-to-end automated mapping

**Implementation**:
- Create mapping pipeline
- Implement confidence thresholds
- Build bidirectional synchronization
- Create audit trails

**Files to create**:
```
src/automated_mapper.py
src/mapping_pipeline.py
src/sync_manager.py
```

## Configuration

### Extended config.yaml

```yaml
# Add to existing config.yaml
product_management:
  requirement_system:
    enabled: true
    type: "jira"  # jira, azure_devops, linear, etc.
    url: "https://your-domain.atlassian.net"
    username: "your-email@domain.com"
    api_token: null  # Set via PM_API_TOKEN env var
    project_key: "PROJ"
    
  git:
    enabled: true
    repo_path: "/path/to/your/repo"
    requirement_patterns:
      - "([A-Z]+-\\d+)"      # PROJ-123
      - "#([A-Z]+-\\d+)"     # #PROJ-123
    
  mapping:
    confidence_threshold: 0.8
    evidence_sources: ["semantic", "git", "requirements"]
    validation_strategy: "cross_reference"
    git_evidence_weight: 0.3
    semantic_evidence_weight: 0.5
    requirement_evidence_weight: 0.2
```

## Usage Examples

### 1. Analyze Code and Map to Requirements

```bash
# Analyze code structure and map to requirements
python -m src.cli analyze file.cbl --map-to-requirements --confidence-threshold 0.8

# Analyze with git history integration
python -m src.cli analyze file.cbl --map-to-requirements --include-git-history

# Export mapping results
python -m src.cli analyze file.cbl --map-to-requirements --export-mappings mappings.json
```

### 2. Validate Existing Mappings

```bash
# Validate existing code-requirement mappings
python -m src.cli validate-mappings --project PROJ

# Check mapping confidence scores
python -m src.cli validate-mappings --show-confidence-scores
```

### 3. Synchronize Changes

```bash
# Sync new code changes to requirements
python -m src.cli sync-changes --direction code-to-requirements

# Sync requirement changes to code analysis
python -m src.cli sync-changes --direction requirements-to-code

# Full bidirectional sync
python -m src.cli sync-changes --direction both
```

## Evidence Types

### 1. Semantic Evidence
- Business logic similarity between code and Jira descriptions
- Domain and operation matching
- Business rule overlap

### 2. Git Evidence
- Commit messages linking to requirements
- Actual file edits for specific requirements
- Function/section modifications
- Business logic changes in code

### 3. Requirement Evidence
- Requirement descriptions and acceptance criteria
- Business capability extraction
- Requirement pattern matching

## Confidence Scoring

Each mapping receives a confidence score based on:

```python
confidence = (
    semantic_similarity * 0.5 +
    git_evidence_strength * 0.3 +
    requirement_content_match * 0.2
)
```

Where:
- **semantic_similarity**: How well business logic matches (0.0-1.0)
- **git_evidence_strength**: Quality of git history evidence (0.0-1.0)
- **requirement_content_match**: How well requirement content matches code (0.0-1.0)

## Output Formats

### Mapping Results

```json
{
  "code_node_id": "VALIDATE-CUSTOMER-SECTION",
  "mappings": [
    {
      "requirement_id": "PROJ-123",
      "confidence": 0.92,
      "evidence_sources": ["semantic", "git", "requirements"],
      "evidence_details": {
        "semantic_similarity": 0.89,
        "git_commits": ["abc123", "def456"],
        "functions_modified": ["VALIDATE-CUSTOMER", "CHECK-ACCOUNT-STATUS"],
        "business_logic_changes": ["customer validation", "fraud detection"]
      }
    }
  ]
}
```

## Implementation Guidelines

### 1. Follow Existing Patterns
- Use the same LLM provider pattern as `lang/base/parser/llm_provider.py`
- Follow configuration management from `src/config_manager.py`
- Maintain CLI interface consistency

### 2. Error Handling
- Graceful degradation when requirement system API is unavailable
- Fallback to semantic-only matching when git history is missing
- Clear error messages for configuration issues

### 3. Performance Considerations
- Cache requirement system API responses
- Batch process multiple files
- Use async operations for API calls
- Implement rate limiting for external APIs

### 4. Security
- Store API tokens in environment variables
- Never commit credentials to repository
- Use secure connections for API calls
- Implement proper authentication

## Testing Strategy

### 1. Unit Tests
- Test individual analyzers in isolation
- Mock external API calls
- Validate confidence scoring algorithms

### 2. Integration Tests
- Test with real requirement system instance (sandbox)
- Validate git history parsing
- Test end-to-end mapping pipeline

### 3. Validation Tests
- Compare automated mappings with manual mappings
- Measure accuracy against known relationships
- Test with different codebases and requirement systems

## Next Steps

1. **Start with Phase 1**: Implement enhanced business logic extraction
2. **Create proof of concept**: Test with a small subset of code and requirements
3. **Iterate and refine**: Adjust confidence scoring based on results
4. **Scale gradually**: Expand to larger codebases and more complex requirements

## Dependencies

Add to `requirements.txt`:
```
requests>=2.31.0
python-dotenv>=1.0.0
sentence-transformers>=2.2.0
GitPython>=3.1.0
```

## Related Documentation

- [Model Architecture](MODEL_ARCHITECTURE.md)
- [Data Models](DATA_MODELS.md)
- [API Specification](API_SPECIFICATION.md)
