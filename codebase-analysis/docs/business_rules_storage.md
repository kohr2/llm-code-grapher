# Business Rules Storage

## Overview
Business rules are dynamically analyzed and stored using LLM-based analysis across multiple locations:

## Storage Locations

### 1. **Neo4j Database** (Primary)
- **Type**: `BusinessRule` nodes
- **Source**: LLM analysis of code sections
- **Format**: Cypher nodes with properties

### 2. **LLM Analysis** (Dynamic Generation)
- **Method**: `ParserResultConverter._analyze_business_rule_with_llm()`
- **Provider**: Configurable LLM provider (OpenAI, Ollama, etc.)
- **Analysis**: Contextual understanding of business logic

### 3. **COBOL Code** (Implementation)
- **File**: `data/fixtures/vasu_fraud_management_cobol_reformatted.cbl`
- **Sections**: 2610-2695 (rule implementation sections)

### 4. **Runtime Objects** (Memory)
- **Class**: `BusinessRule` instances generated from LLM analysis
- **Container**: Dynamic rule collection based on code analysis

## Dynamic Rule Analysis

Business rules are now analyzed dynamically using LLM:

| Analysis Method | Description | Confidence |
|-----------------|-------------|------------|
| LLM Analysis | Contextual understanding of business logic | 0.85-0.95 |
| Code Pattern Recognition | Identifies rule implementations in code | 0.8-0.9 |
| Risk Assessment | Dynamic priority and risk level determination | 0.8-0.9 |

## Quick Access

### Neo4j Queries
```cypher
// All dynamically analyzed rules
MATCH (r:BusinessRule) RETURN r.rule_id, r.description, r.priority, r.analysis_method

// High-priority rules
MATCH (r:BusinessRule) WHERE r.priority = 'HIGH' RETURN r.rule_id, r.description

// LLM-analyzed rules
MATCH (r:BusinessRule) WHERE r.analysis_method = 'LLM' RETURN r.rule_id, r.analysis_confidence
```

### Python Access
```python
from src.neo4j_converter import ParserResultConverter
from lang.base.parser.llm_provider import LLMProviderConfig

# Dynamic rule analysis
llm_config = LLMProviderConfig(provider="openai", model="gpt-4", api_key="your-key")
converter = ParserResultConverter(llm_config)
rules = converter._analyze_business_rules_from_sections(code_sections)
```

## Data Flow
```
COBOL Code → LLM Analysis → Dynamic Rule Generation → Neo4j Converter → Graph Database
```
