# LLM Analysis Integration

## Overview
Neo4j converter now uses LLM-based analysis for business rule analysis, providing more accurate and contextual analysis across programming languages.

## Key Changes

### 1. LLM Integration
```python
from lang.base.parser.llm_provider import LLMProviderConfig
from src.neo4j_converter import ParserResultConverter

llm_config = LLMProviderConfig(
    provider="openai",
    model="gpt-4",
    api_key="your-api-key"
)
converter = ParserResultConverter(llm_config, "cobol")
```

### 2. Enhanced Business Rule Properties
```python
{
    "description": "LLM-generated contextual description",
    "priority": "HIGH|MEDIUM|LOW",  # LLM-determined
    "risk_level": "CRITICAL|HIGH|MEDIUM|LOW",  # LLM-assessed
    "functional_area": "ANALYZE-TRANSACTION|ADVANCED-ANALYTICS|REAL-TIME-SCORING",
    "analysis_confidence": 0.85,  # LLM confidence score
    "analysis_method": "LLM"  # Always LLM now
}
```

### 3. LLM Analysis Prompt
```
Analyze this COBOL business rule implementation:

Rule Name: [rule_name]
Business Logic Code: [business_logic]

Provide analysis for:
1. DESCRIPTION: Clear description of what this rule does
2. PRIORITY: HIGH/MEDIUM/LOW based on business criticality
3. RISK_LEVEL: CRITICAL/HIGH/MEDIUM/LOW based on complexity
4. FUNCTIONAL_AREA: Which functional area this rule belongs to
5. CONFIDENCE: Analysis confidence (0.0 to 1.0)
```

## LLM-Only Analysis
- LLM configuration is mandatory for Neo4j conversion
- All business rules are dynamically analyzed using LLM
- No hardcoded rule fallbacks - ensures consistent analysis quality
- Clear error messages when LLM is unavailable
- Dynamic rule generation based on actual code analysis

## Usage Examples

### CLI Usage
```bash
# With LLM analysis (API key required)
export OPENAI_API_KEY="your-api-key"
python -m src.cli analyze input.cbl --neo4j

# Without API key (will fail with clear error)
python -m src.cli analyze input.cbl --neo4j
```

### Programmatic Usage
```python
from lang.base.parser.llm_provider import LLMProviderConfig
from src.neo4j_converter import convert_parser_result_to_neo4j

llm_config = LLMProviderConfig(
    provider="openai",
    model="gpt-4",
    api_key="your-key"
)
graph_data = convert_parser_result_to_neo4j(parser_result, llm_config)
```

## Benefits
- **More Accurate Analysis**: LLM provides contextual understanding
- **Dynamic Assessment**: Priority and risk levels based on actual code analysis
- **Intelligent Mapping**: Better functional area classification
- **Higher Confidence**: LLM analysis typically has higher confidence scores
- **Consistent Quality**: No hardcoded fallbacks ensure uniform analysis quality

## Configuration

### config.yaml
```yaml
llm:
  provider: "openai"
  model: "gpt-4"
  api_key: "${OPENAI_API_KEY}"
  max_tokens: 2000
  temperature: 0.1
```

### Environment Variables
```bash
export OPENAI_API_KEY="your-api-key"
export LLM_PROVIDER="openai"
export LLM_MODEL="gpt-4"
```

## Testing
```bash
python examples/llm_analysis_example.py
```
