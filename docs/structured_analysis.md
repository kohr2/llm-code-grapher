# Structured LLM Analysis for COBOL

## Overview

The structured analysis feature provides a comprehensive approach to analyzing COBOL files by sending the complete file to an LLM and requesting a structured response that contains all information necessary to build graphs and extract business rules.

## Features

### Complete File Analysis
- Analyzes the entire COBOL file in one LLM call
- Provides comprehensive understanding of the program structure
- Extracts detailed business logic and relationships

### Structured Response Format
The LLM response includes:
- **Overview**: Program purpose and main functionality
- **Sections**: Detailed analysis of each section/division
- **Relationships**: Dependencies and calls between sections
- **Business Rules**: Extracted business logic with conditions and actions
- **Data Elements**: Important data structures and their purposes
- **Risk Assessment**: Overall risk evaluation and mitigation suggestions
- **Suggestions**: Modernization, performance, and maintainability recommendations

### Graph-Ready Information
All response data is structured to directly support:
- Neo4j graph database insertion
- Business rule extraction and storage
- Relationship mapping
- Risk analysis and visualization

## Usage

### Command Line Interface

#### Structured Analysis (requires API key)
```bash
python -m src.cli analyze your_file.cbl --structured-analysis
```

#### Basic Parser Analysis (no API key required)
```bash
python -m src.cli analyze your_file.cbl
```

### Configuration

Set up your OpenAI API key:
```bash
export OPENAI_API_KEY='your-api-key-here'
```

Or configure in `config.yaml`:
```yaml
llm:
  provider: "openai"
  model: "gpt-4"
  api_key: "your-api-key-here"
  max_tokens: 4000
  temperature: 0.1
```

### Example Output Structure

```json
{
  "program_name": "FRAUD-MANAGEMENT",
  "language": "COBOL",
  "analysis_strategy": "structured_llm",
  "overview": "Fraud detection and management system...",
  "sections": [
    {
      "name": "1000-MAIN-PROCESSING",
      "type": "PARAGRAPH",
      "line_range": [270, 330],
      "business_logic": "Main processing logic for fraud detection",
      "complexity_score": 0.3,
      "risk_level": "MEDIUM",
      "confidence": 0.9,
      "operations": ["OPEN", "PERFORM", "CLOSE"],
      "parameters": ["FRAUD-FILE", "REPORT-FILE"],
      "dependencies": ["2000-PROCESS-RECORDS", "3000-GENERATE-REPORT"]
    }
  ],
  "business_rules": [
    {
      "rule_id": "BR001",
      "rule_type": "VALIDATION",
      "description": "High-risk transaction detection",
      "conditions": "RISK-SCORE > 700",
      "actions": "Add to high-risk count and write to report",
      "section_reference": "2100-ANALYZE-TRANSACTION",
      "line_reference": "430-470",
      "risk_level": "HIGH",
      "complexity": 0.4
    }
  ],
  "relationships": [
    {
      "source": "1000-MAIN-PROCESSING",
      "target": "2000-PROCESS-RECORDS",
      "relationship_type": "CALLS",
      "confidence": 1.0,
      "strength": 1.0,
      "description": "Main processing calls record processing"
    }
  ],
  "data_elements": [
    {
      "name": "RISK-SCORE",
      "type": "FIELD",
      "section_reference": "DATA DIVISION",
      "business_purpose": "Numeric risk assessment score",
      "usage_pattern": "Used in transaction validation logic"
    }
  ],
  "risk_assessment": {
    "overall_risk": "MEDIUM",
    "risk_factors": ["Complex business logic", "File I/O operations"],
    "mitigation_suggestions": ["Add error handling", "Implement logging"]
  },
  "suggestions": {
    "modernization": ["Use modern file handling", "Implement object-oriented design"],
    "performance": ["Optimize file I/O", "Add indexing"],
    "maintainability": ["Add documentation", "Simplify nested logic"],
    "security": ["Validate input data", "Implement access controls"]
  },
  "confidence": 0.85
}
```

## Implementation Details

### LLM Prompt Structure
The structured analysis uses a comprehensive prompt that requests:
1. Program overview and purpose
2. Section-by-section analysis with metadata
3. Relationship mapping between components
4. Business rule extraction with conditions and actions
5. Data element cataloging
6. Risk assessment and mitigation
7. Improvement suggestions

### Response Parsing
The `_parse_structured_response` method processes the LLM output and creates a structured dictionary containing all analysis results.

### Integration with Existing System
- Maintains compatibility with existing parser-based analysis
- Can be used as an optional strategy alongside current methods
- Integrates with Neo4j conversion and output generation
- Supports both CLI and programmatic usage

## Benefits

### For Graph Building
- Complete relationship mapping between all program components
- Detailed metadata for nodes and edges
- Business rule extraction for specialized nodes
- Risk and complexity metrics for visualization

### For Business Rule Extraction
- Structured identification of business logic
- Condition and action mapping
- Risk assessment for each rule
- Section and line references for traceability

### For Analysis Quality
- Comprehensive understanding of program purpose
- Detailed business logic descriptions
- Modernization and improvement suggestions
- Risk assessment and mitigation strategies

## Fallback Behavior

If structured analysis fails:
- Falls back to basic parser analysis
- Logs the error but continues processing
- Maintains system reliability
- Provides graceful degradation

## Example Usage

See `examples/structured_analysis_example.py` for a complete demonstration of the structured analysis functionality.

## Future Enhancements

- Support for other programming languages
- Customizable prompt templates
- Batch processing capabilities
- Integration with additional LLM providers
- Enhanced business rule categorization
