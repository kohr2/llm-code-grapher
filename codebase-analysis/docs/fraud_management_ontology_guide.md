# Fraud Management Ontology Guide

## Overview
Guide for using base ontology models to represent COBOL fraud management system logic.

## System Components

### Program Structure
- **Name**: FRAUD-MGMT-SYSTEM
- **Language**: COBOL
- **Domain**: Financial Services - Fraud Prevention
- **Complexity**: HIGH (45.2 cyclomatic complexity)

### Main Sections
| Section | Purpose | Risk Level |
|---------|---------|------------|
| 0000-MAIN-CONTROL | Program entry point | LOW |
| 2000-PROCESS-TRANSACTIONS | Transaction processing | MEDIUM |
| 2600-EXECUTE-FRAUD-RULES | Core fraud detection | HIGH |
| 4000-ADVANCED-ANALYTICS | ML-based detection | HIGH |
| 5000-REAL-TIME-SCORING | Real-time scoring | HIGH |

### Business Rules (Dynamic Analysis)
Business rules are dynamically analyzed from COBOL code sections:

| Analysis Method | Description | Confidence Range |
|-----------------|-------------|------------------|
| LLM Analysis | Contextual understanding of business logic | 0.85-0.95 |
| Pattern Recognition | Identifies rule implementations in code | 0.8-0.9 |
| Risk Assessment | Dynamic priority and risk level determination | 0.8-0.9 |

**Example Rule Types Detected:**
- High Amount Transaction Detection
- Transaction Velocity Analysis  
- Geographical Location Analysis
- Merchant Risk Assessment
- Time Pattern Analysis
- Card Not Present Risk
- Suspicious Category Combinations
- Customer Behavioral Analysis
- Account Age Risk
- Cross-validation of Risk Factors

### Data Items
- **TRANSACTION-RECORD**: Main transaction data
- **CUSTOMER-RECORD**: Customer profile and risk data
- **VELOCITY-RECORD**: Transaction velocity tracking
- **WS-TOTAL-RISK-SCORE**: Aggregated risk score

## Usage Examples

### Basic Usage
```python
from examples.fraud_management_ontology_example import FraudManagementOntology

ontology = FraudManagementOntology()
ontology_result = ontology.create_ontology_result()
print(f"Program: {ontology_result.program.name}")
```

### Enhanced Parser
```python
from examples.cobol_parser_integration_example import EnhancedCOBOLParser

parser = EnhancedCOBOLParser()
result = parser.parse_with_ontology("path/to/fraud_system.cbl")
business_context = result['business_context']
```

### Metrics Analysis
```python
complexity_metrics = ontology.get_complexity_metrics()
quality_indicators = ontology.get_quality_indicators()
ontology_metrics = ontology.get_ontology_metrics()
```

## Quality Metrics

| Metric | Value | Level |
|--------|-------|-------|
| Cyclomatic Complexity | 45.2 | HIGH |
| Maintainability Index | 65.0 | MEDIUM |
| Technical Debt | 78.5 | HIGH |
| Business Rule Coverage | HIGH | HIGH |

## Modernization Priorities

### High Priority
1. Add comprehensive test coverage for fraud rules
2. Implement proper error handling for high-risk sections
3. Add logging and monitoring for business rule execution

### Medium Priority
1. Refactor complex sections to reduce cyclomatic complexity
2. Implement configuration management for fraud thresholds
3. Add performance monitoring for real-time scoring

## Best Practices

### Ontology Models
- Use descriptive names reflecting business purpose
- Set appropriate risk levels based on business impact
- Include comprehensive metadata for context
- Validate relationships between components

### COBOL Analysis
- Identify business domains first
- Map technical components to business functions
- Assess risk levels based on business impact
- Validate completeness of business rule implementation
