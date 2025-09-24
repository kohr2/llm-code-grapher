# Fraud Management System Ontology Guide

This guide explains how to use the base ontology models to represent high-level logic for the COBOL fraud management system.

## Overview

The fraud management system is a complex COBOL application that implements real-time fraud detection using multiple business rules and advanced analytics. This guide shows how to model this system using the base ontology classes defined in `lang/base/ontology/base_models.py`.

## Key Components

### 1. Program Structure

The fraud management system is represented as a `BaseProgram` with the following characteristics:

- **Name**: FRAUD-MGMT-SYSTEM
- **Language**: COBOL
- **Domain**: Financial Services - Fraud Prevention
- **Complexity**: HIGH (45.2 cyclomatic complexity)
- **Business Value**: CRITICAL

### 2. Main Sections

The system is organized into several main sections, each represented as `BaseSection`:

#### Core Processing Sections
- **0000-MAIN-CONTROL**: Program entry point and main control flow
- **2000-PROCESS-TRANSACTIONS**: Main transaction processing loop
- **2600-EXECUTE-FRAUD-RULES**: Core fraud detection logic

#### Advanced Analytics Sections
- **4000-ADVANCED-ANALYTICS**: ML-based fraud detection
- **5000-REAL-TIME-SCORING**: Real-time risk scoring with ensemble models

### 3. Business Rules

The system implements 10 fraud detection rules, each modeled as `BaseBusinessRule`:

| Rule ID | Description | Priority | Risk Level |
|---------|-------------|----------|------------|
| RULE-01 | High Amount Transaction | HIGH | MEDIUM |
| RULE-02 | Transaction Velocity Analysis | HIGH | MEDIUM |
| RULE-03 | Geographical Location Analysis | MEDIUM | MEDIUM |
| RULE-04 | Merchant Risk Assessment | MEDIUM | LOW |
| RULE-05 | Unusual Time Pattern | LOW | LOW |
| RULE-06 | Card Not Present Risk | HIGH | MEDIUM |
| RULE-07 | Suspicious Category Combinations | MEDIUM | MEDIUM |
| RULE-08 | Customer Behavioral Analysis | HIGH | HIGH |
| RULE-09 | New Account Risk | MEDIUM | MEDIUM |
| RULE-10 | Cross-validation of Risk Factors | HIGH | HIGH |

### 4. Data Items

Key data structures are modeled as `BaseDataItem`:

- **TRANSACTION-RECORD**: Main transaction data structure
- **CUSTOMER-RECORD**: Customer profile and risk data
- **VELOCITY-RECORD**: Transaction velocity tracking
- **WS-TOTAL-RISK-SCORE**: Aggregated risk score

### 5. Operations

Granular operations are modeled as `BaseOperation`:

- **check-high-amount**: Amount threshold validation
- **check-velocity-limits**: Velocity limit checking
- **calculate-risk-score**: Risk score computation
- **detect-round-dollar-pattern**: Pattern detection

## Usage Examples

### Basic Ontology Creation

```python
from examples.fraud_management_ontology_example import FraudManagementOntology

# Create the fraud management ontology
fraud_ontology = FraudManagementOntology()

# Get the complete ontology result
ontology_result = fraud_ontology.create_ontology_result()

# Access program information
print(f"Program: {ontology_result.program.name}")
print(f"Purpose: {ontology_result.program.metadata['purpose']}")
```

### Enhanced Parser Integration

```python
from examples.cobol_parser_integration_example import EnhancedCOBOLParser

# Initialize enhanced parser
parser = EnhancedCOBOLParser()

# Parse COBOL file with business context
result = parser.parse_with_ontology("path/to/fraud_system.cbl")

# Access enhanced results
business_context = result['business_context']
enhanced_metrics = result['enhanced_metrics']
```

### Metrics and Analysis

```python
# Get complexity metrics
complexity_metrics = fraud_ontology.get_complexity_metrics()
print(f"Cyclomatic Complexity: {complexity_metrics['cyclomatic_complexity']}")

# Get quality indicators
quality_indicators = fraud_ontology.get_quality_indicators()
print(f"Business Rule Coverage: {quality_indicators['business_rule_coverage']}")

# Get ontology metrics
ontology_metrics = fraud_ontology.get_ontology_metrics()
print(f"Total Business Rules: {ontology_metrics['total_business_rules']}")
```

## Business Logic Mapping

### Section to Business Context Mapping

| COBOL Section | Business Purpose | Risk Level | Complexity |
|---------------|------------------|------------|------------|
| 0000-MAIN-CONTROL | Program orchestration | LOW | 0.3 |
| 2000-PROCESS-TRANSACTIONS | Transaction processing | MEDIUM | 0.4 |
| 2600-EXECUTE-FRAUD-RULES | Fraud detection | HIGH | 0.8 |
| 4000-ADVANCED-ANALYTICS | ML-based analysis | HIGH | 0.9 |
| 5000-REAL-TIME-SCORING | Real-time scoring | HIGH | 0.85 |

### Operation to Business Function Mapping

| COBOL Operation | Business Function | Parameters | Risk Level |
|-----------------|-------------------|------------|------------|
| check-high-amount | Amount validation | TRANS-AMOUNT, SUSPICIOUS-AMOUNT | LOW |
| check-velocity-limits | Velocity analysis | VELO-TRANS-COUNT-1H, MAX-HOURLY-VELOCITY | MEDIUM |
| calculate-risk-score | Risk computation | Multiple risk factors | MEDIUM |
| detect-round-dollar-pattern | Pattern detection | TRANS-AMOUNT, 100.00 | LOW |

## Risk Assessment

### Component Risk Levels

- **HIGH RISK**: Advanced analytics, real-time scoring, behavioral analysis
- **MEDIUM RISK**: Transaction processing, velocity checks, location analysis
- **LOW RISK**: Main control, basic validations, data movement

### Business Impact

- **CRITICAL**: Fraud prevention system failure could result in significant financial losses
- **HIGH**: System availability and performance are essential for business operations
- **MEDIUM**: Data accuracy and rule effectiveness impact fraud detection rates

## Quality Metrics

### Complexity Metrics

- **Cyclomatic Complexity**: 45.2 (HIGH)
- **Maintainability Index**: 65.0 (MEDIUM)
- **Technical Debt**: 78.5 (HIGH)
- **Business Rule Complexity**: 85.0 (VERY HIGH)

### Quality Indicators

- **Code Coverage**: UNKNOWN (legacy system)
- **Documentation Quality**: MEDIUM
- **Test Coverage**: LOW
- **Business Rule Coverage**: HIGH
- **Data Validation**: HIGH
- **Error Handling**: MEDIUM

## Validation and Consistency

### Parser Validation

- Duplicate section names
- Duplicate subsection names
- Structural integrity

### Ontology Validation

- Missing business rules
- High-risk components without relationships
- Data items without operations

### Business Consistency

- Fraud rule implementation completeness
- Error handling in high-risk sections
- Data flow consistency

## Modernization Recommendations

### High Priority

1. **Add comprehensive test coverage** for all fraud rules
2. **Implement proper error handling** for high-risk sections
3. **Add logging and monitoring** for business rule execution
4. **Document business logic** for each fraud rule

### Medium Priority

1. **Refactor complex sections** to reduce cyclomatic complexity
2. **Implement configuration management** for fraud thresholds
3. **Add performance monitoring** for real-time scoring
4. **Create data validation framework**

### Low Priority

1. **Modernize data structures** to use more flexible formats
2. **Implement microservices architecture** for scalability
3. **Add API interfaces** for external system integration
4. **Create automated deployment pipeline**

## Integration with Existing Parser

The enhanced parser (`EnhancedCOBOLParser`) provides:

1. **Business Context Mapping**: Maps COBOL sections to business purposes
2. **Rule Identification**: Identifies fraud rules in the code
3. **Risk Assessment**: Evaluates business risk of components
4. **Enhanced Metrics**: Combines technical and business metrics
5. **Consistency Validation**: Ensures business logic consistency

## Best Practices

### When Creating Ontology Models

1. **Use descriptive names** that reflect business purpose
2. **Set appropriate risk levels** based on business impact
3. **Include comprehensive metadata** for context
4. **Validate relationships** between components
5. **Document business logic** clearly

### When Analyzing COBOL Code

1. **Identify business domains** first
2. **Map technical components** to business functions
3. **Assess risk levels** based on business impact
4. **Validate completeness** of business rule implementation
5. **Consider modernization** opportunities

## Conclusion

The base ontology models provide a powerful framework for representing complex business logic in legacy COBOL systems. By mapping technical components to business concepts, we can better understand, maintain, and modernize these critical systems.

The fraud management system example demonstrates how to:
- Model complex business rules
- Represent data flow and relationships
- Assess risk and complexity
- Plan modernization efforts
- Validate system consistency

This approach can be applied to other COBOL systems and extended to support additional programming languages and business domains.
