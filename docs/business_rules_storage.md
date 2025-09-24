# High-Level Business Rules Storage Locations

## Overview

The **10 high-level business rules** from the fraud management system are stored in **multiple locations** across the system:

## 1. **Neo4j Graph Database** (Primary Storage)

### Location
- **Database**: Neo4j graph database
- **Node Type**: `BusinessRule` nodes
- **Count**: 10 business rule nodes

### Storage Format
```cypher
(:BusinessRule {
  name: "RULE-01",
  rule_id: "RULE-01", 
  description: "High Amount Transaction - Detects transactions exceeding suspicious amount threshold",
  priority: "HIGH",
  risk_level: "MEDIUM",
  business_impact: "CRITICAL",
  detection_accuracy: "85-90%",
  false_positive_rate: "5-10%",
  confidence: 0.9,
  scope: "TRANSACTION_PROCESSING"
})
```

### All 10 Business Rules Stored:

| Rule ID | Description | Priority | Risk Level | Business Impact |
|---------|-------------|----------|------------|-----------------|
| **RULE-01** | High Amount Transaction - Detects transactions exceeding suspicious amount threshold | HIGH | MEDIUM | CRITICAL |
| **RULE-02** | Transaction Velocity Analysis - Monitors hourly and daily transaction frequency | HIGH | MEDIUM | CRITICAL |
| **RULE-03** | Geographical Location Analysis - Detects unusual location patterns | MEDIUM | MEDIUM | CRITICAL |
| **RULE-04** | Merchant Risk Assessment - Evaluates merchant risk levels and categories | MEDIUM | LOW | CRITICAL |
| **RULE-05** | Unusual Time Pattern - Detects transactions at unusual times | LOW | LOW | CRITICAL |
| **RULE-06** | Card Not Present Risk - Higher risk for online/telephone transactions | HIGH | MEDIUM | CRITICAL |
| **RULE-07** | Suspicious Category Combinations - Multiple merchant categories | MEDIUM | MEDIUM | CRITICAL |
| **RULE-08** | Customer Behavioral Analysis - Analyzes customer spending patterns | HIGH | HIGH | CRITICAL |
| **RULE-09** | New Account Risk - Higher risk for new accounts | MEDIUM | MEDIUM | CRITICAL |
| **RULE-10** | Cross-validation of multiple risk factors - Combines multiple rule triggers | HIGH | HIGH | CRITICAL |

## 2. **Python Source Code** (Definition)

### Location
- **File**: `examples/fraud_management_ontology_example.py`
- **Class**: `FraudManagementOntology`
- **Method**: `_create_business_rules()`
- **Lines**: 450-550

### Storage Format
```python
# Rule 1: High Amount Transaction
rules.append(FraudManagementBusinessRule(
    rule_id="RULE-01",
    description="High Amount Transaction - Detects transactions exceeding suspicious amount threshold",
    scope="TRANSACTION_PROCESSING",
    confidence=0.9,
    risk_level=RiskLevel.MEDIUM,
    priority="HIGH"
))
```

## 3. **COBOL Source Code** (Implementation)

### Location
- **File**: `data/fixtures/vasu_fraud_management_cobol_reformatted.cbl`
- **Sections**: 2610-2695 (Rule implementation sections)

### Storage Format
```cobol
2610-RULE-HIGH-AMOUNT.
* Rule 1: High Amount Transaction                                        
     IF TRANS-AMOUNT > SUSPICIOUS-AMOUNT
     MOVE 'Y' TO RULE-01-TRIGGERED
     ADD 150 TO WS-TRANSACTION-RISK
     END-IF
```

## 4. **Python Objects** (Runtime)

### Location
- **Class**: `FraudManagementBusinessRule` instances
- **Container**: `FraudManagementOntology.business_rules` list
- **Memory**: Runtime Python objects

### Access
```python
from examples.fraud_management_ontology_example import FraudManagementOntology

ontology = FraudManagementOntology()
for rule in ontology.business_rules:
    print(f"{rule.rule_id}: {rule.description}")
```

## 5. **Documentation Files** (Reference)

### Locations
- **File**: `docs/fraud_management_ontology_guide.md`
- **File**: `docs/fraud_ontology_graph_storage.md`
- **File**: `docs/data_storage_locations.md`

### Storage Format
Markdown documentation with rule descriptions and metadata.

## Query Examples

### From Neo4j Database
```cypher
// Get all business rules
MATCH (r:BusinessRule)
RETURN r.rule_id, r.description, r.priority, r.risk_level
ORDER BY r.priority DESC

// Get high-priority rules
MATCH (r:BusinessRule)
WHERE r.priority = 'HIGH'
RETURN r.rule_id, r.description

// Get high-risk rules
MATCH (r:BusinessRule)
WHERE r.risk_level = 'HIGH'
RETURN r.rule_id, r.description, r.business_impact
```

### From Python Objects
```python
# Get all rules
ontology = FraudManagementOntology()
for rule in ontology.business_rules:
    print(f"{rule.rule_id}: {rule.description}")

# Get high-priority rules
high_priority_rules = [r for r in ontology.business_rules if r.priority == "HIGH"]

# Get high-risk rules
high_risk_rules = [r for r in ontology.business_rules if r.risk_level == RiskLevel.HIGH]
```

## Data Flow

```
COBOL Source Code (Implementation)
       ↓
Python Source Code (Definition)
       ↓
Python Objects (Runtime)
       ↓
Neo4j Converter
       ↓
Neo4j Database (Storage)
       ↓
Documentation (Reference)
```

## Key Properties Stored

Each business rule contains:

### Core Properties
- **rule_id**: Unique identifier (RULE-01 to RULE-10)
- **description**: Human-readable description
- **priority**: HIGH, MEDIUM, LOW
- **risk_level**: HIGH, MEDIUM, LOW
- **scope**: TRANSACTION_PROCESSING

### Business Properties
- **business_impact**: CRITICAL, HIGH, MEDIUM, LOW
- **detection_accuracy**: Percentage range (e.g., "85-90%")
- **false_positive_rate**: Percentage range (e.g., "5-10%")
- **confidence**: Numerical confidence score (0.0-1.0)

### Technical Properties
- **total_rules**: Total number of rules (10)
- **rule_coverage**: COMPREHENSIVE
- **update_frequency**: QUARTERLY
- **language**: COBOL

## Summary

The **10 high-level business rules** are stored in:

1. **Neo4j Graph Database** - Primary persistent storage with full metadata
2. **Python Source Code** - Definition and implementation
3. **COBOL Source Code** - Actual business logic implementation
4. **Python Objects** - Runtime representation
5. **Documentation Files** - Reference and documentation

The **Neo4j database** provides the most comprehensive storage with all metadata, enabling complex queries and analysis of the business rules and their relationships to the system components.
