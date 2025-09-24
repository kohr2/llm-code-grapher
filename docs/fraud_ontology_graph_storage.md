# Fraud Management Ontology Graph Storage

This document explains how the fraud management ontology data is stored in the Neo4j graph database and how to query it.

## Overview

The fraud management ontology is converted to a Neo4j graph format that captures:
- **Structural relationships** between program components
- **Business logic** and rule implementations
- **Data flow** between operations and data items
- **Risk assessments** and complexity metrics
- **Quality indicators** and system metrics

## Graph Structure

### Node Types

| Node Type | Count | Description | Key Properties |
|-----------|-------|-------------|----------------|
| **Program** | 1 | Main fraud management system | `name`, `language`, `cyclomatic_complexity`, `business_domain` |
| **Section** | 5 | Main program sections | `name`, `type`, `risk_level`, `complexity_score`, `business_logic` |
| **Subsection** | 5 | Subsections/paragraphs | `name`, `parent_section`, `rule_id`, `algorithm`, `patterns` |
| **Operation** | 4 | Granular operations | `name`, `operation_type`, `parameters`, `condition`, `threshold_type` |
| **DataItem** | 4 | Data structures | `name`, `data_type`, `operations`, `transaction_volume`, `data_complexity` |
| **BusinessRule** | 10 | Fraud detection rules | `rule_id`, `description`, `priority`, `risk_level`, `business_impact` |

### Relationship Types

| Relationship Type | Count | Description | Example |
|-------------------|-------|-------------|---------|
| **CONTAINS** | 13 | Structural containment | Program → Section → Subsection → Operation |
| **USES** | 4 | Data usage | Operation → DataItem |
| **IMPLEMENTS** | 10 | Rule implementation | BusinessRule → Subsection |
| **RELATED_TO** | 6 | Risk relationships | High-risk Section ↔ High-risk BusinessRule |

## Data Storage Details

### Program Node
```cypher
(:Program {
  name: "FRAUD-MGMT-SYSTEM",
  language: "COBOL",
  author: "FRAUD-DETECTION-TEAM",
  purpose: "Real-time fraud detection and risk assessment system",
  business_domain: "Financial Services - Fraud Prevention",
  cyclomatic_complexity: 45.2,
  maintainability_index: 65.0,
  technical_debt: 78.5,
  business_rule_coverage: "HIGH",
  system_reliability: "HIGH"
})
```

### Section Nodes
```cypher
(:Section {
  name: "2600-EXECUTE-FRAUD-RULES",
  type: "PROCEDURE_SECTION",
  risk_level: "HIGH",
  complexity_score: 0.8,
  business_logic: "Executes all 10 fraud detection rules in sequence",
  purpose: "Core fraud detection logic"
})
```

### Business Rule Nodes
```cypher
(:BusinessRule {
  rule_id: "RULE-01",
  description: "High Amount Transaction - Detects transactions exceeding suspicious amount threshold",
  priority: "HIGH",
  risk_level: "MEDIUM",
  business_impact: "CRITICAL",
  detection_accuracy: "85-90%",
  false_positive_rate: "5-10%"
})
```

### Data Item Nodes
```cypher
(:DataItem {
  name: "TRANSACTION-RECORD",
  data_type: "RECORD",
  description: "Main transaction data structure containing all transaction details",
  operations: ["READ", "WRITE", "VALIDATE"],
  transaction_volume: "HIGH",
  data_complexity: "HIGH",
  update_frequency: "REAL_TIME"
})
```

## Query Examples

### 1. Get All Business Rules
```cypher
MATCH (r:BusinessRule)
RETURN r.rule_id, r.description, r.priority, r.risk_level
ORDER BY r.priority DESC
```

### 2. Find High-Risk Components
```cypher
MATCH (n)
WHERE n.risk_level = 'HIGH'
RETURN labels(n)[0] as node_type, n.name as name, n.business_logic as description
ORDER BY node_type, name
```

### 3. Analyze Data Flow
```cypher
MATCH (o:Operation)-[r:USES]->(d:DataItem)
RETURN o.name as operation, d.name as data_item, r.description as description
ORDER BY o.name
```

### 4. Business Rule Implementation
```cypher
MATCH (r:BusinessRule)-[rel:IMPLEMENTS]->(s:Subsection)
RETURN r.rule_id, s.name as subsection, rel.description
ORDER BY r.rule_id
```

### 5. Risk Assessment Analysis
```cypher
MATCH (s:Section)-[r:RELATED_TO]->(br:BusinessRule)
WHERE s.risk_level = 'HIGH' AND br.risk_level = 'HIGH'
RETURN s.name as section, br.rule_id as rule, r.description
ORDER BY s.name
```

### 6. Complexity Analysis
```cypher
MATCH (s:Section)
RETURN s.name, s.complexity_score, s.risk_level
ORDER BY s.complexity_score DESC
```

### 7. System Architecture Overview
```cypher
MATCH (n)
RETURN labels(n)[0] as node_type, count(n) as count
ORDER BY count DESC
```

### 8. Business Impact Analysis
```cypher
MATCH (r:BusinessRule)
RETURN r.rule_id, r.business_impact, r.priority, r.detection_accuracy
ORDER BY r.business_impact DESC, r.priority DESC
```

## Advanced Queries

### Find Critical Path
```cypher
MATCH path = (p:Program)-[:CONTAINS*]->(o:Operation)
WHERE o.risk_level = 'HIGH'
RETURN path
ORDER BY length(path) DESC
LIMIT 5
```

### Data Dependencies
```cypher
MATCH (o:Operation)-[:USES]->(d:DataItem)<-[:USES]-(o2:Operation)
WHERE o <> o2
RETURN o.name as operation1, d.name as shared_data, o2.name as operation2
```

### Rule Coverage Analysis
```cypher
MATCH (r:BusinessRule)
OPTIONAL MATCH (r)-[:IMPLEMENTS]->(s:Subsection)
RETURN r.rule_id, r.description, 
       CASE WHEN s IS NULL THEN 'NOT IMPLEMENTED' ELSE 'IMPLEMENTED' END as status
ORDER BY status, r.rule_id
```

### Risk Propagation
```cypher
MATCH path = (s:Section)-[:RELATED_TO*1..3]->(br:BusinessRule)
WHERE s.risk_level = 'HIGH'
RETURN path
ORDER BY length(path)
```

## Benefits of Graph Storage

### 1. **Structural Analysis**
- Visualize program hierarchy
- Identify component relationships
- Analyze code organization

### 2. **Business Logic Mapping**
- Map business rules to code implementation
- Track rule coverage and completeness
- Identify missing implementations

### 3. **Risk Assessment**
- Identify high-risk components
- Analyze risk propagation paths
- Prioritize maintenance efforts

### 4. **Data Flow Analysis**
- Track data usage across operations
- Identify data dependencies
- Optimize data access patterns

### 5. **Quality Metrics**
- Monitor complexity trends
- Track technical debt
- Measure system reliability

### 6. **Impact Analysis**
- Understand change impact
- Identify affected components
- Plan modernization efforts

## Usage in Practice

### 1. **Development Teams**
- Understand system architecture
- Identify areas for improvement
- Plan refactoring efforts

### 2. **Business Analysts**
- Map business rules to code
- Verify rule implementation
- Track business logic changes

### 3. **Quality Assurance**
- Identify high-risk areas for testing
- Plan test coverage
- Monitor system quality

### 4. **Maintenance Teams**
- Prioritize maintenance tasks
- Understand system dependencies
- Plan modernization efforts

## Conclusion

The fraud management ontology stored in Neo4j provides a comprehensive view of the system that enables:

- **Deep Analysis**: Complex queries across multiple dimensions
- **Visualization**: Graph-based system understanding
- **Traceability**: Business logic to code mapping
- **Risk Management**: Proactive risk identification
- **Quality Assurance**: Comprehensive quality metrics
- **Modernization Planning**: Data-driven improvement decisions

This graph-based approach transforms static code analysis into a dynamic, queryable knowledge base that supports informed decision-making and system evolution.
