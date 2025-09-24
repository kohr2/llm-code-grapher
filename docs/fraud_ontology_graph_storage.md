# Fraud Ontology Graph Storage

## Overview
Fraud management ontology stored in Neo4j graph database capturing structural relationships, business logic, data flow, and risk assessments.

## Graph Structure

### Node Types
| Node Type | Count | Key Properties |
|-----------|-------|----------------|
| Program | 1 | name, language, cyclomatic_complexity, business_domain |
| Section | 5 | name, type, risk_level, complexity_score, business_logic |
| Subsection | 5 | name, parent_section, rule_id, algorithm, patterns |
| Operation | 4 | name, operation_type, parameters, condition, threshold_type |
| DataItem | 4 | name, data_type, operations, transaction_volume, data_complexity |
| BusinessRule | 10 | rule_id, description, priority, risk_level, business_impact |

### Relationship Types
| Relationship Type | Count | Description |
|-------------------|-------|-------------|
| CONTAINS | 13 | Structural containment (Program → Section → Subsection → Operation) |
| USES | 4 | Data usage (Operation → DataItem) |
| IMPLEMENTS | 10 | Rule implementation (BusinessRule → Subsection) |
| RELATED_TO | 6 | Risk relationships (High-risk Section ↔ High-risk BusinessRule) |

## Key Node Examples

### Program Node
```cypher
(:Program {
  name: "FRAUD-MGMT-SYSTEM",
  language: "COBOL",
  business_domain: "Financial Services - Fraud Prevention",
  cyclomatic_complexity: 45.2,
  maintainability_index: 65.0,
  technical_debt: 78.5
})
```

### Business Rule Node
```cypher
(:BusinessRule {
  rule_id: "DYNAMIC_RULE_001",
  description: "LLM-generated description of business logic",
  priority: "HIGH",
  risk_level: "MEDIUM",
  business_impact: "CRITICAL",
  analysis_method: "LLM",
  analysis_confidence: 0.9,
  functional_area: "TRANSACTION_PROCESSING"
})
```

## Essential Queries

### Business Rules
```cypher
// All dynamically analyzed business rules
MATCH (r:BusinessRule) RETURN r.rule_id, r.description, r.priority, r.analysis_method

// High-priority rules
MATCH (r:BusinessRule) WHERE r.priority = 'HIGH' RETURN r.rule_id, r.description

// LLM-analyzed rules with confidence
MATCH (r:BusinessRule) WHERE r.analysis_method = 'LLM' 
RETURN r.rule_id, r.description, r.analysis_confidence
```

### Risk Analysis
```cypher
// High-risk components
MATCH (n) WHERE n.risk_level = 'HIGH' 
RETURN labels(n)[0] as node_type, n.name as name

// Risk relationships
MATCH (s:Section)-[r:RELATED_TO]->(br:BusinessRule)
WHERE s.risk_level = 'HIGH' AND br.risk_level = 'HIGH'
RETURN s.name as section, br.rule_id as rule
```

### Data Flow
```cypher
// Data usage
MATCH (o:Operation)-[r:USES]->(d:DataItem)
RETURN o.name as operation, d.name as data_item

// Rule implementation
MATCH (r:BusinessRule)-[rel:IMPLEMENTS]->(s:Subsection)
RETURN r.rule_id, s.name as subsection
```

### System Overview
```cypher
// Architecture summary
MATCH (n) RETURN labels(n)[0] as node_type, count(n) as count

// Complexity analysis
MATCH (s:Section) RETURN s.name, s.complexity_score, s.risk_level
ORDER BY s.complexity_score DESC
```

## Benefits
- **Structural Analysis**: Visualize program hierarchy and relationships
- **Business Logic Mapping**: Map business rules to code implementation
- **Risk Assessment**: Identify high-risk components and propagation paths
- **Data Flow Analysis**: Track data usage and dependencies
- **Quality Metrics**: Monitor complexity trends and technical debt
- **Impact Analysis**: Understand change impact and plan modernization
