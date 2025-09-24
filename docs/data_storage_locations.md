# Data Storage Locations

## Overview
Fraud management ontology data is stored across multiple formats:

## Storage Locations

### 1. **Neo4j Database** (Primary)
- **URI**: `bolt://localhost:7687`
- **Content**: 29 nodes + 33 relationships
- **Types**: Program(1), BusinessRule(10), Section(5), DataItem(4), Operation(4), Subsection(5)

### 2. **Python Objects** (Runtime)
- **File**: `examples/fraud_management_ontology_example.py`
- **Classes**: `FraudManagementOntology`, `FraudManagementProgram`
- **Memory**: Dynamic ontology structure generated from LLM analysis

### 3. **Output Files**
- **JSON**: `output/json/` - Structured analysis results
- **Text**: `output/text/` - Human-readable reports
- **Config**: `config.yaml` - Settings and parameters

## Key Node Types

| Type | Count | Key Properties |
|------|-------|----------------|
| Program | 1 | name, language, complexity, business_domain |
| BusinessRule | Dynamic | rule_id, priority, risk_level, business_impact, analysis_method |
| Section | 5 | name, type, risk_level, complexity_score |
| DataItem | 4 | name, data_type, operations, transaction_volume |
| Operation | 4 | name, operation_type, parameters |
| Subsection | 5 | name, parent_section, rule_id |

## Data Flow
```
COBOL → Parser → LLM Analysis → Dynamic Rule Generation → Neo4j Converter → Graph Database → Output Files
```

## Quick Access

### Neo4j Database
```python
from src.neo4j_database import Neo4jDatabase, Neo4jConfig

config = Neo4jConfig.from_environment()
db = Neo4jDatabase(config)
rules = db.query("MATCH (r:BusinessRule) RETURN r")
```

### Python Objects
```python
from examples.fraud_management_ontology_example import FraudManagementOntology

ontology = FraudManagementOntology()
metrics = ontology.get_complexity_metrics()
```

## Storage Characteristics

| Location | Persistence | Query Language | Best For |
|----------|-------------|----------------|----------|
| Neo4j | Disk-based, ACID | Cypher | Complex analysis, visualization |
| Python Objects | Memory only | Method calls | Runtime analysis, validation |
| JSON/Text Files | Disk-based | File I/O | Reports, debugging |
