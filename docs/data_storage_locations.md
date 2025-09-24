# Data Storage Locations - Fraud Management Ontology

## Overview

The fraud management ontology information is stored in **multiple locations** depending on the format and purpose:

## 1. **Neo4j Graph Database** (Primary Storage)

### Location
- **Database**: Neo4j graph database
- **URI**: `bolt://localhost:7687` (default)
- **Database Name**: `neo4j`
- **Connection**: Via Neo4j driver with credentials from environment variables

### What's Stored
**29 Nodes + 33 Relationships** containing:

#### Program Node (1)
```cypher
(:Program {
  name: "FRAUD-MGMT-SYSTEM",
  language: "COBOL",
  purpose: "Real-time fraud detection and risk assessment system",
  business_domain: "Financial Services - Fraud Prevention",
  cyclomatic_complexity: 45.2,
  maintainability_index: 65.0,
  technical_debt: 78.5,
  business_rule_coverage: "HIGH",
  system_reliability: "HIGH"
})
```

#### Business Rule Nodes (10)
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

#### Section Nodes (5)
```cypher
(:Section {
  name: "2600-EXECUTE-FRAUD-RULES",
  section_type: "PROCEDURE_SECTION",
  risk_level: "HIGH",
  complexity_score: 0.8,
  business_logic: "Executes all 10 fraud detection rules in sequence",
  purpose: "Core fraud detection logic"
})
```

#### Data Item Nodes (4)
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

### Relationships (33)
- **CONTAINS** (13): Structural hierarchy
- **IMPLEMENTS** (10): Business rule implementations
- **RELATED_TO** (6): Risk assessment relationships
- **USES** (4): Data flow relationships

## 2. **Python Objects** (In-Memory)

### Location
- **Files**: `examples/fraud_management_ontology_example.py`
- **Classes**: `FraudManagementOntology`, `FraudManagementProgram`, etc.
- **Memory**: Runtime Python objects

### What's Stored
- Complete ontology structure as Python objects
- All business logic and metadata
- Methods for analysis and validation
- Complexity and quality metrics

## 3. **JSON Files** (Output Format)

### Location
- **Directory**: `output/json/`
- **Files**: Generated analysis results
- **Format**: JSON with structured data

### What's Stored
- Parser results from COBOL analysis
- Business context mappings
- Enhanced metrics and validation results

## 4. **Text Files** (Human-Readable)

### Location
- **Directory**: `output/text/`
- **Files**: Analysis reports and summaries
- **Format**: Plain text with formatted output

### What's Stored
- Human-readable analysis reports
- Business logic descriptions
- Risk assessments and recommendations

## 5. **Configuration Files**

### Location
- **File**: `config.yaml`
- **Environment**: `.env` file
- **Format**: YAML and environment variables

### What's Stored
- Neo4j connection settings
- Analysis parameters
- Output configuration

## Data Flow

```
COBOL Source Code
       ↓
   Parser Analysis
       ↓
  Ontology Objects (Python)
       ↓
  Neo4j Converter
       ↓
  Graph Database (Neo4j)
       ↓
  Query & Analysis
       ↓
  JSON/Text Output
```

## Accessing the Data

### 1. **From Neo4j Database**
```python
from src.neo4j_database import Neo4jDatabase, Neo4jConfig

config = Neo4jConfig.from_environment()
db = Neo4jDatabase(config)
db.connect()

# Query business rules
rules = db.query("MATCH (r:BusinessRule) RETURN r")
```

### 2. **From Python Objects**
```python
from examples.fraud_management_ontology_example import FraudManagementOntology

ontology = FraudManagementOntology()
metrics = ontology.get_complexity_metrics()
```

### 3. **From Files**
```python
import json
with open('output/json/analysis_result.json', 'r') as f:
    data = json.load(f)
```

## Physical Storage Details

### Neo4j Database
- **Storage Engine**: Neo4j graph database
- **Data Format**: Nodes and relationships with properties
- **Query Language**: Cypher
- **Persistence**: Disk-based storage with memory caching
- **Backup**: Neo4j backup mechanisms

### File System
- **JSON Files**: `output/json/` directory
- **Text Files**: `output/text/` directory
- **Logs**: `logs/` directory
- **Configuration**: Root directory files

## Data Persistence

### Neo4j Database
- **Persistent**: Yes, stored on disk
- **Durability**: ACID compliant
- **Backup**: Can be backed up using Neo4j tools
- **Replication**: Supports clustering and replication

### Python Objects
- **Persistent**: No, in-memory only
- **Durability**: Lost when process ends
- **Backup**: Not applicable

### Files
- **Persistent**: Yes, stored on disk
- **Durability**: File system dependent
- **Backup**: Standard file backup mechanisms

## Query Capabilities

### Neo4j Database
- **Complex Queries**: Full Cypher query language support
- **Graph Traversal**: Path finding and relationship analysis
- **Aggregations**: Statistical analysis and metrics
- **Visualization**: Graph visualization tools

### Python Objects
- **Method Calls**: Direct object method calls
- **Analysis**: Built-in analysis methods
- **Validation**: Consistency checking
- **Metrics**: Complexity and quality calculations

## Summary

The fraud management ontology information is **primarily stored in the Neo4j graph database** as a persistent, queryable knowledge base. Additional copies exist as:

1. **Neo4j Graph Database** - Primary persistent storage (29 nodes, 33 relationships)
2. **Python Objects** - In-memory runtime representation
3. **JSON Files** - Structured output format
4. **Text Files** - Human-readable reports
5. **Configuration Files** - Settings and parameters

The Neo4j database provides the most comprehensive and queryable storage of all the fraud management ontology information, enabling complex analysis, visualization, and real-time querying of the system structure and business logic.
