# Neo4j Graph Analyzer - Usage Examples

## Basic Usage

### 1. Analyze Default Database
```bash
python graph_analyzer.py --uri bolt://localhost:7687 --user neo4j --password password
```

### 2. Analyze Specific Database
```bash
python graph_analyzer.py --uri bolt://localhost:7687 --user neo4j --password password --database fraud_db
```

### 3. JSON Output
```bash
python graph_analyzer.py --uri bolt://localhost:7687 --user neo4j --password password --output json
```

### 4. Limited Depth Analysis
```bash
python graph_analyzer.py --uri bolt://localhost:7687 --user neo4j --password password --max-depth 3
```

### 5. Export Results
```bash
python graph_analyzer.py --uri bolt://localhost:7687 --user neo4j --password password --export analysis_results.json
```

## Advanced Usage

### 6. Complete Analysis with Export
```bash
python graph_analyzer.py \
  --uri bolt://localhost:7687 \
  --user neo4j \
  --password password \
  --database fraud_db \
  --max-depth 5 \
  --output table \
  --export fraud_analysis.json
```

### 7. Quick Overview (Shallow Analysis)
```bash
python graph_analyzer.py \
  --uri bolt://localhost:7687 \
  --user neo4j \
  --password password \
  --max-depth 2 \
  --output table
```

## Sample Output

### Table Format
```
================================================================================
📊 NEO4J GRAPH STRUCTURE ANALYSIS
================================================================================

🗄️  Database: fraud_db
📈 Total Nodes: 281
🔗 Total Relationships: 3950

📋 Node Types Summary:
   • Program: 1
   • Operation: 234
   • Subsection: 30
   • BusinessRule: 16

🔗 Relationship Types Summary:
   • DEPENDS_ON: 3655
   • CONTAINS: 169
   • IMPLEMENTS: 16
   • NEXT: 110

🌳 Hierarchical Structure Analysis:
------------------------------------------------------------

Level 0:
   📁 Program: 1

Level 1:
   └─ Section (via CONTAINS): 16
   └─ Subsection (via CONTAINS): 30

Level 2:
   └─ Operation (via CONTAINS): 234
   └─ BusinessRule (via IMPLEMENTS): 16

Level 3:
   └─ Operation (via DEPENDS_ON): 234
   └─ Operation (via NEXT): 110

🛤️  Most Common Paths:
------------------------------------------------------------
   Program --[CONTAINS]--> Section: 16
   Program --[CONTAINS]--> Subsection: 30
   Section --[CONTAINS]--> Operation: 234
   Subsection --[CONTAINS]--> Operation: 234
```

### JSON Format
```json
{
  "database_info": {
    "total_nodes": {"count": 281},
    "total_relationships": {"count": 3950},
    "node_types": [
      {"labels": ["Program"], "count": 1},
      {"labels": ["Operation"], "count": 234},
      {"labels": ["Subsection"], "count": 30},
      {"labels": ["BusinessRule"], "count": 16}
    ],
    "relationship_types": [
      {"type": "DEPENDS_ON", "count": 3655},
      {"type": "CONTAINS", "count": 169},
      {"type": "IMPLEMENTS", "count": 16},
      {"type": "NEXT", "count": 110}
    ]
  },
  "level_analysis": {
    "0": [{"nodeType": ["Program"], "count": 1}],
    "1": [
      {"nodeType": ["Section"], "relationshipType": "CONTAINS", "count": 16},
      {"nodeType": ["Subsection"], "relationshipType": "CONTAINS", "count": 30}
    ]
  }
}
```

## Environment Variables

You can also use environment variables to avoid passing credentials in the command line:

```bash
export NEO4J_URI="bolt://localhost:7687"
export NEO4J_USER="neo4j"
export NEO4J_PASSWORD="password"
export NEO4J_DATABASE="fraud_db"

python graph_analyzer.py --uri $NEO4J_URI --user $NEO4J_USER --password $NEO4J_PASSWORD --database $NEO4J_DATABASE
```

## Troubleshooting

### Connection Issues
- Verify Neo4j is running: `neo4j status`
- Check connection details: `neo4j-admin dbms info`
- Test connection: `cypher-shell -u neo4j -p password`

### Permission Issues
- Ensure the user has read access to the database
- Check database exists: `SHOW DATABASES`

### Performance Issues
- Reduce max-depth for large graphs
- Use specific database instead of default
- Consider running during off-peak hours
