# Graph Analyzer Usage Examples

## Using .env Configuration

The `analyze_graph.py` launcher script automatically uses your `.env` file configuration and allows command line overrides.

### Your .env Configuration
```bash
# From your .env file
NEO4J_URI=bolt://localhost:7687
NEO4J_USERNAME=neo4j
NEO4J_PASSWORD=codegrapher
NEO4J_DATABASE=llmcodegrapher
```

## Usage Examples

### 1. Use Default .env Configuration
```bash
python analyze_graph.py
```
Uses all settings from `.env` file.

### 2. Override Database
```bash
python analyze_graph.py --database fraud_db
```
Uses `.env` settings but switches to `fraud_db` database.

### 3. Override Multiple Settings
```bash
python analyze_graph.py --database fraud_db --max-depth 3 --output json
```
Uses `.env` for connection details but overrides database, depth, and output format.

### 4. Show Current Configuration
```bash
python analyze_graph.py --show-config
```
Displays the current configuration (useful for debugging).

### 5. Export Results
```bash
python analyze_graph.py --export fraud_analysis.json
```
Analyzes using `.env` settings and exports results to JSON file.

### 6. Complete Override Example
```bash
python analyze_graph.py \
  --uri bolt://localhost:7687 \
  --user neo4j \
  --password codegrapher \
  --database fraud_db \
  --max-depth 5 \
  --output table \
  --export detailed_analysis.json
```

## Configuration Priority

1. **Command line arguments** (highest priority)
2. **Environment variables** from `.env` file
3. **Default values** (lowest priority)

## Environment Variables

The script looks for these environment variables in your `.env` file:

- `NEO4J_URI`: Neo4j database URI
- `NEO4J_USERNAME`: Neo4j username  
- `NEO4J_PASSWORD`: Neo4j password
- `NEO4J_DATABASE`: Database name

## Troubleshooting

### Check Configuration
```bash
python analyze_graph.py --show-config
```

### Test Connection
```bash
python analyze_graph.py --max-depth 1
```

### Debug Mode
```bash
python analyze_graph.py --show-config --database test_db
```

## Integration with Main Project

The launcher script integrates seamlessly with your existing LLM Code Grapher setup:

1. **Uses existing .env**: No need for separate configuration
2. **Same credentials**: Uses your existing Neo4j setup
3. **Easy access**: Run from project root directory
4. **Consistent interface**: Similar to other project tools
