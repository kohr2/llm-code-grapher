# PM Analysis API Specification

## Overview

REST API for automatically mapping code structure to product requirements through multi-source evidence analysis.

## Base URL

```
https://api.your-domain.com/pm-analysis/v1
```

## Authentication

```
Authorization: Bearer <jwt_token>
Content-Type: application/json
```

## Core Endpoints

### Analysis

#### Analyze Codebase
```http
POST /analyze/codebase
```

**Request:**
```json
{
  "repo_path": "/path/to/repository",
  "project_key": "PROJ",
  "options": {
    "max_depth": 4,
    "confidence_threshold": 0.8,
    "include_git_history": true
  }
}
```

**Response:**
```json
{
  "analysis_id": "analysis_12345",
  "status": "in_progress",
  "estimated_duration": 300,
  "total_files": 150
}
```

#### Get Analysis Results
```http
GET /analyze/{analysis_id}/results
```

**Response:**
```json
{
  "analysis_id": "analysis_12345",
  "metadata": {
    "total_files_analyzed": 150,
    "total_mappings": 45,
    "confidence_distribution": {
      "mean": 0.72,
      "median": 0.75
    }
  },
  "results": [
    {
      "code_node_id": "VALIDATE-CUSTOMER-SECTION",
      "file_path": "src/validation/customer.py",
      "business_capabilities": [
        {
          "domain": "customer_management",
          "operation": "validates",
          "confidence": 0.89
        }
      ],
      "mappings": [
        {
          "requirement_id": "PROJ-123",
          "confidence": 0.92,
          "evidence_sources": ["semantic", "git", "requirements"]
        }
      ]
    }
  ]
}
```

### Mappings

#### Create Manual Mapping
```http
POST /mappings
```

**Request:**
```json
{
  "code_node_id": "VALIDATE-CUSTOMER-SECTION",
  "requirement_id": "PROJ-123",
  "confidence": 1.0,
  "evidence_sources": ["manual"]
}
```

#### Validate Mapping
```http
POST /mappings/{mapping_id}/validate
```

**Request:**
```json
{
  "validation_status": "validated",
  "confidence": 0.95,
  "validated_by": "user@company.com"
}
```

### Synchronization

#### Sync Changes
```http
POST /sync
```

**Request:**
```json
{
  "direction": "code-to-requirements",
  "project_key": "PROJ",
  "options": {
    "include_new_mappings": true,
    "batch_size": 50
  }
}
```

### Reporting

#### Generate Report
```http
POST /reports
```

**Request:**
```json
{
  "report_type": "mapping_summary",
  "project_key": "PROJ",
  "filters": {
    "confidence_min": 0.8,
    "date_from": "2024-01-01",
    "date_to": "2024-01-31"
  },
  "format": "pdf"
}
```

## Data Schemas

### BusinessCapability
```json
{
  "type": "object",
  "properties": {
    "id": {"type": "string"},
    "domain": {"type": "string"},
    "operation": {"type": "string"},
    "entity": {"type": "string"},
    "confidence": {"type": "number", "minimum": 0, "maximum": 1}
  },
  "required": ["id", "domain", "operation", "entity", "confidence"]
}
```

### Mapping
```json
{
  "type": "object",
  "properties": {
    "mapping_id": {"type": "string"},
    "code_node_id": {"type": "string"},
    "requirement_id": {"type": "string"},
    "confidence": {"type": "number", "minimum": 0, "maximum": 1},
    "evidence_sources": {
      "type": "array",
      "items": {"type": "string"}
    },
    "validation_status": {"type": "string", "enum": ["pending", "validated", "rejected"]}
  },
  "required": ["mapping_id", "code_node_id", "requirement_id", "confidence"]
}
```

### Evidence
```json
{
  "type": "object",
  "properties": {
    "semantic_similarity": {"type": "number", "minimum": 0, "maximum": 1},
    "git_evidence": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "commit_hash": {"type": "string"},
          "commit_message": {"type": "string"},
          "relevance_score": {"type": "number", "minimum": 0, "maximum": 1}
        }
      }
    },
    "requirement_evidence": {
      "type": "object",
      "properties": {
        "requirement_id": {"type": "string"},
        "summary": {"type": "string"},
        "business_value": {"type": "string"}
      }
    },
    "confidence_score": {"type": "number", "minimum": 0, "maximum": 1}
  }
}
```

## Error Responses

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Invalid confidence threshold",
    "details": {
      "field": "confidence_threshold",
      "value": 1.5,
      "constraint": "Must be between 0.0 and 1.0"
    }
  }
}
```

## Error Codes

| Code | HTTP Status | Description |
|------|-------------|-------------|
| `VALIDATION_ERROR` | 400 | Request validation failed |
| `AUTHENTICATION_ERROR` | 401 | Authentication failed |
| `AUTHORIZATION_ERROR` | 403 | Insufficient permissions |
| `NOT_FOUND` | 404 | Resource not found |
| `RATE_LIMIT_EXCEEDED` | 429 | Rate limit exceeded |
| `INTERNAL_ERROR` | 500 | Internal server error |

## Rate Limiting

- **Standard endpoints**: 100 requests/minute
- **Analysis endpoints**: 10 requests/minute
- **Report generation**: 5 requests/minute

Headers:
```
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 1642252800
```

## Webhooks

Events: `analysis.completed`, `mapping.created`, `mapping.validated`

Payload:
```json
{
  "event": "analysis.completed",
  "data": {
    "analysis_id": "analysis_12345",
    "status": "completed",
    "total_mappings": 45
  },
  "timestamp": "2024-01-15T12:00:00Z"
}
```

## SDK Examples

### Python
```python
from pm_analysis import PMAnalysisClient

client = PMAnalysisClient(api_key="your_api_key")

# Analyze codebase
analysis = client.analyze_codebase(
    repo_path="/path/to/repo",
    project_key="PROJ",
    confidence_threshold=0.8
)

# Get results
results = client.get_analysis_results(analysis.analysis_id)
```

### JavaScript
```javascript
const client = new PMAnalysisClient({ apiKey: 'your_api_key' });

// Analyze codebase
const analysis = await client.analyzeCodebase({
  repoPath: '/path/to/repo',
  projectKey: 'PROJ',
  confidenceThreshold: 0.8
});
```

## Integration Patterns

### CI/CD Integration
```yaml
# .github/workflows/pm-analysis.yml
name: PM Analysis
on: [push, pull_request]

jobs:
  pm-analysis:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run PM Analysis
        run: |
          curl -X POST https://api.domain.com/pm-analysis/v1/analyze/codebase \
            -H "Authorization: Bearer ${{ secrets.PM_ANALYSIS_TOKEN }}" \
            -d '{"repo_path": "${{ github.workspace }}", "project_key": "PROJ"}'
```

This API provides a comprehensive interface for integrating PM Analysis functionality into various tools and workflows.