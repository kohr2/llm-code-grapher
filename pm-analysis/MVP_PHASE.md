# MVP Phase - PM Analysis System

## Overview

The MVP phase focuses on implementing the core functionality to establish automated code-to-requirement mapping with minimal complexity while proving the concept's viability.

## MVP Scope

### Phase 1: Core Foundation (2-3 weeks)

#### 1.1 Basic Graph Analysis
- ✅ **Already Available**: `tools/graph_analyzer/graph_analyzer.py`
- Extract business capabilities from existing Neo4j graph
- Generate simple business logic summaries per program/section

#### 1.2 Git History Analysis
```python
# Priority: High
class GitEvidenceExtractor:
    def extract_requirement_ids(self, commit_messages: List[str]) -> List[str]:
        """Extract requirement IDs from commit messages (PROJ-123, #456, etc.)"""
        
    def analyze_file_changes(self, commit_hash: str) -> List[FileEdit]:
        """Analyze which files were changed and how"""
        
    def link_commits_to_requirements(self) -> Dict[str, List[Commit]]:
        """Map requirement IDs to their related commits"""
```

#### 1.3 Simple Requirement System Integration
```python
# Priority: High - Start with one system (e.g., Jira)
class RequirementSystemProvider:
    def fetch_requirement(self, requirement_id: str) -> Requirement:
        """Fetch single requirement details"""
        
    def search_requirements(self, query: str) -> List[Requirement]:
        """Search requirements by text content"""
```

#### 1.4 Basic Semantic Matching
```python
# Priority: Medium - Use existing LLM infrastructure
class SemanticMatcher:
    def compare_business_logic_to_requirement(
        self, 
        business_logic: str, 
        requirement_content: str
    ) -> float:
        """Return similarity score 0.0-1.0"""
```

### Phase 2: Mapping Engine (2-3 weeks)

#### 2.1 Evidence Aggregation
```python
class MVPMappingEngine:
    def map_code_to_requirements(self, program_name: str) -> List[Mapping]:
        """Core mapping logic combining all evidence sources"""
        
    def calculate_confidence_score(self, evidence: List[Evidence]) -> float:
        """Simple weighted scoring: git=0.5, semantic=0.3, content=0.2"""
```

#### 2.2 Output Generation
- Generate mapping reports in JSON format
- Create simple HTML visualization
- Export to CSV for manual review

#### 2.3 Configuration
```yaml
# config.yaml extension
product_management:
  requirement_system:
    type: "jira"  # Start with one system
    api_url: "https://your-domain.atlassian.net"
    project_key: "PROJ"
  
  mapping:
    confidence_threshold: 0.6
    max_matches_per_program: 10
    
  git:
    analyze_commits: true
    lookback_days: 90
```

### Phase 3: Validation & Feedback (1-2 weeks)

#### 3.1 Manual Validation Tools
- CLI tool to review and validate mappings
- Ability to mark mappings as correct/incorrect
- Feedback collection for improving algorithms

#### 3.2 Basic Metrics
- Mapping accuracy tracking
- Coverage statistics (how many programs mapped)
- Confidence score distribution

## MVP Deliverables

### Core Files to Create

1. **`src/pm_analysis/`**
   ```
   pm_analysis/
   ├── __init__.py
   ├── git_analyzer.py          # Git history analysis
   ├── requirement_provider.py  # Requirement system integration
   ├── semantic_matcher.py      # LLM-based matching
   ├── mapping_engine.py        # Core mapping logic
   └── mvp_main.py             # MVP entry point
   ```

2. **`examples/mvp_demo.py`**
   - Complete working example
   - Sample output and visualization

3. **`tests/test_mvp.py`**
   - Unit tests for core functionality
   - Integration tests with mock data

### CLI Interface

```bash
# MVP Commands
python -m pm_analysis.mvp_main \
  --program "FRAUD_MANAGEMENT" \
  --output-dir ./mvp_results \
  --confidence-threshold 0.6

python -m pm_analysis.mvp_main \
  --analyze-all \
  --export-csv \
  --validate-mappings
```

## Success Criteria

### Functional Requirements
- [ ] Extract business logic from at least 5 programs
- [ ] Map to requirements with >60% confidence
- [ ] Generate human-readable mapping reports
- [ ] Support manual validation and feedback

### Technical Requirements
- [ ] Process 100+ commits in <30 seconds
- [ ] Handle 1000+ requirements without timeout
- [ ] Generate reports in <10 seconds
- [ ] 90%+ test coverage on core modules

### Quality Metrics
- [ ] Manual validation shows >70% accurate mappings
- [ ] Confidence scores correlate with accuracy
- [ ] System handles edge cases gracefully
- [ ] Documentation enables independent usage

## Risk Mitigation

### Technical Risks
- **LLM API limits**: Implement caching and batch processing
- **Git history complexity**: Start with simple regex patterns
- **Requirement system API**: Use official SDKs, handle rate limits

### Business Risks
- **Mapping accuracy**: Start with high-confidence matches only
- **User adoption**: Focus on clear value proposition
- **Scalability**: Design for incremental improvements

## Post-MVP Roadmap

### Phase 4: Enhanced Features (4-6 weeks)
- Multiple requirement system support
- Advanced semantic analysis
- Bidirectional synchronization
- Real-time updates

### Phase 5: Production Ready (6-8 weeks)
- Performance optimization
- Enterprise features (SSO, audit logs)
- Advanced analytics and reporting
- API for third-party integration

## Implementation Priority

### Week 1-2: Foundation
1. Set up basic project structure
2. Implement Git history analysis
3. Create simple requirement provider (Jira)

### Week 3-4: Core Logic
1. Build semantic matching with existing LLM
2. Implement basic mapping engine
3. Create output generation

### Week 5-6: Validation
1. Build validation tools
2. Create comprehensive tests
3. Generate MVP documentation

## Getting Started

### Prerequisites
```bash
# Install additional dependencies
pip install gitpython requests sentence-transformers

# Set up environment
export JIRA_URL="https://your-domain.atlassian.net"
export JIRA_TOKEN="your-api-token"
export GITHUB_TOKEN="your-github-token"  # Optional
```

### Quick Start
```bash
# Run MVP analysis
python -m pm_analysis.mvp_main --program "FRAUD_MANAGEMENT"

# View results
open mvp_results/mapping_report.html
```

This MVP approach ensures rapid validation of the core concept while building a solid foundation for future enhancements.

