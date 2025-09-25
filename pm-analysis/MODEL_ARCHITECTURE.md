# PM Analysis Model Architecture

## Overview

The PM Analysis model is designed to automatically map code structure and business logic to product requirements through triangulated evidence from multiple sources. This document describes the complete architectural model and data structures.

## Core Architecture

### 1. Multi-Source Evidence Engine

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Code Analysis │    │   Git History   │    │  Requirements   │
│   (LLM-based)   │    │   (File Edits)  │    │   (API calls)   │
└─────────┬───────┘    └─────────┬───────┘    └─────────┬───────┘
          │                      │                      │
          └──────────────────────┼──────────────────────┘
                                 │
                    ┌─────────────▼─────────────┐
                    │   Evidence Aggregator     │
                    │   (Triangulated Analysis) │
                    └─────────────┬─────────────┘
                                  │
                    ┌─────────────▼─────────────┐
                    │  Confidence Calculator   │
                    │  (Multi-factor Scoring)  │
                    └─────────────┬─────────────┘
                                  │
                    ┌─────────────▼─────────────┐
                    │   Mapping Generator       │
                    │   (Code ↔ Requirements)   │
                    └───────────────────────────┘
```

### 2. Data Flow Model

The system processes evidence through five distinct phases:

1. **Business Logic Extraction** - Extract structured business information from code
2. **Git History Analysis** - Find concrete evidence from commit history and file edits
3. **Requirement Content Analysis** - Analyze requirement descriptions and acceptance criteria
4. **Semantic Matching** - Use embeddings to find similar business concepts
5. **Confidence Calculation** - Combine evidence sources with weighted scoring

## Data Structures

### Core Domain Models

```python
@dataclass
class BusinessCapability:
    """Represents a business capability extracted from code analysis"""
    domain: str              # "fraud_detection", "customer_management"
    operation: str           # "validates", "calculates", "processes"
    entity: str             # "customer_account", "transaction"
    business_rules: List[str] # ["validation_rules", "approval_criteria"]
    confidence: float        # 0.0-1.0
    source_code: str         # Original code section
    extracted_at: datetime   # When this was extracted

@dataclass
class GitEvidence:
    """Evidence from git history linking code changes to requirements"""
    commit_hash: str
    requirement_key: str
    commit_message: str
    files_changed: List[str]
    business_logic_changes: List[str]
    functions_modified: List[str]
    lines_added: int
    lines_deleted: int
    commit_date: datetime
    recency_score: float     # Decay based on age
    relevance_score: float   # Based on file changes and message content

@dataclass
class RequirementEvidence:
    """Evidence from requirement content"""
    requirement_key: str
    requirement_type: str         # "Epic", "Story", "Task", "Bug"
    summary: str
    description: str
    acceptance_criteria: List[str]
    business_capabilities: List[BusinessCapability]
    business_value: str
    labels: List[str]
    components: List[str]
    created_date: datetime
    updated_date: datetime

@dataclass
class MappingEvidence:
    """Combined evidence for a code-requirement mapping"""
    semantic_similarity: float    # 0.0-1.0
    git_evidence: List[GitEvidence]
    jira_evidence: JiraEvidence
    cross_reference_score: float  # How well sources agree
    confidence_score: float       # Final weighted score
    evidence_sources: List[str]   # ["semantic", "git", "jira"]
    mapping_strength: str         # "strong", "medium", "weak"
```

### Processing Pipeline Models

```python
@dataclass
class AnalysisContext:
    """Context information for analysis"""
    repo_path: str
    file_path: str
    code_node_id: str
    analysis_timestamp: datetime
    config: Dict[str, Any]

@dataclass
class PMAnalysisResult:
    """Complete result of PM analysis for a code node"""
    code_node_id: str
    file_path: str
    business_capabilities: List[BusinessCapability]
    mappings: List[CodeRequirementMapping]
    analysis_metadata: AnalysisMetadata
    confidence_distribution: ConfidenceDistribution

@dataclass
class CodeRequirementMapping:
    """Individual mapping between code and requirement"""
    jira_key: str
    confidence: float
    evidence_sources: List[str]
    evidence_details: MappingEvidence
    business_impact: str
    mapping_type: str          # "direct", "inferred", "validated"
    created_at: datetime
    last_updated: datetime
    validation_status: str     # "pending", "validated", "rejected"

@dataclass
class AnalysisMetadata:
    """Metadata about the analysis process"""
    total_nodes_analyzed: int
    high_confidence_mappings: int    # > 0.8
    medium_confidence_mappings: int  # 0.5-0.8
    low_confidence_mappings: int     # < 0.5
    analysis_duration: float
    git_commits_analyzed: int
    jira_issues_analyzed: int
    semantic_queries_executed: int
    errors_encountered: List[str]

@dataclass
class ConfidenceDistribution:
    """Distribution of confidence scores"""
    mean_confidence: float
    median_confidence: float
    std_deviation: float
    high_confidence_count: int
    medium_confidence_count: int
    low_confidence_count: int
```

## Processing Pipeline

### Main Pipeline Class

```python
class PMAnalysisPipeline:
    """Main pipeline for PM analysis"""
    
    def __init__(self, config: PMAnalysisConfig):
        self.config = config
        self.business_logic_extractor = BusinessLogicExtractor(config)
        self.git_analyzer = GitJiraAnalyzer(config)
        self.jira_analyzer = JiraBusinessAnalyzer(config)
        self.semantic_matcher = SemanticMatcher(config)
        self.confidence_calculator = ConfidenceCalculator(config)
        self.evidence_aggregator = EvidenceAggregator(config)
    
    async def analyze_code_to_requirements(
        self, 
        code_node: CodeNode, 
        context: AnalysisContext
    ) -> PMAnalysisResult:
        """Complete analysis pipeline"""
        
        # Phase 1: Extract business logic from code
        business_capabilities = await self.business_logic_extractor.extract(
            code_node, context
        )
        
        # Phase 2: Find git evidence
        git_evidence = await self.git_analyzer.analyze_file_changes(
            context.repo_path, context.file_path
        )
        
        # Phase 3: Analyze requirement content
        requirements = await self.requirement_analyzer.fetch_related_requirements(
            business_capabilities, context
        )
        
        # Phase 4: Semantic matching
        semantic_matches = await self.semantic_matcher.find_matches(
            business_capabilities, requirements
        )
        
        # Phase 5: Aggregate evidence
        aggregated_evidence = await self.evidence_aggregator.aggregate(
            semantic_matches, git_evidence, requirements
        )
        
        # Phase 6: Calculate confidence and generate mappings
        mappings = await self.confidence_calculator.calculate_mappings(
            aggregated_evidence
        )
        
        # Phase 7: Generate metadata
        metadata = self._generate_metadata(
            code_node, mappings, git_evidence, requirements
        )
        
        return PMAnalysisResult(
            code_node_id=code_node.id,
            file_path=context.file_path,
            business_capabilities=business_capabilities,
            mappings=mappings,
            analysis_metadata=metadata,
            confidence_distribution=self._calculate_confidence_distribution(mappings)
        )
```

### Component Interfaces

```python
class BusinessLogicExtractor:
    """Extracts structured business logic from code analysis"""
    
    async def extract(self, code_node: CodeNode, context: AnalysisContext) -> List[BusinessCapability]:
        """Extract business capabilities from code node"""
        pass

class GitJiraAnalyzer:
    """Analyzes git history for Jira connections"""
    
    async def analyze_file_changes(self, repo_path: str, file_path: str) -> List[GitEvidence]:
        """Analyze git history for file changes linked to Jira"""
        pass
    
    async def extract_jira_keys_from_commits(self, commits: List[Commit]) -> Dict[str, List[Commit]]:
        """Extract Jira issue keys from commit messages"""
        pass

class RequirementAnalyzer:
    """Analyzes requirements for business context"""
    
    async def fetch_related_requirements(self, capabilities: List[BusinessCapability], context: AnalysisContext) -> List[RequirementEvidence]:
        """Fetch requirements related to business capabilities"""
        pass
    
    async def extract_business_capabilities(self, requirement: Requirement) -> List[BusinessCapability]:
        """Extract business capabilities from requirement"""
        pass

class SemanticMatcher:
    """Performs semantic matching between code and requirements"""
    
    async def find_matches(self, capabilities: List[BusinessCapability], requirements: List[RequirementEvidence]) -> List[SemanticMatch]:
        """Find semantic matches between capabilities and requirements"""
        pass
    
    async def calculate_similarity(self, text1: str, text2: str) -> float:
        """Calculate semantic similarity between two texts"""
        pass

class EvidenceAggregator:
    """Aggregates evidence from multiple sources"""
    
    async def aggregate(self, semantic_matches: List[SemanticMatch], git_evidence: List[GitEvidence], requirement_evidence: List[RequirementEvidence]) -> List[AggregatedEvidence]:
        """Aggregate evidence from all sources"""
        pass

class ConfidenceCalculator:
    """Calculates confidence scores for mappings"""
    
    async def calculate_mappings(self, evidence: List[AggregatedEvidence]) -> List[CodeRequirementMapping]:
        """Calculate final confidence scores and generate mappings"""
        pass
```

## Configuration Model

```yaml
# pm-analysis/config.yaml
pm_analysis:
  business_logic:
    extraction_model: "gpt-4"
    confidence_threshold: 0.7
    max_tokens: 2000
    temperature: 0.1
    
  git_integration:
    repo_path: "/path/to/repo"
    jira_patterns:
      - "([A-Z]+-\\d+)"      # PROJ-123
      - "#([A-Z]+-\\d+)"     # #PROJ-123
      - "JIRA-(\\d+)"        # JIRA-456
    recency_decay_days: 365
    max_commits_to_analyze: 1000
    
  requirement_system:
    type: "jira"  # jira, azure_devops, linear, etc.
    url: "https://domain.atlassian.net"
    username: "user@domain.com"
    api_token_env: "PM_API_TOKEN"
    project_key: "PROJ"
    batch_size: 50
    rate_limit: 100  # requests per minute
    timeout: 30
    
  semantic_matching:
    embedding_model: "sentence-transformers/all-MiniLM-L6-v2"
    similarity_threshold: 0.8
    max_candidates: 10
    batch_size: 32
    
  confidence_scoring:
    weights:
      semantic: 0.5
      git: 0.3
      jira: 0.2
    threshold:
      high: 0.8
      medium: 0.5
      low: 0.3
    cross_reference_boost: 0.1
    
  performance:
    cache_ttl: 3600  # seconds
    max_concurrent_requests: 10
    batch_processing: true
    async_processing: true
```

## API Interface Model

```python
class PMAnalysisAPI:
    """Public API for PM Analysis functionality"""
    
    def __init__(self, config: PMAnalysisConfig):
        self.pipeline = PMAnalysisPipeline(config)
        self.repository = PMAnalysisRepository(config)
    
    async def analyze_codebase(self, repo_path: str, project_key: str) -> List[PMAnalysisResult]:
        """Analyze entire codebase for PM mappings"""
        pass
    
    async def analyze_single_file(self, file_path: str, repo_path: str) -> PMAnalysisResult:
        """Analyze single file for PM mappings"""
        pass
    
    async def map_code_to_requirement(self, code_node_id: str, requirement_key: str) -> CodeRequirementMapping:
        """Create manual mapping between code and requirement"""
        pass
    
    async def validate_mapping(self, mapping_id: str) -> ValidationResult:
        """Validate existing mapping"""
        pass
    
    async def sync_changes(self, direction: str, project_key: str) -> SyncResult:
        """Sync changes between code and requirements"""
        pass
    
    async def get_confidence_scores(self, project_key: str) -> List[ConfidenceScore]:
        """Get confidence score distribution for project"""
        pass
    
    async def export_mappings(self, format: str, filters: Dict[str, Any]) -> str:
        """Export mappings in specified format"""
        pass
    
    async def get_mapping_history(self, code_node_id: str) -> List[MappingHistoryEntry]:
        """Get history of mappings for code node"""
        pass
```

## Integration Points

### LLM Code Grapher Integration

```python
class PMAnalysisIntegration:
    """Integration with existing LLM Code Grapher"""
    
    def __init__(self, llm_code_grapher: LLMCodeGrapher, pm_config: PMAnalysisConfig):
        self.code_grapher = llm_code_grapher
        self.pm_analyzer = PMAnalysisAPI(pm_config)
    
    async def enhance_graph_analysis(self, graph_analysis: GraphAnalysis) -> EnhancedGraphAnalysis:
        """Enhance existing graph analysis with PM mappings"""
        enhanced_nodes = []
        
        for node in graph_analysis.nodes:
            # Run PM analysis on each node
            pm_result = await self.pm_analyzer.analyze_single_file(
                node.file_path, graph_analysis.repo_path
            )
            
            # Enhance node with PM data
            enhanced_node = EnhancedGraphNode(
                original_node=node,
                business_capabilities=pm_result.business_capabilities,
                requirement_mappings=pm_result.mappings,
                confidence_scores=pm_result.confidence_distribution
            )
            enhanced_nodes.append(enhanced_node)
        
        return EnhancedGraphAnalysis(
            nodes=enhanced_nodes,
            original_analysis=graph_analysis,
            pm_metadata=pm_result.analysis_metadata
        )
    
    async def generate_pm_report(self, enhanced_analysis: EnhancedGraphAnalysis) -> PMReport:
        """Generate comprehensive PM report"""
        pass
```

## Key Model Features

### 1. Triangulated Evidence
- **Semantic Analysis**: LLM-based business logic extraction
- **Git History**: Concrete evidence from commit messages and file edits
- **Jira Content**: Requirement descriptions and acceptance criteria
- **Cross-Validation**: Evidence sources validate each other

### 2. Confidence Scoring
- **Multi-Factor**: Weighted combination of evidence sources
- **Configurable Weights**: Adjust based on data quality
- **Threshold-Based**: High/Medium/Low confidence classifications
- **Cross-Reference Boost**: Additional confidence when sources agree

### 3. Scalable Architecture
- **Async Processing**: Non-blocking operations
- **Batch Processing**: Efficient handling of large datasets
- **Caching**: Reduce redundant API calls
- **Rate Limiting**: Respect external API limits

### 4. Audit Trail
- **Complete History**: Track all mapping changes
- **Evidence Preservation**: Store all evidence sources
- **Validation Tracking**: Track manual validations
- **Performance Metrics**: Monitor analysis performance

### 5. Extensibility
- **Plugin Architecture**: Easy to add new evidence sources
- **Configurable Scoring**: Adjust algorithms without code changes
- **Multiple Output Formats**: JSON, CSV, HTML reports
- **API Integration**: RESTful API for external tools

This model provides a comprehensive framework for automatically connecting code analysis to product management systems through intelligent, evidence-based mapping with full traceability and audit capabilities.
