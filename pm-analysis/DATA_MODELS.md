# PM Analysis Data Models

## Overview

This document defines the complete data models used in the PM Analysis system. These models represent the core data structures that flow through the analysis pipeline.

## Core Domain Models

### BusinessCapability

Represents a business capability extracted from code analysis.

```python
@dataclass
class BusinessCapability:
    """Business capability extracted from code analysis"""
    
    # Core identification
    id: str                           # Unique identifier
    domain: str                       # Business domain (e.g., "fraud_detection")
    operation: str                    # Operation type (e.g., "validates", "calculates")
    entity: str                       # Data entity (e.g., "customer_account")
    
    # Business context
    business_rules: List[str]         # Extracted business rules
    business_value: str              # Business value description
    acceptance_criteria: List[str]    # Acceptance criteria
    data_flows: List[DataFlow]       # Input/output data flows
    
    # Technical context
    source_code: str                 # Original code section
    code_location: CodeLocation      # File and line information
    function_names: List[str]        # Related function names
    
    # Metadata
    confidence: float                # Extraction confidence (0.0-1.0)
    extraction_method: str           # How this was extracted
    extracted_at: datetime          # When this was extracted
    validated: bool                 # Whether manually validated
    validation_date: Optional[datetime]  # When validated
```

### CodeLocation

Represents a location in source code.

```python
@dataclass
class CodeLocation:
    """Location information in source code"""
    file_path: str                   # Relative file path
    line_start: int                  # Starting line number
    line_end: int                    # Ending line number
    column_start: Optional[int]      # Starting column
    column_end: Optional[int]        # Ending column
    function_name: Optional[str]     # Containing function
    class_name: Optional[str]        # Containing class
```

### DataFlow

Represents data flow information.

```python
@dataclass
class DataFlow:
    """Data flow information"""
    flow_type: str                   # "input", "output", "internal"
    data_type: str                   # Type of data
    source: str                      # Source of data
    destination: str                 # Destination of data
    transformation: Optional[str]    # Transformation applied
```

## Evidence Models

### GitEvidence

Evidence from git history linking code changes to Jira issues.

```python
@dataclass
class GitEvidence:
    """Evidence from git history"""
    
    # Commit information
    commit_hash: str                 # Full commit hash
    short_hash: str                  # Short commit hash
    commit_message: str              # Full commit message
    commit_date: datetime           # Commit timestamp
    author: str                     # Commit author
    
    # Requirement connection
    requirement_key: str            # Related requirement key
    pattern_matched: str            # Pattern that matched
    
    # File changes
    files_changed: List[str]        # List of changed files
    lines_added: int                # Lines added in commit
    lines_deleted: int              # Lines deleted in commit
    functions_modified: List[str]   # Functions that were modified
    
    # Business logic changes
    business_logic_changes: List[str]  # Business logic descriptions
    code_sections_changed: List[CodeSection]  # Specific code sections
    
    # Scoring
    recency_score: float            # Decay based on age (0.0-1.0)
    relevance_score: float          # Based on content relevance (0.0-1.0)
    impact_score: float             # Based on lines changed (0.0-1.0)
    
    # Metadata
    analyzed_at: datetime           # When this evidence was analyzed
    confidence: float               # Overall confidence (0.0-1.0)
```

### CodeSection

Represents a specific section of code that changed.

```python
@dataclass
class CodeSection:
    """Specific section of code that changed"""
    file_path: str                  # File path
    line_start: int                 # Starting line
    line_end: int                   # Ending line
    change_type: str                # "added", "deleted", "modified"
    content: str                    # Code content
    business_logic: Optional[str]   # Extracted business logic
```

### RequirementEvidence

Evidence from requirement content.

```python
@dataclass
class RequirementEvidence:
    """Evidence from requirement"""
    
    # Requirement identification
    requirement_key: str            # Requirement key (e.g., "PROJ-123")
    requirement_type: str           # Requirement type (e.g., "Epic", "Story", "Task")
    requirement_id: str             # Internal requirement ID
    
    # Content
    summary: str                    # Requirement summary/title
    description: str                # Requirement description
    acceptance_criteria: List[str]  # Acceptance criteria
    business_value: str             # Business value statement
    
    # Categorization
    labels: List[str]               # Requirement labels
    components: List[str]           # Requirement components
    version: List[str]              # Versions
    priority: str                   # Requirement priority
    
    # Relationships
    epic_link: Optional[str]        # Parent epic
    parent_requirement: Optional[str] # Parent requirement
    sub_requirements: List[str]     # Sub-requirement keys
    linked_requirements: List[str]  # Linked requirement keys
    
    # Business capabilities
    business_capabilities: List[BusinessCapability]  # Extracted capabilities
    requirement_patterns: List[str] # Detected requirement patterns
    
    # Metadata
    created_date: datetime         # Requirement creation date
    updated_date: datetime         # Last update date
    resolved_date: Optional[datetime]  # Resolution date
    status: str                    # Current status
    assignee: Optional[str]        # Current assignee
    reporter: str                  # Requirement reporter
    
    # Analysis metadata
    analyzed_at: datetime          # When this was analyzed
    confidence: float              # Analysis confidence (0.0-1.0)
```

## Mapping Models

### MappingEvidence

Combined evidence for a code-requirement mapping.

```python
@dataclass
class MappingEvidence:
    """Combined evidence for code-requirement mapping"""
    
    # Evidence sources
    semantic_similarity: float      # Semantic similarity score (0.0-1.0)
    git_evidence: List[GitEvidence] # Git history evidence
    requirement_evidence: RequirementEvidence  # Requirement evidence
    
    # Cross-validation
    cross_reference_score: float   # How well sources agree (0.0-1.0)
    evidence_consistency: float    # Consistency between sources (0.0-1.0)
    conflicting_evidence: List[str] # Any conflicting evidence
    
    # Final scoring
    confidence_score: float        # Final weighted confidence (0.0-1.0)
    evidence_sources: List[str]    # Sources used: ["semantic", "git", "requirements"]
    mapping_strength: str          # "strong", "medium", "weak"
    
    # Analysis details
    semantic_details: SemanticAnalysisDetails
    git_details: GitAnalysisDetails
    requirement_details: RequirementAnalysisDetails
    
    # Metadata
    analyzed_at: datetime          # When evidence was analyzed
    analysis_duration: float       # Analysis time in seconds
```

### SemanticAnalysisDetails

Details of semantic analysis.

```python
@dataclass
class SemanticAnalysisDetails:
    """Details of semantic analysis"""
    embedding_model: str           # Model used for embeddings
    similarity_metrics: Dict[str, float]  # Various similarity scores
    matched_concepts: List[str]    # Matched business concepts
    unmatched_concepts: List[str]  # Unmatched concepts
    confidence_factors: Dict[str, float]  # Confidence factors
```

### GitAnalysisDetails

Details of git analysis.

```python
@dataclass
class GitAnalysisDetails:
    """Details of git analysis"""
    total_commits: int             # Total commits analyzed
    relevant_commits: int          # Commits with Jira links
    time_range: Tuple[datetime, datetime]  # Time range of commits
    most_recent_commit: Optional[GitEvidence]  # Most recent relevant commit
    commit_frequency: float        # Commits per month
    file_change_frequency: float   # File changes per commit
```

### RequirementAnalysisDetails

Details of requirement analysis.

```python
@dataclass
class RequirementAnalysisDetails:
    """Details of requirement analysis"""
    creation_date: datetime        # When requirement was created
    last_updated: datetime         # Last update date
    status_history: List[StatusChange]  # Status change history
    comment_count: int             # Number of comments
    attachment_count: int          # Number of attachments
    work_log_entries: List[WorkLogEntry]  # Work log entries
```

### StatusChange

Represents a status change in requirements.

```python
@dataclass
class StatusChange:
    """Status change in requirement"""
    from_status: str               # Previous status
    to_status: str                 # New status
    change_date: datetime          # When status changed
    changed_by: str                # Who made the change
    comment: Optional[str]         # Associated comment
```

### WorkLogEntry

Represents work logged on requirement.

```python
@dataclass
class WorkLogEntry:
    """Work logged on requirement"""
    author: str                    # Who logged the work
    time_spent: int                # Time spent in seconds
    date_started: datetime         # When work was started
    comment: Optional[str]         # Work log comment
```

## Result Models

### PMAnalysisResult

Complete result of PM analysis for a code node.

```python
@dataclass
class PMAnalysisResult:
    """Complete PM analysis result"""
    
    # Core identification
    code_node_id: str              # Unique code node identifier
    file_path: str                 # File path
    analysis_timestamp: datetime   # When analysis was performed
    
    # Extracted capabilities
    business_capabilities: List[BusinessCapability]  # Extracted capabilities
    
    # Generated mappings
    mappings: List[CodeRequirementMapping]  # Code-requirement mappings
    
    # Analysis metadata
    analysis_metadata: AnalysisMetadata
    confidence_distribution: ConfidenceDistribution
    
    # Quality metrics
    analysis_quality: AnalysisQuality
    data_quality: DataQuality
```

### CodeRequirementMapping

Individual mapping between code and requirement.

```python
@dataclass
class CodeRequirementMapping:
    """Individual code-requirement mapping"""
    
    # Core identification
    mapping_id: str                # Unique mapping identifier
    code_node_id: str              # Code node identifier
    requirement_key: str           # Requirement key
    
    # Mapping details
    confidence: float              # Mapping confidence (0.0-1.0)
    evidence_sources: List[str]    # Evidence sources used
    evidence_details: MappingEvidence  # Detailed evidence
    
    # Business context
    business_impact: str           # Business impact description
    mapping_type: str              # "direct", "inferred", "validated"
    relationship_type: str         # "implements", "supports", "depends_on"
    
    # Lifecycle
    created_at: datetime           # When mapping was created
    last_updated: datetime         # Last update
    validated_at: Optional[datetime]  # When manually validated
    validation_status: str         # "pending", "validated", "rejected"
    validated_by: Optional[str]    # Who validated it
    
    # Change tracking
    change_history: List[MappingChange]  # History of changes
    version: int                   # Mapping version number
```

### MappingChange

Represents a change to a mapping.

```python
@dataclass
class MappingChange:
    """Change to a mapping"""
    change_type: str               # "created", "updated", "validated", "rejected"
    changed_by: str                # Who made the change
    change_date: datetime          # When change was made
    previous_value: Optional[str]  # Previous value
    new_value: Optional[str]       # New value
    reason: Optional[str]          # Reason for change
```

### AnalysisMetadata

Metadata about the analysis process.

```python
@dataclass
class AnalysisMetadata:
    """Analysis process metadata"""
    
    # Analysis scope
    total_nodes_analyzed: int      # Total code nodes analyzed
    files_analyzed: int            # Total files analyzed
    lines_of_code_analyzed: int    # Total lines analyzed
    
    # Mapping results
    high_confidence_mappings: int  # Mappings with confidence > 0.8
    medium_confidence_mappings: int # Mappings with confidence 0.5-0.8
    low_confidence_mappings: int   # Mappings with confidence < 0.5
    total_mappings: int            # Total mappings generated
    
    # Processing metrics
    analysis_duration: float       # Total analysis time (seconds)
    git_commits_analyzed: int      # Git commits analyzed
    requirements_analyzed: int     # Requirements analyzed
    semantic_queries_executed: int # Semantic similarity queries
    
    # Quality metrics
    errors_encountered: List[str]  # Errors during analysis
    warnings_generated: List[str]  # Warnings generated
    performance_metrics: Dict[str, float]  # Performance metrics
    
    # Configuration
    config_version: str            # Configuration version used
    model_versions: Dict[str, str] # Versions of models used
```

### ConfidenceDistribution

Distribution of confidence scores.

```python
@dataclass
class ConfidenceDistribution:
    """Confidence score distribution"""
    mean_confidence: float         # Mean confidence score
    median_confidence: float       # Median confidence score
    std_deviation: float           # Standard deviation
    min_confidence: float          # Minimum confidence
    max_confidence: float          # Maximum confidence
    
    # Counts by confidence level
    high_confidence_count: int     # Count > 0.8
    medium_confidence_count: int   # Count 0.5-0.8
    low_confidence_count: int      # Count < 0.5
    
    # Percentiles
    percentile_25: float           # 25th percentile
    percentile_75: float           # 75th percentile
    percentile_90: float           # 90th percentile
    percentile_95: float           # 95th percentile
```

### AnalysisQuality

Quality metrics for the analysis.

```python
@dataclass
class AnalysisQuality:
    """Quality metrics for analysis"""
    completeness_score: float      # How complete the analysis is (0.0-1.0)
    accuracy_score: float          # Estimated accuracy (0.0-1.0)
    consistency_score: float       # Internal consistency (0.0-1.0)
    coverage_score: float          # Coverage of codebase (0.0-1.0)
    
    # Detailed metrics
    missing_data_points: List[str] # Missing data identified
    data_inconsistencies: List[str] # Inconsistencies found
    quality_warnings: List[str]    # Quality warnings
```

### DataQuality

Quality of input data.

```python
@dataclass
class DataQuality:
    """Quality of input data"""
    git_data_quality: float        # Quality of git data (0.0-1.0)
    jira_data_quality: float       # Quality of Jira data (0.0-1.0)
    code_data_quality: float       # Quality of code data (0.0-1.0)
    
    # Issues identified
    missing_git_history: List[str] # Missing git history
    incomplete_jira_data: List[str] # Incomplete Jira data
    code_parsing_issues: List[str] # Code parsing issues
    
    # Data completeness
    git_coverage: float            # Git history coverage (0.0-1.0)
    jira_coverage: float           # Jira data coverage (0.0-1.0)
    code_coverage: float           # Code analysis coverage (0.0-1.0)
```

## Configuration Models

### PMAnalysisConfig

Main configuration for PM analysis.

```python
@dataclass
class PMAnalysisConfig:
    """Configuration for PM analysis"""
    
    # Business logic extraction
    business_logic: BusinessLogicConfig
    
    # Git integration
    git_integration: GitIntegrationConfig
    
    # Requirement system integration
    requirement_system: RequirementSystemConfig
    
    # Semantic matching
    semantic_matching: SemanticMatchingConfig
    
    # Confidence scoring
    confidence_scoring: ConfidenceScoringConfig
    
    # Performance settings
    performance: PerformanceConfig
```

### BusinessLogicConfig

Configuration for business logic extraction.

```python
@dataclass
class BusinessLogicConfig:
    """Configuration for business logic extraction"""
    extraction_model: str          # LLM model to use
    confidence_threshold: float    # Minimum confidence threshold
    max_tokens: int                # Maximum tokens for analysis
    temperature: float             # LLM temperature
    batch_size: int                # Batch size for processing
```

### GitIntegrationConfig

Configuration for git integration.

```python
@dataclass
class GitIntegrationConfig:
    """Configuration for git integration"""
    repo_path: str                 # Repository path
    jira_patterns: List[str]       # Patterns to match Jira keys
    recency_decay_days: int        # Days for recency decay
    max_commits_to_analyze: int    # Maximum commits to analyze
    include_merge_commits: bool    # Include merge commits
```

### RequirementSystemConfig

Configuration for requirement system integration.

```python
@dataclass
class RequirementSystemConfig:
    """Configuration for requirement system integration"""
    type: str                      # System type (jira, azure_devops, linear, etc.)
    url: str                       # System URL
    username: str                  # Username
    api_token_env: str             # Environment variable for API token
    project_key: str               # Project key
    batch_size: int                # Batch size for API calls
    rate_limit: int                # Rate limit (requests per minute)
    timeout: int                   # Request timeout (seconds)
```

### SemanticMatchingConfig

Configuration for semantic matching.

```python
@dataclass
class SemanticMatchingConfig:
    """Configuration for semantic matching"""
    embedding_model: str           # Embedding model to use
    similarity_threshold: float    # Similarity threshold
    max_candidates: int            # Maximum candidates to consider
    batch_size: int                # Batch size for processing
```

### ConfidenceScoringConfig

Configuration for confidence scoring.

```python
@dataclass
class ConfidenceScoringConfig:
    """Configuration for confidence scoring"""
    weights: Dict[str, float]      # Weights for different evidence sources
    thresholds: Dict[str, float]   # Thresholds for confidence levels
    cross_reference_boost: float   # Boost for cross-referenced evidence
```

### PerformanceConfig

Configuration for performance settings.

```python
@dataclass
class PerformanceConfig:
    """Configuration for performance"""
    cache_ttl: int                 # Cache time-to-live (seconds)
    max_concurrent_requests: int   # Maximum concurrent requests
    batch_processing: bool         # Enable batch processing
    async_processing: bool         # Enable async processing
    memory_limit_mb: int           # Memory limit in MB
```

These data models provide a comprehensive foundation for the PM Analysis system, enabling rich data representation, analysis, and reporting capabilities.
