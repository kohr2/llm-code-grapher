"""
Base Ontology Models
Abstract base classes for program analysis across all languages
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Dict, Any, Optional, Union
from enum import Enum


class RiskLevel(Enum):
    """Standard risk levels across all languages"""
    LOW = "LOW"
    MEDIUM = "MEDIUM"
    HIGH = "HIGH"
    CRITICAL = "CRITICAL"


class QualityLevel(Enum):
    """Standard quality levels across all languages"""
    LOW = "LOW"
    MEDIUM = "MEDIUM"
    HIGH = "HIGH"


class ModernizationLevel(Enum):
    """Standard modernization potential levels"""
    LOW = "LOW"
    MEDIUM = "MEDIUM"
    HIGH = "HIGH"


@dataclass
class BaseProgram(ABC):
    """Abstract base class for program representation"""
    name: str
    language: str
    metadata: Dict[str, Any]
    
    @abstractmethod
    def get_complexity_metrics(self) -> Dict[str, float]:
        """Get complexity metrics for the program"""
        pass
    
    @abstractmethod
    def get_quality_indicators(self) -> Dict[str, Union[str, float]]:
        """Get quality indicators for the program"""
        pass


@dataclass
class BaseSection(ABC):
    """Abstract base class for section representation"""
    name: str
    type: str
    line_range: tuple[int, int]
    line_count: int
    business_logic: str
    confidence: float
    complexity_score: float
    risk_level: RiskLevel
    metadata: Optional[Dict[str, Any]] = None
    
    def get_dependencies(self) -> List[str]:
        """Get list of dependencies for this section (default implementation)"""
        return []
    
    def get_metrics(self) -> Dict[str, Any]:
        """Get section-specific metrics (default implementation)"""
        return {
            "line_count": self.line_count,
            "complexity_score": self.complexity_score,
            "risk_level": self.risk_level.value if isinstance(self.risk_level, RiskLevel) else self.risk_level
        }


@dataclass
class BaseSubsection(ABC):
    """Abstract base class for subsection representation"""
    name: str
    parent_section: str
    line_range: tuple[int, int]
    line_count: int
    business_logic: str
    confidence: float
    complexity_score: float
    risk_level: RiskLevel
    metadata: Optional[Dict[str, Any]] = None
    
    def get_dependencies(self) -> List[str]:
        """Get list of dependencies for this subsection (default implementation)"""
        return []
    
    def get_metrics(self) -> Dict[str, Any]:
        """Get subsection-specific metrics (default implementation)"""
        return {
            "line_count": self.line_count,
            "complexity_score": self.complexity_score,
            "risk_level": self.risk_level.value if isinstance(self.risk_level, RiskLevel) else self.risk_level
        }


@dataclass
class BaseRelationship(ABC):
    """Abstract base class for relationship representation"""
    source: str
    target: str
    relationship_type: str
    confidence: float
    strength: float
    
    def get_relationship_metadata(self) -> Dict[str, Any]:
        """Get relationship-specific metadata (default implementation)"""
        return {
            "confidence": self.confidence,
            "strength": self.strength,
            "type": self.relationship_type
        }


@dataclass
class BaseDataItem(ABC):
    """Abstract base class for data item representation"""
    name: str
    data_type: str
    scope_level: int
    parent_scope: Optional[str]
    description: str
    operations: List[str]
    
    @abstractmethod
    def get_usage_metrics(self) -> Dict[str, Any]:
        """Get usage metrics for this data item"""
        pass


@dataclass
class BaseBusinessRule(ABC):
    """Abstract base class for business rule representation"""
    rule_id: str
    description: str
    scope: str
    confidence: float
    risk_level: RiskLevel
    priority: str
    
    @abstractmethod
    def get_rule_metadata(self) -> Dict[str, Any]:
        """Get rule-specific metadata"""
        pass


@dataclass
class BaseOntologyResult(ABC):
    """Abstract base class for ontology analysis results"""
    program: BaseProgram
    sections: List[BaseSection]
    subsections: List[BaseSubsection]
    relationships: List[BaseRelationship]
    data_items: List[BaseDataItem]
    business_rules: List[BaseBusinessRule]
    
    @abstractmethod
    def get_ontology_metrics(self) -> Dict[str, Any]:
        """Get overall ontology metrics"""
        pass
    
    @abstractmethod
    def validate_consistency(self) -> List[str]:
        """Validate ontology consistency and return errors"""
        pass
