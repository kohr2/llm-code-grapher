"""
Base Ontology
Abstract base classes for ontology definition and validation
"""

from abc import ABC, abstractmethod
from pathlib import Path
from typing import List, Dict, Any, Optional, Type
from dataclasses import dataclass

from .base_models import (
    BaseProgram, BaseSection, BaseSubsection, BaseRelationship,
    BaseDataItem, BaseBusinessRule, BaseOntologyResult,
    RiskLevel, QualityLevel, ModernizationLevel
)


@dataclass
class ValidationResult:
    """Standard validation result across all languages"""
    is_valid: bool
    errors: List[str]
    warnings: List[str]
    metrics: Dict[str, Any]
    language: str
    ontology_version: str


class BaseOntology(ABC):
    """Abstract base class for language-specific ontologies"""
    
    def __init__(self, ontology_file: Optional[Path] = None):
        """Initialize base ontology"""
        self.language = self.get_language()
        self.ontology_version = self.get_ontology_version()
        self.ontology_schema = self._load_ontology_schema(ontology_file)
        
        # Standard validation rules
        self.valid_risk_levels = [level.value for level in RiskLevel]
        self.valid_quality_levels = [level.value for level in QualityLevel]
        self.valid_modernization_levels = [level.value for level in ModernizationLevel]
    
    @abstractmethod
    def get_language(self) -> str:
        """Get the language this ontology supports"""
        pass
    
    @abstractmethod
    def get_ontology_version(self) -> str:
        """Get the ontology version"""
        pass
    
    @abstractmethod
    def get_relationship_types(self) -> List[str]:
        """Get valid relationship types for this language"""
        pass
    
    @abstractmethod
    def get_section_types(self) -> List[str]:
        """Get valid section types for this language"""
        pass
    
    @abstractmethod
    def _load_ontology_schema(self, ontology_file: Optional[Path]) -> Dict[str, Any]:
        """Load language-specific ontology schema"""
        pass
    
    def validate_risk_level(self, risk_level: str) -> bool:
        """Validate risk level against standard values"""
        return risk_level in self.valid_risk_levels
    
    def validate_quality_level(self, quality_level: str) -> bool:
        """Validate quality level against standard values"""
        return quality_level in self.valid_quality_levels
    
    def validate_modernization_level(self, modernization_level: str) -> bool:
        """Validate modernization level against standard values"""
        return modernization_level in self.valid_modernization_levels
    
    def validate_confidence_score(self, confidence: float) -> bool:
        """Validate confidence score is in valid range"""
        return 0.0 <= confidence <= 1.0
    
    def validate_complexity_score(self, complexity_score: float) -> bool:
        """Validate complexity score is in valid range"""
        return 0.0 <= complexity_score <= 1.0
    
    def validate_required_property(self, obj: Any, property_name: str, obj_name: str = "Object") -> tuple[bool, str]:
        """Validate that an object has a required property"""
        if not hasattr(obj, property_name) or not getattr(obj, property_name):
            return False, f"{obj_name} missing required '{property_name}' property"
        return True, ""
    
    def validate_property_type(self, obj: Any, property_name: str, expected_type: type, obj_name: str = "Object") -> tuple[bool, str]:
        """Validate that an object property has the expected type"""
        if hasattr(obj, property_name):
            value = getattr(obj, property_name)
            if not isinstance(value, expected_type):
                return False, f"{obj_name} property '{property_name}' must be {expected_type.__name__}, got {type(value).__name__}"
        return True, ""


class BaseOntologyValidator(ABC):
    """Abstract base class for ontology validators"""
    
    def __init__(self, ontology: BaseOntology):
        """Initialize validator with ontology"""
        self.ontology = ontology
        self.language = ontology.get_language()
    
    @abstractmethod
    def validate_program(self, program: BaseProgram) -> ValidationResult:
        """Validate a program against the ontology"""
        pass
    
    @abstractmethod
    def validate_section(self, section: BaseSection) -> ValidationResult:
        """Validate a section against the ontology"""
        pass
    
    @abstractmethod
    def validate_subsection(self, subsection: BaseSubsection) -> ValidationResult:
        """Validate a subsection against the ontology"""
        pass
    
    @abstractmethod
    def validate_relationship(self, relationship: BaseRelationship) -> ValidationResult:
        """Validate a relationship against the ontology"""
        pass
    
    @abstractmethod
    def validate_data_item(self, data_item: BaseDataItem) -> ValidationResult:
        """Validate a data item against the ontology"""
        pass
    
    @abstractmethod
    def validate_business_rule(self, business_rule: BaseBusinessRule) -> ValidationResult:
        """Validate a business rule against the ontology"""
        pass
    
    def validate_analysis_result(self, result: BaseOntologyResult) -> ValidationResult:
        """Validate a complete analysis result"""
        errors = []
        warnings = []
        metrics = {}
        
        # Validate program
        program_result = self.validate_program(result.program)
        errors.extend(program_result.errors)
        warnings.extend(program_result.warnings)
        metrics.update(program_result.metrics)
        
        # Validate sections
        for section in result.sections:
            section_result = self.validate_section(section)
            errors.extend(section_result.errors)
            warnings.extend(section_result.warnings)
        
        # Validate subsections
        for subsection in result.subsections:
            subsection_result = self.validate_subsection(subsection)
            errors.extend(subsection_result.errors)
            warnings.extend(subsection_result.warnings)
        
        # Validate relationships
        for relationship in result.relationships:
            relationship_result = self.validate_relationship(relationship)
            errors.extend(relationship_result.errors)
            warnings.extend(relationship_result.warnings)
        
        # Validate data items
        for data_item in result.data_items:
            data_item_result = self.validate_data_item(data_item)
            errors.extend(data_item_result.errors)
            warnings.extend(data_item_result.warnings)
        
        # Validate business rules
        for business_rule in result.business_rules:
            business_rule_result = self.validate_business_rule(business_rule)
            errors.extend(business_rule_result.errors)
            warnings.extend(business_rule_result.warnings)
        
        # Calculate overall metrics
        metrics.update(self._calculate_overall_metrics(result))
        
        return ValidationResult(
            is_valid=len(errors) == 0,
            errors=errors,
            warnings=warnings,
            metrics=metrics,
            language=self.language,
            ontology_version=self.ontology.get_ontology_version()
        )
    
    def _calculate_overall_metrics(self, result: BaseOntologyResult) -> Dict[str, Any]:
        """Calculate overall ontology metrics"""
        return {
            "total_components": len(result.sections) + len(result.subsections),
            "total_relationships": len(result.relationships),
            "total_data_items": len(result.data_items),
            "total_business_rules": len(result.business_rules),
            "coverage_score": 0.0,  # To be implemented by subclasses
            "consistency_score": 0.0,  # To be implemented by subclasses
            "language": self.language,
            "ontology_version": self.ontology.get_ontology_version()
        }
    
    def _validate_common_properties(self, obj: Any, obj_name: str, required_properties: List[str]) -> tuple[List[str], List[str]]:
        """Validate common properties for any object"""
        errors = []
        warnings = []
        
        # Validate required properties
        for prop in required_properties:
            is_valid, error_msg = self.ontology.validate_required_property(obj, prop, obj_name)
            if not is_valid:
                errors.append(error_msg)
        
        # Validate confidence if present
        if hasattr(obj, 'confidence'):
            if not self.ontology.validate_confidence_score(obj.confidence):
                errors.append(f"{obj_name} has invalid confidence: {obj.confidence}")
        else:
            warnings.append(f"{obj_name} missing confidence score")
        
        # Validate complexity score if present
        if hasattr(obj, 'complexity_score'):
            if not self.ontology.validate_complexity_score(obj.complexity_score):
                errors.append(f"{obj_name} has invalid complexity_score: {obj.complexity_score}")
        else:
            warnings.append(f"{obj_name} missing complexity_score")
        
        # Validate risk level if present
        if hasattr(obj, 'risk_level'):
            if not self.ontology.validate_risk_level(obj.risk_level):
                errors.append(f"{obj_name} has invalid risk_level: {obj.risk_level}")
        else:
            warnings.append(f"{obj_name} missing risk_level")
        
        return errors, warnings
    
    def _validate_duplicate_names(self, objects: List[Any], name_attr: str, object_type: str) -> List[str]:
        """Validate that objects don't have duplicate names"""
        errors = []
        names = [getattr(obj, name_attr) for obj in objects if hasattr(obj, name_attr)]
        if len(names) != len(set(names)):
            errors.append(f"Duplicate {object_type} names found")
        return errors
