"""
COBOL Program Ontology Validator
Validates that parsed COBOL programs conform to the ontology schema
"""

import yaml
from pathlib import Path
from typing import List, Dict, Any, Optional
from dataclasses import dataclass


@dataclass
class ValidationResult:
    """Result of ontology validation"""
    is_valid: bool
    errors: List[str]
    warnings: List[str]
    metrics: Dict[str, Any]


class COBOLOntologyValidator:
    """Validates COBOL program analysis results against the ontology schema"""
    
    def __init__(self, ontology_file: Optional[Path] = None):
        """Initialize validator with ontology schema"""
        if ontology_file is None:
            ontology_file = Path(__file__).parent / "cobol_program_ontology.yaml"
        
        self.ontology = self._load_ontology(ontology_file)
        self.valid_risk_levels = ["LOW", "MEDIUM", "HIGH", "CRITICAL"]
        self.valid_relationship_types = [
            "CALLS", "USES", "MODIFIES", "DEPENDS_ON", 
            "DATA_FLOW", "CONTROL_FLOW", "IMPLEMENTS", "CONTAINS"
        ]
        self.valid_quality_levels = ["LOW", "MEDIUM", "HIGH"]
        self.valid_modernization_levels = ["LOW", "MEDIUM", "HIGH"]
    
    def _load_ontology(self, ontology_file: Path) -> Dict[str, Any]:
        """Load ontology schema from YAML file"""
        with open(ontology_file, 'r', encoding='utf-8') as f:
            return yaml.safe_load(f)
    
    def validate_analysis_result(self, result) -> ValidationResult:
        """Validate a COBOL analysis result against the ontology"""
        errors = []
        warnings = []
        metrics = {}
        
        # Validate sections
        section_errors, section_warnings, section_metrics = self._validate_sections(result.sections)
        errors.extend(section_errors)
        warnings.extend(section_warnings)
        metrics.update(section_metrics)
        
        # Validate subsections
        subsection_errors, subsection_warnings, subsection_metrics = self._validate_subsections(
            result.subsections, result.sections
        )
        errors.extend(subsection_errors)
        warnings.extend(subsection_warnings)
        metrics.update(subsection_metrics)
        
        # Validate relationships
        relationship_errors, relationship_warnings, relationship_metrics = self._validate_relationships(
            result.relationships, result.sections, result.subsections
        )
        errors.extend(relationship_errors)
        warnings.extend(relationship_warnings)
        metrics.update(relationship_metrics)
        
        # Validate ontology consistency
        consistency_errors, consistency_warnings = self._validate_consistency(result)
        errors.extend(consistency_errors)
        warnings.extend(consistency_warnings)
        
        # Calculate overall metrics
        metrics.update(self._calculate_overall_metrics(result))
        
        return ValidationResult(
            is_valid=len(errors) == 0,
            errors=errors,
            warnings=warnings,
            metrics=metrics
        )
    
    def _validate_sections(self, sections) -> tuple[List[str], List[str], Dict[str, Any]]:
        """Validate sections against ontology schema"""
        errors = []
        warnings = []
        metrics = {
            "total_sections": len(sections),
            "section_types": {},
            "risk_distribution": {},
            "confidence_distribution": {"high": 0, "medium": 0, "low": 0}
        }
        
        for section in sections:
            # Validate required properties
            if not hasattr(section, 'name') or not section.name:
                errors.append(f"Section missing required 'name' property")
            
            if not hasattr(section, 'type') or not section.type:
                errors.append(f"Section {section.name} missing required 'type' property")
            
            # Validate confidence
            if hasattr(section, 'confidence'):
                if not (0.0 <= section.confidence <= 1.0):
                    errors.append(f"Section {section.name} has invalid confidence: {section.confidence}")
                elif section.confidence >= 0.8:
                    metrics["confidence_distribution"]["high"] += 1
                elif section.confidence >= 0.5:
                    metrics["confidence_distribution"]["medium"] += 1
                else:
                    metrics["confidence_distribution"]["low"] += 1
            else:
                warnings.append(f"Section {section.name} missing confidence score")
            
            # Validate complexity score
            if hasattr(section, 'complexity_score'):
                if not (0.0 <= section.complexity_score <= 1.0):
                    errors.append(f"Section {section.name} has invalid complexity_score: {section.complexity_score}")
            else:
                warnings.append(f"Section {section.name} missing complexity_score")
            
            # Validate risk level
            if hasattr(section, 'risk_level'):
                if section.risk_level not in self.valid_risk_levels:
                    errors.append(f"Section {section.name} has invalid risk_level: {section.risk_level}")
                else:
                    metrics["risk_distribution"][section.risk_level] = metrics["risk_distribution"].get(section.risk_level, 0) + 1
            else:
                warnings.append(f"Section {section.name} missing risk_level")
            
            # Track section types
            if hasattr(section, 'type'):
                metrics["section_types"][section.type] = metrics["section_types"].get(section.type, 0) + 1
        
        return errors, warnings, metrics
    
    def _validate_subsections(self, subsections, sections) -> tuple[List[str], List[str], Dict[str, Any]]:
        """Validate subsections against ontology schema"""
        errors = []
        warnings = []
        metrics = {
            "total_subsections": len(subsections),
            "parent_validation": {"valid": 0, "invalid": 0}
        }
        
        section_names = {s.name for s in sections}
        
        for subsection in subsections:
            # Validate required properties
            if not hasattr(subsection, 'name') or not subsection.name:
                errors.append(f"Subsection missing required 'name' property")
            
            if not hasattr(subsection, 'parent_section') or not subsection.parent_section:
                errors.append(f"Subsection {subsection.name} missing required 'parent_section' property")
            else:
                if subsection.parent_section in section_names:
                    metrics["parent_validation"]["valid"] += 1
                else:
                    metrics["parent_validation"]["invalid"] += 1
                    errors.append(f"Subsection {subsection.name} references non-existent parent: {subsection.parent_section}")
            
            # Validate confidence
            if hasattr(subsection, 'confidence'):
                if not (0.0 <= subsection.confidence <= 1.0):
                    errors.append(f"Subsection {subsection.name} has invalid confidence: {subsection.confidence}")
            else:
                warnings.append(f"Subsection {subsection.name} missing confidence score")
            
            # Validate complexity score
            if hasattr(subsection, 'complexity_score'):
                if not (0.0 <= subsection.complexity_score <= 1.0):
                    errors.append(f"Subsection {subsection.name} has invalid complexity_score: {subsection.complexity_score}")
            else:
                warnings.append(f"Subsection {subsection.name} missing complexity_score")
            
            # Validate risk level
            if hasattr(subsection, 'risk_level'):
                if subsection.risk_level not in self.valid_risk_levels:
                    errors.append(f"Subsection {subsection.name} has invalid risk_level: {subsection.risk_level}")
            else:
                warnings.append(f"Subsection {subsection.name} missing risk_level")
        
        return errors, warnings, metrics
    
    def _validate_relationships(self, relationships, sections, subsections) -> tuple[List[str], List[str], Dict[str, Any]]:
        """Validate relationships against ontology schema"""
        errors = []
        warnings = []
        metrics = {
            "total_relationships": len(relationships),
            "relationship_types": {},
            "self_references": 0,
            "invalid_references": 0
        }
        
        all_names = {s.name for s in sections} | {s.name for s in subsections}
        
        for relationship in relationships:
            # Validate required properties
            if not hasattr(relationship, 'source') or not relationship.source:
                errors.append(f"Relationship missing required 'source' property")
            
            if not hasattr(relationship, 'target') or not relationship.target:
                errors.append(f"Relationship missing required 'target' property")
            
            if not hasattr(relationship, 'relationship_type') or not relationship.relationship_type:
                errors.append(f"Relationship missing required 'relationship_type' property")
            else:
                if relationship.relationship_type not in self.valid_relationship_types:
                    errors.append(f"Relationship has invalid type: {relationship.relationship_type}")
                else:
                    metrics["relationship_types"][relationship.relationship_type] = metrics["relationship_types"].get(relationship.relationship_type, 0) + 1
            
            # Validate references
            if hasattr(relationship, 'source') and hasattr(relationship, 'target'):
                if relationship.source == relationship.target:
                    metrics["self_references"] += 1
                    warnings.append(f"Relationship has self-reference: {relationship.source} -> {relationship.target}")
                
                if relationship.source not in all_names:
                    metrics["invalid_references"] += 1
                    errors.append(f"Relationship source not found: {relationship.source}")
                
                if relationship.target not in all_names:
                    metrics["invalid_references"] += 1
                    errors.append(f"Relationship target not found: {relationship.target}")
            
            # Validate confidence
            if hasattr(relationship, 'confidence'):
                if not (0.0 <= relationship.confidence <= 1.0):
                    errors.append(f"Relationship has invalid confidence: {relationship.confidence}")
            else:
                warnings.append(f"Relationship missing confidence score")
            
            # Validate strength
            if hasattr(relationship, 'strength'):
                if not (0.0 <= relationship.strength <= 1.0):
                    errors.append(f"Relationship has invalid strength: {relationship.strength}")
            else:
                warnings.append(f"Relationship missing strength score")
        
        return errors, warnings, metrics
    
    def _validate_consistency(self, result) -> tuple[List[str], List[str]]:
        """Validate overall ontology consistency"""
        errors = []
        warnings = []
        
        # Check for duplicate names
        section_names = [s.name for s in result.sections]
        subsection_names = [s.name for s in result.subsections]
        
        if len(section_names) != len(set(section_names)):
            errors.append("Duplicate section names found")
        
        if len(subsection_names) != len(set(subsection_names)):
            errors.append("Duplicate subsection names found")
        
        # Check for name conflicts between sections and subsections
        name_conflicts = set(section_names) & set(subsection_names)
        if name_conflicts:
            errors.append(f"Name conflicts between sections and subsections: {name_conflicts}")
        
        return errors, warnings
    
    def _calculate_overall_metrics(self, result) -> Dict[str, Any]:
        """Calculate overall ontology metrics"""
        metrics = {
            "ontology_version": "1.0",
            "total_components": len(result.sections) + len(result.subsections),
            "coverage_score": 0.0,
            "consistency_score": 0.0
        }
        
        # Calculate coverage score (percentage of components with complete data)
        total_components = len(result.sections) + len(result.subsections)
        complete_components = 0
        
        for section in result.sections:
            if (hasattr(section, 'business_logic') and section.business_logic and
                hasattr(section, 'confidence') and 0.0 <= section.confidence <= 1.0):
                complete_components += 1
        
        for subsection in result.subsections:
            if (hasattr(subsection, 'business_logic') and subsection.business_logic and
                hasattr(subsection, 'confidence') and 0.0 <= subsection.confidence <= 1.0):
                complete_components += 1
        
        if total_components > 0:
            metrics["coverage_score"] = complete_components / total_components
        
        # Calculate consistency score (based on validation results)
        # This would be calculated based on the validation results
        metrics["consistency_score"] = 1.0  # Placeholder
        
        return metrics
