"""
Unit tests for COBOL Ontology Validator
Tests validation of COBOL program analysis results against ontology schema
"""

import pytest
from pathlib import Path
from unittest.mock import Mock
from ontology.cobol.cobol_ontology_validator import COBOLOntologyValidator, ValidationResult


class TestCOBOLOntologyValidator:
    """Test cases for COBOL ontology validator"""
    
    @pytest.fixture
    def validator(self):
        """Create ontology validator instance"""
        return COBOLOntologyValidator()
    
    @pytest.fixture
    def mock_analysis_result(self):
        """Create mock analysis result for testing"""
        # Mock sections
        section1 = Mock()
        section1.name = "MAIN-SECTION"
        section1.type = "PROCEDURE"
        section1.business_logic = "Main processing logic"
        section1.confidence = 0.9
        section1.complexity_score = 0.7
        section1.risk_level = "MEDIUM"
        
        section2 = Mock()
        section2.name = "DATA-SECTION"
        section2.type = "DATA"
        section2.business_logic = "Data definitions"
        section2.confidence = 0.95
        section2.complexity_score = 0.3
        section2.risk_level = "LOW"
        
        # Mock subsections
        subsection1 = Mock()
        subsection1.name = "INIT-SUBSECTION"
        subsection1.parent_section = "MAIN-SECTION"
        subsection1.business_logic = "Initialization logic"
        subsection1.confidence = 0.85
        subsection1.complexity_score = 0.5
        subsection1.risk_level = "LOW"
        
        # Mock relationships
        relationship1 = Mock()
        relationship1.source = "MAIN-SECTION"
        relationship1.target = "INIT-SUBSECTION"
        relationship1.relationship_type = "CALLS"
        relationship1.confidence = 0.9
        relationship1.strength = 1.0
        
        # Mock analysis result
        result = Mock()
        result.sections = [section1, section2]
        result.subsections = [subsection1]
        result.relationships = [relationship1]
        
        return result
    
    def test_validator_initialization(self, validator):
        """Test validator initialization"""
        assert validator is not None
        assert validator.valid_risk_levels == ["LOW", "MEDIUM", "HIGH", "CRITICAL"]
        assert len(validator.valid_relationship_types) > 0
        assert len(validator.valid_quality_levels) > 0
    
    def test_validate_valid_result(self, validator, mock_analysis_result):
        """Test validation of a valid analysis result"""
        result = validator.validate_analysis_result(mock_analysis_result)
        
        assert isinstance(result, ValidationResult)
        assert result.is_valid == True
        assert len(result.errors) == 0
        assert len(result.warnings) >= 0  # May have some warnings
        assert "total_sections" in result.metrics
        assert "total_subsections" in result.metrics
        assert "total_relationships" in result.metrics
    
    def test_validate_sections(self, validator):
        """Test section validation"""
        # Create mock sections with various issues
        invalid_section = Mock()
        invalid_section.name = ""
        invalid_section.type = "PROCEDURE"
        invalid_section.confidence = 1.5  # Invalid confidence
        invalid_section.complexity_score = -0.1  # Invalid complexity
        invalid_section.risk_level = "INVALID"  # Invalid risk level
        
        valid_section = Mock()
        valid_section.name = "VALID-SECTION"
        valid_section.type = "PROCEDURE"
        valid_section.confidence = 0.8
        valid_section.complexity_score = 0.6
        valid_section.risk_level = "MEDIUM"
        
        sections = [invalid_section, valid_section]
        errors, warnings, metrics = validator._validate_sections(sections)
        
        # Should have errors for invalid section
        assert len(errors) > 0
        assert any("invalid confidence" in error.lower() for error in errors)
        assert any("invalid complexity_score" in error.lower() for error in errors)
        assert any("invalid risk_level" in error.lower() for error in errors)
        
        # Should have metrics
        assert metrics["total_sections"] == 2
        assert "section_types" in metrics
        assert "risk_distribution" in metrics
    
    def test_validate_subsections(self, validator):
        """Test subsection validation"""
        # Create mock sections for parent validation
        parent_section = Mock()
        parent_section.name = "PARENT-SECTION"
        
        # Create mock subsections
        valid_subsection = Mock()
        valid_subsection.name = "VALID-SUBSECTION"
        valid_subsection.parent_section = "PARENT-SECTION"
        valid_subsection.confidence = 0.8
        valid_subsection.complexity_score = 0.6
        valid_subsection.risk_level = "MEDIUM"
        
        invalid_subsection = Mock()
        invalid_subsection.name = "INVALID-SUBSECTION"
        invalid_subsection.parent_section = "NON-EXISTENT-SECTION"
        invalid_subsection.confidence = 0.8
        invalid_subsection.complexity_score = 0.6
        invalid_subsection.risk_level = "MEDIUM"
        
        subsections = [valid_subsection, invalid_subsection]
        sections = [parent_section]
        
        errors, warnings, metrics = validator._validate_subsections(subsections, sections)
        
        # Should have error for invalid parent reference
        assert len(errors) > 0
        assert any("non-existent parent" in error.lower() for error in errors)
        
        # Should have metrics
        assert metrics["total_subsections"] == 2
        assert "parent_validation" in metrics
        assert metrics["parent_validation"]["valid"] == 1
        assert metrics["parent_validation"]["invalid"] == 1
    
    def test_validate_relationships(self, validator):
        """Test relationship validation"""
        # Create mock components
        section = Mock()
        section.name = "TEST-SECTION"
        
        subsection = Mock()
        subsection.name = "TEST-SUBSECTION"
        
        # Create mock relationships
        valid_relationship = Mock()
        valid_relationship.source = "TEST-SECTION"
        valid_relationship.target = "TEST-SUBSECTION"
        valid_relationship.relationship_type = "CALLS"
        valid_relationship.confidence = 0.9
        valid_relationship.strength = 1.0
        
        invalid_relationship = Mock()
        invalid_relationship.source = "TEST-SECTION"
        invalid_relationship.target = "NON-EXISTENT"
        invalid_relationship.relationship_type = "INVALID-TYPE"
        invalid_relationship.confidence = 1.5  # Invalid confidence
        invalid_relationship.strength = -0.1  # Invalid strength
        
        self_reference = Mock()
        self_reference.source = "TEST-SECTION"
        self_reference.target = "TEST-SECTION"
        self_reference.relationship_type = "CALLS"
        self_reference.confidence = 0.8
        self_reference.strength = 1.0
        
        relationships = [valid_relationship, invalid_relationship, self_reference]
        sections = [section]
        subsections = [subsection]
        
        errors, warnings, metrics = validator._validate_relationships(relationships, sections, subsections)
        
        # Should have errors for invalid relationship
        assert len(errors) > 0
        assert any("invalid type" in error.lower() for error in errors)
        assert any("invalid confidence" in error.lower() for error in errors)
        assert any("invalid strength" in error.lower() for error in errors)
        assert any("target not found" in error.lower() for error in errors)
        
        # Should have warnings for self-reference
        assert len(warnings) > 0
        assert any("self-reference" in warning.lower() for warning in warnings)
        
        # Should have metrics
        assert metrics["total_relationships"] == 3
        assert metrics["self_references"] == 1
        assert metrics["invalid_references"] == 1
    
    def test_validate_consistency(self, validator):
        """Test consistency validation"""
        # Create mock result with duplicate names
        section1 = Mock()
        section1.name = "DUPLICATE-NAME"
        
        section2 = Mock()
        section2.name = "DUPLICATE-NAME"  # Duplicate
        
        subsection1 = Mock()
        subsection1.name = "UNIQUE-SUBSECTION"
        
        result = Mock()
        result.sections = [section1, section2]
        result.subsections = [subsection1]
        
        errors, warnings = validator._validate_consistency(result)
        
        # Should have error for duplicate names
        assert len(errors) > 0
        assert any("duplicate section names" in error.lower() for error in errors)
    
    def test_calculate_overall_metrics(self, validator, mock_analysis_result):
        """Test overall metrics calculation"""
        metrics = validator._calculate_overall_metrics(mock_analysis_result)
        
        assert "ontology_version" in metrics
        assert "total_components" in metrics
        assert "coverage_score" in metrics
        assert "consistency_score" in metrics
        
        assert metrics["ontology_version"] == "1.0"
        assert metrics["total_components"] == 3  # 2 sections + 1 subsection
        assert 0.0 <= metrics["coverage_score"] <= 1.0
        assert 0.0 <= metrics["consistency_score"] <= 1.0
    
    def test_validation_result_creation(self):
        """Test ValidationResult creation"""
        result = ValidationResult(
            is_valid=True,
            errors=["Error 1", "Error 2"],
            warnings=["Warning 1"],
            metrics={"test_metric": 0.5}
        )
        
        assert result.is_valid == True
        assert len(result.errors) == 2
        assert len(result.warnings) == 1
        assert result.metrics["test_metric"] == 0.5
    
    def test_validation_with_missing_properties(self, validator):
        """Test validation with missing properties"""
        # Create mock section with missing properties
        incomplete_section = Mock()
        incomplete_section.name = "INCOMPLETE-SECTION"
        incomplete_section.type = "PROCEDURE"
        # Missing confidence, complexity_score, risk_level
        
        result = Mock()
        result.sections = [incomplete_section]
        result.subsections = []
        result.relationships = []
        
        validation_result = validator.validate_analysis_result(result)
        
        # Should have warnings for missing properties
        assert len(validation_result.warnings) > 0
        assert any("missing confidence" in warning.lower() for warning in validation_result.warnings)
        assert any("missing complexity_score" in warning.lower() for warning in validation_result.warnings)
        assert any("missing risk_level" in warning.lower() for warning in validation_result.warnings)
    
    def test_validation_with_empty_result(self, validator):
        """Test validation with empty analysis result"""
        empty_result = Mock()
        empty_result.sections = []
        empty_result.subsections = []
        empty_result.relationships = []
        
        validation_result = validator.validate_analysis_result(empty_result)
        
        # Should be valid but with zero metrics
        assert validation_result.is_valid == True
        assert validation_result.metrics["total_sections"] == 0
        assert validation_result.metrics["total_subsections"] == 0
        assert validation_result.metrics["total_relationships"] == 0
        assert validation_result.metrics["total_components"] == 0
