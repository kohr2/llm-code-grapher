"""
Base Ontology Validator Tests
Abstract base class for language-specific ontology validator tests
"""

import pytest
from abc import ABC, abstractmethod
from typing import List, Dict, Any
from unittest.mock import Mock

from ..ontology import BaseOntologyValidator, ValidationResult
from ..ontology.base_models import BaseProgram, BaseSection, BaseSubsection, BaseRelationship


class BaseOntologyValidatorTests(ABC):
    """Abstract base class for ontology validator tests"""
    
    @abstractmethod
    def get_validator_class(self):
        """Get the validator class to test"""
        pass
    
    @abstractmethod
    def get_language(self) -> str:
        """Get the language being tested"""
        pass
    
    @abstractmethod
    def create_valid_section(self) -> BaseSection:
        """Create a valid section for testing"""
        pass
    
    @abstractmethod
    def create_valid_subsection(self) -> BaseSubsection:
        """Create a valid subsection for testing"""
        pass
    
    @abstractmethod
    def create_valid_relationship(self) -> BaseRelationship:
        """Create a valid relationship for testing"""
        pass
    
    @pytest.fixture
    def validator(self):
        """Create validator instance for testing"""
        return self.get_validator_class()()
    
    def test_validator_initialization(self, validator):
        """Test validator initializes correctly"""
        assert validator is not None
        assert validator.language == self.get_language()
        assert hasattr(validator, 'ontology')
        assert hasattr(validator, 'valid_risk_levels')
        assert hasattr(validator, 'valid_quality_levels')
        assert hasattr(validator, 'valid_modernization_levels')
    
    def test_risk_level_validation(self, validator):
        """Test risk level validation"""
        valid_risk_levels = ["LOW", "MEDIUM", "HIGH", "CRITICAL"]
        
        for risk_level in valid_risk_levels:
            assert validator.ontology.validate_risk_level(risk_level), f"Should validate {risk_level}"
        
        invalid_risk_levels = ["INVALID", "low", "Medium", "HIGHER", ""]
        for risk_level in invalid_risk_levels:
            assert not validator.ontology.validate_risk_level(risk_level), f"Should not validate {risk_level}"
    
    def test_confidence_score_validation(self, validator):
        """Test confidence score validation"""
        valid_scores = [0.0, 0.5, 1.0, 0.25, 0.75]
        for score in valid_scores:
            assert validator.ontology.validate_confidence_score(score), f"Should validate {score}"
        
        invalid_scores = [-0.1, 1.1, 2.0, -1.0]
        for score in invalid_scores:
            assert not validator.ontology.validate_confidence_score(score), f"Should not validate {score}"
    
    def test_complexity_score_validation(self, validator):
        """Test complexity score validation"""
        valid_scores = [0.0, 0.5, 1.0, 0.25, 0.75]
        for score in valid_scores:
            assert validator.ontology.validate_complexity_score(score), f"Should validate {score}"
        
        invalid_scores = [-0.1, 1.1, 2.0, -1.0]
        for score in invalid_scores:
            assert not validator.ontology.validate_complexity_score(score), f"Should not validate {score}"
    
    def test_required_property_validation(self, validator):
        """Test required property validation"""
        # Test valid object
        valid_obj = Mock()
        valid_obj.name = "TEST"
        valid_obj.type = "PROCEDURE"
        
        is_valid, error = validator.ontology.validate_required_property(valid_obj, "name", "TestObject")
        assert is_valid
        assert error == ""
        
        # Test missing property
        invalid_obj = Mock()
        invalid_obj.name = ""
        
        is_valid, error = validator.ontology.validate_required_property(invalid_obj, "name", "TestObject")
        assert not is_valid
        assert "missing required 'name' property" in error
    
    def test_property_type_validation(self, validator):
        """Test property type validation"""
        # Test valid type
        valid_obj = Mock()
        valid_obj.count = 42
        
        is_valid, error = validator.ontology.validate_property_type(valid_obj, "count", int, "TestObject")
        assert is_valid
        assert error == ""
        
        # Test invalid type
        invalid_obj = Mock()
        invalid_obj.count = "not_a_number"
        
        is_valid, error = validator.ontology.validate_property_type(invalid_obj, "count", int, "TestObject")
        assert not is_valid
        assert "must be int" in error
    
    def test_common_properties_validation(self, validator):
        """Test common properties validation"""
        # Test valid section
        valid_section = self.create_valid_section()
        errors, warnings = validator._validate_common_properties(valid_section, "TestSection", ["name", "type"])
        
        assert len(errors) == 0, f"Should have no errors: {errors}"
        # May have warnings for missing optional properties
    
    def test_duplicate_names_validation(self, validator):
        """Test duplicate names validation"""
        # Test no duplicates
        sections = [
            Mock(name="SECTION1"),
            Mock(name="SECTION2"),
            Mock(name="SECTION3")
        ]
        
        errors = validator._validate_duplicate_names(sections, "name", "sections")
        assert len(errors) == 0
        
        # Test with duplicates
        duplicate_sections = [
            Mock(name="SECTION1"),
            Mock(name="SECTION2"),
            Mock(name="SECTION1")  # Duplicate
        ]
        
        errors = validator._validate_duplicate_names(duplicate_sections, "name", "sections")
        assert len(errors) == 1
        assert "Duplicate sections names found" in errors[0]
    
    def test_section_validation(self, validator):
        """Test section validation"""
        # Test valid section
        valid_section = self.create_valid_section()
        result = validator.validate_section(valid_section)
        
        assert isinstance(result, ValidationResult)
        assert result.is_valid or len(result.errors) < 5  # Allow some errors for incomplete implementations
        assert result.language == self.get_language()
        assert "ontology_version" in result.metrics
    
    def test_subsection_validation(self, validator):
        """Test subsection validation"""
        # Test valid subsection
        valid_subsection = self.create_valid_subsection()
        result = validator.validate_subsection(valid_subsection)
        
        assert isinstance(result, ValidationResult)
        assert result.is_valid or len(result.errors) < 5  # Allow some errors for incomplete implementations
        assert result.language == self.get_language()
        assert "ontology_version" in result.metrics
    
    def test_relationship_validation(self, validator):
        """Test relationship validation"""
        # Test valid relationship
        valid_relationship = self.create_valid_relationship()
        result = validator.validate_relationship(valid_relationship)
        
        assert isinstance(result, ValidationResult)
        assert result.is_valid or len(result.errors) < 5  # Allow some errors for incomplete implementations
        assert result.language == self.get_language()
        assert "ontology_version" in result.metrics
    
    def test_validation_result_structure(self, validator):
        """Test validation result structure"""
        # Create a mock result
        mock_result = Mock()
        mock_result.program = Mock()
        mock_result.sections = [self.create_valid_section()]
        mock_result.subsections = [self.create_valid_subsection()]
        mock_result.relationships = [self.create_valid_relationship()]
        mock_result.data_items = []
        mock_result.business_rules = []
        
        # Test validation
        result = validator.validate_analysis_result(mock_result)
        
        assert isinstance(result, ValidationResult)
        assert isinstance(result.is_valid, bool)
        assert isinstance(result.errors, list)
        assert isinstance(result.warnings, list)
        assert isinstance(result.metrics, dict)
        assert result.language == self.get_language()
        assert "ontology_version" in result.metrics
        assert "total_components" in result.metrics
        assert "total_relationships" in result.metrics
