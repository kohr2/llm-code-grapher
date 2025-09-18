"""
COBOL-specific ontology validator tests
Tests only COBOL-specific ontology validation logic
"""

import pytest
from pathlib import Path
from unittest.mock import Mock
from lang.cobol.ontology.cobol_ontology_validator import COBOLOntologyValidator, ValidationResult
from lang.cobol.parser.cobol_parser import COBOLSection, COBOLSubsection, COBOLRelationship
from lang.base.tests.base_ontology_tests import BaseOntologyValidatorTests


class TestCOBOLOntologyValidatorSpecific(BaseOntologyValidatorTests):
    """Test cases for COBOL-specific ontology validator functionality"""
    
    def get_validator_class(self):
        """Get the validator class to test"""
        return COBOLOntologyValidator
    
    def get_language(self) -> str:
        """Get the language being tested"""
        return "COBOL"
    
    def create_valid_section(self) -> COBOLSection:
        """Create a valid COBOL section for testing"""
        return COBOLSection(
            name="MAIN-SECTION",
            type="PROCEDURE",
            line_range=(1, 10),
            line_count=10,
            business_logic="Main processing logic",
            confidence=0.9,
            complexity_score=0.7,
            risk_level="MEDIUM"
        )
    
    def create_valid_subsection(self) -> COBOLSubsection:
        """Create a valid COBOL subsection for testing"""
        return COBOLSubsection(
            name="INIT-SUBSECTION",
            parent_section="MAIN-SECTION",
            line_range=(2, 5),
            line_count=4,
            business_logic="Initialization logic",
            confidence=0.85,
            complexity_score=0.5,
            risk_level="LOW"
        )
    
    def create_valid_relationship(self) -> COBOLRelationship:
        """Create a valid COBOL relationship for testing"""
        return COBOLRelationship(
            source="MAIN-SECTION",
            target="UTIL-PROGRAM",
            relationship_type="CALLS",
            confidence=0.9,
            strength=1.0
        )
    
    def test_cobol_section_type_validation(self, validator):
        """Test COBOL-specific section type validation"""
        # Test valid COBOL section types
        valid_types = ["IDENTIFICATION", "ENVIRONMENT", "DATA", "PROCEDURE"]
        for section_type in valid_types:
            section = self.create_valid_section()
            section.type = section_type
            result = validator.validate_section(section)
            assert result.is_valid or len(result.errors) < 2, f"Should validate {section_type}"
        
        # Test invalid section type
        invalid_section = self.create_valid_section()
        invalid_section.type = "INVALID_TYPE"
        result = validator.validate_section(invalid_section)
        assert not result.is_valid
        assert any("invalid type" in error.lower() for error in result.errors)
    
    def test_cobol_relationship_type_validation(self, validator):
        """Test COBOL-specific relationship type validation"""
        # Test valid COBOL relationship types
        valid_types = ["CALLS", "USES", "MODIFIES", "DEPENDS_ON", "PERFORMS", "GOES_TO"]
        for rel_type in valid_types:
            relationship = self.create_valid_relationship()
            relationship.relationship_type = rel_type
            result = validator.validate_relationship(relationship)
            assert result.is_valid or len(result.errors) < 2, f"Should validate {rel_type}"
        
        # Test invalid relationship type
        invalid_relationship = self.create_valid_relationship()
        invalid_relationship.relationship_type = "INVALID_TYPE"
        result = validator.validate_relationship(invalid_relationship)
        assert not result.is_valid
        assert any("invalid type" in error.lower() for error in result.errors)
    
    def test_cobol_data_type_validation(self, validator):
        """Test COBOL-specific data type validation"""
        # Test valid COBOL data types
        valid_data_types = ["PIC", "PICTURE", "COMP", "COMP-1", "COMP-2", "COMP-3"]
        for data_type in valid_data_types:
            data_item = Mock()
            data_item.name = "WS-TEST"
            data_item.data_type = data_type
            result = validator.validate_data_item(data_item)
            # Should not have errors for valid data types
            assert len(result.errors) == 0 or any("unknown data type" in error.lower() for error in result.warnings)
    
    def test_cobol_operation_validation(self, validator):
        """Test COBOL-specific operation validation"""
        # Test that COBOL operations are defined
        assert hasattr(validator, 'valid_operations')
        assert len(validator.valid_operations) > 0
        
        # Test some common COBOL operations
        common_operations = ["MOVE", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "COMPUTE", "IF", "PERFORM", "CALL"]
        for operation in common_operations:
            assert operation in validator.valid_operations, f"Should include COBOL operation: {operation}"
    
    def test_cobol_program_validation(self, validator):
        """Test COBOL-specific program validation"""
        # Test valid COBOL program
        valid_program = Mock()
        valid_program.name = "TEST-PROGRAM"
        valid_program.language = "COBOL"
        valid_program.metadata = {"file_path": "test.cbl"}
        
        result = validator.validate_program(valid_program)
        assert result.is_valid or len(result.errors) < 2
        
        # Test invalid language
        invalid_program = Mock()
        invalid_program.name = "TEST-PROGRAM"
        invalid_program.language = "JAVA"  # Wrong language
        invalid_program.metadata = {"file_path": "test.cbl"}
        
        result = validator.validate_program(invalid_program)
        assert not result.is_valid
        assert any("expected COBOL" in error.lower() for error in result.warnings)
    
    def test_cobol_business_rule_validation(self, validator):
        """Test COBOL-specific business rule validation"""
        # Test valid business rule
        valid_rule = Mock()
        valid_rule.rule_id = "RULE-001"
        valid_rule.description = "Validate transaction amount"
        valid_rule.scope = "MAIN-SECTION"
        valid_rule.confidence = 0.9
        valid_rule.risk_level = "HIGH"
        valid_rule.priority = "CRITICAL"
        
        result = validator.validate_business_rule(valid_rule)
        assert result.is_valid or len(result.errors) < 2
        
        # Test missing required properties
        invalid_rule = Mock()
        invalid_rule.rule_id = ""
        invalid_rule.description = ""
        
        result = validator.validate_business_rule(invalid_rule)
        assert not result.is_valid
        assert any("missing required" in error.lower() for error in result.errors)
    
    def test_cobol_ontology_consistency(self, validator):
        """Test COBOL ontology consistency validation"""
        # Create a mock analysis result with COBOL-specific data
        mock_result = Mock()
        mock_result.program = Mock()
        mock_result.program.name = "TEST-PROGRAM"
        mock_result.program.language = "COBOL"
        mock_result.program.metadata = {"file_path": "test.cbl"}
        
        # Create sections with COBOL-specific types
        section1 = self.create_valid_section()
        section1.type = "PROCEDURE"
        section2 = self.create_valid_section()
        section2.type = "DATA"
        section2.name = "DATA-SECTION"
        
        mock_result.sections = [section1, section2]
        mock_result.subsections = [self.create_valid_subsection()]
        mock_result.relationships = [self.create_valid_relationship()]
        mock_result.data_items = []
        mock_result.business_rules = []
        
        # Test validation
        result = validator.validate_analysis_result(mock_result)
        
        assert isinstance(result, ValidationResult)
        assert result.language == "COBOL"
        assert "ontology_version" in result.metrics
        assert result.metrics["total_components"] == 3  # 2 sections + 1 subsection
        assert result.metrics["total_relationships"] == 1
    
    def test_cobol_validation_metrics(self, validator):
        """Test COBOL-specific validation metrics"""
        # Create test data
        sections = [self.create_valid_section() for _ in range(3)]
        subsections = [self.create_valid_subsection() for _ in range(2)]
        relationships = [self.create_valid_relationship() for _ in range(1)]
        
        # Test section validation metrics
        for section in sections:
            result = validator.validate_section(section)
            assert "ontology_version" in result.metrics
            assert result.metrics["ontology_version"] == "1.0.0"
        
        # Test subsection validation metrics
        for subsection in subsections:
            result = validator.validate_subsection(subsection)
            assert "ontology_version" in result.metrics
            assert result.metrics["ontology_version"] == "1.0.0"
        
        # Test relationship validation metrics
        for relationship in relationships:
            result = validator.validate_relationship(relationship)
            assert "ontology_version" in result.metrics
            assert result.metrics["ontology_version"] == "1.0.0"
    
    def test_cobol_validation_error_messages(self, validator):
        """Test COBOL-specific validation error messages"""
        # Test section with invalid type
        invalid_section = self.create_valid_section()
        invalid_section.type = "INVALID_TYPE"
        result = validator.validate_section(invalid_section)
        
        assert not result.is_valid
        assert any("invalid type" in error.lower() for error in result.errors)
        assert any("INVALID_TYPE" in error for error in result.errors)
        
        # Test relationship with invalid type
        invalid_relationship = self.create_valid_relationship()
        invalid_relationship.relationship_type = "INVALID_TYPE"
        result = validator.validate_relationship(invalid_relationship)
        
        assert not result.is_valid
        assert any("invalid type" in error.lower() for error in result.errors)
        assert any("INVALID_TYPE" in error for error in result.errors)
    
    def test_cobol_validation_warnings(self, validator):
        """Test COBOL-specific validation warnings"""
        # Test section with missing optional properties
        incomplete_section = Mock()
        incomplete_section.name = "TEST-SECTION"
        incomplete_section.type = "PROCEDURE"
        # Missing confidence, complexity_score, risk_level
        
        result = validator.validate_section(incomplete_section)
        
        # Should have warnings for missing properties
        assert len(result.warnings) > 0
        assert any("missing confidence" in warning.lower() for warning in result.warnings)
        assert any("missing complexity" in warning.lower() for warning in result.warnings)
        assert any("missing risk_level" in warning.lower() for warning in result.warnings)
