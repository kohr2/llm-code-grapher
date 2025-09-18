"""
Integration tests for COBOL Code Grapher
Tests end-to-end functionality with the fraud management COBOL file
"""

import pytest
import json
from pathlib import Path
from src.cobol_parser import COBOLParser
from src.llm_analyzer import LLMAnalyzer
from src.output_generator import OutputGenerator
from src.models import COBOLAnalysisResult
from ontology.cobol.cobol_ontology_validator import COBOLOntologyValidator


class TestCOBOLIntegration:
    """Integration tests for the complete COBOL analysis pipeline"""
    
    @pytest.fixture
    def fraud_cobol_file(self):
        """Load the fraud management COBOL file"""
        file_path = Path("data/fixtures/vasu_fraud_management_cobol_reformatted.cbl")
        return file_path.read_text(encoding='utf-8')
    
    @pytest.fixture
    def parser(self):
        """Create COBOL parser instance"""
        return COBOLParser()
    
    @pytest.fixture
    def llm_analyzer(self):
        """Create LLM analyzer instance (mock for testing)"""
        return LLMAnalyzer()
    
    @pytest.fixture
    def output_generator(self):
        """Create output generator instance"""
        return OutputGenerator()

    def test_fraud_file_structure_analysis(self, parser, fraud_cobol_file):
        """Test that the fraud file structure is correctly analyzed"""
        result = parser.parse(fraud_cobol_file)
        
        # Should identify main divisions
        assert len(result.sections) >= 4
        
        # Check for expected main divisions
        section_names = [s.name for s in result.sections]
        expected_divisions = [
            "IDENTIFICATION DIVISION",
            "ENVIRONMENT DIVISION", 
            "DATA DIVISION",
            "PROCEDURE DIVISION"
        ]
        
        for division in expected_divisions:
            assert division in section_names, f"Missing division: {division}"
        
        # Check for specific procedure sections
        procedure_sections = [s for s in result.sections if s.type == "PROCEDURE_SECTION"]
        assert len(procedure_sections) > 0
        
        # Should have main control section
        main_section = next((s for s in procedure_sections if "MAIN-CONTROL" in s.name), None)
        assert main_section is not None, "Main control section not found"
        
        # Should have initialization section
        init_section = next((s for s in procedure_sections if "INITIALIZE-PROGRAM" in s.name), None)
        assert init_section is not None, "Initialize program section not found"
        
        # Should have transaction processing section
        process_section = next((s for s in procedure_sections if "PROCESS-TRANSACTIONS" in s.name), None)
        assert process_section is not None, "Process transactions section not found"

    def test_fraud_file_subsections_analysis(self, parser, fraud_cobol_file):
        """Test that subsections are correctly identified in the fraud file"""
        result = parser.parse(fraud_cobol_file)
        
        # Should have many subsections
        assert len(result.subsections) > 20, f"Expected >20 subsections, got {len(result.subsections)}"
        
        # Check for specific subsections
        subsection_names = [s.name for s in result.subsections]
        
        # Main process subsection
        assert "0000-MAIN-PROCESS" in subsection_names, "Main process subsection not found"
        
        # Initialization subsections
        init_subsections = [s for s in subsection_names if "1000-" in s]
        assert len(init_subsections) > 0, "No initialization subsections found"
        
        # Transaction processing subsections
        process_subsections = [s for s in subsection_names if "2000-" in s or "2100-" in s or "2200-" in s]
        assert len(process_subsections) > 0, "No transaction processing subsections found"
        
        # Fraud detection subsections
        fraud_subsections = [s for s in subsection_names if "2600-" in s or "2610-" in s]
        assert len(fraud_subsections) > 0, "No fraud detection subsections found"

    def test_fraud_file_business_logic_analysis(self, parser, fraud_cobol_file):
        """Test that business logic is correctly extracted from fraud file"""
        result = parser.parse(fraud_cobol_file)
        
        # All sections should have business logic (even if placeholder)
        for section in result.sections:
            assert section.business_logic is not None
            assert len(section.business_logic) > 0
            assert 0.0 <= section.confidence <= 1.0
        
        # All subsections should have business logic
        for subsection in result.subsections:
            assert subsection.business_logic is not None
            assert len(subsection.business_logic) > 0
            assert 0.0 <= subsection.confidence <= 1.0

    def test_fraud_file_line_range_accuracy(self, parser, fraud_cobol_file):
        """Test that line ranges are accurately calculated"""
        result = parser.parse(fraud_cobol_file)
        
        # All sections should have valid line ranges
        for section in result.sections:
            assert section.line_range[0] > 0, f"Invalid start line for {section.name}"
            assert section.line_range[1] >= section.line_range[0], f"Invalid end line for {section.name}"
            assert section.line_count > 0, f"Invalid line count for {section.name}"
            
            # Line count should match range
            expected_count = section.line_range[1] - section.line_range[0] + 1
            assert section.line_count == expected_count, f"Line count mismatch for {section.name}"
        
        # All subsections should have valid line ranges
        for subsection in result.subsections:
            assert subsection.line_range[0] > 0, f"Invalid start line for {subsection.name}"
            assert subsection.line_range[1] >= subsection.line_range[0], f"Invalid end line for {subsection.name}"
            assert subsection.line_count > 0, f"Invalid line count for {subsection.name}"

    def test_fraud_file_section_hierarchy(self, parser, fraud_cobol_file):
        """Test that section hierarchy is correctly established"""
        result = parser.parse(fraud_cobol_file)
        
        # Find main control section
        main_section = next((s for s in result.sections if "MAIN-CONTROL" in s.name), None)
        assert main_section is not None
        
        # Find subsections that belong to main control
        main_subsections = [s for s in result.subsections if s.parent_section == main_section.name]
        assert len(main_subsections) > 0, "No subsections found for main control section"
        
        # Check that subsections are within parent's line range
        for subsection in main_subsections:
            assert subsection.line_range[0] >= main_section.line_range[0]
            assert subsection.line_range[1] <= main_section.line_range[1]

    def test_fraud_file_confidence_scoring(self, parser, fraud_cobol_file):
        """Test that confidence scoring works correctly"""
        result = parser.parse(fraud_cobol_file)
        
        # All sections should have confidence scores
        for section in result.sections:
            assert 0.0 <= section.confidence <= 1.0, f"Invalid confidence for {section.name}"
        
        # All subsections should have confidence scores
        for subsection in result.subsections:
            assert 0.0 <= subsection.confidence <= 1.0, f"Invalid confidence for {subsection.name}"
        
        # High-confidence sections should exist
        high_confidence_sections = [s for s in result.sections if s.confidence >= 0.8]
        assert len(high_confidence_sections) > 0, "No high-confidence sections found"

    def test_fraud_file_performance(self, parser, fraud_cobol_file):
        """Test that fraud file processing completes within reasonable time"""
        import time
        
        start_time = time.time()
        result = parser.parse(fraud_cobol_file)
        end_time = time.time()
        
        processing_time = end_time - start_time
        
        # Should complete within 10 seconds
        assert processing_time < 10.0, f"Processing took too long: {processing_time:.2f} seconds"
        
        # Should still produce results
        assert len(result.sections) > 0
        assert len(result.subsections) > 0

    def test_fraud_file_output_generation(self, parser, output_generator, fraud_cobol_file):
        """Test that output can be generated from fraud file analysis"""
        result = parser.parse(fraud_cobol_file)
        
        # Generate JSON output
        json_output = output_generator.generate_json(result)
        
        # Should be valid JSON
        parsed_json = json.loads(json_output)
        assert "program_name" in parsed_json
        assert "sections" in parsed_json
        assert "subsections" in parsed_json
        assert "relationships" in parsed_json
        
        # Should have expected data
        assert parsed_json["total_sections"] == len(result.sections)
        assert parsed_json["total_subsections"] == len(result.subsections)
        assert len(parsed_json["sections"]) == len(result.sections)
        assert len(parsed_json["subsections"]) == len(result.subsections)

    def test_fraud_file_validation_consistency(self, parser, fraud_cobol_file):
        """Test that analysis results are internally consistent"""
        result = parser.parse(fraud_cobol_file)
        
        # Check that counts match actual data
        assert result.total_sections == len(result.sections)
        assert result.total_subsections == len(result.subsections)
        
        # Check that subsection parents exist
        section_names = [s.name for s in result.sections]
        for subsection in result.subsections:
            assert subsection.parent_section in section_names, f"Parent section {subsection.parent_section} not found for subsection {subsection.name}"
        
        # Check that line ranges don't overlap inappropriately
        sections = sorted(result.sections, key=lambda s: s.line_range[0])
        for i in range(len(sections) - 1):
            current_end = sections[i].line_range[1]
            next_start = sections[i + 1].line_range[0]
            # Sections can be adjacent but shouldn't overlap
            assert current_end < next_start or current_end == next_start - 1, f"Overlapping sections: {sections[i].name} and {sections[i + 1].name}"

    def test_fraud_file_specific_patterns(self, parser, fraud_cobol_file):
        """Test that specific COBOL patterns in fraud file are correctly identified"""
        result = parser.parse(fraud_cobol_file)
        
        # Should identify file control sections
        file_sections = [s for s in result.sections if "FILE" in s.name or s.type == "FILE"]
        assert len(file_sections) > 0, "No file sections found"
        
        # Should identify working storage section
        ws_sections = [s for s in result.sections if "WORKING-STORAGE" in s.name]
        assert len(ws_sections) > 0, "No working storage section found"
        
        # Should identify procedure sections
        proc_sections = [s for s in result.sections if s.type == "PROCEDURE_SECTION"]
        assert len(proc_sections) > 0, "No procedure sections found"
        
        # Should identify fraud detection subsections
        fraud_subsections = [s for s in result.subsections if "RULE" in s.name or "FRAUD" in s.name]
        assert len(fraud_subsections) > 0, "No fraud detection subsections found"

    def test_fraud_file_error_handling(self, parser, fraud_cobol_file):
        """Test that error handling works correctly with fraud file"""
        # Test with corrupted data
        corrupted_data = fraud_cobol_file[:1000] + "INVALID COBOL CODE" + fraud_cobol_file[2000:]
        
        result = parser.parse(corrupted_data)
        
        # Should still process what it can
        assert len(result.sections) > 0, "Should still identify some sections"
        
        # Should handle errors gracefully
        for section in result.sections:
            assert section.business_logic is not None
            assert 0.0 <= section.confidence <= 1.0

    def test_fraud_file_completeness(self, parser, fraud_cobol_file):
        """Test that fraud file analysis is complete and comprehensive"""
        result = parser.parse(fraud_cobol_file)
        
        # Should have comprehensive coverage
        assert len(result.sections) >= 10, f"Expected >=10 sections, got {len(result.sections)}"
        assert len(result.subsections) >= 50, f"Expected >=50 subsections, got {len(result.subsections)}"
        
        # Should cover all major areas
        section_types = set(s.type for s in result.sections)
        expected_types = {"IDENTIFICATION", "ENVIRONMENT", "DATA", "PROCEDURE", "PROCEDURE_SECTION"}
        
        for expected_type in expected_types:
            assert expected_type in section_types, f"Missing section type: {expected_type}"
        
        # Should have reasonable confidence distribution
        high_confidence = len([s for s in result.sections if s.confidence >= 0.8])
        medium_confidence = len([s for s in result.sections if 0.5 <= s.confidence < 0.8])
        low_confidence = len([s for s in result.sections if s.confidence < 0.5])
        
        # Most sections should have reasonable confidence
        assert high_confidence + medium_confidence > low_confidence, "Too many low-confidence sections"

    def test_fraud_file_ontology_validation(self, parser, fraud_cobol_file):
        """Test that fraud file analysis produces valid ontology structure"""
        result = parser.parse(fraud_cobol_file)
        
        # Test ontology structure validation
        self._validate_ontology_structure(result)
        
        # Test that all required ontology properties are present
        for section in result.sections:
            assert hasattr(section, 'complexity_score')
            assert hasattr(section, 'risk_level')
            assert 0.0 <= section.complexity_score <= 1.0
            assert section.risk_level in ["LOW", "MEDIUM", "HIGH", "CRITICAL"]
        
        for subsection in result.subsections:
            assert hasattr(subsection, 'complexity_score')
            assert hasattr(subsection, 'risk_level')
            assert 0.0 <= subsection.complexity_score <= 1.0
            assert subsection.risk_level in ["LOW", "MEDIUM", "HIGH", "CRITICAL"]

    def _validate_ontology_structure(self, result):
        """Helper method to validate ontology structure consistency"""
        # Check that subsection parents exist
        section_names = [s.name for s in result.sections]
        for subsection in result.subsections:
            assert subsection.parent_section in section_names, f"Parent section {subsection.parent_section} not found for subsection {subsection.name}"
        
        # Check that relationships reference existing components
        all_names = section_names + [s.name for s in result.subsections]
        for relationship in result.relationships:
            assert relationship.source in all_names, f"Relationship source {relationship.source} not found"
            assert relationship.target in all_names, f"Relationship target {relationship.target} not found"
            assert relationship.relationship_type in ["CALLS", "USES", "MODIFIES", "DEPENDS_ON", "DATA_FLOW", "CONTROL_FLOW", "IMPLEMENTS", "CONTAINS"]
            assert 0.0 <= relationship.confidence <= 1.0
            assert 0.0 <= relationship.strength <= 1.0

    def test_fraud_file_ontology_metrics(self, parser, fraud_cobol_file):
        """Test that ontology metrics are calculated correctly"""
        result = parser.parse(fraud_cobol_file)
        
        # Test complexity score distribution
        complexity_scores = [s.complexity_score for s in result.sections + result.subsections]
        assert len(complexity_scores) > 0
        assert all(0.0 <= score <= 1.0 for score in complexity_scores)
        
        # Test risk level distribution
        risk_levels = [s.risk_level for s in result.sections + result.subsections]
        valid_risk_levels = ["LOW", "MEDIUM", "HIGH", "CRITICAL"]
        assert all(risk in valid_risk_levels for risk in risk_levels)
        
        # Test that we have a reasonable distribution of risk levels
        risk_counts = {risk: risk_levels.count(risk) for risk in valid_risk_levels}
        total_components = len(risk_levels)
        assert total_components > 0
        
        # Most components should be LOW or MEDIUM risk
        low_medium_count = risk_counts.get("LOW", 0) + risk_counts.get("MEDIUM", 0)
        assert low_medium_count > total_components * 0.5, "Too many HIGH/CRITICAL risk components"

    def test_fraud_file_ontology_validation_with_validator(self, parser, fraud_cobol_file):
        """Test that fraud file analysis passes ontology validation"""
        result = parser.parse(fraud_cobol_file)
        
        # Create ontology validator
        validator = COBOLOntologyValidator()
        
        # Validate the analysis result
        validation_result = validator.validate_analysis_result(result)
        
        # Should pass validation (or have minimal errors/warnings)
        assert validation_result.is_valid or len(validation_result.errors) < 5, f"Too many validation errors: {validation_result.errors}"
        
        # Should have meaningful metrics
        assert validation_result.metrics["total_sections"] > 0
        assert validation_result.metrics["total_subsections"] > 0
        assert validation_result.metrics["total_components"] > 0
        
        # Coverage should be reasonable
        assert validation_result.metrics["coverage_score"] > 0.5, f"Low coverage score: {validation_result.metrics['coverage_score']}"
