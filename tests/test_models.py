"""
Unit tests for data models
Tests COBOLSection, COBOLSubsection, and related data structures
"""

import pytest
from dataclasses import asdict
from src.models import COBOLSection, COBOLSubsection, COBOLAnalysisResult, Relationship


class TestCOBOLSection:
    """Test cases for COBOLSection data model"""
    
    def test_cobol_section_creation(self):
        """Test creating a COBOLSection instance"""
        section = COBOLSection(
            name="TEST-SECTION",
            type="PROCEDURE_SECTION",
            line_range=(1, 10),
            line_count=10,
            business_logic="Test section for fraud detection",
            confidence=0.95
        )
        
        assert section.name == "TEST-SECTION"
        assert section.type == "PROCEDURE_SECTION"
        assert section.line_range == (1, 10)
        assert section.line_count == 10
        assert section.business_logic == "Test section for fraud detection"
        assert section.confidence == 0.95

    def test_cobol_section_validation(self):
        """Test COBOLSection validation rules"""
        # Valid section
        valid_section = COBOLSection(
            name="VALID-SECTION",
            type="PROCEDURE_SECTION",
            line_range=(1, 10),
            line_count=10,
            business_logic="Valid section",
            confidence=0.8
        )
        assert valid_section.line_count > 0
        assert 0.0 <= valid_section.confidence <= 1.0
        
        # Test line range validation
        assert valid_section.line_range[0] <= valid_section.line_range[1]
        assert valid_section.line_range[1] - valid_section.line_range[0] + 1 == valid_section.line_count

    def test_cobol_section_serialization(self):
        """Test that COBOLSection can be serialized to dict"""
        section = COBOLSection(
            name="SERIALIZATION-TEST",
            type="DATA_SECTION",
            line_range=(5, 15),
            line_count=11,
            business_logic="Serialization test section",
            confidence=0.85
        )
        
        section_dict = asdict(section)
        
        assert section_dict["name"] == "SERIALIZATION-TEST"
        assert section_dict["type"] == "DATA_SECTION"
        assert section_dict["line_range"] == (5, 15)
        assert section_dict["line_count"] == 11
        assert section_dict["business_logic"] == "Serialization test section"
        assert section_dict["confidence"] == 0.85

    def test_cobol_section_types(self):
        """Test different COBOL section types"""
        section_types = [
            "IDENTIFICATION",
            "ENVIRONMENT", 
            "DATA",
            "FILE",
            "WORKING-STORAGE",
            "PROCEDURE",
            "PROCEDURE_SECTION"
        ]
        
        for section_type in section_types:
            section = COBOLSection(
                name=f"TEST-{section_type}",
                type=section_type,
                line_range=(1, 5),
                line_count=5,
                business_logic=f"Test {section_type} section",
                confidence=0.9
            )
            assert section.type == section_type

    def test_cobol_section_edge_cases(self):
        """Test COBOLSection with edge case values"""
        # Single line section
        single_line = COBOLSection(
            name="SINGLE-LINE",
            type="PROCEDURE_SECTION",
            line_range=(10, 10),
            line_count=1,
            business_logic="Single line section",
            confidence=1.0
        )
        assert single_line.line_count == 1
        assert single_line.line_range == (10, 10)
        
        # Zero confidence section
        zero_confidence = COBOLSection(
            name="UNCERTAIN-SECTION",
            type="PROCEDURE_SECTION",
            line_range=(1, 5),
            line_count=5,
            business_logic="Uncertain analysis",
            confidence=0.0
        )
        assert zero_confidence.confidence == 0.0


class TestCOBOLSubsection:
    """Test cases for COBOLSubsection data model"""
    
    def test_cobol_subsection_creation(self):
        """Test creating a COBOLSubsection instance"""
        subsection = COBOLSubsection(
            name="TEST-SUBSECTION",
            parent_section="MAIN-CONTROL-SECTION",
            line_range=(5, 8),
            line_count=4,
            business_logic="Test subsection for processing",
            confidence=0.88
        )
        
        assert subsection.name == "TEST-SUBSECTION"
        assert subsection.parent_section == "MAIN-CONTROL-SECTION"
        assert subsection.line_range == (5, 8)
        assert subsection.line_count == 4
        assert subsection.business_logic == "Test subsection for processing"
        assert subsection.confidence == 0.88

    def test_cobol_subsection_parent_relationship(self):
        """Test that subsection correctly references parent section"""
        parent_section = COBOLSection(
            name="PARENT-SECTION",
            type="PROCEDURE_SECTION",
            line_range=(1, 20),
            line_count=20,
            business_logic="Parent section",
            confidence=0.9
        )
        
        subsection = COBOLSubsection(
            name="CHILD-SUBSECTION",
            parent_section=parent_section.name,
            line_range=(5, 10),
            line_count=6,
            business_logic="Child subsection",
            confidence=0.85
        )
        
        assert subsection.parent_section == parent_section.name
        # Subsection should be within parent's line range
        assert subsection.line_range[0] >= parent_section.line_range[0]
        assert subsection.line_range[1] <= parent_section.line_range[1]

    def test_cobol_subsection_serialization(self):
        """Test that COBOLSubsection can be serialized to dict"""
        subsection = COBOLSubsection(
            name="SERIALIZATION-SUBSECTION",
            parent_section="TEST-PARENT",
            line_range=(10, 15),
            line_count=6,
            business_logic="Serialization test subsection",
            confidence=0.75
        )
        
        subsection_dict = asdict(subsection)
        
        assert subsection_dict["name"] == "SERIALIZATION-SUBSECTION"
        assert subsection_dict["parent_section"] == "TEST-PARENT"
        assert subsection_dict["line_range"] == (10, 15)
        assert subsection_dict["line_count"] == 6
        assert subsection_dict["business_logic"] == "Serialization test subsection"
        assert subsection_dict["confidence"] == 0.75


class TestRelationship:
    """Test cases for Relationship data model"""
    
    def test_relationship_creation(self):
        """Test creating a Relationship instance"""
        relationship = Relationship(
            source="MAIN-CONTROL-SECTION",
            target="INITIALIZE-PROGRAM-SECTION",
            relationship_type="CALLS",
            confidence=0.92
        )
        
        assert relationship.source == "MAIN-CONTROL-SECTION"
        assert relationship.target == "INITIALIZE-PROGRAM-SECTION"
        assert relationship.relationship_type == "CALLS"
        assert relationship.confidence == 0.92

    def test_relationship_types(self):
        """Test different relationship types"""
        relationship_types = [
            "CALLS",
            "DATA_FLOW",
            "CONTROL_FLOW",
            "DEPENDS_ON",
            "USES",
            "MODIFIES"
        ]
        
        for rel_type in relationship_types:
            relationship = Relationship(
                source="SOURCE-SECTION",
                target="TARGET-SECTION",
                relationship_type=rel_type,
                confidence=0.8
            )
            assert relationship.relationship_type == rel_type

    def test_relationship_serialization(self):
        """Test that Relationship can be serialized to dict"""
        relationship = Relationship(
            source="SOURCE-SECTION",
            target="TARGET-SECTION",
            relationship_type="DATA_FLOW",
            confidence=0.85
        )
        
        rel_dict = asdict(relationship)
        
        assert rel_dict["source"] == "SOURCE-SECTION"
        assert rel_dict["target"] == "TARGET-SECTION"
        assert rel_dict["relationship_type"] == "DATA_FLOW"
        assert rel_dict["confidence"] == 0.85


class TestCOBOLAnalysisResult:
    """Test cases for COBOLAnalysisResult data model"""
    
    def test_analysis_result_creation(self):
        """Test creating a COBOLAnalysisResult instance"""
        sections = [
            COBOLSection(
                name="TEST-SECTION-1",
                type="PROCEDURE_SECTION",
                line_range=(1, 10),
                line_count=10,
                business_logic="Test section 1",
                confidence=0.9
            ),
            COBOLSection(
                name="TEST-SECTION-2",
                type="DATA_SECTION",
                line_range=(11, 20),
                line_count=10,
                business_logic="Test section 2",
                confidence=0.85
            )
        ]
        
        subsections = [
            COBOLSubsection(
                name="TEST-SUBSECTION-1",
                parent_section="TEST-SECTION-1",
                line_range=(2, 5),
                line_count=4,
                business_logic="Test subsection 1",
                confidence=0.88
            )
        ]
        
        relationships = [
            Relationship(
                source="TEST-SECTION-1",
                target="TEST-SECTION-2",
                relationship_type="CALLS",
                confidence=0.92
            )
        ]
        
        result = COBOLAnalysisResult(
            program_name="TEST-PROGRAM",
            total_sections=2,
            total_subsections=1,
            sections=sections,
            subsections=subsections,
            relationships=relationships,
            processing_time=1.5,
            confidence_threshold=0.7
        )
        
        assert result.program_name == "TEST-PROGRAM"
        assert result.total_sections == 2
        assert result.total_subsections == 1
        assert len(result.sections) == 2
        assert len(result.subsections) == 1
        assert len(result.relationships) == 1
        assert result.processing_time == 1.5
        assert result.confidence_threshold == 0.7

    def test_analysis_result_serialization(self):
        """Test that COBOLAnalysisResult can be serialized to dict"""
        result = COBOLAnalysisResult(
            program_name="SERIALIZATION-TEST",
            total_sections=1,
            total_subsections=0,
            sections=[],
            subsections=[],
            relationships=[],
            processing_time=0.5,
            confidence_threshold=0.8
        )
        
        result_dict = asdict(result)
        
        assert result_dict["program_name"] == "SERIALIZATION-TEST"
        assert result_dict["total_sections"] == 1
        assert result_dict["total_subsections"] == 0
        assert result_dict["sections"] == []
        assert result_dict["subsections"] == []
        assert result_dict["relationships"] == []
        assert result_dict["processing_time"] == 0.5
        assert result_dict["confidence_threshold"] == 0.8

    def test_analysis_result_statistics(self):
        """Test that analysis result calculates statistics correctly"""
        sections = [
            COBOLSection("SECTION-1", "PROCEDURE", (1, 10), 10, "Logic 1", 0.9),
            COBOLSection("SECTION-2", "DATA", (11, 20), 10, "Logic 2", 0.8),
            COBOLSection("SECTION-3", "PROCEDURE", (21, 30), 10, "Logic 3", 0.7)
        ]
        
        result = COBOLAnalysisResult(
            program_name="STATS-TEST",
            total_sections=3,
            total_subsections=0,
            sections=sections,
            subsections=[],
            relationships=[],
            processing_time=1.0,
            confidence_threshold=0.75
        )
        
        # Test confidence statistics
        high_confidence_sections = [s for s in result.sections if s.confidence >= 0.8]
        assert len(high_confidence_sections) == 2
        
        # Test section type distribution
        procedure_sections = [s for s in result.sections if s.type == "PROCEDURE"]
        data_sections = [s for s in result.sections if s.type == "DATA"]
        assert len(procedure_sections) == 2
        assert len(data_sections) == 1

    def test_analysis_result_validation(self):
        """Test that analysis result validates data consistency"""
        sections = [
            COBOLSection("SECTION-1", "PROCEDURE", (1, 10), 10, "Logic 1", 0.9)
        ]
        
        subsections = [
            COBOLSubsection("SUBSECTION-1", "SECTION-1", (2, 5), 4, "Sub logic 1", 0.8)
        ]
        
        result = COBOLAnalysisResult(
            program_name="VALIDATION-TEST",
            total_sections=1,
            total_subsections=1,
            sections=sections,
            subsections=subsections,
            relationships=[],
            processing_time=1.0,
            confidence_threshold=0.7
        )
        
        # Test that counts match actual data
        assert result.total_sections == len(result.sections)
        assert result.total_subsections == len(result.subsections)
        
        # Test that subsection parent exists
        subsection_parents = [s.name for s in result.sections]
        for subsection in result.subsections:
            assert subsection.parent_section in subsection_parents

    def test_analysis_result_edge_cases(self):
        """Test analysis result with edge cases"""
        # Empty result
        empty_result = COBOLAnalysisResult(
            program_name="EMPTY-PROGRAM",
            total_sections=0,
            total_subsections=0,
            sections=[],
            subsections=[],
            relationships=[],
            processing_time=0.0,
            confidence_threshold=0.5
        )
        
        assert empty_result.total_sections == 0
        assert empty_result.total_subsections == 0
        assert len(empty_result.sections) == 0
        assert len(empty_result.subsections) == 0
        assert len(empty_result.relationships) == 0
        
        # Very large result
        large_sections = [
            COBOLSection(f"SECTION-{i}", "PROCEDURE", (i*10+1, (i+1)*10), 10, f"Logic {i}", 0.8)
            for i in range(100)
        ]
        
        large_result = COBOLAnalysisResult(
            program_name="LARGE-PROGRAM",
            total_sections=100,
            total_subsections=0,
            sections=large_sections,
            subsections=[],
            relationships=[],
            processing_time=10.0,
            confidence_threshold=0.7
        )
        
        assert large_result.total_sections == 100
        assert len(large_result.sections) == 100
