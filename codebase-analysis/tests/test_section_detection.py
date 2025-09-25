"""
Comprehensive tests for COBOL section detection functionality
Tests the _find_sections method and related section detection logic
"""

import pytest
from unittest.mock import patch, MagicMock
from pathlib import Path
from lang.cobol.parser.cobol_parser import COBOLParser, COBOLSection
from lang.base.ontology.base_models import RiskLevel


class TestCOBOLSectionDetection:
    """Test cases for COBOL section detection functionality"""
    
    @pytest.fixture
    def parser(self):
        """Create a COBOL parser instance for testing"""
        return COBOLParser()
    
    def test_find_sections_basic_program(self, parser):
        """Test section detection with a basic COBOL program"""
        lines = [
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST-PROG.",
            "AUTHOR. TEST-AUTHOR.",
            "",
            "ENVIRONMENT DIVISION.",
            "CONFIGURATION SECTION.",
            "SOURCE-COMPUTER. IBM-370.",
            "",
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        # Should find 4 sections
        assert len(sections) == 4
        
        # Check section types
        section_types = [section.type for section in sections]
        assert "IDENTIFICATION" in section_types
        assert "ENVIRONMENT" in section_types
        assert "DATA" in section_types
        assert "PROCEDURE" in section_types
        
        # Check section names
        section_names = [section.name for section in sections]
        assert "IDENTIFICATION-DIVISION" in section_names
        assert "ENVIRONMENT-DIVISION" in section_names
        assert "DATA-DIVISION" in section_names
        assert "PROCEDURE-DIVISION" in section_names
    
    def test_find_sections_with_whitespace(self, parser):
        """Test section detection with various whitespace patterns"""
        lines = [
            "   IDENTIFICATION   DIVISION   .",
            "PROGRAM-ID. TEST-PROG.",
            "",
            "    ENVIRONMENT    DIVISION    .",
            "CONFIGURATION SECTION.",
            "",
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "01 WS-DATA PIC X(10).",
            "",
            "PROCEDURE DIVISION.",
            "MAIN.",
            "    DISPLAY 'Test'.",
            "STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        # Should find 4 sections despite whitespace
        assert len(sections) == 4
        
        # Check that sections are properly detected
        section_types = [section.type for section in sections]
        assert "IDENTIFICATION" in section_types
        assert "ENVIRONMENT" in section_types
        assert "DATA" in section_types
        assert "PROCEDURE" in section_types
    
    def test_find_sections_case_insensitive(self, parser):
        """Test section detection is case insensitive"""
        lines = [
            "identification division.",
            "program-id. test-prog.",
            "",
            "environment division.",
            "configuration section.",
            "",
            "data division.",
            "working-storage section.",
            "01 ws-counter pic 9(3).",
            "",
            "procedure division.",
            "main-paragraph.",
            "    display 'Hello World'.",
            "    stop run."
        ]
        
        sections = parser._find_sections(lines)
        
        # Should find 4 sections despite lowercase
        assert len(sections) == 4
        
        # Check section types (should be uppercase)
        section_types = [section.type for section in sections]
        assert "IDENTIFICATION" in section_types
        assert "ENVIRONMENT" in section_types
        assert "DATA" in section_types
        assert "PROCEDURE" in section_types
    
    def test_find_sections_missing_divisions(self, parser):
        """Test section detection with missing divisions"""
        lines = [
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST-PROG.",
            "",
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        # Should find 3 sections (missing ENVIRONMENT)
        assert len(sections) == 3
        
        section_types = [section.type for section in sections]
        assert "IDENTIFICATION" in section_types
        assert "DATA" in section_types
        assert "PROCEDURE" in section_types
        assert "ENVIRONMENT" not in section_types
    
    def test_find_sections_duplicate_divisions(self, parser):
        """Test section detection with duplicate divisions"""
        lines = [
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST-PROG.",
            "",
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "DATA DIVISION.",  # Duplicate
            "LINKAGE SECTION.",
            "01 LS-DATA PIC X(10).",
            "",
            "PROCEDURE DIVISION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        # Should find 4 sections (including duplicate DATA)
        assert len(sections) == 4
        
        # Check that both DATA divisions are detected
        data_sections = [s for s in sections if s.type == "DATA"]
        assert len(data_sections) == 2
    
    def test_find_sections_empty_file(self, parser):
        """Test section detection with empty file"""
        lines = []
        
        sections = parser._find_sections(lines)
        
        # Should find no sections
        assert len(sections) == 0
    
    def test_find_sections_no_divisions(self, parser):
        """Test section detection with no divisions"""
        lines = [
            "PROGRAM-ID. TEST-PROG.",
            "AUTHOR. TEST-AUTHOR.",
            "01 WS-COUNTER PIC 9(3).",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        # Should find no sections
        assert len(sections) == 0
    
    def test_find_sections_line_ranges(self, parser):
        """Test that section line ranges are calculated correctly"""
        lines = [
            "IDENTIFICATION DIVISION.",  # Line 0
            "PROGRAM-ID. TEST-PROG.",    # Line 1
            "AUTHOR. TEST-AUTHOR.",      # Line 2
            "",                          # Line 3
            "ENVIRONMENT DIVISION.",     # Line 4
            "CONFIGURATION SECTION.",    # Line 5
            "SOURCE-COMPUTER. IBM-370.", # Line 6
            "",                          # Line 7
            "DATA DIVISION.",            # Line 8
            "WORKING-STORAGE SECTION.",  # Line 9
            "01 WS-COUNTER PIC 9(3).",   # Line 10
            "",                          # Line 11
            "PROCEDURE DIVISION.",       # Line 12
            "MAIN-PARAGRAPH.",           # Line 13
            "    DISPLAY 'Hello World'.", # Line 14
            "    STOP RUN."              # Line 15
        ]
        
        sections = parser._find_sections(lines)
        
        # Check line ranges for each section
        for section in sections:
            if section.type == "IDENTIFICATION":
                assert section.line_range[0] == 1  # 1-based line number
                assert section.line_range[1] == 4  # End before ENVIRONMENT
            elif section.type == "ENVIRONMENT":
                assert section.line_range[0] == 5  # 1-based line number
                assert section.line_range[1] == 8  # End before DATA
            elif section.type == "DATA":
                assert section.line_range[0] == 9  # 1-based line number
                assert section.line_range[1] == 12  # End before PROCEDURE
            elif section.type == "PROCEDURE":
                assert section.line_range[0] == 13  # 1-based line number
                assert section.line_range[1] == 16  # End of file
    
    def test_find_sections_business_logic_extraction(self, parser):
        """Test that business logic is extracted for each section"""
        lines = [
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST-PROG.",
            "AUTHOR. TEST-AUTHOR.",
            "DATE-WRITTEN. 2024-01-01.",
            "",
            "ENVIRONMENT DIVISION.",
            "CONFIGURATION SECTION.",
            "SOURCE-COMPUTER. IBM-370.",
            "OBJECT-COMPUTER. IBM-370.",
            "",
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "01 WS-TOTAL PIC 9(5).",
            "",
            "PROCEDURE DIVISION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    ADD 1 TO WS-COUNTER.",
            "    STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        # Check that business logic is extracted
        for section in sections:
            assert section.business_logic is not None
            assert isinstance(section.business_logic, str)
            assert len(section.business_logic) > 0
    
    def test_find_sections_complexity_calculation(self, parser):
        """Test that complexity scores are calculated for each section"""
        lines = [
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST-PROG.",
            "",
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        # Check that complexity scores are calculated
        for section in sections:
            assert section.complexity_score is not None
            assert isinstance(section.complexity_score, (int, float))
            assert 0 <= section.complexity_score <= 10  # Assuming 0-10 scale
    
    def test_find_sections_risk_assessment(self, parser):
        """Test that risk levels are assessed for each section"""
        lines = [
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST-PROG.",
            "",
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        # Check that risk levels are assessed
        for section in sections:
            assert section.risk_level is not None
            # Risk level is returned as string from base parser
            assert isinstance(section.risk_level, str)
            assert section.risk_level in ['LOW', 'MEDIUM', 'HIGH', 'CRITICAL']
    
    def test_find_sections_confidence_scores(self, parser):
        """Test that confidence scores are set for each section"""
        lines = [
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST-PROG.",
            "",
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        # Check that confidence scores are set
        for section in sections:
            assert section.confidence is not None
            assert isinstance(section.confidence, (int, float))
            assert 0 <= section.confidence <= 1
    
    def test_find_sections_with_comments(self, parser):
        """Test section detection with comments and blank lines"""
        lines = [
            "* This is a comment",
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST-PROG.",
            "* Another comment",
            "",
            "ENVIRONMENT DIVISION.",
            "CONFIGURATION SECTION.",
            "",
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        # Should find 4 sections despite comments
        assert len(sections) == 4
        
        section_types = [section.type for section in sections]
        assert "IDENTIFICATION" in section_types
        assert "ENVIRONMENT" in section_types
        assert "DATA" in section_types
        assert "PROCEDURE" in section_types
    
    def test_find_sections_malformed_divisions(self, parser):
        """Test section detection with malformed division statements"""
        lines = [
            "IDENTIFICATION DIVISION",  # Missing period
            "PROGRAM-ID. TEST-PROG.",
            "",
            "ENVIRONMENT DIVISION.",    # Correct
            "CONFIGURATION SECTION.",
            "",
            "DATA DIVISION",            # Missing period
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",      # Correct
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        # The current parser implementation finds all sections regardless of malformed syntax
        # This is because the regex patterns are more flexible
        assert len(sections) == 4
        
        section_types = [section.type for section in sections]
        assert "IDENTIFICATION" in section_types
        assert "ENVIRONMENT" in section_types
        assert "DATA" in section_types
        assert "PROCEDURE" in section_types
    
    def test_find_sections_nested_structure(self, parser):
        """Test section detection with nested structure"""
        lines = [
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST-PROG.",
            "",
            "ENVIRONMENT DIVISION.",
            "CONFIGURATION SECTION.",
            "SOURCE-COMPUTER. IBM-370.",
            "INPUT-OUTPUT SECTION.",
            "FILE-CONTROL.",
            "    SELECT INPUT-FILE ASSIGN TO 'INPUT'.",
            "",
            "DATA DIVISION.",
            "FILE SECTION.",
            "FD INPUT-FILE.",
            "01 INPUT-RECORD PIC X(80).",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        # Should find 4 sections
        assert len(sections) == 4
        
        section_types = [section.type for section in sections]
        assert "IDENTIFICATION" in section_types
        assert "ENVIRONMENT" in section_types
        assert "DATA" in section_types
        assert "PROCEDURE" in section_types
    
    @patch.object(COBOLParser, '_find_section_end')
    def test_find_sections_section_end_detection(self, mock_find_end, parser):
        """Test section detection with mocked section end detection"""
        # Mock the _find_section_end method to return specific end lines
        mock_find_end.side_effect = [3, 7, 11, 15]  # End lines for each section
        
        lines = [
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST-PROG.",
            "AUTHOR. TEST-AUTHOR.",
            "",
            "ENVIRONMENT DIVISION.",
            "CONFIGURATION SECTION.",
            "SOURCE-COMPUTER. IBM-370.",
            "",
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        # Verify _find_section_end was called for each section
        assert mock_find_end.call_count == 4
        
        # Check that sections were created with correct line ranges
        expected_ends = [3, 7, 11, 15]  # Convert side_effect to list
        for i, section in enumerate(sections):
            expected_start = i * 4 + 1  # 1, 5, 9, 13
            expected_end = expected_ends[i] + 1  # 1-based
            assert section.line_range[0] == expected_start
            assert section.line_range[1] == expected_end
    
    def test_find_sections_performance_large_file(self, parser):
        """Test section detection performance with large file"""
        # Create a large COBOL file
        lines = ["IDENTIFICATION DIVISION.", "PROGRAM-ID. TEST-PROG."]
        
        # Add many lines to DATA DIVISION
        lines.extend([
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION."
        ])
        
        for i in range(1000):
            lines.append(f"01 WS-FIELD-{i:03d} PIC 9(3).")
        
        lines.extend([
            "",
            "PROCEDURE DIVISION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ])
        
        sections = parser._find_sections(lines)
        
        # Should find 3 sections
        assert len(sections) == 3
        
        section_types = [section.type for section in sections]
        assert "IDENTIFICATION" in section_types
        assert "DATA" in section_types
        assert "PROCEDURE" in section_types
    
    def test_find_sections_edge_cases(self, parser):
        """Test section detection with edge cases"""
        # Test with only division headers
        lines = [
            "IDENTIFICATION DIVISION.",
            "ENVIRONMENT DIVISION.",
            "DATA DIVISION.",
            "PROCEDURE DIVISION."
        ]
        
        sections = parser._find_sections(lines)
        
        # Should find 4 sections
        assert len(sections) == 4
        
        # Test with division at end of file
        lines = [
            "PROGRAM-ID. TEST-PROG.",
            "IDENTIFICATION DIVISION."
        ]
        
        sections = parser._find_sections(lines)
        
        # Should find 1 section
        assert len(sections) == 1
        assert sections[0].type == "IDENTIFICATION"
    
    def test_find_sections_section_attributes(self, parser):
        """Test that all section attributes are properly set"""
        lines = [
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST-PROG.",
            "",
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        sections = parser._find_sections(lines)
        
        for section in sections:
            # Check required attributes
            assert hasattr(section, 'name')
            assert hasattr(section, 'type')
            assert hasattr(section, 'line_range')
            assert hasattr(section, 'business_logic')
            assert hasattr(section, 'confidence')
            assert hasattr(section, 'complexity_score')
            assert hasattr(section, 'risk_level')
            
            # Check attribute types
            assert isinstance(section.name, str)
            assert isinstance(section.type, str)
            assert isinstance(section.line_range, tuple)
            assert len(section.line_range) == 2
            assert isinstance(section.business_logic, str)
            assert isinstance(section.confidence, (int, float))
            assert isinstance(section.complexity_score, (int, float))
            # Risk level is returned as string from base parser
            assert isinstance(section.risk_level, str)
            assert section.risk_level in ['LOW', 'MEDIUM', 'HIGH', 'CRITICAL']
            
            # Check attribute values
            assert len(section.name) > 0
            assert len(section.type) > 0
            assert section.line_range[0] > 0
            assert section.line_range[1] > section.line_range[0]
            assert 0 <= section.confidence <= 1
            assert 0 <= section.complexity_score <= 10
