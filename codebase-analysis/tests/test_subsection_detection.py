"""
Comprehensive tests for COBOL subsection detection functionality
Tests the _find_subsections method and related subsection detection logic
"""

import pytest
from unittest.mock import patch, MagicMock
from pathlib import Path
from lang.cobol.parser.cobol_parser import COBOLParser, COBOLSection, COBOLSubsection
from lang.base.ontology.base_models import RiskLevel


class TestCOBOLSubsectionDetection:
    """Test cases for COBOL subsection detection functionality"""
    
    @pytest.fixture
    def parser(self):
        """Create a COBOL parser instance for testing"""
        return COBOLParser()
    
    @pytest.fixture
    def mock_sections(self):
        """Create mock sections for testing"""
        return [
            COBOLSection(
                name="DATA-DIVISION",
                type="DATA",
                line_range=(1, 7),  # DATA DIVISION ends before PROCEDURE DIVISION
                line_count=7,
                business_logic="Data definitions",
                confidence=0.9,
                complexity_score=2.0,
                risk_level=RiskLevel.LOW
            ),
            COBOLSection(
                name="PROCEDURE-DIVISION",
                type="PROCEDURE",
                line_range=(8, 20),  # PROCEDURE DIVISION starts after DATA DIVISION
                line_count=13,
                business_logic="Main program logic",
                confidence=0.9,
                complexity_score=5.0,
                risk_level=RiskLevel.MEDIUM
            )
        ]
    
    def test_find_subsections_basic_program(self, parser, mock_sections):
        """Test subsection detection with a basic COBOL program"""
        lines = [
            "DATA DIVISION.",
            "FILE SECTION.",
            "FD INPUT-FILE.",
            "01 INPUT-RECORD PIC X(80).",
            "",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "01 WS-TOTAL PIC 9(5).",
            "",
            "LINKAGE SECTION.",
            "01 LS-DATA PIC X(10).",
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Should find 4 subsections
        assert len(subsections) == 4
        
        # Check subsection names (type info is in the name)
        subsection_names = [subsection.name for subsection in subsections]
        assert "FILE-SECTION" in subsection_names
        assert "WORKING-STORAGE-SECTION" in subsection_names
        assert "LINKAGE-SECTION" in subsection_names
        assert "PROCEDURE_SECTION-SECTION" in subsection_names
    
    def test_find_subsections_with_whitespace(self, parser, mock_sections):
        """Test subsection detection with various whitespace patterns"""
        lines = [
            "DATA DIVISION.",
            "   FILE   SECTION   .",
            "FD INPUT-FILE.",
            "",
            "    WORKING-STORAGE    SECTION    .",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "LINKAGE SECTION.",
            "01 LS-DATA PIC X(10).",
            "",
            "PROCEDURE DIVISION.",
            "    PROCEDURE    SECTION    .",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Test'.",
            "STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Should find 4 subsections despite whitespace
        assert len(subsections) == 4
        
        # Check that subsections are properly detected
        subsection_names = [subsection.name for subsection in subsections]
        assert "FILE-SECTION" in subsection_names
        assert "WORKING-STORAGE-SECTION" in subsection_names
        assert "LINKAGE-SECTION" in subsection_names
        assert "PROCEDURE_SECTION-SECTION" in subsection_names
    
    def test_find_subsections_case_insensitive(self, parser, mock_sections):
        """Test subsection detection is case insensitive"""
        lines = [
            "DATA DIVISION.",
            "file section.",
            "FD INPUT-FILE.",
            "",
            "working-storage section.",
            "01 ws-counter pic 9(3).",
            "",
            "linkage section.",
            "01 ls-data pic x(10).",
            "",
            "PROCEDURE DIVISION.",
            "procedure section.",
            "MAIN-PARAGRAPH.",
            "    display 'Hello World'.",
            "    stop run."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Should find 4 subsections despite lowercase
        assert len(subsections) == 4
        
        # Check subsection names (should be uppercase)
        subsection_names = [subsection.name for subsection in subsections]
        assert "FILE-SECTION" in subsection_names
        assert "WORKING-STORAGE-SECTION" in subsection_names
        assert "LINKAGE-SECTION" in subsection_names
        assert "PROCEDURE_SECTION-SECTION" in subsection_names
    
    def test_find_subsections_missing_sections(self, parser, mock_sections):
        """Test subsection detection with missing sections"""
        lines = [
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Should find 2 subsections (missing FILE and LINKAGE)
        assert len(subsections) == 2
        
        subsection_names = [subsection.name for subsection in subsections]
        assert "WORKING-STORAGE-SECTION" in subsection_names
        assert "PROCEDURE_SECTION-SECTION" in subsection_names
        assert "FILE-SECTION" not in subsection_names
        assert "LINKAGE-SECTION" not in subsection_names
    
    def test_find_subsections_duplicate_sections(self, parser, mock_sections):
        """Test subsection detection with duplicate sections"""
        lines = [
            "DATA DIVISION.",
            "FILE SECTION.",
            "FD INPUT-FILE.",
            "",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "FILE SECTION.",  # Duplicate
            "FD OUTPUT-FILE.",
            "01 OUTPUT-RECORD PIC X(80).",
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Should find 4 subsections (including duplicate FILE)
        assert len(subsections) == 4
        
        # Check that both FILE sections are detected
        file_subsections = [s for s in subsections if "FILE-SECTION" in s.name]
        assert len(file_subsections) == 2
    
    def test_find_subsections_empty_file(self, parser, mock_sections):
        """Test subsection detection with empty file"""
        lines = []
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Should find no subsections
        assert len(subsections) == 0
    
    def test_find_subsections_no_sections(self, parser, mock_sections):
        """Test subsection detection with no sections"""
        lines = [
            "01 WS-COUNTER PIC 9(3).",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Should find no subsections
        assert len(subsections) == 0
    
    def test_find_subsections_line_ranges(self, parser, mock_sections):
        """Test that subsection line ranges are calculated correctly"""
        lines = [
            "DATA DIVISION.",            # Line 0
            "FILE SECTION.",              # Line 1
            "FD INPUT-FILE.",             # Line 2
            "01 INPUT-RECORD PIC X(80).", # Line 3
            "",                           # Line 4
            "WORKING-STORAGE SECTION.",   # Line 5
            "01 WS-COUNTER PIC 9(3).",    # Line 6
            "01 WS-TOTAL PIC 9(5).",      # Line 7
            "",                           # Line 8
            "LINKAGE SECTION.",           # Line 9
            "01 LS-DATA PIC X(10).",      # Line 10
            "",                           # Line 11
            "PROCEDURE DIVISION.",        # Line 12
            "PROCEDURE SECTION.",         # Line 13
            "MAIN-PARAGRAPH.",            # Line 14
            "    DISPLAY 'Hello World'."  # Line 15
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Check line ranges for each subsection
        for subsection in subsections:
            if "FILE-SECTION" in subsection.name:
                assert subsection.line_range[0] == 2  # 1-based line number
                assert subsection.line_range[1] == 5  # End before WORKING-STORAGE
            elif "WORKING-STORAGE-SECTION" in subsection.name:
                assert subsection.line_range[0] == 6  # 1-based line number
                assert subsection.line_range[1] == 9  # End before LINKAGE
            elif "LINKAGE-SECTION" in subsection.name:
                assert subsection.line_range[0] == 10  # 1-based line number
                assert subsection.line_range[1] == 12  # End before PROCEDURE
            elif "PROCEDURE_SECTION-SECTION" in subsection.name:
                assert subsection.line_range[0] == 14  # 1-based line number
                assert subsection.line_range[1] == 16  # End of file
    
    def test_find_subsections_parent_section_assignment(self, parser, mock_sections):
        """Test that parent sections are correctly assigned to subsections"""
        lines = [
            "DATA DIVISION.",
            "FILE SECTION.",
            "FD INPUT-FILE.",
            "",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Check parent section assignments
        for subsection in subsections:
            if any(section_type in subsection.name for section_type in ["FILE-SECTION", "WORKING-STORAGE-SECTION", "LINKAGE-SECTION"]):
                assert subsection.parent_section == "DATA-DIVISION"
            elif "PROCEDURE_SECTION-SECTION" in subsection.name:
                assert subsection.parent_section == "PROCEDURE-DIVISION"
    
    def test_find_subsections_business_logic_extraction(self, parser, mock_sections):
        """Test that business logic is extracted for each subsection"""
        lines = [
            "DATA DIVISION.",
            "FILE SECTION.",
            "FD INPUT-FILE.",
            "01 INPUT-RECORD PIC X(80).",
            "",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "01 WS-TOTAL PIC 9(5).",
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    ADD 1 TO WS-COUNTER.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Check that business logic is extracted
        for subsection in subsections:
            assert subsection.business_logic is not None
            assert isinstance(subsection.business_logic, str)
            assert len(subsection.business_logic) > 0
    
    def test_find_subsections_complexity_calculation(self, parser, mock_sections):
        """Test that complexity scores are calculated for each subsection"""
        lines = [
            "DATA DIVISION.",
            "FILE SECTION.",
            "FD INPUT-FILE.",
            "",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Check that complexity scores are calculated
        for subsection in subsections:
            assert subsection.complexity_score is not None
            assert isinstance(subsection.complexity_score, (int, float))
            assert 0 <= subsection.complexity_score <= 10  # Assuming 0-10 scale
    
    def test_find_subsections_risk_assessment(self, parser, mock_sections):
        """Test that risk levels are assessed for each subsection"""
        lines = [
            "DATA DIVISION.",
            "FILE SECTION.",
            "FD INPUT-FILE.",
            "",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Check that risk levels are assessed
        for subsection in subsections:
            assert subsection.risk_level is not None
            # Risk level is returned as string from base parser
            assert isinstance(subsection.risk_level, str)
            assert subsection.risk_level in ['LOW', 'MEDIUM', 'HIGH', 'CRITICAL']
    
    def test_find_subsections_confidence_scores(self, parser, mock_sections):
        """Test that confidence scores are set for each subsection"""
        lines = [
            "DATA DIVISION.",
            "FILE SECTION.",
            "FD INPUT-FILE.",
            "",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Check that confidence scores are set
        for subsection in subsections:
            assert subsection.confidence is not None
            assert isinstance(subsection.confidence, (int, float))
            assert 0 <= subsection.confidence <= 1
    
    def test_find_subsections_with_comments(self, parser, mock_sections):
        """Test subsection detection with comments and blank lines"""
        lines = [
            "DATA DIVISION.",
            "* This is a comment",
            "FILE SECTION.",
            "FD INPUT-FILE.",
            "* Another comment",
            "",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Should find 3 subsections despite comments
        assert len(subsections) == 3
        
        subsection_names = [subsection.name for subsection in subsections]
        assert "FILE-SECTION" in subsection_names
        assert "WORKING-STORAGE-SECTION" in subsection_names
        assert "PROCEDURE_SECTION-SECTION" in subsection_names
    
    def test_find_subsections_malformed_sections(self, parser, mock_sections):
        """Test subsection detection with malformed section statements"""
        lines = [
            "DATA DIVISION.",
            "FILE SECTION",  # Missing period
            "FD INPUT-FILE.",
            "",
            "WORKING-STORAGE SECTION.",  # Correct
            "01 WS-COUNTER PIC 9(3).",
            "",
            "LINKAGE SECTION",  # Missing period
            "01 LS-DATA PIC X(10).",
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",  # Correct
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # The current parser implementation finds all subsections regardless of malformed syntax
        # This is because the regex patterns are more flexible
        assert len(subsections) == 4
        
        subsection_names = [subsection.name for subsection in subsections]
        assert "FILE-SECTION" in subsection_names
        assert "WORKING-STORAGE-SECTION" in subsection_names
        assert "LINKAGE-SECTION" in subsection_names
        assert "PROCEDURE_SECTION-SECTION" in subsection_names
    
    def test_find_subsections_nested_structure(self, parser, mock_sections):
        """Test subsection detection with nested structure"""
        lines = [
            "DATA DIVISION.",
            "FILE SECTION.",
            "FD INPUT-FILE.",
            "01 INPUT-RECORD PIC X(80).",
            "FD OUTPUT-FILE.",
            "01 OUTPUT-RECORD PIC X(80).",
            "",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "01 WS-TOTAL PIC 9(5).",
            "01 WS-FLAGS.",
            "    05 WS-EOF-FLAG PIC X VALUE 'N'.",
            "    05 WS-ERROR-FLAG PIC X VALUE 'N'.",
            "",
            "LINKAGE SECTION.",
            "01 LS-PARAMETER PIC X(10).",
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Should find 4 subsections
        assert len(subsections) == 4
        
        subsection_names = [subsection.name for subsection in subsections]
        assert "FILE-SECTION" in subsection_names
        assert "WORKING-STORAGE-SECTION" in subsection_names
        assert "LINKAGE-SECTION" in subsection_names
        assert "PROCEDURE_SECTION-SECTION" in subsection_names
    
    @patch.object(COBOLParser, '_find_subsection_end')
    def test_find_subsections_subsection_end_detection(self, mock_find_end, parser, mock_sections):
        """Test subsection detection with mocked subsection end detection"""
        # Mock the _find_subsection_end method to return specific end lines
        mock_find_end.side_effect = [3, 7, 9, 12]  # End lines for each subsection
        
        lines = [
            "DATA DIVISION.",
            "FILE SECTION.",
            "FD INPUT-FILE.",
            "",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "01 WS-TOTAL PIC 9(5).",
            "",
            "LINKAGE SECTION.",
            "01 LS-DATA PIC X(10).",
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Verify _find_subsection_end was called for each subsection
        assert mock_find_end.call_count == 4
        
        # Check that subsections were created with correct line ranges
        expected_ends = [3, 7, 9, 12]  # Convert side_effect to list
        # The actual subsection positions in the test data (0-based to 1-based)
        expected_starts = [2, 5, 9, 13]  # FILE(1), WORKING-STORAGE(5), LINKAGE(9), PROCEDURE_SECTION(13)
        for i, subsection in enumerate(subsections):
            expected_start = expected_starts[i]  # 1-based line numbers
            expected_end = expected_ends[i] + 1  # 1-based
            assert subsection.line_range[0] == expected_start
            assert subsection.line_range[1] == expected_end
    
    def test_find_subsections_performance_large_file(self, parser, mock_sections):
        """Test subsection detection performance with large file"""
        # Create a large COBOL file
        lines = [
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION."
        ]
        
        # Add many lines to WORKING-STORAGE SECTION
        for i in range(1000):
            lines.append(f"01 WS-FIELD-{i:03d} PIC 9(3).")
        
        lines.extend([
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ])
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Should find 2 subsections
        assert len(subsections) == 2
        
        subsection_names = [subsection.name for subsection in subsections]
        assert "WORKING-STORAGE-SECTION" in subsection_names
        assert "PROCEDURE_SECTION-SECTION" in subsection_names
    
    def test_find_subsections_edge_cases(self, parser, mock_sections):
        """Test subsection detection with edge cases"""
        # Test with only subsection headers
        lines = [
            "FILE SECTION.",
            "WORKING-STORAGE SECTION.",
            "LINKAGE SECTION.",
            "PROCEDURE SECTION."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Should find 4 subsections
        assert len(subsections) == 4
        
        # Test with subsection at end of file
        lines = [
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        # Should find 1 subsection
        assert len(subsections) == 1
        assert "WORKING-STORAGE-SECTION" in subsections[0].name
    
    def test_find_subsections_subsection_attributes(self, parser, mock_sections):
        """Test that all subsection attributes are properly set"""
        lines = [
            "DATA DIVISION.",
            "FILE SECTION.",
            "FD INPUT-FILE.",
            "",
            "WORKING-STORAGE SECTION.",
            "01 WS-COUNTER PIC 9(3).",
            "",
            "PROCEDURE DIVISION.",
            "PROCEDURE SECTION.",
            "MAIN-PARAGRAPH.",
            "    DISPLAY 'Hello World'.",
            "    STOP RUN."
        ]
        
        subsections = parser._find_subsections(lines, mock_sections)
        
        for subsection in subsections:
            # Check required attributes
            assert hasattr(subsection, 'name')
            assert hasattr(subsection, 'parent_section')
            assert hasattr(subsection, 'line_range')
            assert hasattr(subsection, 'line_count')
            assert hasattr(subsection, 'business_logic')
            assert hasattr(subsection, 'confidence')
            assert hasattr(subsection, 'complexity_score')
            assert hasattr(subsection, 'risk_level')
            
            # Check attribute types
            assert isinstance(subsection.name, str)
            assert isinstance(subsection.parent_section, str)
            assert isinstance(subsection.line_range, tuple)
            assert len(subsection.line_range) == 2
            assert isinstance(subsection.line_count, int)
            assert isinstance(subsection.business_logic, str)
            assert isinstance(subsection.confidence, (int, float))
            assert isinstance(subsection.complexity_score, (int, float))
            # Risk level is returned as string from base parser
            assert isinstance(subsection.risk_level, str)
            assert subsection.risk_level in ['LOW', 'MEDIUM', 'HIGH', 'CRITICAL']
            
            # Check attribute values
            assert len(subsection.name) > 0
            assert len(subsection.parent_section) > 0
            assert subsection.line_range[0] > 0
            assert subsection.line_range[1] > subsection.line_range[0]
            assert subsection.line_count > 0
            assert 0 <= subsection.confidence <= 1
            assert 0 <= subsection.complexity_score <= 10
    
    def test_find_subsections_parent_section_finding(self, parser):
        """Test parent section finding logic"""
        # Create sections with specific line ranges
        sections = [
            COBOLSection(
                name="DATA-DIVISION",
                type="DATA",
                line_range=(1, 10),
                line_count=10,
                business_logic="Data definitions",
                confidence=0.9,
                complexity_score=2.0,
                risk_level=RiskLevel.LOW
            ),
            COBOLSection(
                name="PROCEDURE-DIVISION",
                type="PROCEDURE",
                line_range=(11, 20),
                line_count=10,
                business_logic="Main program logic",
                confidence=0.9,
                complexity_score=5.0,
                risk_level=RiskLevel.MEDIUM
            )
        ]
        
        # Test parent section finding for different line numbers
        assert parser._find_parent_section(sections, 5) == "DATA-DIVISION"  # Line 5 in DATA
        assert parser._find_parent_section(sections, 15) == "PROCEDURE-DIVISION"  # Line 15 in PROCEDURE
        assert parser._find_parent_section(sections, 25) == "UNKNOWN"  # Line 25 beyond last section
        assert parser._find_parent_section(sections, 0) == "DATA-DIVISION"  # Line 0 before first section
