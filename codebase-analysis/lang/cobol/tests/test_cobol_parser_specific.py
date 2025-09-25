"""
COBOL-specific parser tests
Tests only COBOL-specific parsing logic and validation
"""

import pytest
import re
from pathlib import Path
from lang.cobol.parser.cobol_parser import COBOLParser, COBOLSection, COBOLSubsection
from lang.base.tests.base_parser_tests import BaseParserTests


class TestCOBOLParserSpecific(BaseParserTests):
    """Test cases for COBOL-specific parser functionality"""
    
    def get_parser_class(self):
        """Get the parser class to test"""
        return COBOLParser
    
    def get_language(self) -> str:
        """Get the language being tested"""
        return "COBOL"
    
    def get_sample_code(self) -> str:
        """Get sample code for testing"""
        return """
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. FRAUD-MGMT-SYSTEM.
000300 AUTHOR. FRAUD-DETECTION-TEAM.
000400 DATE-WRITTEN. 2025-08-06.

000700 ENVIRONMENT DIVISION.
000800 CONFIGURATION SECTION.
000900 SOURCE-COMPUTER. IBM-Z15.

001200 INPUT-OUTPUT SECTION.
001300 FILE-CONTROL.
001400     SELECT TRANSACTION-FILE ASSIGN TO 'TRANFILE'.

004200 DATA DIVISION.
004300 FILE SECTION.

004500 FD  TRANSACTION-FILE
004600     RECORDING MODE IS F
004700 RECORD CONTAINS 200 CHARACTERS.

013300 WORKING-STORAGE SECTION.

021400 PROCEDURE DIVISION.

021600 0000-MAIN-CONTROL SECTION.
021700 0000-MAIN-PROCESS.
021800     PERFORM 1000-INITIALIZE-PROGRAM
"""
    
    def get_expected_sections(self) -> list:
        """Get expected sections for sample code"""
        return [
            {"name": "IDENTIFICATION-DIVISION", "type": "IDENTIFICATION"},
            {"name": "ENVIRONMENT-DIVISION", "type": "ENVIRONMENT"},
            {"name": "DATA-DIVISION", "type": "DATA"},
            {"name": "PROCEDURE-DIVISION", "type": "PROCEDURE"}
        ]
    
    def get_expected_subsections(self) -> list:
        """Get expected subsections for sample code"""
        return [
            {"name": "FILE-SECTION", "type": "FILE"},
            {"name": "WORKING-STORAGE-SECTION", "type": "WORKING-STORAGE"}
        ]
    
    @pytest.fixture
    def fraud_cobol_file(self):
        """Load the actual fraud management COBOL file"""
        file_path = Path("data/fixtures/vasu_fraud_management_cobol_reformatted.cbl")
        return file_path.read_text(encoding='utf-8')
    
    def test_cobol_comment_detection(self, parser):
        """Test COBOL-specific comment line detection"""
        # COBOL comments start with *
        comment_lines = [
            "* This is a COBOL comment",
            "    * Indented comment",
            "000100 * Comment in line 100",
            "/* This is also a comment */"
        ]
        
        non_comment_lines = [
            "000100 IDENTIFICATION DIVISION.",
            "    MOVE 1 TO WS-COUNTER",
            "    PERFORM 1000-PROCESS",
            "    IF WS-FLAG = 'Y'"
        ]
        
        for line in comment_lines:
            assert parser._is_comment_line(line), f"Should detect as comment: {line}"
        
        for line in non_comment_lines:
            assert not parser._is_comment_line(line), f"Should not detect as comment: {line}"
    
    def test_cobol_section_patterns(self, parser):
        """Test COBOL-specific section patterns"""
        test_cases = [
            ("IDENTIFICATION DIVISION.", "IDENTIFICATION"),
            ("ENVIRONMENT DIVISION.", "ENVIRONMENT"),
            ("DATA DIVISION.", "DATA"),
            ("PROCEDURE DIVISION.", "PROCEDURE")
        ]
        
        for line, expected_type in test_cases:
            line_upper = line.upper().strip()
            matched = False
            for section_type, pattern in parser.section_patterns.items():
                if re.match(pattern, line_upper):
                    assert section_type == expected_type
                    matched = True
                    break
            assert matched, f"Should match {expected_type} for line: {line}"
    
    def test_cobol_subsection_patterns(self, parser):
        """Test COBOL-specific subsection patterns"""
        test_cases = [
            ("FILE SECTION.", "FILE"),
            ("WORKING-STORAGE SECTION.", "WORKING-STORAGE"),
            ("LINKAGE SECTION.", "LINKAGE"),
            ("PROCEDURE SECTION.", "PROCEDURE_SECTION")
        ]
        
        for line, expected_type in test_cases:
            line_upper = line.upper().strip()
            matched = False
            for subsection_type, pattern in parser.subsection_patterns.items():
                if re.match(pattern, line_upper):
                    assert subsection_type == expected_type
                    matched = True
                    break
            assert matched, f"Should match {expected_type} for line: {line}"
    
    def test_cobol_relationship_patterns(self, parser):
        """Test COBOL-specific relationship patterns"""
        test_cases = [
            ("CALL 'SUBPROGRAM'", "CALL", "SUBPROGRAM"),
            ("CALL 'UTIL-PROG' USING WS-DATA", "CALL", "UTIL-PROG"),
            ("PERFORM 1000-PROCESS", "PERFORM", "1000-PROCESS"),
            ("GO TO 2000-EXIT", "GO_TO", "2000-EXIT")
        ]
        
        for line, expected_type, expected_target in test_cases:
            line_upper = line.upper()
            matched = False
            for rel_type, pattern in parser.relationship_patterns.items():
                match = re.search(pattern, line_upper)
                if match:
                    assert rel_type == expected_type
                    if match.groups():
                        assert match.group(1) == expected_target
                    matched = True
                    break
            assert matched, f"Should match {expected_type} for line: {line}"
    
    def test_cobol_program_name_extraction(self, parser):
        """Test COBOL program name extraction"""
        test_cases = [
            ("PROGRAM-ID. TEST-PROGRAM.", "TEST-PROGRAM"),
            ("PROGRAM-ID. FRAUD-MGMT-SYSTEM.", "FRAUD-MGMT-SYSTEM"),
            ("PROGRAM-ID. UTIL-001.", "UTIL-001"),
            ("PROGRAM-ID. MAIN.", "MAIN")
        ]
        
        for line, expected_name in test_cases:
            lines = [line]
            result = parser._extract_program_name(lines)
            assert result == expected_name, f"Should extract {expected_name} from {line}"
    
    def test_cobol_section_creation(self, parser):
        """Test COBOL-specific section creation"""
        section = parser._create_section(
            name="TEST-DIVISION",
            section_type="PROCEDURE",
            line_range=(1, 10),
            business_logic="Test COBOL logic"
        )
        
        assert isinstance(section, COBOLSection)
        assert section.name == "TEST-DIVISION"
        assert section.type == "PROCEDURE"
        assert section.line_range == (1, 10)
        assert section.line_count == 10
        assert section.business_logic == "Test COBOL logic"
        assert section.confidence == 0.9
        assert section.complexity_score == 0.5
        assert section.risk_level == "LOW"
    
    def test_cobol_subsection_creation(self, parser):
        """Test COBOL-specific subsection creation"""
        subsection = parser._create_subsection(
            name="FILE-SECTION",
            parent_section="DATA-DIVISION",
            line_range=(5, 8),
            business_logic="File definitions"
        )
        
        assert isinstance(subsection, COBOLSubsection)
        assert subsection.name == "FILE-SECTION"
        assert subsection.parent_section == "DATA-DIVISION"
        assert subsection.line_range == (5, 8)
        assert subsection.line_count == 4
        assert subsection.business_logic == "File definitions"
        assert subsection.confidence == 0.8
        assert subsection.complexity_score == 0.4
        assert subsection.risk_level == "LOW"
    
    def test_cobol_parsing_with_real_file(self, parser, fraud_cobol_file, tmp_path):
        """Test COBOL parsing with real fraud management file"""
        # Create temporary file
        test_file = tmp_path / "fraud_test.cbl"
        test_file.write_text(fraud_cobol_file)
        
        # Parse the file
        result = parser.parse(test_file)
        
        # Verify basic structure
        assert result is not None
        assert result.program.name == "FRAUD-MGMT-SYSTEM"
        assert result.program.language == "COBOL"
        assert len(result.sections) > 0
        assert len(result.subsections) > 0
        
        # Verify COBOL-specific sections are found
        section_names = [s.name for s in result.sections]
        assert "IDENTIFICATION-DIVISION" in section_names
        assert "ENVIRONMENT-DIVISION" in section_names
        assert "DATA-DIVISION" in section_names
        assert "PROCEDURE-DIVISION" in section_names
        
        # Verify COBOL-specific subsections are found
        subsection_names = [s.name for s in result.subsections]
        assert "FILE-SECTION" in subsection_names
        assert "WORKING-STORAGE-SECTION" in subsection_names
    
    def test_cobol_parsing_accuracy(self, parser, fraud_cobol_file, tmp_path):
        """Test COBOL parsing accuracy with known file"""
        # Create temporary file
        test_file = tmp_path / "fraud_test.cbl"
        test_file.write_text(fraud_cobol_file)
        
        # Parse the file
        result = parser.parse(test_file)
        
        # Test parsing accuracy
        assert result is not None
        assert result.program.name == "FRAUD-MGMT-SYSTEM"
        
        # Should have reasonable number of sections
        assert len(result.sections) >= 4  # At least 4 main divisions
        assert len(result.subsections) >= 2  # At least FILE and WORKING-STORAGE
        
        # All sections should have valid properties
        for section in result.sections:
            assert section.name
            assert section.type
            assert section.line_range[0] > 0
            assert section.line_range[1] >= section.line_range[0]
            assert section.line_count > 0
            assert 0.0 <= section.confidence <= 1.0
            assert 0.0 <= section.complexity_score <= 1.0
            assert section.risk_level in ["LOW", "MEDIUM", "HIGH", "CRITICAL"]
        
        # All subsections should have valid properties
        for subsection in result.subsections:
            assert subsection.name
            assert subsection.parent_section
            assert subsection.line_range[0] > 0
            assert subsection.line_range[1] >= subsection.line_range[0]
            assert subsection.line_count > 0
            assert 0.0 <= subsection.confidence <= 1.0
            assert 0.0 <= subsection.complexity_score <= 1.0
            assert subsection.risk_level in ["LOW", "MEDIUM", "HIGH", "CRITICAL"]
    
    def test_cobol_parsing_performance(self, parser, fraud_cobol_file, tmp_path):
        """Test COBOL parsing performance"""
        import time
        
        # Create temporary file
        test_file = tmp_path / "fraud_test.cbl"
        test_file.write_text(fraud_cobol_file)
        
        # Measure parsing time
        start_time = time.time()
        result = parser.parse(test_file)
        end_time = time.time()
        
        parsing_time = end_time - start_time
        
        # Should parse reasonably quickly (less than 5 seconds for this file)
        assert parsing_time < 5.0, f"Parsing took too long: {parsing_time:.2f} seconds"
        
        # Should produce valid result
        assert result is not None
        assert result.program.name == "FRAUD-MGMT-SYSTEM"
