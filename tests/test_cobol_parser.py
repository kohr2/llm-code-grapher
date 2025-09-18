"""
Unit tests for COBOL parser functionality
Tests section detection, validation, and parsing accuracy
"""

import pytest
import re
from pathlib import Path
from src.cobol_parser import COBOLParser, COBOLSection, COBOLSubsection


class TestCOBOLParser:
    """Test cases for COBOL parser functionality"""
    
    @pytest.fixture
    def parser(self):
        """Create a COBOL parser instance for testing"""
        return COBOLParser()
    
    @pytest.fixture
    def sample_cobol_code(self):
        """Sample COBOL code for testing"""
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
021900     PERFORM 2000-PROCESS-TRANSACTIONS
022000     PERFORM 9000-FINALIZE-PROGRAM
022100     STOP RUN.

022300 1000-INITIALIZE-PROGRAM SECTION.
022400 1000-INIT-START.
022500     DISPLAY 'FRAUD MANAGEMENT SYSTEM - INITIALIZING'
022600     PERFORM 1100-OPEN-FILES
022700     PERFORM 1200-INITIALIZE-VARIABLES
022800     PERFORM 1300-LOAD-FRAUD-PARAMETERS.

023000 1100-OPEN-FILES.
023100     OPEN INPUT TRANSACTION-FILE
023200     IF WS-TRANS-STATUS NOT = '00'
023300     DISPLAY ERR-FILE-NOT-FOUND ' - TRANSACTION FILE'
023400     STOP RUN
023500     END-IF.
"""

    @pytest.fixture
    def fraud_cobol_file(self):
        """Load the actual fraud management COBOL file"""
        file_path = Path("data/fixtures/vasu_fraud_management_cobol_reformatted.cbl")
        return file_path.read_text(encoding='utf-8')

    def test_section_patterns(self, parser):
        """Test that section patterns correctly identify COBOL sections"""
        test_code = """
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. TEST-PROGRAM.
000700 ENVIRONMENT DIVISION.
004200 DATA DIVISION.
013300 WORKING-STORAGE SECTION.
021400 PROCEDURE DIVISION.
021600 0000-MAIN-CONTROL SECTION.
022300 1000-INITIALIZE-PROGRAM SECTION.
"""
        
        sections = parser._extract_sections(test_code)
        
        # Should identify main divisions
        assert len(sections) >= 4
        section_names = [s.name for s in sections]
        
        assert "IDENTIFICATION DIVISION" in section_names
        assert "ENVIRONMENT DIVISION" in section_names
        assert "DATA DIVISION" in section_names
        assert "PROCEDURE DIVISION" in section_names

    def test_subsection_patterns(self, parser):
        """Test that subsection patterns correctly identify COBOL subsections"""
        test_code = """
021600 0000-MAIN-CONTROL SECTION.
021700 0000-MAIN-PROCESS.
022300 1000-INITIALIZE-PROGRAM SECTION.
022400 1000-INIT-START.
023000 1100-OPEN-FILES.
"""
        
        subsections = parser._extract_subsections(test_code)
        
        # Should identify subsections
        assert len(subsections) >= 3
        subsection_names = [s.name for s in subsections]
        
        assert "0000-MAIN-PROCESS" in subsection_names
        assert "1000-INIT-START" in subsection_names
        assert "1100-OPEN-FILES" in subsection_names

    def test_line_range_calculation(self, parser):
        """Test that line ranges are calculated correctly"""
        test_code = """000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. TEST-PROGRAM.
000300 AUTHOR. TEST-AUTHOR.
000400 DATE-WRITTEN. 2025-01-01.

000700 ENVIRONMENT DIVISION.
000800 CONFIGURATION SECTION.
"""
        
        sections = parser._extract_sections(test_code)
        id_section = next(s for s in sections if s.name == "IDENTIFICATION DIVISION")
        
        assert id_section.line_range == (1, 4)  # Lines 1-4
        assert id_section.line_count == 4

    def test_section_type_classification(self, parser):
        """Test that sections are correctly classified by type"""
        test_code = """
000100 IDENTIFICATION DIVISION.
000700 ENVIRONMENT DIVISION.
004200 DATA DIVISION.
013300 WORKING-STORAGE SECTION.
021400 PROCEDURE DIVISION.
021600 0000-MAIN-CONTROL SECTION.
"""
        
        sections = parser._extract_sections(test_code)
        
        for section in sections:
            if section.name == "IDENTIFICATION DIVISION":
                assert section.type == "IDENTIFICATION"
            elif section.name == "ENVIRONMENT DIVISION":
                assert section.type == "ENVIRONMENT"
            elif section.name == "DATA DIVISION":
                assert section.type == "DATA"
            elif section.name == "WORKING-STORAGE SECTION":
                assert section.type == "WORKING-STORAGE"
            elif section.name == "PROCEDURE DIVISION":
                assert section.type == "PROCEDURE"
            elif "SECTION" in section.name:
                assert section.type == "PROCEDURE_SECTION"

    def test_parse_fraud_management_file(self, parser, fraud_cobol_file):
        """Test parsing the actual fraud management COBOL file"""
        result = parser.parse(fraud_cobol_file)
        
        # Should identify main divisions
        assert len(result.sections) >= 4
        
        # Check for expected sections
        section_names = [s.name for s in result.sections]
        assert "IDENTIFICATION DIVISION" in section_names
        assert "ENVIRONMENT DIVISION" in section_names
        assert "DATA DIVISION" in section_names
        assert "PROCEDURE DIVISION" in section_names
        
        # Check for specific procedure sections
        procedure_sections = [s for s in result.sections if s.type == "PROCEDURE_SECTION"]
        assert len(procedure_sections) > 0
        
        # Check for main control section
        main_section = next((s for s in procedure_sections if "MAIN-CONTROL" in s.name), None)
        assert main_section is not None
        assert main_section.line_count > 0

    def test_subsections_in_fraud_file(self, parser, fraud_cobol_file):
        """Test that subsections are correctly identified in the fraud file"""
        result = parser.parse(fraud_cobol_file)
        
        # Should have many subsections
        assert len(result.subsections) > 10
        
        # Check for specific subsections
        subsection_names = [s.name for s in result.subsections]
        
        # Main process subsection
        assert "0000-MAIN-PROCESS" in subsection_names
        
        # Initialization subsections
        assert "1000-INIT-START" in subsection_names
        assert "1100-OPEN-FILES" in subsection_names
        
        # Transaction processing subsections
        assert "2000-PROCESS-START" in subsection_names
        assert "2100-READ-TRANSACTION" in subsection_names
        assert "2200-ANALYSIS-START" in subsection_names

    def test_section_validation(self, parser):
        """Test that section validation works correctly"""
        # Valid section
        valid_section = COBOLSection(
            name="TEST-SECTION",
            type="PROCEDURE_SECTION",
            line_range=(1, 10),
            line_count=10,
            business_logic="Test section",
            confidence=0.9
        )
        assert parser._validate_section(valid_section) == True
        
        # Invalid section (negative line count)
        invalid_section = COBOLSection(
            name="INVALID-SECTION",
            type="PROCEDURE_SECTION",
            line_range=(1, 10),
            line_count=-1,
            business_logic="Invalid section",
            confidence=0.9
        )
        assert parser._validate_section(invalid_section) == False

    def test_subsection_parent_assignment(self, parser, fraud_cobol_file):
        """Test that subsections are correctly assigned to parent sections"""
        result = parser.parse(fraud_cobol_file)
        
        # Find main control section
        main_section = next((s for s in result.sections if "MAIN-CONTROL" in s.name), None)
        assert main_section is not None
        
        # Find subsections that belong to main control
        main_subsections = [s for s in result.subsections if s.parent_section == main_section.name]
        assert len(main_subsections) > 0
        
        # Check that 0000-MAIN-PROCESS belongs to main control
        main_process = next((s for s in main_subsections if s.name == "0000-MAIN-PROCESS"), None)
        assert main_process is not None

    def test_business_logic_extraction_placeholder(self, parser):
        """Test that business logic extraction returns placeholder text"""
        test_code = """
021600 0000-MAIN-CONTROL SECTION.
021700 0000-MAIN-PROCESS.
021800     PERFORM 1000-INITIALIZE-PROGRAM
021900     PERFORM 2000-PROCESS-TRANSACTIONS
022000     PERFORM 9000-FINALIZE-PROGRAM
022100     STOP RUN.
"""
        
        result = parser.parse(test_code)
        
        # Should have business logic placeholder
        main_section = next((s for s in result.sections if "MAIN-CONTROL" in s.name), None)
        assert main_section is not None
        assert main_section.business_logic == "Business logic analysis pending"
        assert main_section.confidence == 0.0  # Placeholder confidence

    def test_error_handling_invalid_code(self, parser):
        """Test that parser handles invalid COBOL code gracefully"""
        invalid_code = "This is not COBOL code at all!"
        
        result = parser.parse(invalid_code)
        
        # Should return empty result without crashing
        assert len(result.sections) == 0
        assert len(result.subsections) == 0

    def test_empty_file_handling(self, parser):
        """Test that parser handles empty files gracefully"""
        empty_code = ""
        
        result = parser.parse(empty_code)
        
        # Should return empty result without crashing
        assert len(result.sections) == 0
        assert len(result.subsections) == 0

    def test_whitespace_handling(self, parser):
        """Test that parser handles whitespace and formatting correctly"""
        messy_code = """
    
000100 IDENTIFICATION DIVISION.    
000200 PROGRAM-ID. TEST-PROGRAM.   
000300 AUTHOR. TEST-AUTHOR.        
000400 DATE-WRITTEN. 2025-01-01.   

000700 ENVIRONMENT DIVISION.       
000800 CONFIGURATION SECTION.      
"""
        
        result = parser.parse(messy_code)
        
        # Should still identify sections correctly
        assert len(result.sections) >= 2
        section_names = [s.name for s in result.sections]
        assert "IDENTIFICATION DIVISION" in section_names
        assert "ENVIRONMENT DIVISION" in section_names

    def test_performance_large_file(self, parser, fraud_cobol_file):
        """Test that parser can handle large COBOL files efficiently"""
        import time
        
        start_time = time.time()
        result = parser.parse(fraud_cobol_file)
        end_time = time.time()
        
        # Should complete within reasonable time (less than 5 seconds)
        assert (end_time - start_time) < 5.0
        
        # Should still produce results
        assert len(result.sections) > 0
        assert len(result.subsections) > 0

    def test_confidence_scoring(self, parser):
        """Test that confidence scoring works for different section types"""
        test_code = """
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. CLEAR-PROGRAM.
000700 ENVIRONMENT DIVISION.
004200 DATA DIVISION.
021400 PROCEDURE DIVISION.
021600 0000-MAIN-CONTROL SECTION.
021700 0000-MAIN-PROCESS.
"""
        
        result = parser.parse(test_code)
        
        # All sections should have confidence scores
        for section in result.sections:
            assert 0.0 <= section.confidence <= 1.0
        
        # Clear sections should have higher confidence
        id_section = next((s for s in result.sections if s.name == "IDENTIFICATION DIVISION"), None)
        assert id_section is not None
        assert id_section.confidence > 0.8  # High confidence for clear divisions

    def test_duplicate_section_handling(self, parser):
        """Test that parser handles duplicate section names correctly"""
        test_code = """
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. TEST-PROGRAM.
000700 ENVIRONMENT DIVISION.
000800 CONFIGURATION SECTION.
000900 SOURCE-COMPUTER. IBM-Z15.
001000 OBJECT-COMPUTER. IBM-Z15.
"""
        
        result = parser.parse(test_code)
        
        # Should handle duplicates gracefully
        assert len(result.sections) > 0
        
        # Check that we don't have duplicate entries
        section_names = [s.name for s in result.sections]
        assert len(section_names) == len(set(section_names))  # No duplicates
