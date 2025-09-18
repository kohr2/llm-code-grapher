"""
COBOL-specific pytest configuration and fixtures
Extends the base conftest.py with COBOL-specific test data
"""

import pytest
from pathlib import Path
from unittest.mock import Mock


@pytest.fixture(scope="session")
def cobol_test_data_dir():
    """Provide path to COBOL test data directory"""
    return Path("lang/cobol/tests/test_fixtures")


@pytest.fixture(scope="session")
def fraud_cobol_file_path():
    """Provide path to the fraud management COBOL file"""
    return Path("data/fixtures/vasu_fraud_management_cobol_reformatted.cbl")


@pytest.fixture(scope="session")
def fraud_cobol_content(fraud_cobol_file_path):
    """Load the fraud management COBOL file content"""
    if fraud_cobol_file_path.exists():
        return fraud_cobol_file_path.read_text(encoding='utf-8')
    else:
        pytest.skip(f"Fraud COBOL file not found: {fraud_cobol_file_path}")


@pytest.fixture
def sample_cobol_code():
    """Provide sample COBOL code for testing"""
    return """
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. TEST-PROGRAM.
000300 AUTHOR. TEST-AUTHOR.
000400 DATE-WRITTEN. 2025-01-01.

000700 ENVIRONMENT DIVISION.
000800 CONFIGURATION SECTION.
000900 SOURCE-COMPUTER. IBM-Z15.

004200 DATA DIVISION.
004300 FILE SECTION.

004500 FD  TEST-FILE
004600     RECORDING MODE IS F
004700 RECORD CONTAINS 80 CHARACTERS.

013300 WORKING-STORAGE SECTION.

013500* Control Variables
014000 01  WS-EOF-FLAG            PIC X VALUE 'N'.
014100 88  EOF-REACHED        VALUE 'Y'.

021400 PROCEDURE DIVISION.

021600 0000-MAIN-CONTROL SECTION.
021700 0000-MAIN-PROCESS.
021800     PERFORM 1000-INITIALIZE-PROGRAM
021900     PERFORM 2000-PROCESS-DATA
022000     PERFORM 9000-FINALIZE-PROGRAM
022100     STOP RUN.

022300 1000-INITIALIZE-PROGRAM SECTION.
022400 1000-INIT-START.
022500     DISPLAY 'TEST PROGRAM - INITIALIZING'
022600     PERFORM 1100-OPEN-FILES.

022900 1100-OPEN-FILES.
023000     OPEN INPUT TEST-FILE
023100     IF WS-EOF-FLAG = 'Y'
023200     DISPLAY 'ERROR OPENING FILE'
023300     STOP RUN
023400     END-IF.

024700 2000-PROCESS-DATA SECTION.
024800 2000-PROCESS-START.
024900     DISPLAY 'BEGINNING DATA PROCESSING'
025000     PERFORM 2100-READ-DATA
025100     PERFORM UNTIL EOF-REACHED
025200     PERFORM 2200-PROCESS-RECORD
025300     PERFORM 2100-READ-DATA
025400     END-PERFORM.

025700 2100-READ-DATA.
025800     READ TEST-FILE AT END SET EOF-REACHED TO TRUE.

026500 2200-PROCESS-RECORD.
026600     DISPLAY 'PROCESSING RECORD'
026700     ADD 1 TO WS-RECORD-COUNT.

027600 9000-FINALIZE-PROGRAM SECTION.
027700 9000-FINALIZE-START.
027800     PERFORM 9100-CLOSE-FILES
027900     DISPLAY 'TEST PROGRAM - PROCESSING COMPLETED'.

028200 9100-CLOSE-FILES.
028300     CLOSE TEST-FILE.
"""


@pytest.fixture
def expected_cobol_sections():
    """Provide expected COBOL section names for validation"""
    return [
        "IDENTIFICATION DIVISION",
        "ENVIRONMENT DIVISION",
        "DATA DIVISION",
        "WORKING-STORAGE SECTION",
        "PROCEDURE DIVISION"
    ]


@pytest.fixture
def expected_cobol_subsections():
    """Provide expected COBOL subsection names for validation"""
    return [
        "0000-MAIN-PROCESS",
        "1000-INIT-START",
        "1100-OPEN-FILES",
        "2000-PROCESS-START",
        "2100-READ-DATA",
        "2200-PROCESS-RECORD",
        "9000-FINALIZE-START",
        "9100-CLOSE-FILES"
    ]


@pytest.fixture
def cobol_validation_data():
    """Provide COBOL-specific validation data for testing"""
    return {
        "fraud_file_expected_sections": 15,
        "fraud_file_expected_subsections": 50,
        "fraud_file_expected_relationships": 20,
        "min_confidence_threshold": 0.7,
        "max_processing_time": 5.0,
        "cobol_section_types": [
            "IDENTIFICATION", "ENVIRONMENT", "DATA", "PROCEDURE",
            "FILE", "WORKING-STORAGE", "LINKAGE", "PROCEDURE_SECTION"
        ],
        "cobol_relationship_types": [
            "CALLS", "USES", "MODIFIES", "DEPENDS_ON", "DATA_FLOW",
            "CONTROL_FLOW", "IMPLEMENTS", "CONTAINS", "PERFORMS",
            "GOES_TO", "EXITS", "RETURNS"
        ]
    }


@pytest.fixture
def mock_cobol_analyzer():
    """Provide a mock COBOL LLM analyzer for testing"""
    mock_analyzer = Mock()
    mock_analyzer.analyze_section.return_value = {
        "business_logic": "Mock COBOL section analysis",
        "confidence": 0.85,
        "complexity_score": 0.7,
        "risk_level": "MEDIUM"
    }
    mock_analyzer.analyze_subsection.return_value = {
        "business_logic": "Mock COBOL subsection analysis",
        "confidence": 0.80,
        "complexity_score": 0.6,
        "risk_level": "LOW"
    }
    return mock_analyzer


@pytest.fixture
def cobol_ontology():
    """Provide a COBOL ontology instance for testing"""
    from lang.cobol.ontology.cobol_ontology import COBOLOntology
    return COBOLOntology()


@pytest.fixture
def cobol_ontology_validator():
    """Provide a COBOL ontology validator instance for testing"""
    from lang.cobol.ontology.cobol_ontology_validator import COBOLOntologyValidator
    return COBOLOntologyValidator()