"""
Pytest configuration and fixtures for COBOL Code Grapher tests
"""

import pytest
import tempfile
import os
from pathlib import Path
from unittest.mock import Mock, patch


@pytest.fixture(scope="session")
def test_data_dir():
    """Provide path to test data directory"""
    return Path("tests/test_fixtures")


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
def mock_llm_analyzer():
    """Provide a mock LLM analyzer for testing"""
    mock_analyzer = Mock()
    mock_analyzer.analyze_section.return_value = {
        "business_logic": "Mock business logic analysis",
        "confidence": 0.85
    }
    mock_analyzer.analyze_subsection.return_value = {
        "business_logic": "Mock subsection analysis",
        "confidence": 0.80
    }
    return mock_analyzer


@pytest.fixture
def mock_openai_client():
    """Provide a mock OpenAI client for testing"""
    with patch('openai.OpenAI') as mock_client:
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = "Description: Test business logic\nConfidence: 0.85"
        mock_response.usage = Mock()
        mock_response.usage.total_tokens = 100
        
        mock_client.return_value.chat.completions.create.return_value = mock_response
        yield mock_client


@pytest.fixture
def temp_output_dir():
    """Provide a temporary directory for output files"""
    with tempfile.TemporaryDirectory() as temp_dir:
        yield Path(temp_dir)


@pytest.fixture
def expected_sections():
    """Provide expected section names for validation"""
    return [
        "IDENTIFICATION DIVISION",
        "ENVIRONMENT DIVISION",
        "DATA DIVISION",
        "WORKING-STORAGE SECTION",
        "PROCEDURE DIVISION"
    ]


@pytest.fixture
def expected_subsections():
    """Provide expected subsection names for validation"""
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
def config_data():
    """Provide test configuration data"""
    return {
        "llm": {
            "provider": "openai",
            "model": "gpt-4",
            "max_tokens": 4000,
            "temperature": 0.1
        },
        "processing": {
            "chunk_size": 2000,
            "overlap": 200,
            "max_retries": 3,
            "confidence_threshold": 0.7
        },
        "output": {
            "format": "json",
            "include_confidence": True,
            "generate_visualization": False
        }
    }


@pytest.fixture(autouse=True)
def setup_test_environment():
    """Set up test environment before each test"""
    # Set test environment variables
    os.environ["TESTING"] = "true"
    os.environ["LOG_LEVEL"] = "DEBUG"
    
    yield
    
    # Cleanup after each test
    if "TESTING" in os.environ:
        del os.environ["TESTING"]


@pytest.fixture
def mock_file_operations():
    """Mock file operations for testing"""
    with patch('pathlib.Path.read_text') as mock_read, \
         patch('pathlib.Path.write_text') as mock_write, \
         patch('pathlib.Path.exists') as mock_exists:
        
        mock_exists.return_value = True
        mock_read.return_value = "Mock file content"
        
        yield {
            'read_text': mock_read,
            'write_text': mock_write,
            'exists': mock_exists
        }


@pytest.fixture
def performance_thresholds():
    """Provide performance thresholds for testing"""
    return {
        "max_processing_time": 10.0,  # seconds
        "max_memory_usage": 500,      # MB
        "min_accuracy": 0.8,          # 80%
        "max_error_rate": 0.05        # 5%
    }


@pytest.fixture
def validation_data():
    """Provide validation data for testing"""
    return {
        "fraud_file_expected_sections": 15,
        "fraud_file_expected_subsections": 50,
        "fraud_file_expected_relationships": 20,
        "min_confidence_threshold": 0.7,
        "max_processing_time": 5.0
    }


# Pytest configuration
def pytest_configure(config):
    """Configure pytest with custom markers"""
    config.addinivalue_line(
        "markers", "slow: marks tests as slow (deselect with '-m \"not slow\"')"
    )
    config.addinivalue_line(
        "markers", "integration: marks tests as integration tests"
    )
    config.addinivalue_line(
        "markers", "unit: marks tests as unit tests"
    )


def pytest_collection_modifyitems(config, items):
    """Modify test collection to add markers based on test names"""
    for item in items:
        # Add markers based on test file names
        if "test_integration" in item.nodeid:
            item.add_marker(pytest.mark.integration)
        elif "test_models" in item.nodeid or "test_cobol_parser" in item.nodeid:
            item.add_marker(pytest.mark.unit)
        
        # Add slow marker for performance tests
        if "performance" in item.name or "large_file" in item.name:
            item.add_marker(pytest.mark.slow)
