"""
Pytest configuration and fixtures for LLM Code Grapher tests
Generic fixtures that work across all programming languages
"""

import pytest
import tempfile
import os
from pathlib import Path
from unittest.mock import Mock, patch


@pytest.fixture(scope="session")
def test_data_dir():
    """Provide path to test data directory"""
    return Path("data/fixtures")


@pytest.fixture(scope="session")
def sample_code_file_path():
    """Provide path to sample code files for testing"""
    return Path("data/fixtures")


@pytest.fixture
def sample_code_content():
    """Provide sample code content for testing (generic)"""
    return """
// Sample code for testing
public class TestClass {
    public void testMethod() {
        System.out.println("Hello World");
    }
}
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
def expected_code_structure():
    """Provide expected code structure for validation (generic)"""
    return {
        "sections": ["class", "method", "function", "module"],
        "subsections": ["constructor", "getter", "setter", "helper"],
        "relationships": ["inherits", "implements", "calls", "uses"]
    }


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
    """Provide validation data for testing (generic)"""
    return {
        "expected_sections": 10,
        "expected_subsections": 25,
        "expected_relationships": 15,
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
        elif "test_models" in item.nodeid or "test_parser" in item.nodeid or "test_ontology" in item.nodeid:
            item.add_marker(pytest.mark.unit)
        
        # Add slow marker for performance tests
        if "performance" in item.name or "large_file" in item.name:
            item.add_marker(pytest.mark.slow)
