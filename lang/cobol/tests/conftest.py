"""
Pytest configuration and fixtures for COBOL tests
"""

import pytest
from pathlib import Path


@pytest.fixture
def test_data_dir():
    """Path to test data directory"""
    return Path(__file__).parent / "test_fixtures"


@pytest.fixture
def sample_cobol_file(test_data_dir):
    """Path to sample COBOL file"""
    return test_data_dir / "sample_small.cbl"


@pytest.fixture
def fraud_cobol_file(test_data_dir):
    """Path to fraud management COBOL file"""
    return test_data_dir / "vasu_fraud_management_cobol_reformatted.cbl"


@pytest.fixture
def expected_output_file(test_data_dir):
    """Path to expected output JSON file"""
    return test_data_dir / "expected_output.json"
