"""
Real API Tests for LLM Business Rule Analysis

These tests use the actual OPENAI_API_KEY to make real API calls to OpenAI.
This ensures the LLM integration works correctly with the real service.
"""

import pytest
import os
from pathlib import Path
from typing import Dict, Any
from unittest.mock import Mock

# Add src to path for imports
import sys
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from src.neo4j_converter import ParserResultConverter
from lang.base.parser.llm_provider import LLMProviderConfig
from lang.base.ontology.base_models import BaseProgram, BaseSection, BaseSubsection, RiskLevel


class MockProgram(BaseProgram):
    """Concrete implementation of BaseProgram for testing"""
    
    def __init__(self, name: str, language: str):
        super().__init__(name=name, language=language, metadata={})
        self.line_count = 1000
    
    def get_complexity_metrics(self) -> Dict[str, float]:
        return {"cyclomatic_complexity": 5.2, "cognitive_complexity": 3.8}
    
    def get_quality_indicators(self) -> Dict[str, Any]:
        return {"maintainability": "MEDIUM", "testability": 0.7}


class MockSection(BaseSection):
    """Concrete implementation of BaseSection for testing"""
    
    def __init__(self, name: str, section_type: str, line_range: tuple[int, int]):
        super().__init__(
            name=name,
            type=section_type,
            line_range=line_range,
            line_count=line_range[1] - line_range[0] + 1,
            business_logic=f"Business logic for {name}",
            confidence=0.8,
            complexity_score=0.5,
            risk_level=RiskLevel.MEDIUM
        )


class MockSubsection(BaseSubsection):
    """Concrete implementation of BaseSubsection for testing"""
    
    def __init__(self, name: str, parent_section: str, line_range: tuple[int, int], business_logic: str):
        super().__init__(
            name=name,
            parent_section=parent_section,
            line_range=line_range,
            line_count=line_range[1] - line_range[0] + 1,
            business_logic=business_logic,
            confidence=0.8,
            complexity_score=0.5,
            risk_level=RiskLevel.MEDIUM
        )


@pytest.fixture
def real_llm_config():
    """Create real LLM configuration using environment API key"""
    # Load .env file to ensure API key is available
    from dotenv import load_dotenv
    load_dotenv()
    
    api_key = os.getenv('OPENAI_API_KEY')
    if not api_key:
        raise ValueError("OPENAI_API_KEY not found in environment - please set it in .env file")
    
    return LLMProviderConfig(
        provider="openai",
        model="gpt-4",
        api_key=api_key,
        max_tokens=2000,
        temperature=0.1
    )


@pytest.fixture
def real_converter(real_llm_config):
    """Create a converter with real LLM analyzer"""
    converter = ParserResultConverter(real_llm_config, "cobol")
    return converter


class TestRealLLMBusinessRuleDetection:
    """Test business rule detection with real OpenAI API calls"""
    
    def test_real_business_rule_detection_yes(self, real_converter):
        """Test that real LLM correctly identifies a business rule"""
        # Arrange - Create a subsection that should be identified as a business rule
        subsection = MockSubsection(
            "VALIDATE-HIGH-AMOUNT",
            "FRAUD-DETECTION-SECTION",
            (100, 120),
            """
            IF WS-TRANSACTION-AMOUNT > WS-HIGH-AMOUNT-THRESHOLD
                SET WS-FRAUD-FLAG TO TRUE
                MOVE 'HIGH AMOUNT TRANSACTION DETECTED' TO WS-ERROR-MESSAGE
                PERFORM 9000-LOG-FRAUD-ATTEMPT
            END-IF
            """
        )
        
        # Act
        result = real_converter._is_business_rule(subsection)
        
        # Assert
        assert result is True, "Real LLM should identify this as a business rule"
    
    def test_real_business_rule_detection_no(self, real_converter):
        """Test that real LLM correctly identifies non-business rule code"""
        # Arrange - Create a subsection that should NOT be identified as a business rule
        subsection = MockSubsection(
            "DISPLAY-WELCOME-MESSAGE",
            "UI-SECTION",
            (100, 120),
            """
            DISPLAY 'WELCOME TO THE BANKING SYSTEM'
            DISPLAY 'PLEASE ENTER YOUR ACCOUNT NUMBER'
            ACCEPT WS-ACCOUNT-NUMBER
            """
        )
        
        # Act
        result = real_converter._is_business_rule(subsection)
        
        # Assert
        assert result is False, "Real LLM should NOT identify this as a business rule"
    
    def test_real_business_rule_analysis(self, real_converter):
        """Test real business rule analysis with structured output"""
        # Arrange
        business_logic = """
        IF WS-TRANSACTION-AMOUNT > WS-HIGH-AMOUNT-THRESHOLD
            SET WS-FRAUD-FLAG TO TRUE
            MOVE 'HIGH AMOUNT TRANSACTION DETECTED' TO WS-ERROR-MESSAGE
            PERFORM 9000-LOG-FRAUD-ATTEMPT
            PERFORM 8000-SEND-ALERT-TO-SECURITY
        END-IF
        """
        rule_name = "VALIDATE-HIGH-AMOUNT-TRANSACTION"
        
        # Act
        result = real_converter._analyze_business_rule_with_llm(business_logic, rule_name)
        
        # Assert
        assert "description" in result
        assert "priority" in result
        assert "risk_level" in result
        assert "functional_area" in result
        assert "confidence" in result
        
        # Verify the analysis makes sense
        assert result["priority"] in ["HIGH", "MEDIUM", "LOW"]
        assert result["risk_level"] in ["CRITICAL", "HIGH", "MEDIUM", "LOW"]
        assert result["confidence"] >= 0.0 and result["confidence"] <= 1.0
        assert len(result["description"]) > 10, "Description should be meaningful"
        assert len(result["functional_area"]) > 0, "Functional area should be specified"
        
        print(f"Real LLM Analysis Result: {result}")
    
    def test_real_business_rule_analysis_fraud_detection(self, real_converter):
        """Test real analysis of fraud detection business rule"""
        # Arrange - More complex fraud detection logic
        business_logic = """
        EVALUATE WS-TRANSACTION-TYPE
            WHEN 'WITHDRAWAL'
                IF WS-TRANSACTION-AMOUNT > WS-DAILY-LIMIT
                    SET WS-FRAUD-FLAG TO TRUE
                    MOVE 'EXCEEDS DAILY LIMIT' TO WS-ERROR-MESSAGE
                END-IF
            WHEN 'TRANSFER'
                IF WS-TRANSACTION-AMOUNT > WS-TRANSFER-LIMIT
                    SET WS-FRAUD-FLAG TO TRUE
                    MOVE 'EXCEEDS TRANSFER LIMIT' TO WS-ERROR-MESSAGE
                END-IF
        END-EVALUATE
        
        IF WS-FRAUD-FLAG = TRUE
            PERFORM 9000-LOG-FRAUD-ATTEMPT
            PERFORM 8000-SEND-ALERT-TO-SECURITY
            PERFORM 7000-BLOCK-ACCOUNT
        END-IF
        """
        rule_name = "FRAUD-DETECTION-MULTI-TYPE"
        
        # Act
        result = real_converter._analyze_business_rule_with_llm(business_logic, rule_name)
        
        # Assert
        assert "description" in result
        assert "priority" in result
        assert "risk_level" in result
        assert "functional_area" in result
        assert "confidence" in result
        
        # Verify fraud detection specific analysis
        assert "fraud" in result["description"].lower() or "security" in result["description"].lower()
        assert result["priority"] in ["HIGH", "MEDIUM", "CRITICAL"], "Priority should be reasonable"
        assert result["risk_level"] in ["CRITICAL", "HIGH", "MEDIUM"], "Risk level should be reasonable"
        
        print(f"Fraud Detection Analysis: {result}")
    
    def test_real_business_rule_analysis_data_validation(self, real_converter):
        """Test real analysis of data validation business rule"""
        # Arrange - Data validation logic
        business_logic = """
        IF WS-ACCOUNT-NUMBER IS NUMERIC
            AND WS-ACCOUNT-NUMBER > ZERO
            AND WS-ACCOUNT-NUMBER <= 999999999
            CONTINUE
        ELSE
            SET WS-VALIDATION-ERROR TO TRUE
            MOVE 'INVALID ACCOUNT NUMBER FORMAT' TO WS-ERROR-MESSAGE
            PERFORM 6000-DISPLAY-ERROR-MESSAGE
        END-IF
        
        IF WS-PIN-NUMBER IS NUMERIC
            AND WS-PIN-NUMBER >= 1000
            AND WS-PIN-NUMBER <= 9999
            CONTINUE
        ELSE
            SET WS-VALIDATION-ERROR TO TRUE
            MOVE 'INVALID PIN FORMAT' TO WS-ERROR-MESSAGE
            PERFORM 6000-DISPLAY-ERROR-MESSAGE
        END-IF
        """
        rule_name = "VALIDATE-ACCOUNT-DATA"
        
        # Act
        result = real_converter._analyze_business_rule_with_llm(business_logic, rule_name)
        
        # Assert
        assert "description" in result
        assert "priority" in result
        assert "risk_level" in result
        assert "functional_area" in result
        assert "confidence" in result
        
        # Verify data validation specific analysis
        assert "validation" in result["description"].lower() or "format" in result["description"].lower()
        assert "validation" in result["functional_area"].lower(), "Should identify as validation"
        
        print(f"Data Validation Analysis: {result}")


class TestRealLLMIntegration:
    """Test real LLM integration with different languages"""
    
    def test_cobol_analyzer_real_api(self, real_llm_config):
        """Test that COBOL analyzer works with real API"""
        # Arrange & Act
        converter = ParserResultConverter(real_llm_config, "cobol")
        
        # Assert
        assert converter.analyzer is not None, "COBOL analyzer should be initialized"
        assert converter.analyzer.provider is not None, "Provider should be available"
    
    def test_java_analyzer_real_api(self, real_llm_config):
        """Test that Java analyzer works with real API"""
        # Arrange & Act - Java analyzer not implemented yet, so this should fail gracefully
        with pytest.raises(RuntimeError, match="Failed to initialize LLM analyzer for java"):
            ParserResultConverter(real_llm_config, "java")
    
    def test_python_analyzer_real_api(self, real_llm_config):
        """Test that Python analyzer works with real API"""
        # Arrange & Act - Python analyzer not implemented yet, so this should fail gracefully
        with pytest.raises(RuntimeError, match="Failed to initialize LLM analyzer for python"):
            ParserResultConverter(real_llm_config, "python")


class TestRealLLMErrorHandling:
    """Test error handling with real API calls"""
    
    def test_real_api_key_validation(self):
        """Test that API key validation works"""
        # Test with missing API key
        with pytest.raises(ValueError, match="LLM configuration is required"):
            ParserResultConverter(None, "cobol")
        
        # Test with empty API key
        empty_config = LLMProviderConfig(
            provider="openai",
            model="gpt-4",
            api_key="",
            max_tokens=2000,
            temperature=0.1
        )
        
        # Test with empty API key - should fall back to pattern matching
        converter = ParserResultConverter(empty_config, "cobol")
        subsection = MockSubsection("TEST", "TEST", (1, 10), "TEST CODE")
        
        # This should fall back to pattern matching due to invalid API key
        result = converter._is_business_rule(subsection)
        # Should return False since "TEST" doesn't match business rule patterns
        assert result is False, "Should fall back to pattern matching and return False"


# Mark tests that require real API calls
pytestmark = pytest.mark.integration
