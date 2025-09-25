"""
Test-Driven Development for Language-Agnostic Business Rule Analysis

This module tests the language-agnostic business rule analysis functionality
in the Neo4j converter using TDD principles.
"""

import pytest
import os
from unittest.mock import Mock, patch, MagicMock
from typing import Dict, Any, List

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
            complexity_score=0.6,
            risk_level=RiskLevel.MEDIUM
        )


@pytest.fixture
def mock_llm_config():
    """Create a mock LLM configuration for testing"""
    return LLMProviderConfig(
        provider="openai",
        model="gpt-4",
        api_key="test-key",
        max_tokens=2000,
        temperature=0.1
    )


@pytest.fixture
def mock_analyzer():
    """Create a mock analyzer for testing"""
    analyzer = Mock()
    analyzer.provider = Mock()
    return analyzer


@pytest.fixture
def converter_with_mock_analyzer(mock_llm_config, mock_analyzer):
    """Create a converter with mocked analyzer"""
    with patch('src.neo4j_converter.ParserResultConverter._get_language_analyzer', return_value=mock_analyzer):
        converter = ParserResultConverter(mock_llm_config, "cobol")
        converter.analyzer = mock_analyzer
        return converter


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


class TestBusinessRuleDetection:
    """Test business rule detection functionality"""
    
    def test_is_business_rule_with_real_llm_yes_response(self, real_converter):
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
    
    def test_is_business_rule_with_real_llm_no_response(self, real_converter):
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
    
    def test_is_business_rule_with_llm_yes_response(self, converter_with_mock_analyzer):
        """Test that LLM correctly identifies a business rule"""
        # Arrange
        subsection = MockSubsection(
            "VALIDATE-AMOUNT",
            "VALIDATION-SECTION",
            (100, 120),
            "IF amount > 10000 THEN reject transaction"
        )
        
        converter_with_mock_analyzer.analyzer.provider.generate_response.return_value = "YES"
        
        # Act
        result = converter_with_mock_analyzer._is_business_rule(subsection)
        
        # Assert
        assert result is True
        converter_with_mock_analyzer.analyzer.provider.generate_response.assert_called_once()
    
    def test_is_business_rule_with_llm_no_response(self, converter_with_mock_analyzer):
        """Test that LLM correctly identifies non-business rule code"""
        # Arrange
        subsection = MockSubsection(
            "DISPLAY-MESSAGE",
            "UI-SECTION",
            (100, 120),
            "DISPLAY 'Hello World'"
        )
        
        converter_with_mock_analyzer.analyzer.provider.generate_response.return_value = "NO"
        
        # Act
        result = converter_with_mock_analyzer._is_business_rule(subsection)
        
        # Assert
        assert result is False
    
    def test_is_business_rule_fallback_patterns(self, converter_with_mock_analyzer):
        """Test fallback pattern detection when LLM fails"""
        # Arrange
        subsection = MockSubsection(
            "RULE-VALIDATION",
            "VALIDATION-SECTION",
            (100, 120),
            "IF condition THEN action"
        )
        
        converter_with_mock_analyzer.analyzer.provider.generate_response.side_effect = Exception("LLM Error")
        
        # Act
        result = converter_with_mock_analyzer._is_business_rule(subsection)
        
        # Assert
        assert result is True  # Should fallback to pattern matching
    
    def test_is_business_rule_no_business_logic(self, converter_with_mock_analyzer):
        """Test that subsection without business logic is not a business rule"""
        # Arrange
        subsection = MockSubsection(
            "EMPTY-SECTION",
            "SECTION",
            (100, 120),
            ""
        )
        
        # Act
        result = converter_with_mock_analyzer._is_business_rule(subsection)
        
        # Assert
        assert result is False
    
    def test_is_business_rule_no_name(self, converter_with_mock_analyzer):
        """Test that subsection without name is not a business rule"""
        # Arrange
        subsection = MockSubsection(
            "",
            "SECTION",
            (100, 120),
            "IF condition THEN action"
        )
        
        # Act
        result = converter_with_mock_analyzer._is_business_rule(subsection)
        
        # Assert
        assert result is False


class TestBusinessRuleAnalysis:
    """Test business rule analysis functionality"""
    
    def test_analyze_business_rule_with_real_llm(self, real_converter):
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
    
    def test_analyze_business_rule_success(self, converter_with_mock_analyzer):
        """Test successful business rule analysis"""
        # Arrange
        business_logic = "IF amount > 10000 THEN reject transaction"
        rule_name = "HIGH-AMOUNT-CHECK"
        
        mock_response = """
DESCRIPTION: Validates transaction amounts against high-value threshold
PRIORITY: HIGH
RISK_LEVEL: MEDIUM
FUNCTIONAL_AREA: AMOUNT-VALIDATION
CONFIDENCE: 0.9
"""
        
        converter_with_mock_analyzer.analyzer.provider.generate_response.return_value = mock_response
        
        # Act
        result = converter_with_mock_analyzer._analyze_business_rule_with_llm(business_logic, rule_name)
        
        # Assert
        assert result["description"] == "Validates transaction amounts against high-value threshold"
        assert result["priority"] == "HIGH"
        assert result["risk_level"] == "MEDIUM"
        assert result["functional_area"] == "AMOUNT-VALIDATION"
        assert result["confidence"] == 0.9
    
    def test_analyze_business_rule_invalid_priority(self, converter_with_mock_analyzer):
        """Test analysis with invalid priority falls back to default"""
        # Arrange
        business_logic = "IF condition THEN action"
        rule_name = "TEST-RULE"
        
        mock_response = """
DESCRIPTION: Test rule description
PRIORITY: INVALID
RISK_LEVEL: HIGH
FUNCTIONAL_AREA: TEST-AREA
CONFIDENCE: 0.8
"""
        
        converter_with_mock_analyzer.analyzer.provider.generate_response.return_value = mock_response
        
        # Act
        result = converter_with_mock_analyzer._analyze_business_rule_with_llm(business_logic, rule_name)
        
        # Assert
        assert result["priority"] == "MEDIUM"  # Should fallback to default
    
    def test_analyze_business_rule_invalid_confidence(self, converter_with_mock_analyzer):
        """Test analysis with invalid confidence falls back to default"""
        # Arrange
        business_logic = "IF condition THEN action"
        rule_name = "TEST-RULE"
        
        mock_response = """
DESCRIPTION: Test rule description
PRIORITY: HIGH
RISK_LEVEL: MEDIUM
FUNCTIONAL_AREA: TEST-AREA
CONFIDENCE: invalid
"""
        
        converter_with_mock_analyzer.analyzer.provider.generate_response.return_value = mock_response
        
        # Act
        result = converter_with_mock_analyzer._analyze_business_rule_with_llm(business_logic, rule_name)
        
        # Assert
        assert result["confidence"] == 0.5  # Should fallback to default
    
    def test_analyze_business_rule_no_analyzer(self, mock_llm_config):
        """Test that analysis fails when no analyzer is available"""
        # Arrange
        converter = ParserResultConverter(mock_llm_config, "cobol")
        converter.analyzer = None
        
        # Act & Assert
        with pytest.raises(RuntimeError, match="LLM analyzer is required"):
            converter._analyze_business_rule_with_llm("test logic", "test rule")
    
    def test_analyze_business_rule_llm_error(self, converter_with_mock_analyzer):
        """Test that LLM errors are properly handled"""
        # Arrange
        converter_with_mock_analyzer.analyzer.provider.generate_response.side_effect = Exception("LLM Error")
        
        # Act & Assert
        with pytest.raises(RuntimeError, match="LLM analysis failed"):
            converter_with_mock_analyzer._analyze_business_rule_with_llm("test logic", "test rule")


class TestLanguageAgnosticFunctionality:
    """Test language-agnostic functionality"""
    
    def test_cobol_analyzer_initialization(self, mock_llm_config):
        """Test that COBOL analyzer is properly initialized"""
        # Arrange & Act
        with patch('src.neo4j_converter.ParserResultConverter._get_language_analyzer') as mock_get_analyzer:
            converter = ParserResultConverter(mock_llm_config, "cobol")
            
        # Assert
        mock_get_analyzer.assert_called_once()
    
    def test_java_analyzer_initialization(self, mock_llm_config):
        """Test that Java analyzer is properly initialized"""
        # Arrange & Act
        with patch('src.neo4j_converter.ParserResultConverter._get_language_analyzer') as mock_get_analyzer:
            converter = ParserResultConverter(mock_llm_config, "java")
            
        # Assert
        mock_get_analyzer.assert_called_once()
    
    def test_python_analyzer_initialization(self, mock_llm_config):
        """Test that Python analyzer is properly initialized"""
        # Arrange & Act
        with patch('src.neo4j_converter.ParserResultConverter._get_language_analyzer') as mock_get_analyzer:
            converter = ParserResultConverter(mock_llm_config, "python")
            
        # Assert
        mock_get_analyzer.assert_called_once()
    
    def test_language_agnostic_prompt(self, converter_with_mock_analyzer):
        """Test that prompt includes the correct language"""
        # Arrange
        business_logic = "IF condition THEN action"
        rule_name = "TEST-RULE"
        
        # Act
        prompt = converter_with_mock_analyzer._create_business_rule_analysis_prompt(business_logic, rule_name)
        
        # Assert
        assert "COBOL" in prompt
        assert rule_name in prompt
        assert business_logic in prompt
    
    def test_language_agnostic_prompt_java(self, mock_llm_config, mock_analyzer):
        """Test that Java prompt includes the correct language"""
        # Arrange
        with patch('src.neo4j_converter.ParserResultConverter._get_language_analyzer', return_value=mock_analyzer):
            converter = ParserResultConverter(mock_llm_config, "java")
            converter.analyzer = mock_analyzer
        
        business_logic = "if (amount > 10000) { rejectTransaction(); }"
        rule_name = "HighAmountCheck"
        
        # Act
        prompt = converter._create_business_rule_analysis_prompt(business_logic, rule_name)
        
        # Assert
        assert "JAVA" in prompt
        assert rule_name in prompt
        assert business_logic in prompt


class TestFunctionalAreaMapping:
    """Test functional area mapping functionality"""
    
    def test_get_logical_section_with_functional_area(self, converter_with_mock_analyzer):
        """Test that functional area from LLM is used correctly"""
        # Arrange
        sections = [
            MockSection("DATA-VALIDATION", "SECTION", (100, 200)),
            MockSection("BUSINESS-LOGIC", "SECTION", (201, 300))
        ]
        
        # Act
        result = converter_with_mock_analyzer._get_logical_section_for_rule(
            "TEST-RULE", sections, "DATA-VALIDATION"
        )
        
        # Assert
        assert result is not None
        assert result.name == "DATA-VALIDATION"
    
    def test_get_logical_section_no_functional_area(self, converter_with_mock_analyzer):
        """Test that None is returned when no functional area is provided"""
        # Arrange
        sections = [
            MockSection("DATA-VALIDATION", "SECTION", (100, 200))
        ]
        
        # Act
        result = converter_with_mock_analyzer._get_logical_section_for_rule(
            "TEST-RULE", sections, None
        )
        
        # Assert
        assert result is None
    
    def test_get_logical_section_case_insensitive_match(self, converter_with_mock_analyzer):
        """Test that functional area matching is case insensitive"""
        # Arrange
        sections = [
            MockSection("data-validation", "SECTION", (100, 200))
        ]
        
        # Act
        result = converter_with_mock_analyzer._get_logical_section_for_rule(
            "TEST-RULE", sections, "DATA-VALIDATION"
        )
        
        # Assert
        assert result is not None
        assert result.name == "data-validation"


class TestBusinessRulePatterns:
    """Test business rule pattern detection"""
    
    def test_has_business_rule_patterns_rule_keyword(self, converter_with_mock_analyzer):
        """Test detection of RULE keyword in name"""
        # Arrange
        subsection = MockSubsection(
            "RULE-VALIDATION",
            "SECTION",
            (100, 120),
            "Some code"
        )
        
        # Act
        result = converter_with_mock_analyzer._has_business_rule_patterns(subsection)
        
        # Assert
        assert result is True
    
    def test_has_business_rule_patterns_validate_keyword(self, converter_with_mock_analyzer):
        """Test detection of VALIDATE keyword in logic"""
        # Arrange
        subsection = MockSubsection(
            "SOME-SECTION",
            "SECTION",
            (100, 120),
            "VALIDATE input data"
        )
        
        # Act
        result = converter_with_mock_analyzer._has_business_rule_patterns(subsection)
        
        # Assert
        assert result is True
    
    def test_has_business_rule_patterns_no_match(self, converter_with_mock_analyzer):
        """Test that non-business rule patterns return False"""
        # Arrange
        subsection = MockSubsection(
            "DISPLAY-SECTION",
            "SECTION",
            (100, 120),
            "DISPLAY 'Hello World'"
        )
        
        # Act
        result = converter_with_mock_analyzer._has_business_rule_patterns(subsection)
        
        # Assert
        assert result is False


class TestErrorHandling:
    """Test error handling functionality"""
    
    def test_converter_requires_llm_config(self):
        """Test that converter requires LLM configuration"""
        # Act & Assert
        with pytest.raises(ValueError, match="LLM configuration is required"):
            ParserResultConverter(None, "cobol")
    
    def test_converter_handles_analyzer_initialization_failure(self, mock_llm_config):
        """Test that converter handles analyzer initialization failure"""
        # Arrange
        with patch('src.neo4j_converter.ParserResultConverter._get_language_analyzer', side_effect=Exception("Import Error")):
            # Act & Assert
            with pytest.raises(RuntimeError, match="Failed to initialize LLM analyzer"):
                ParserResultConverter(mock_llm_config, "cobol")


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
