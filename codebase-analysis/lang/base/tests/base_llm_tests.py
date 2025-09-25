"""
Base LLM Analyzer Tests
Abstract base class for language-specific LLM analyzer tests
"""

import pytest
from abc import ABC, abstractmethod
from typing import List, Dict, Any
from unittest.mock import Mock, patch

from ..parser import BaseLLMAnalyzer, BaseLLMAnalysisResult


class BaseLLMAnalyzerTests(ABC):
    """Abstract base class for LLM analyzer tests"""
    
    @abstractmethod
    def get_analyzer_class(self):
        """Get the analyzer class to test"""
        pass
    
    @abstractmethod
    def get_language(self) -> str:
        """Get the language being tested"""
        pass
    
    @abstractmethod
    def get_sample_code(self) -> str:
        """Get sample code for testing"""
        pass
    
    @pytest.fixture
    def analyzer(self):
        """Create analyzer instance for testing"""
        return self.get_analyzer_class()()
    
    @pytest.fixture
    def sample_code(self):
        """Get sample code for testing"""
        return self.get_sample_code()
    
    def test_analyzer_initialization(self, analyzer):
        """Test analyzer initializes correctly"""
        assert analyzer is not None
        assert analyzer.language == self.get_language()
        assert hasattr(analyzer, 'model')
        assert hasattr(analyzer, 'client')
    
    def test_generic_analysis_prompt_creation(self, analyzer):
        """Test generic analysis prompt creation"""
        code = "SAMPLE CODE"
        section_name = "TEST-SECTION"
        section_type = "PROCEDURE"
        
        prompt = analyzer._create_generic_analysis_prompt(code, section_name, section_type)
        
        assert isinstance(prompt, str)
        assert len(prompt) > 0
        assert self.get_language() in prompt
        assert section_name in prompt
        assert section_type in prompt
        assert code in prompt
        assert "BUSINESS_LOGIC:" in prompt
        assert "COMPLEXITY:" in prompt
        assert "RISK:" in prompt
        assert "CONFIDENCE:" in prompt
        assert "SUGGESTIONS:" in prompt
    
    def test_generic_response_parsing(self, analyzer):
        """Test generic response parsing"""
        response = """
BUSINESS_LOGIC: This section processes user input and validates data
COMPLEXITY: 0.7
RISK: HIGH
CONFIDENCE: 0.8
SUGGESTIONS: Add error handling, refactor complex logic
"""
        
        result = analyzer._parse_generic_llm_response(response)
        
        assert isinstance(result, BaseLLMAnalysisResult)
        assert result.business_logic == "This section processes user input and validates data"
        assert result.complexity_score == 0.7
        assert result.risk_level == "HIGH"
        assert result.confidence == 0.8
        assert len(result.suggestions) == 2
        assert "Add error handling" in result.suggestions
        assert "refactor complex logic" in result.suggestions
        assert result.language == self.get_language()
    
    def test_response_parsing_with_missing_fields(self, analyzer):
        """Test response parsing with missing fields"""
        response = """
BUSINESS_LOGIC: Basic processing
COMPLEXITY: invalid_score
RISK: INVALID_RISK
"""
        
        result = analyzer._parse_generic_llm_response(response)
        
        assert isinstance(result, BaseLLMAnalysisResult)
        assert result.business_logic == "Basic processing"
        assert result.complexity_score == 0.5  # Default value
        assert result.risk_level == "MEDIUM"  # Default value
        assert result.confidence == 0.5  # Default value
        assert result.suggestions == []  # Default value
    
    def test_response_parsing_with_empty_response(self, analyzer):
        """Test response parsing with empty response"""
        result = analyzer._parse_generic_llm_response("")
        
        assert isinstance(result, BaseLLMAnalysisResult)
        assert result.business_logic == "No analysis available"
        assert result.complexity_score == 0.5
        assert result.risk_level == "MEDIUM"
        assert result.confidence == 0.5
        assert result.suggestions == []
    
    def test_analysis_result_validation(self, analyzer):
        """Test analysis result validation"""
        # Test valid result
        valid_result = BaseLLMAnalysisResult(
            business_logic="Valid business logic description",
            complexity_score=0.5,
            risk_level="MEDIUM",
            confidence=0.8,
            suggestions=["suggestion1", "suggestion2"],
            language=self.get_language()
        )
        
        errors = analyzer._validate_analysis_result(valid_result)
        assert len(errors) == 0
        
        # Test invalid confidence
        invalid_confidence = BaseLLMAnalysisResult(
            business_logic="Valid business logic description",
            complexity_score=0.5,
            risk_level="MEDIUM",
            confidence=1.5,  # Invalid
            suggestions=[],
            language=self.get_language()
        )
        
        errors = analyzer._validate_analysis_result(invalid_confidence)
        assert len(errors) == 1
        assert "Invalid confidence score" in errors[0]
        
        # Test invalid complexity
        invalid_complexity = BaseLLMAnalysisResult(
            business_logic="Valid business logic description",
            complexity_score=2.0,  # Invalid
            risk_level="MEDIUM",
            confidence=0.8,
            suggestions=[],
            language=self.get_language()
        )
        
        errors = analyzer._validate_analysis_result(invalid_complexity)
        assert len(errors) == 1
        assert "Invalid complexity score" in errors[0]
        
        # Test invalid risk level
        invalid_risk = BaseLLMAnalysisResult(
            business_logic="Valid business logic description",
            complexity_score=0.5,
            risk_level="INVALID",  # Invalid
            confidence=0.8,
            suggestions=[],
            language=self.get_language()
        )
        
        errors = analyzer._validate_analysis_result(invalid_risk)
        assert len(errors) == 1
        assert "Invalid risk level" in errors[0]
        
        # Test short business logic
        short_logic = BaseLLMAnalysisResult(
            business_logic="Short",  # Too short
            complexity_score=0.5,
            risk_level="MEDIUM",
            confidence=0.8,
            suggestions=[],
            language=self.get_language()
        )
        
        errors = analyzer._validate_analysis_result(short_logic)
        assert len(errors) == 1
        assert "Business logic description too short" in errors[0]
    
    def test_analysis_statistics(self, analyzer):
        """Test analysis statistics calculation"""
        results = [
            BaseLLMAnalysisResult(
                business_logic="Logic 1",
                complexity_score=0.3,
                risk_level="LOW",
                confidence=0.9,
                suggestions=["suggestion1"],
                language=self.get_language()
            ),
            BaseLLMAnalysisResult(
                business_logic="Logic 2",
                complexity_score=0.7,
                risk_level="HIGH",
                confidence=0.6,
                suggestions=["suggestion2", "suggestion3"],
                language=self.get_language()
            ),
            BaseLLMAnalysisResult(
                business_logic="Logic 3",
                complexity_score=0.5,
                risk_level="MEDIUM",
                confidence=0.8,
                suggestions=[],
                language=self.get_language()
            )
        ]
        
        stats = analyzer.get_analysis_statistics(results)
        
        assert isinstance(stats, dict)
        assert stats["total_analyses"] == 3
        assert stats["average_confidence"] == (0.9 + 0.6 + 0.8) / 3
        assert stats["average_complexity"] == (0.3 + 0.7 + 0.5) / 3
        assert stats["language"] == self.get_language()
        
        risk_dist = stats["risk_distribution"]
        assert risk_dist["LOW"] == 1
        assert risk_dist["MEDIUM"] == 1
        assert risk_dist["HIGH"] == 1
        assert risk_dist["CRITICAL"] == 0
    
    def test_analysis_statistics_empty(self, analyzer):
        """Test analysis statistics with empty results"""
        stats = analyzer.get_analysis_statistics([])
        
        assert isinstance(stats, dict)
        assert stats["total_analyses"] == 0
    
    def test_analysis_result_metrics(self, analyzer):
        """Test analysis result metrics"""
        result = BaseLLMAnalysisResult(
            business_logic="Test business logic for metrics",
            complexity_score=0.6,
            risk_level="MEDIUM",
            confidence=0.7,
            suggestions=["suggestion1", "suggestion2"],
            language=self.get_language()
        )
        
        metrics = result.get_analysis_metrics()
        
        assert isinstance(metrics, dict)
        assert metrics["business_logic_length"] == len("Test business logic for metrics")
        assert metrics["suggestions_count"] == 2
        assert metrics["complexity_score"] == 0.6
        assert metrics["risk_level"] == "MEDIUM"
        assert metrics["confidence"] == 0.7
    
    def test_analysis_result_validation_method(self, analyzer):
        """Test analysis result validation method"""
        result = BaseLLMAnalysisResult(
            business_logic="Valid business logic description",
            complexity_score=0.5,
            risk_level="MEDIUM",
            confidence=0.8,
            suggestions=["suggestion1"],
            language=self.get_language()
        )
        
        errors = result.validate_analysis()
        assert len(errors) == 0
        
        # Test with invalid values
        result.complexity_score = 1.5  # Invalid
        errors = result.validate_analysis()
        assert len(errors) == 1
        assert "Invalid complexity score" in errors[0]
