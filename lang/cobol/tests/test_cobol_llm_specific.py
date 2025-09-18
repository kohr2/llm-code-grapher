"""
COBOL-specific LLM analyzer tests
Tests only COBOL-specific LLM analysis logic
"""

import pytest
from unittest.mock import Mock, patch
from lang.cobol.parser.llm_analyzer import COBOLAnalyzer, COBOLAnalysisResult
from lang.base.tests.base_llm_tests import BaseLLMAnalyzerTests


class TestCOBOLAnalyzerSpecific(BaseLLMAnalyzerTests):
    """Test cases for COBOL-specific LLM analyzer functionality"""
    
    def get_analyzer_class(self):
        """Get the analyzer class to test"""
        return COBOLAnalyzer
    
    def get_language(self) -> str:
        """Get the language being tested"""
        return "COBOL"
    
    def get_sample_code(self) -> str:
        """Get sample COBOL code for testing"""
        return """
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. TEST-PROGRAM.
000300 AUTHOR. TEST-AUTHOR.
000400 DATE-WRITTEN. 2025-01-01.

000700 ENVIRONMENT DIVISION.
000800 CONFIGURATION SECTION.
000900 SOURCE-COMPUTER. IBM-Z15.

004200 DATA DIVISION.
004300 WORKING-STORAGE SECTION.
004400 01 WS-COUNTER PIC 9(5) VALUE ZERO.
004500 01 WS-FLAG PIC X(1) VALUE 'N'.

021400 PROCEDURE DIVISION.
021500 0000-MAIN-PROCESS.
021600     MOVE 1 TO WS-COUNTER
021700     PERFORM 1000-PROCESS
021800     IF WS-FLAG = 'Y'
021900         DISPLAY 'PROCESSING COMPLETE'
022000     END-IF
022100     STOP RUN.

022300 1000-PROCESS.
022400     ADD 1 TO WS-COUNTER
022500     IF WS-COUNTER > 10
022600         MOVE 'Y' TO WS-FLAG
022700     END-IF.
"""
    
    def test_cobol_analyzer_initialization(self, analyzer):
        """Test COBOL analyzer initializes correctly"""
        assert analyzer is not None
        assert analyzer.language == "COBOL"
        assert analyzer.model == "gpt-4"
        assert hasattr(analyzer, 'client')
    
    def test_cobol_analysis_prompt_creation(self, analyzer):
        """Test COBOL-specific analysis prompt creation"""
        code = "MOVE 1 TO WS-COUNTER"
        section_name = "MAIN-PROCESS"
        section_type = "PROCEDURE"
        
        prompt = analyzer._create_analysis_prompt(code, section_name, section_type)
        
        assert isinstance(prompt, str)
        assert len(prompt) > 0
        assert "COBOL" in prompt
        assert section_name in prompt
        assert section_type in prompt
        assert code in prompt
        assert "BUSINESS_LOGIC:" in prompt
        assert "COMPLEXITY:" in prompt
        assert "RISK:" in prompt
        assert "CONFIDENCE:" in prompt
        assert "SUGGESTIONS:" in prompt
    
    def test_cobol_response_parsing(self, analyzer):
        """Test COBOL-specific response parsing"""
        response = """
BUSINESS_LOGIC: This COBOL section initializes variables and performs main processing logic
COMPLEXITY: 0.6
RISK: MEDIUM
CONFIDENCE: 0.85
SUGGESTIONS: Consider adding error handling, refactor complex IF statements
"""
        
        result = analyzer._parse_llm_response(response)
        
        assert isinstance(result, COBOLAnalysisResult)
        assert result.business_logic == "This COBOL section initializes variables and performs main processing logic"
        assert result.complexity_score == 0.6
        assert result.risk_level == "MEDIUM"
        assert result.confidence == 0.85
        assert len(result.suggestions) == 2
        assert "Consider adding error handling" in result.suggestions
        assert "refactor complex IF statements" in result.suggestions
        assert result.language == "COBOL"
    
    def test_cobol_analysis_result_creation(self, analyzer):
        """Test COBOL analysis result creation"""
        result = COBOLAnalysisResult(
            business_logic="COBOL main processing logic",
            complexity_score=0.7,
            risk_level="HIGH",
            confidence=0.9,
            suggestions=["Add error handling", "Refactor complex logic"],
            language="COBOL"
        )
        
        assert isinstance(result, COBOLAnalysisResult)
        assert result.business_logic == "COBOL main processing logic"
        assert result.complexity_score == 0.7
        assert result.risk_level == "HIGH"
        assert result.confidence == 0.9
        assert len(result.suggestions) == 2
        assert result.language == "COBOL"
    
    def test_cobol_analysis_metrics(self, analyzer):
        """Test COBOL analysis result metrics"""
        result = COBOLAnalysisResult(
            business_logic="COBOL processing logic for data validation",
            complexity_score=0.8,
            risk_level="HIGH",
            confidence=0.75,
            suggestions=["suggestion1", "suggestion2", "suggestion3"],
            language="COBOL"
        )
        
        metrics = result.get_analysis_metrics()
        
        assert isinstance(metrics, dict)
        assert metrics["business_logic_length"] == len("COBOL processing logic for data validation")
        assert metrics["suggestions_count"] == 3
        assert metrics["complexity_score"] == 0.8
        assert metrics["risk_level"] == "HIGH"
        assert metrics["confidence"] == 0.75
    
    def test_cobol_analysis_validation(self, analyzer):
        """Test COBOL analysis result validation"""
        # Test valid result
        valid_result = COBOLAnalysisResult(
            business_logic="Valid COBOL business logic description",
            complexity_score=0.5,
            risk_level="MEDIUM",
            confidence=0.8,
            suggestions=["suggestion1"],
            language="COBOL"
        )
        
        errors = valid_result.validate_analysis()
        assert len(errors) == 0
        
        # Test invalid complexity
        invalid_result = COBOLAnalysisResult(
            business_logic="Valid COBOL business logic description",
            complexity_score=1.5,  # Invalid
            risk_level="MEDIUM",
            confidence=0.8,
            suggestions=[],
            language="COBOL"
        )
        
        errors = invalid_result.validate_analysis()
        assert len(errors) == 1
        assert "Invalid complexity score" in errors[0]
    
    @patch('openai.OpenAI')
    def test_cobol_analyzer_with_mock_client(self, mock_openai, analyzer):
        """Test COBOL analyzer with mocked OpenAI client"""
        # Mock the OpenAI client
        mock_client = Mock()
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = """
BUSINESS_LOGIC: This COBOL section processes transaction data
COMPLEXITY: 0.7
RISK: HIGH
CONFIDENCE: 0.9
SUGGESTIONS: Add input validation, improve error handling
"""
        mock_client.chat.completions.create.return_value = mock_response
        mock_openai.return_value = mock_client
        
        # Test analysis
        code = "MOVE WS-DATA TO WS-OUTPUT"
        section_name = "PROCESS-DATA"
        section_type = "PROCEDURE"
        
        result = analyzer.analyze_section(code, section_name, section_type)
        
        assert isinstance(result, COBOLAnalysisResult)
        assert result.business_logic == "This COBOL section processes transaction data"
        assert result.complexity_score == 0.7
        assert result.risk_level == "HIGH"
        assert result.confidence == 0.9
        assert len(result.suggestions) == 2
        assert result.language == "COBOL"
    
    def test_cobol_analyzer_error_handling(self, analyzer):
        """Test COBOL analyzer error handling"""
        # Test with invalid input
        result = analyzer.analyze_section("", "", "")
        
        assert isinstance(result, COBOLAnalysisResult)
        assert "Analysis failed due to LLM error" in result.business_logic
        assert result.complexity_score == 0.5
        assert result.risk_level == "MEDIUM"
        assert result.confidence == 0.0
        assert len(result.suggestions) == 1
        assert "LLM analysis failed" in result.suggestions[0]
        assert result.language == "COBOL"
    
    def test_cobol_analyzer_statistics(self, analyzer):
        """Test COBOL analyzer statistics calculation"""
        results = [
            COBOLAnalysisResult(
                business_logic="COBOL data processing logic",
                complexity_score=0.3,
                risk_level="LOW",
                confidence=0.9,
                suggestions=["suggestion1"],
                language="COBOL"
            ),
            COBOLAnalysisResult(
                business_logic="COBOL validation logic",
                complexity_score=0.7,
                risk_level="HIGH",
                confidence=0.6,
                suggestions=["suggestion2", "suggestion3"],
                language="COBOL"
            ),
            COBOLAnalysisResult(
                business_logic="COBOL output formatting logic",
                complexity_score=0.5,
                risk_level="MEDIUM",
                confidence=0.8,
                suggestions=[],
                language="COBOL"
            )
        ]
        
        stats = analyzer.get_analysis_statistics(results)
        
        assert isinstance(stats, dict)
        assert stats["total_analyses"] == 3
        assert stats["average_confidence"] == (0.9 + 0.6 + 0.8) / 3
        assert stats["average_complexity"] == (0.3 + 0.7 + 0.5) / 3
        assert stats["language"] == "COBOL"
        
        risk_dist = stats["risk_distribution"]
        assert risk_dist["LOW"] == 1
        assert risk_dist["MEDIUM"] == 1
        assert risk_dist["HIGH"] == 1
        assert risk_dist["CRITICAL"] == 0
    
    def test_cobol_analyzer_prompt_specificity(self, analyzer):
        """Test that COBOL analyzer creates language-specific prompts"""
        code = "MOVE WS-INPUT TO WS-OUTPUT"
        section_name = "DATA-MOVEMENT"
        section_type = "PROCEDURE"
        
        prompt = analyzer._create_analysis_prompt(code, section_name, section_type)
        
        # Should contain COBOL-specific terminology
        assert "COBOL" in prompt
        assert "section" in prompt.lower()
        assert "business logic" in prompt.lower()
        assert "complexity" in prompt.lower()
        assert "risk" in prompt.lower()
        assert "confidence" in prompt.lower()
        assert "suggestions" in prompt.lower()
        
        # Should be specific to the provided code
        assert code in prompt
        assert section_name in prompt
        assert section_type in prompt
