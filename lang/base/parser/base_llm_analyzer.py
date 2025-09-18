"""
Base LLM Analyzer
Abstract base class for language-specific LLM analyzers
"""

from abc import ABC, abstractmethod
from typing import List, Dict, Any, Optional
from dataclasses import dataclass


@dataclass
class BaseLLMAnalysisResult(ABC):
    """Abstract base class for LLM analysis results"""
    business_logic: str
    complexity_score: float
    risk_level: str
    confidence: float
    suggestions: List[str]
    language: str
    
    @abstractmethod
    def get_analysis_metrics(self) -> Dict[str, Any]:
        """Get analysis-specific metrics"""
        pass
    
    @abstractmethod
    def validate_analysis(self) -> List[str]:
        """Validate analysis results and return errors"""
        pass


class BaseLLMAnalyzer(ABC):
    """Abstract base class for language-specific LLM analyzers"""
    
    def __init__(self, language: str, api_key: Optional[str] = None, model: str = "gpt-4"):
        """Initialize base LLM analyzer"""
        self.language = language
        self.api_key = api_key
        self.model = model
        self.client = self._initialize_client()
    
    @abstractmethod
    def _initialize_client(self):
        """Initialize the LLM client (OpenAI, Anthropic, etc.)"""
        pass
    
    @abstractmethod
    def analyze_section(self, code: str, section_name: str, section_type: str) -> BaseLLMAnalysisResult:
        """Analyze a code section using LLM"""
        pass
    
    @abstractmethod
    def analyze_program(self, code: str, program_name: str) -> BaseLLMAnalysisResult:
        """Analyze a complete program using LLM"""
        pass
    
    @abstractmethod
    def _create_analysis_prompt(self, code: str, section_name: str, section_type: str) -> str:
        """Create a language-specific analysis prompt"""
        pass
    
    @abstractmethod
    def _parse_llm_response(self, response: str) -> BaseLLMAnalysisResult:
        """Parse LLM response into structured result"""
        pass
    
    def _validate_analysis_result(self, result: BaseLLMAnalysisResult) -> List[str]:
        """Validate analysis result for common issues"""
        errors = []
        
        # Validate confidence
        if not (0.0 <= result.confidence <= 1.0):
            errors.append(f"Invalid confidence score: {result.confidence}")
        
        # Validate complexity score
        if not (0.0 <= result.complexity_score <= 1.0):
            errors.append(f"Invalid complexity score: {result.complexity_score}")
        
        # Validate risk level
        valid_risk_levels = ["LOW", "MEDIUM", "HIGH", "CRITICAL"]
        if result.risk_level not in valid_risk_levels:
            errors.append(f"Invalid risk level: {result.risk_level}")
        
        # Validate business logic
        if not result.business_logic or len(result.business_logic.strip()) < 10:
            errors.append("Business logic description too short or empty")
        
        return errors
    
    def get_analysis_statistics(self, results: List[BaseLLMAnalysisResult]) -> Dict[str, Any]:
        """Get analysis statistics for multiple results"""
        if not results:
            return {"total_analyses": 0}
        
        confidences = [r.confidence for r in results]
        complexity_scores = [r.complexity_score for r in results]
        risk_levels = [r.risk_level for r in results]
        
        return {
            "total_analyses": len(results),
            "average_confidence": sum(confidences) / len(confidences),
            "average_complexity": sum(complexity_scores) / len(complexity_scores),
            "risk_distribution": {
                "LOW": risk_levels.count("LOW"),
                "MEDIUM": risk_levels.count("MEDIUM"),
                "HIGH": risk_levels.count("HIGH"),
                "CRITICAL": risk_levels.count("CRITICAL")
            },
            "language": self.language
        }
    
    def _create_generic_analysis_prompt(self, code: str, section_name: str, section_type: str) -> str:
        """Create a generic analysis prompt (can be overridden by subclasses)"""
        return f"""
Analyze this {self.language} {section_type} section named "{section_name}":

{code}

Please provide:
1. Business logic description (what this section does)
2. Complexity score (0.0 to 1.0, where 1.0 is most complex)
3. Risk level (LOW, MEDIUM, HIGH, CRITICAL)
4. Confidence in your analysis (0.0 to 1.0)
5. Suggestions for improvement or modernization

Format your response as:
BUSINESS_LOGIC: [description]
COMPLEXITY: [score]
RISK: [level]
CONFIDENCE: [score]
SUGGESTIONS: [list of suggestions]
"""
    
    def _parse_generic_llm_response(self, response: str) -> BaseLLMAnalysisResult:
        """Parse LLM response into structured result (generic implementation)"""
        lines = response.strip().split('\n')
        
        business_logic = "No analysis available"
        complexity_score = 0.5
        risk_level = "MEDIUM"
        confidence = 0.5
        suggestions = []
        
        for line in lines:
            line = line.strip()
            if line.startswith('BUSINESS_LOGIC:'):
                business_logic = line.replace('BUSINESS_LOGIC:', '').strip()
            elif line.startswith('COMPLEXITY:'):
                try:
                    complexity_score = float(line.replace('COMPLEXITY:', '').strip())
                except ValueError:
                    complexity_score = 0.5
            elif line.startswith('RISK:'):
                risk = line.replace('RISK:', '').strip().upper()
                if risk in ['LOW', 'MEDIUM', 'HIGH', 'CRITICAL']:
                    risk_level = risk
            elif line.startswith('CONFIDENCE:'):
                try:
                    confidence = float(line.replace('CONFIDENCE:', '').strip())
                except ValueError:
                    confidence = 0.5
            elif line.startswith('SUGGESTIONS:'):
                suggestions_text = line.replace('SUGGESTIONS:', '').strip()
                suggestions = [s.strip() for s in suggestions_text.split(',') if s.strip()]
        
        return BaseLLMAnalysisResult(
            business_logic=business_logic,
            complexity_score=complexity_score,
            risk_level=risk_level,
            confidence=confidence,
            suggestions=suggestions,
            language=self.language
        )
