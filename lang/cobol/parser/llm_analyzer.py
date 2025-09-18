"""
LLM Analyzer for COBOL
Uses LLM to analyze COBOL code and extract semantic information
"""

from typing import List, Dict, Any, Optional
from dataclasses import dataclass
import openai

from lang.base.parser import BaseLLMAnalyzer, BaseLLMAnalysisResult


@dataclass
class COBOLAnalysisResult(BaseLLMAnalysisResult):
    """Result of COBOL LLM analysis"""
    def get_analysis_metrics(self) -> Dict[str, Any]:
        """Get analysis-specific metrics"""
        return {
            "business_logic_length": len(self.business_logic),
            "suggestions_count": len(self.suggestions),
            "complexity_score": self.complexity_score,
            "risk_level": self.risk_level,
            "confidence": self.confidence
        }
    
    def validate_analysis(self) -> List[str]:
        """Validate analysis results and return errors"""
        errors = []
        
        # Validate confidence
        if not (0.0 <= self.confidence <= 1.0):
            errors.append(f"Invalid confidence score: {self.confidence}")
        
        # Validate complexity score
        if not (0.0 <= self.complexity_score <= 1.0):
            errors.append(f"Invalid complexity score: {self.complexity_score}")
        
        # Validate risk level
        valid_risk_levels = ["LOW", "MEDIUM", "HIGH", "CRITICAL"]
        if self.risk_level not in valid_risk_levels:
            errors.append(f"Invalid risk level: {self.risk_level}")
        
        # Validate business logic
        if not self.business_logic or len(self.business_logic.strip()) < 10:
            errors.append("Business logic description too short or empty")
        
        return errors


class COBOLAnalyzer(BaseLLMAnalyzer):
    """Uses LLM to analyze COBOL code"""
    
    def __init__(self, api_key: Optional[str] = None, model: str = "gpt-4"):
        """Initialize the COBOL LLM analyzer"""
        super().__init__("COBOL", api_key, model)
    
    def analyze_section(self, code: str, section_name: str, section_type: str) -> COBOLAnalysisResult:
        """Analyze a COBOL section using LLM"""
        prompt = self._create_analysis_prompt(code, section_name, section_type)
        
        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": "You are an expert COBOL analyst. Analyze the provided COBOL code and extract business logic, assess complexity, and identify risks."},
                    {"role": "user", "content": prompt}
                ],
                max_tokens=500,
                temperature=0.3
            )
            
            result = self._parse_llm_response(response.choices[0].message.content)
            return result
            
        except Exception as e:
            # Return default result on error
            return COBOLAnalysisResult(
                business_logic="Analysis failed due to LLM error",
                complexity_score=0.5,
                risk_level="MEDIUM",
                confidence=0.0,
                suggestions=[f"LLM analysis failed: {str(e)}"],
                language=self.language
            )
    
    def _create_analysis_prompt(self, code: str, section_name: str, section_type: str) -> str:
        """Create a prompt for LLM analysis (COBOL-specific)"""
        return self._create_generic_analysis_prompt(code, section_name, section_type)
    
    def _parse_llm_response(self, response: str) -> COBOLAnalysisResult:
        """Parse LLM response into structured result (COBOL-specific)"""
        base_result = self._parse_generic_llm_response(response)
        return COBOLAnalysisResult(**base_result.__dict__)
