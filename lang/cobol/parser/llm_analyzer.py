"""
COBOL LLM Analyzer
COBOL-specific LLM analysis implementation
"""

from typing import List, Dict, Any, Optional
from dataclasses import dataclass

from ...base.parser.base_llm_analyzer import BaseLLMAnalyzer, BaseLLMAnalysisResult
from ...base.parser.llm_provider import LLMProviderConfig


@dataclass
class COBOLAnalysisResult(BaseLLMAnalysisResult):
    """COBOL-specific LLM analysis result"""
    
    def get_analysis_metrics(self) -> Dict[str, Any]:
        """Get COBOL-specific analysis metrics"""
        return {
            "business_logic": self.business_logic,
            "complexity_score": self.complexity_score,
            "risk_level": self.risk_level,
            "confidence": self.confidence,
            "suggestions": self.suggestions,
            "language": self.language,
            "cobol_specific": {
                "has_perform_statements": "PERFORM" in self.business_logic.upper(),
                "has_conditional_logic": any(keyword in self.business_logic.upper() 
                                           for keyword in ["IF", "WHEN", "EVALUATE"]),
                "has_data_operations": any(keyword in self.business_logic.upper() 
                                         for keyword in ["MOVE", "COMPUTE", "ADD", "SUBTRACT"])
            }
        }
    
    def validate_analysis(self) -> List[str]:
        """Validate COBOL analysis results"""
        errors = []
        
        # Basic validation from base class
        base_errors = super().validate_analysis()
        errors.extend(base_errors)
        
        # COBOL-specific validation
        if self.language != "cobol":
            errors.append(f"Invalid language for COBOL analysis: {self.language}")
        
        return errors


class COBOLAnalyzer(BaseLLMAnalyzer):
    """COBOL-specific LLM analyzer"""
    
    def __init__(self, provider_config: LLMProviderConfig):
        """Initialize COBOL analyzer"""
        super().__init__("cobol", provider_config)
    
    def analyze_section(self, code: str, section_name: str, section_type: str) -> COBOLAnalysisResult:
        """Analyze a COBOL section using LLM"""
        prompt = self._create_analysis_prompt(code, section_name, section_type)
        
        try:
            response = self.provider.generate_response(prompt)
            result = self._parse_llm_response(response)
            
            # Validate the result
            validation_errors = self._validate_analysis_result(result)
            if validation_errors:
                # Log validation errors but don't fail
                print(f"Warning: Analysis validation errors for {section_name}: {validation_errors}")
            
            return result
            
        except Exception as e:
            # Return a fallback result on error
            return COBOLAnalysisResult(
                business_logic=f"Error analyzing section {section_name}: {str(e)}",
                complexity_score=0.5,
                risk_level="MEDIUM",
                confidence=0.0,
                suggestions=["Manual review required due to analysis error"],
                language="cobol"
            )
    
    def analyze_program(self, code: str, program_name: str) -> COBOLAnalysisResult:
        """Analyze a complete COBOL program using LLM"""
        prompt = self._create_program_analysis_prompt(code, program_name)
        
        try:
            response = self.provider.generate_response(prompt)
            result = self._parse_llm_response(response)
            result.language = "cobol"
            return result
            
        except Exception as e:
            # Return a fallback result on error
            return COBOLAnalysisResult(
                business_logic=f"Error analyzing program {program_name}: {str(e)}",
                complexity_score=0.5,
                risk_level="MEDIUM",
                confidence=0.0,
                suggestions=["Manual review required due to analysis error"],
                language="cobol"
            )
    
    def _create_analysis_prompt(self, code: str, section_name: str, section_type: str) -> str:
        """Create a COBOL-specific analysis prompt"""
        return f"""
Analyze this COBOL {section_type} section named "{section_name}":

{code}

Please provide a detailed analysis focusing on:

1. BUSINESS_LOGIC: What does this section do? Describe the business logic in 2-3 sentences.
2. COMPLEXITY: Rate the complexity from 0.0 (simple) to 1.0 (very complex). Consider:
   - Number of PERFORM statements
   - Nested IF/EVALUATE statements
   - Data manipulation operations
   - Control flow complexity
3. RISK: Assess the risk level (LOW, MEDIUM, HIGH, CRITICAL) based on:
   - Code maintainability
   - Potential for errors
   - Business criticality
4. CONFIDENCE: Rate your confidence in this analysis from 0.0 to 1.0
5. SUGGESTIONS: Provide 2-3 specific suggestions for improvement or modernization

Format your response exactly as:
BUSINESS_LOGIC: [description]
COMPLEXITY: [0.0-1.0]
RISK: [LOW/MEDIUM/HIGH/CRITICAL]
CONFIDENCE: [0.0-1.0]
SUGGESTIONS: [suggestion1, suggestion2, suggestion3]
"""
    
    def _create_program_analysis_prompt(self, code: str, program_name: str) -> str:
        """Create a COBOL program analysis prompt"""
        return f"""
Analyze this complete COBOL program named "{program_name}":

{code}

Please provide a comprehensive analysis focusing on:

1. BUSINESS_LOGIC: What is the overall purpose of this program? Describe the main business functions.
2. COMPLEXITY: Rate the overall program complexity from 0.0 (simple) to 1.0 (very complex).
3. RISK: Assess the overall risk level (LOW, MEDIUM, HIGH, CRITICAL) based on:
   - Code structure and organization
   - Maintainability concerns
   - Business criticality
4. CONFIDENCE: Rate your confidence in this analysis from 0.0 to 1.0
5. SUGGESTIONS: Provide 3-5 specific suggestions for program improvement or modernization

Format your response exactly as:
BUSINESS_LOGIC: [description]
COMPLEXITY: [0.0-1.0]
RISK: [LOW/MEDIUM/HIGH/CRITICAL]
CONFIDENCE: [0.0-1.0]
SUGGESTIONS: [suggestion1, suggestion2, suggestion3, suggestion4, suggestion5]
"""
    
    def _parse_llm_response(self, response: str) -> COBOLAnalysisResult:
        """Parse LLM response into COBOL analysis result"""
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
                    # Ensure it's within valid range
                    complexity_score = max(0.0, min(1.0, complexity_score))
                except ValueError:
                    complexity_score = 0.5
            elif line.startswith('RISK:'):
                risk = line.replace('RISK:', '').strip().upper()
                if risk in ['LOW', 'MEDIUM', 'HIGH', 'CRITICAL']:
                    risk_level = risk
            elif line.startswith('CONFIDENCE:'):
                try:
                    confidence = float(line.replace('CONFIDENCE:', '').strip())
                    # Ensure it's within valid range
                    confidence = max(0.0, min(1.0, confidence))
                except ValueError:
                    confidence = 0.5
            elif line.startswith('SUGGESTIONS:'):
                suggestions_text = line.replace('SUGGESTIONS:', '').strip()
                suggestions = [s.strip() for s in suggestions_text.split(',') if s.strip()]
        
        return COBOLAnalysisResult(
            business_logic=business_logic,
            complexity_score=complexity_score,
            risk_level=risk_level,
            confidence=confidence,
            suggestions=suggestions,
            language="cobol"
        )
    
    def _validate_analysis_result(self, result: COBOLAnalysisResult) -> List[str]:
        """Validate COBOL analysis result"""
        errors = []
        
        # Basic validation from base class
        base_errors = super()._validate_analysis_result(result)
        errors.extend(base_errors)
        
        # COBOL-specific validation
        if result.language != "cobol":
            errors.append(f"Invalid language for COBOL analysis: {result.language}")
        
        # Check for COBOL-specific patterns in business logic
        if result.business_logic and "cobol" not in result.business_logic.lower():
            # This is not necessarily an error, but could be noted
            pass
        
        return errors
