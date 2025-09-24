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
    
    def analyze_complete_file_structured(self, code: str, program_name: str) -> Dict[str, Any]:
        """Analyze complete COBOL file and return structured analysis"""
        prompt = self._create_structured_analysis_prompt(code, program_name)
        
        try:
            response = self.provider.generate_response(prompt)
            result = self._parse_structured_response(response)
            result['language'] = "cobol"
            result['program_name'] = program_name
            return result
            
        except Exception as e:
            # Return a fallback result on error
            return {
                "program_name": program_name,
                "language": "cobol",
                "overview": f"Error analyzing program {program_name}: {str(e)}",
                "sections": [],
                "relationships": [],
                "business_rules": [],
                "risk_assessment": {"overall_risk": "MEDIUM", "details": []},
                "suggestions": ["Manual review required due to analysis error"],
                "confidence": 0.0
            }
    
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
    
    def _create_structured_analysis_prompt(self, code: str, program_name: str) -> str:
        """Create a comprehensive structured analysis prompt for complete COBOL file"""
        return f"""
Analyze this complete COBOL program named "{program_name}" and provide a comprehensive structured analysis that includes all information needed to build a graph representation and extract business rules:

{code}

Please provide a detailed analysis in the following structured format:

OVERVIEW: [Brief description of the program's purpose and main functionality]

SECTIONS:
For each major section/division, provide:
- SECTION_NAME: [name]
- SECTION_TYPE: [IDENTIFICATION/ENVIRONMENT/DATA/PROCEDURE/SECTION/PARAGRAPH]
- LINE_RANGE: [start_line-end_line]
- BUSINESS_LOGIC: [What this section does]
- COMPLEXITY_SCORE: [0.0-1.0]
- RISK_LEVEL: [LOW/MEDIUM/HIGH/CRITICAL]
- CONFIDENCE: [0.0-1.0]
- OPERATIONS: [List of key operations in this section]
- PARAMETERS: [Key data elements used]
- DEPENDENCIES: [Other sections this depends on]

RELATIONSHIPS:
For each relationship between sections/operations:
- SOURCE: [source section/operation name]
- TARGET: [target section/operation name]
- RELATIONSHIP_TYPE: [CALLS/DEPENDS_ON/NEXT/CONTAINS/PARENT_OF]
- CONFIDENCE: [0.0-1.0]
- STRENGTH: [0.0-1.0]
- DESCRIPTION: [Brief description of the relationship]

BUSINESS_RULES:
For each business rule identified:
- RULE_ID: [unique identifier]
- RULE_TYPE: [VALIDATION/CALCULATION/BUSINESS_LOGIC/CONTROL_FLOW]
- DESCRIPTION: [What the rule does]
- CONDITIONS: [When this rule applies]
- ACTIONS: [What happens when conditions are met]
- SECTION_REFERENCE: [Which section contains this rule]
- LINE_REFERENCE: [Approximate line numbers]
- RISK_LEVEL: [LOW/MEDIUM/HIGH/CRITICAL]
- COMPLEXITY: [0.0-1.0]

DATA_ELEMENTS:
For each important data element:
- ELEMENT_NAME: [name]
- ELEMENT_TYPE: [FIELD/FILE/RECORD/WORKING_STORAGE/LINKAGE]
- SECTION_REFERENCE: [Which section defines it]
- BUSINESS_PURPOSE: [What this data represents]
- USAGE_PATTERN: [How it's used in the program]

RISK_ASSESSMENT:
- OVERALL_RISK: [LOW/MEDIUM/HIGH/CRITICAL]
- RISK_FACTORS: [List of specific risk factors]
- MITIGATION_SUGGESTIONS: [How to address risks]

SUGGESTIONS:
- MODERNIZATION: [Suggestions for modernization]
- PERFORMANCE: [Performance improvement suggestions]
- MAINTAINABILITY: [Maintainability improvements]
- SECURITY: [Security considerations]

CONFIDENCE: [Overall confidence in this analysis from 0.0 to 1.0]

Format your response exactly as shown above, with each section clearly marked and all information provided in the specified structure.
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
    
    def _parse_structured_response(self, response: str) -> Dict[str, Any]:
        """Parse structured LLM response into comprehensive analysis result"""
        lines = response.strip().split('\n')
        
        result = {
            "overview": "",
            "sections": [],
            "relationships": [],
            "business_rules": [],
            "data_elements": [],
            "risk_assessment": {"overall_risk": "MEDIUM", "risk_factors": [], "mitigation_suggestions": []},
            "suggestions": {"modernization": [], "performance": [], "maintainability": [], "security": []},
            "confidence": 0.5
        }
        
        current_section = None
        current_relationship = None
        current_rule = None
        current_data_element = None
        
        for line in lines:
            line = line.strip()
            if not line:
                continue
                
            # Overview
            if line.startswith('OVERVIEW:'):
                result["overview"] = line.replace('OVERVIEW:', '').strip()
            
            # Sections
            elif line.startswith('- SECTION_NAME:'):
                if current_section:
                    result["sections"].append(current_section)
                current_section = {"operations": [], "parameters": [], "dependencies": []}
                current_section["name"] = line.replace('- SECTION_NAME:', '').strip()
            elif line.startswith('- SECTION_TYPE:') and current_section:
                current_section["type"] = line.replace('- SECTION_TYPE:', '').strip()
            elif line.startswith('- LINE_RANGE:') and current_section:
                range_str = line.replace('- LINE_RANGE:', '').strip()
                try:
                    if '-' in range_str:
                        start, end = range_str.split('-')
                        current_section["line_range"] = (int(start), int(end))
                except ValueError:
                    current_section["line_range"] = (0, 0)
            elif line.startswith('- BUSINESS_LOGIC:') and current_section:
                current_section["business_logic"] = line.replace('- BUSINESS_LOGIC:', '').strip()
            elif line.startswith('- COMPLEXITY_SCORE:') and current_section:
                try:
                    current_section["complexity_score"] = float(line.replace('- COMPLEXITY_SCORE:', '').strip())
                except ValueError:
                    current_section["complexity_score"] = 0.5
            elif line.startswith('- RISK_LEVEL:') and current_section:
                current_section["risk_level"] = line.replace('- RISK_LEVEL:', '').strip()
            elif line.startswith('- CONFIDENCE:') and current_section:
                try:
                    current_section["confidence"] = float(line.replace('- CONFIDENCE:', '').strip())
                except ValueError:
                    current_section["confidence"] = 0.5
            elif line.startswith('- OPERATIONS:') and current_section:
                operations_str = line.replace('- OPERATIONS:', '').strip()
                current_section["operations"] = [op.strip() for op in operations_str.split(',') if op.strip()]
            elif line.startswith('- PARAMETERS:') and current_section:
                params_str = line.replace('- PARAMETERS:', '').strip()
                current_section["parameters"] = [p.strip() for p in params_str.split(',') if p.strip()]
            elif line.startswith('- DEPENDENCIES:') and current_section:
                deps_str = line.replace('- DEPENDENCIES:', '').strip()
                current_section["dependencies"] = [d.strip() for d in deps_str.split(',') if d.strip()]
            
            # Relationships
            elif line.startswith('- SOURCE:'):
                if current_relationship:
                    result["relationships"].append(current_relationship)
                current_relationship = {}
                current_relationship["source"] = line.replace('- SOURCE:', '').strip()
            elif line.startswith('- TARGET:') and current_relationship:
                current_relationship["target"] = line.replace('- TARGET:', '').strip()
            elif line.startswith('- RELATIONSHIP_TYPE:') and current_relationship:
                current_relationship["relationship_type"] = line.replace('- RELATIONSHIP_TYPE:', '').strip()
            elif line.startswith('- CONFIDENCE:') and current_relationship:
                try:
                    current_relationship["confidence"] = float(line.replace('- CONFIDENCE:', '').strip())
                except ValueError:
                    current_relationship["confidence"] = 0.5
            elif line.startswith('- STRENGTH:') and current_relationship:
                try:
                    current_relationship["strength"] = float(line.replace('- STRENGTH:', '').strip())
                except ValueError:
                    current_relationship["strength"] = 0.5
            elif line.startswith('- DESCRIPTION:') and current_relationship:
                current_relationship["description"] = line.replace('- DESCRIPTION:', '').strip()
            
            # Business Rules
            elif line.startswith('- RULE_ID:'):
                if current_rule:
                    result["business_rules"].append(current_rule)
                current_rule = {}
                current_rule["rule_id"] = line.replace('- RULE_ID:', '').strip()
            elif line.startswith('- RULE_TYPE:') and current_rule:
                current_rule["rule_type"] = line.replace('- RULE_TYPE:', '').strip()
            elif line.startswith('- DESCRIPTION:') and current_rule:
                current_rule["description"] = line.replace('- DESCRIPTION:', '').strip()
            elif line.startswith('- CONDITIONS:') and current_rule:
                current_rule["conditions"] = line.replace('- CONDITIONS:', '').strip()
            elif line.startswith('- ACTIONS:') and current_rule:
                current_rule["actions"] = line.replace('- ACTIONS:', '').strip()
            elif line.startswith('- SECTION_REFERENCE:') and current_rule:
                current_rule["section_reference"] = line.replace('- SECTION_REFERENCE:', '').strip()
            elif line.startswith('- LINE_REFERENCE:') and current_rule:
                current_rule["line_reference"] = line.replace('- LINE_REFERENCE:', '').strip()
            elif line.startswith('- RISK_LEVEL:') and current_rule:
                current_rule["risk_level"] = line.replace('- RISK_LEVEL:', '').strip()
            elif line.startswith('- COMPLEXITY:') and current_rule:
                try:
                    current_rule["complexity"] = float(line.replace('- COMPLEXITY:', '').strip())
                except ValueError:
                    current_rule["complexity"] = 0.5
            
            # Data Elements
            elif line.startswith('- ELEMENT_NAME:'):
                if current_data_element:
                    result["data_elements"].append(current_data_element)
                current_data_element = {}
                current_data_element["name"] = line.replace('- ELEMENT_NAME:', '').strip()
            elif line.startswith('- ELEMENT_TYPE:') and current_data_element:
                current_data_element["type"] = line.replace('- ELEMENT_TYPE:', '').strip()
            elif line.startswith('- SECTION_REFERENCE:') and current_data_element:
                current_data_element["section_reference"] = line.replace('- SECTION_REFERENCE:', '').strip()
            elif line.startswith('- BUSINESS_PURPOSE:') and current_data_element:
                current_data_element["business_purpose"] = line.replace('- BUSINESS_PURPOSE:', '').strip()
            elif line.startswith('- USAGE_PATTERN:') and current_data_element:
                current_data_element["usage_pattern"] = line.replace('- USAGE_PATTERN:', '').strip()
            
            # Risk Assessment
            elif line.startswith('- OVERALL_RISK:'):
                result["risk_assessment"]["overall_risk"] = line.replace('- OVERALL_RISK:', '').strip()
            elif line.startswith('- RISK_FACTORS:'):
                factors_str = line.replace('- RISK_FACTORS:', '').strip()
                result["risk_assessment"]["risk_factors"] = [f.strip() for f in factors_str.split(',') if f.strip()]
            elif line.startswith('- MITIGATION_SUGGESTIONS:'):
                suggestions_str = line.replace('- MITIGATION_SUGGESTIONS:', '').strip()
                result["risk_assessment"]["mitigation_suggestions"] = [s.strip() for s in suggestions_str.split(',') if s.strip()]
            
            # Suggestions
            elif line.startswith('- MODERNIZATION:'):
                suggestions_str = line.replace('- MODERNIZATION:', '').strip()
                result["suggestions"]["modernization"] = [s.strip() for s in suggestions_str.split(',') if s.strip()]
            elif line.startswith('- PERFORMANCE:'):
                suggestions_str = line.replace('- PERFORMANCE:', '').strip()
                result["suggestions"]["performance"] = [s.strip() for s in suggestions_str.split(',') if s.strip()]
            elif line.startswith('- MAINTAINABILITY:'):
                suggestions_str = line.replace('- MAINTAINABILITY:', '').strip()
                result["suggestions"]["maintainability"] = [s.strip() for s in suggestions_str.split(',') if s.strip()]
            elif line.startswith('- SECURITY:'):
                suggestions_str = line.replace('- SECURITY:', '').strip()
                result["suggestions"]["security"] = [s.strip() for s in suggestions_str.split(',') if s.strip()]
            
            # Overall confidence
            elif line.startswith('CONFIDENCE:'):
                try:
                    result["confidence"] = float(line.replace('CONFIDENCE:', '').strip())
                except ValueError:
                    result["confidence"] = 0.5
        
        # Add any remaining items
        if current_section:
            result["sections"].append(current_section)
        if current_relationship:
            result["relationships"].append(current_relationship)
        if current_rule:
            result["business_rules"].append(current_rule)
        if current_data_element:
            result["data_elements"].append(current_data_element)
        
        return result
