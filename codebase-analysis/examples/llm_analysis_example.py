#!/usr/bin/env python3
"""
Example demonstrating LLM-based business rule analysis in Neo4j converter

This example shows how the neo4j_converter uses LLM analysis for dynamic business rule generation
mappings for business rule analysis.
"""

import sys
import os
from pathlib import Path
from typing import Dict, Union, Any, List

# Add the project root to the path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))

from lang.base.parser.llm_provider import LLMProviderConfig
from src.neo4j_converter import ParserResultConverter
from lang.base.parser.base_parser import BaseParserResult
from lang.base.ontology.base_models import BaseProgram, BaseSection, BaseSubsection, RiskLevel, BaseRelationship


class MockProgram(BaseProgram):
    """Concrete implementation of BaseProgram for testing"""
    
    def __init__(self, name: str, language: str):
        super().__init__(name=name, language=language, metadata={})
        self.line_count = 1000
    
    def get_complexity_metrics(self) -> Dict[str, float]:
        return {"cyclomatic_complexity": 5.2, "cognitive_complexity": 3.8}
    
    def get_quality_indicators(self) -> Dict[str, Union[str, float]]:
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


class MockParserResult(BaseParserResult):
    """Concrete implementation of BaseParserResult for testing"""
    
    def get_parsing_metrics(self) -> Dict[str, Any]:
        return {
            "total_sections": len(self.sections),
            "total_subsections": len(self.subsections),
            "total_relationships": len(self.relationships)
        }
    
    def validate_structure(self) -> List[str]:
        errors = []
        if not self.program:
            errors.append("No program defined")
        if not self.sections:
            errors.append("No sections found")
        return errors


def create_mock_parser_result():
    """Create a mock parser result for testing"""
    
    # Create mock program
    program = MockProgram("FRAUD-MANAGEMENT", "COBOL")
    
    # Create mock sections
    sections = [
        MockSection("ANALYZE-TRANSACTION", "PROCEDURE", (100, 200)),
        MockSection("ADVANCED-ANALYTICS", "PROCEDURE", (201, 300))
    ]
    
    # Create mock subsections with business rules
    subsections = [
        MockSubsection(
            "2610-RULE-HIGH-AMOUNT",
            "ANALYZE-TRANSACTION",
            (150, 180),
            """
            * Rule: High Amount Transaction Detection
            * Purpose: Detect transactions exceeding suspicious amount threshold
            * Logic: Check if transaction amount > $10,000
            * Risk: High - false positives could block legitimate transactions
            """
        ),
        MockSubsection(
            "2620-RULE-CUSTOMER-BEHAVIOR",
            "ADVANCED-ANALYTICS",
            (250, 280),
            """
            * Rule: Customer Behavioral Analysis
            * Purpose: Analyze customer spending patterns for anomalies
            * Logic: Compare current transaction against historical patterns
            * Risk: Medium - requires machine learning model accuracy
            """
        )
    ]
    
    # Create parser result
    result = MockParserResult(
        program=program,
        sections=sections,
        subsections=subsections,
        relationships=[],
        metadata={}
    )
    
    return result


def demonstrate_llm_analysis():
    """Demonstrate LLM-based dynamic analysis"""
    
    print("🔍 LLM Business Rule Analysis Demo")
    print("=" * 50)
    
    # Create mock data
    mock_result = create_mock_parser_result()
    
    # Test 1: Demonstrate LLM requirement
    print("\n📋 Test 1: LLM Configuration Required")
    print("-" * 40)
    
    try:
        # This should fail without LLM config
        converter_no_llm = ParserResultConverter(None)
    except ValueError as e:
        print(f"✅ Expected error: {e}")
    
    # Test 2: With LLM (if available)
    print("\n🤖 Test 2: LLM Analysis (If Available)")
    print("-" * 40)
    
    # Try to create LLM config (will fail gracefully if no API key)
    try:
        llm_config = LLMProviderConfig(
            provider="openai",
            model="gpt-4",
            api_key=os.getenv("OPENAI_API_KEY"),  # Will be None if not set
            max_tokens=2000,
            temperature=0.1
        )
        
        if not llm_config.api_key:
            print("❌ No API key found - LLM analysis requires OPENAI_API_KEY environment variable")
            print("   Set OPENAI_API_KEY to test LLM analysis")
            return
        
        converter_llm = ParserResultConverter(llm_config, "cobol")
        print("✅ LLM analyzer initialized successfully")
        
        graph_data_llm = converter_llm.convert_parser_result(mock_result)
        
        # Show business rule nodes with LLM analysis
        business_rules_llm = [node for node in graph_data_llm.nodes if node.node_type == "BusinessRule"]
        for rule in business_rules_llm:
            print(f"\nRule: {rule.name}")
            print(f"  Description: {rule.properties.get('description', 'N/A')}")
            print(f"  Priority: {rule.properties.get('priority', 'N/A')}")
            print(f"  Risk Level: {rule.properties.get('risk_level', 'N/A')}")
            print(f"  Functional Area: {rule.properties.get('functional_area', 'N/A')}")
            print(f"  Analysis Method: {rule.properties.get('analysis_method', 'N/A')}")
            print(f"  Analysis Confidence: {rule.properties.get('analysis_confidence', 'N/A')}")
            
    except Exception as e:
        print(f"❌ Error with LLM analysis: {e}")
        print("   LLM configuration and API key are required")
    
    print("\n✨ Demo completed!")
    print("\nKey improvements with LLM-only analysis:")
    print("• More accurate and contextual business rule descriptions")
    print("• Dynamic priority and risk assessment based on actual code")
    print("• Intelligent functional area mapping")
    print("• Higher confidence scores for LLM-generated analysis")
    print("• Dynamic rule generation - ensures consistent LLM-based analysis")
    print("• Clear error handling when LLM is unavailable")


if __name__ == "__main__":
    demonstrate_llm_analysis()
