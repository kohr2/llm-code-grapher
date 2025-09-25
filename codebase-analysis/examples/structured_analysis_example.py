#!/usr/bin/env python3
"""
Example demonstrating structured LLM analysis for COBOL files
"""

import sys
from pathlib import Path

# Add project root to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))

from lang.cobol.parser.llm_analyzer import COBOLAnalyzer
from lang.base.parser.llm_provider import LLMProviderConfig

def test_structured_analysis():
    """Test the structured analysis functionality"""
    
    # Sample COBOL code for testing
    sample_cobol_code = """
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. FRAUD-MANAGEMENT.
000300 AUTHOR. SYSTEM-ANALYST.
000400 DATE-WRITTEN. 2024-01-01.
000500
000600 ENVIRONMENT DIVISION.
000700 INPUT-OUTPUT SECTION.
000800 FILE-CONTROL.
000900     SELECT FRAUD-FILE ASSIGN TO 'FRAUD.DAT'.
001000     SELECT REPORT-FILE ASSIGN TO 'FRAUD-REPORT.TXT'.
001100
001200 DATA DIVISION.
001300 FILE SECTION.
001400 FD FRAUD-FILE.
001500 01 FRAUD-RECORD.
001600     05 CUSTOMER-ID PIC X(10).
001700     05 TRANSACTION-AMOUNT PIC 9(10)V99.
001800     05 TRANSACTION-DATE PIC X(10).
001900     05 RISK-SCORE PIC 9(3).
002000
002100 WORKING-STORAGE SECTION.
002200 01 WS-EOF-FLAG PIC X VALUE 'N'.
002300 01 WS-HIGH-RISK-COUNT PIC 9(5) VALUE ZERO.
002400 01 WS-TOTAL-AMOUNT PIC 9(12)V99 VALUE ZERO.
002500
002600 PROCEDURE DIVISION.
002700 1000-MAIN-PROCESSING.
002800     OPEN INPUT FRAUD-FILE
002900     OPEN OUTPUT REPORT-FILE
003000     PERFORM 2000-PROCESS-RECORDS
003100     PERFORM 3000-GENERATE-REPORT
003200     CLOSE FRAUD-FILE REPORT-FILE
003300     STOP RUN.
003400
003500 2000-PROCESS-RECORDS.
003600     READ FRAUD-FILE
003700     PERFORM UNTIL WS-EOF-FLAG = 'Y'
003800         PERFORM 2100-ANALYZE-TRANSACTION
003900         READ FRAUD-FILE
004000     END-PERFORM.
004100
004200 2100-ANALYZE-TRANSACTION.
004300     IF RISK-SCORE > 700
004400         ADD 1 TO WS-HIGH-RISK-COUNT
004500         WRITE REPORT-RECORD FROM FRAUD-RECORD
004600     END-IF
004700     ADD TRANSACTION-AMOUNT TO WS-TOTAL-AMOUNT.
004800
004900 3000-GENERATE-REPORT.
005000     DISPLAY 'High Risk Transactions: ' WS-HIGH-RISK-COUNT
005100     DISPLAY 'Total Amount: ' WS-TOTAL-AMOUNT.
"""
    
    # Create LLM provider configuration
    # Note: This is a mock configuration for demonstration
    # In real usage, you would provide actual API credentials
    provider_config = LLMProviderConfig(
        provider="openai",
        model="gpt-4",
        api_key="mock-key-for-demo",  # Replace with actual API key
        max_tokens=4000,
        temperature=0.1
    )
    
    # Create analyzer
    analyzer = COBOLAnalyzer(provider_config)
    
    print("=== Structured COBOL Analysis Demo ===")
    print(f"Sample COBOL code length: {len(sample_cobol_code)} characters")
    print()
    
    try:
        # Perform structured analysis
        print("Performing structured analysis...")
        structured_result = analyzer.analyze_complete_file_structured(
            sample_cobol_code, 
            "FRAUD-MANAGEMENT"
        )
        
        print("✅ Structured analysis completed!")
        print()
        
        # Display results
        print("=== ANALYSIS RESULTS ===")
        print(f"Program: {structured_result.get('program_name', 'Unknown')}")
        print(f"Language: {structured_result.get('language', 'Unknown')}")
        print(f"Overall Confidence: {structured_result.get('confidence', 0.0):.2f}")
        print()
        
        print("OVERVIEW:")
        print(structured_result.get('overview', 'No overview available'))
        print()
        
        print(f"SECTIONS FOUND: {len(structured_result.get('sections', []))}")
        for i, section in enumerate(structured_result.get('sections', []), 1):
            print(f"  {i}. {section.get('name', 'Unknown')} ({section.get('type', 'Unknown')})")
            print(f"     Business Logic: {section.get('business_logic', 'N/A')[:100]}...")
            print(f"     Risk Level: {section.get('risk_level', 'Unknown')}")
            print(f"     Complexity: {section.get('complexity_score', 0.0):.2f}")
            print()
        
        print(f"BUSINESS RULES FOUND: {len(structured_result.get('business_rules', []))}")
        for i, rule in enumerate(structured_result.get('business_rules', []), 1):
            print(f"  {i}. {rule.get('rule_id', 'Unknown')}")
            print(f"     Type: {rule.get('rule_type', 'Unknown')}")
            print(f"     Description: {rule.get('description', 'N/A')[:100]}...")
            print(f"     Risk Level: {rule.get('risk_level', 'Unknown')}")
            print()
        
        print(f"RELATIONSHIPS FOUND: {len(structured_result.get('relationships', []))}")
        for i, rel in enumerate(structured_result.get('relationships', []), 1):
            print(f"  {i}. {rel.get('source', 'Unknown')} -> {rel.get('target', 'Unknown')}")
            print(f"     Type: {rel.get('relationship_type', 'Unknown')}")
            print(f"     Confidence: {rel.get('confidence', 0.0):.2f}")
            print()
        
        print(f"DATA ELEMENTS FOUND: {len(structured_result.get('data_elements', []))}")
        for i, element in enumerate(structured_result.get('data_elements', []), 1):
            print(f"  {i}. {element.get('name', 'Unknown')}")
            print(f"     Type: {element.get('type', 'Unknown')}")
            print(f"     Purpose: {element.get('business_purpose', 'N/A')[:100]}...")
            print()
        
        risk_assessment = structured_result.get('risk_assessment', {})
        print("RISK ASSESSMENT:")
        if isinstance(risk_assessment, dict):
            print(f"  Overall Risk: {risk_assessment.get('overall_risk', 'Unknown')}")
            print(f"  Risk Factors: {len(risk_assessment.get('risk_factors', []))}")
            for factor in risk_assessment.get('risk_factors', []):
                print(f"    - {factor}")
        else:
            print(f"  Overall Risk: {risk_assessment}")
        print()
        
        suggestions = structured_result.get('suggestions', {})
        if isinstance(suggestions, dict):
            print("SUGGESTIONS:")
            print(f"  Modernization: {len(suggestions.get('modernization', []))} suggestions")
            print(f"  Performance: {len(suggestions.get('performance', []))} suggestions")
            print(f"  Maintainability: {len(suggestions.get('maintainability', []))} suggestions")
            print(f"  Security: {len(suggestions.get('security', []))} suggestions")
        else:
            print(f"SUGGESTIONS: {suggestions}")
        
    except Exception as e:
        print(f"❌ Analysis failed: {e}")
        print("This is expected in demo mode without actual API credentials.")
        print("To use this feature, set up your OpenAI API key and run with real credentials.")

def show_usage():
    """Show how to use the structured analysis feature"""
    print("=== How to Use Structured Analysis ===")
    print()
    print("1. Set up your OpenAI API key:")
    print("   export OPENAI_API_KEY='your-api-key-here'")
    print()
    print("2. Run analysis with structured LLM analysis:")
    print("   python -m src.cli analyze your_file.cbl --structured-analysis")
    print()
    print("3. Or use the basic parser analysis (no API key required):")
    print("   python -m src.cli analyze your_file.cbl")
    print()
    print("The structured analysis provides:")
    print("- Complete business rule extraction")
    print("- Detailed relationship mapping")
    print("- Comprehensive risk assessment")
    print("- Data element analysis")
    print("- Modernization suggestions")
    print()
    print("All information needed to build graphs and business rules!")

if __name__ == "__main__":
    test_structured_analysis()
    print("\n" + "="*50 + "\n")
    show_usage()
