#!/usr/bin/env python3
"""
Example script showing how to use different LLM providers
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from lang.base.parser.llm_provider import LLMProviderFactory, LLMProviderConfig
from lang.cobol.parser.llm_analyzer import COBOLAnalyzer

def example_openai_usage():
    """Example using OpenAI provider"""
    print("=== OpenAI Provider Example ===")
    
    # Configure OpenAI
    config = LLMProviderConfig(
        provider="openai",
        model="gpt-4",
        api_key=os.getenv("OPENAI_API_KEY"),  # Set this in your environment
        max_tokens=1000,
        temperature=0.1
    )
    
    # Create analyzer
    analyzer = COBOLAnalyzer(config)
    
    # Example COBOL code
    cobol_code = """
    IDENTIFICATION DIVISION.
    PROGRAM-ID. HELLO-WORLD.
    
    PROCEDURE DIVISION.
    MAIN-PARAGRAPH.
        DISPLAY 'Hello, World!'.
        STOP RUN.
    """
    
    print(f"Provider: {analyzer.provider_config.provider}")
    print(f"Model: {analyzer.provider_config.model}")
    print(f"Available: {analyzer.provider.is_available()}")
    
    if analyzer.provider.is_available():
        try:
            result = analyzer.analyze_section(cobol_code, "MAIN-PARAGRAPH", "PARAGRAPH")
            print(f"Analysis result: {result.business_logic[:100]}...")
        except Exception as e:
            print(f"Analysis failed: {e}")
    else:
        print("Provider not available - check API key")

def example_ollama_usage():
    """Example using Ollama provider"""
    print("\n=== Ollama Provider Example ===")
    
    # Configure Ollama
    config = LLMProviderConfig(
        provider="ollama",
        model="llama2",  # or any model you have installed
        base_url="http://localhost:11434",  # Default Ollama URL
        max_tokens=1000,
        temperature=0.1
    )
    
    # Create analyzer
    analyzer = COBOLAnalyzer(config)
    
    # Example COBOL code
    cobol_code = """
    IDENTIFICATION DIVISION.
    PROGRAM-ID. CALCULATE-SUM.
    
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 NUM1 PIC 9(3).
    01 NUM2 PIC 9(3).
    01 RESULT PIC 9(4).
    
    PROCEDURE DIVISION.
    MAIN-PARAGRAPH.
        MOVE 100 TO NUM1.
        MOVE 200 TO NUM2.
        ADD NUM1 TO NUM2 GIVING RESULT.
        DISPLAY 'Sum: ' RESULT.
        STOP RUN.
    """
    
    print(f"Provider: {analyzer.provider_config.provider}")
    print(f"Model: {analyzer.provider_config.model}")
    print(f"Base URL: {analyzer.provider_config.base_url}")
    print(f"Available: {analyzer.provider.is_available()}")
    
    if analyzer.provider.is_available():
        try:
            result = analyzer.analyze_section(cobol_code, "MAIN-PARAGRAPH", "PARAGRAPH")
            print(f"Analysis result: {result.business_logic[:100]}...")
        except Exception as e:
            print(f"Analysis failed: {e}")
    else:
        print("Provider not available - check if Ollama is running")

if __name__ == "__main__":
    print("LLM Provider Selection Examples")
    print("=" * 40)
    
    example_openai_usage()
    example_ollama_usage()
    
    print("\n" + "=" * 40)
    print("To use from command line:")
    print("  # Using OpenAI:")
    print("  python -m src.cli analyze file.cbl --provider openai")
    print("  # Using Ollama:")
    print("  python -m src.cli analyze file.cbl --provider ollama --model llama2")
    print("  # Using custom Ollama URL:")
    print("  python -m src.cli analyze file.cbl --provider ollama --base-url http://localhost:11434")
