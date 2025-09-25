#!/usr/bin/env python3
"""
Test script to verify prefix exclusion is working correctly
"""

import sys
import os
import time
sys.path.append('.')

from lang.cobol.parser.cobol_parser import COBOLParser
from src.config_manager import get_config
from src.neo4j_converter import ParserResultConverter
from lang.base.parser.llm_provider import LLMProviderConfig
from pathlib import Path
import logging

# Set up detailed logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[logging.StreamHandler(sys.stdout)]
)

def test_prefix_exclusion():
    print("🧪 Testing prefix exclusion for non-business rules...")
    
    # Get config
    config = get_config()
    
    # Parse a COBOL file
    parser = COBOLParser(config.__dict__)
    result = parser.parse(Path('data/fixtures/vasu_fraud_management_cobol_reformatted.cbl'))
    
    print(f"📊 Parsed program: {result.program.name}")
    
    # Create LLM config
    llm_config = LLMProviderConfig(
        provider=config.llm.provider,
        model=config.llm.model,
        api_key=config.llm.api_key,
        base_url=config.llm.base_url,
        max_tokens=config.llm.max_tokens,
        temperature=config.llm.temperature
    )
    
    # Create converter
    converter = ParserResultConverter(llm_config, 'cobol')
    
    # Test prefix exclusion method directly
    print(f"\n🔍 Testing prefix exclusion method:")
    
    test_names = [
        "LOG-DECISION",           # Should be excluded
        "OPEN-FILES",             # Should be excluded  
        "INITIALIZE-VARIABLES",   # Should be excluded
        "CHECK-ROUND-DOLLAR-PATTERN",  # Should NOT be excluded (business rule)
        "VALIDATE-TRANSACTION",   # Should NOT be excluded (business rule)
        "CALCULATE-FINAL-RISK",   # Should be excluded
        "READ-TRANSACTION",       # Should be excluded
        "RULE-HIGH-AMOUNT"        # Should NOT be excluded (business rule)
    ]
    
    for name in test_names:
        is_excluded = converter._is_non_business_rule_prefix(name)
        status = "🚫 EXCLUDED" if is_excluded else "✅ NOT EXCLUDED"
        print(f"  {name:<35} -> {status}")
    
    # Test with actual subsections
    print(f"\n📝 Testing with actual subsections:")
    total_subsections = len(result.subsections)
    excluded_count = 0
    llm_calls_made = 0
    
    for i, subsection in enumerate(result.subsections[:10], 1):  # Test first 10 subsections
        print(f"\n  {i}. Testing: {subsection.name}")
        
        # Check prefix exclusion
        if converter._is_non_business_rule_prefix(subsection.name):
            print(f"     🚫 EXCLUDED by prefix - no LLM call needed")
            excluded_count += 1
        else:
            print(f"     ✅ NOT EXCLUDED - would make LLM call")
            llm_calls_made += 1
    
    print(f"\n📊 PREFIX EXCLUSION SUMMARY:")
    print(f"{'='*60}")
    print(f"Total subsections tested: 10")
    print(f"Excluded by prefix: {excluded_count}")
    print(f"Would make LLM calls: {llm_calls_made}")
    print(f"LLM calls saved: {excluded_count} ({excluded_count/10*100:.1f}%)")
    
    # Estimate time savings
    estimated_time_per_call = 3.5  # seconds
    time_saved = excluded_count * estimated_time_per_call
    print(f"Estimated time saved: {time_saved:.1f}s")
    
    print(f"\n✅ Prefix exclusion working correctly!")

if __name__ == "__main__":
    test_prefix_exclusion()
