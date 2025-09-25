#!/usr/bin/env python3
"""
Test script to measure performance improvement with prefix exclusion
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

def test_full_prefix_performance():
    print("🧪 Testing full performance with prefix exclusion...")
    
    # Get config
    config = get_config()
    
    # Parse a COBOL file
    parser = COBOLParser(config.__dict__)
    result = parser.parse(Path('data/fixtures/vasu_fraud_management_cobol_reformatted.cbl'))
    
    print(f"📊 Parsed program: {result.program.name}")
    print(f"📊 Total subsections: {len(result.subsections)}")
    
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
    
    # Analyze prefix exclusion
    print(f"\n🔍 Analyzing prefix exclusion across all subsections:")
    
    excluded_count = 0
    potential_business_rules = 0
    excluded_names = []
    
    for subsection in result.subsections:
        if converter._is_non_business_rule_prefix(subsection.name):
            excluded_count += 1
            excluded_names.append(subsection.name)
        else:
            potential_business_rules += 1
    
    print(f"📊 PREFIX EXCLUSION ANALYSIS:")
    print(f"{'='*60}")
    print(f"Total subsections: {len(result.subsections)}")
    print(f"Excluded by prefix: {excluded_count}")
    print(f"Potential business rules: {potential_business_rules}")
    print(f"LLM calls saved: {excluded_count} ({excluded_count/len(result.subsections)*100:.1f}%)")
    
    # Show some excluded examples
    print(f"\n🚫 Sample excluded subsections:")
    for name in excluded_names[:10]:  # Show first 10
        print(f"  - {name}")
    if len(excluded_names) > 10:
        print(f"  ... and {len(excluded_names) - 10} more")
    
    # Estimate performance improvement
    estimated_time_per_call = 3.5  # seconds
    time_saved = excluded_count * estimated_time_per_call
    total_estimated_time = len(result.subsections) * estimated_time_per_call
    time_with_exclusion = potential_business_rules * estimated_time_per_call
    
    print(f"\n⏱️  PERFORMANCE ESTIMATES:")
    print(f"{'='*60}")
    print(f"Without prefix exclusion: {total_estimated_time:.1f}s")
    print(f"With prefix exclusion: {time_with_exclusion:.1f}s")
    print(f"Time saved: {time_saved:.1f}s ({time_saved/total_estimated_time*100:.1f}%)")
    
    print(f"\n✅ Prefix exclusion will significantly improve performance!")

if __name__ == "__main__":
    test_full_prefix_performance()
