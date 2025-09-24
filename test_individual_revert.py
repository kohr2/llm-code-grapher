#!/usr/bin/env python3
"""
Test script to verify individual analysis is working after revert
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

def test_individual_revert():
    print("🧪 Testing individual analysis after revert...")
    
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
    
    # Find business rule candidates
    business_rule_candidates = [s for s in result.subsections if any(keyword in s.name.upper() for keyword in ['RULE', 'CHECK', 'VALIDATE', 'FRAUD', 'RISK'])]
    
    if len(business_rule_candidates) < 2:
        print("❌ Need at least 2 business rules to test")
        return
    
    # Test with 3 rules
    test_rules = business_rule_candidates[:3]
    print(f"🎯 Testing individual analysis with {len(test_rules)} business rules: {[r.name for r in test_rules]}")
    
    # Test individual analysis
    print(f"\n🔄 Testing individual analysis...")
    converter = ParserResultConverter(llm_config, 'cobol')
    
    individual_start = time.time()
    individual_results = {}
    for rule in test_rules:
        print(f"  📝 Analyzing: {rule.name}")
        try:
            analysis = converter._analyze_business_rule_with_llm(rule.business_logic, rule.name)
            individual_results[rule.name] = analysis
            print(f"    ✅ Success: {analysis['priority']} priority, {analysis['risk_level']} risk")
        except Exception as e:
            print(f"    ❌ Failed: {e}")
    
    individual_time = time.time() - individual_start
    individual_calls = converter.llm_call_count
    individual_llm_time = converter.total_llm_time
    
    # Performance summary
    print(f"\n📊 INDIVIDUAL ANALYSIS PERFORMANCE:")
    print(f"{'='*60}")
    print(f"Total Time: {individual_time:.3f}s")
    print(f"LLM Calls: {individual_calls}")
    print(f"LLM Time: {individual_llm_time:.3f}s")
    print(f"Avg Time per Rule: {individual_time/len(test_rules):.3f}s")
    print(f"Rules per Call: {len(test_rules)/individual_calls:.1f}")
    
    print(f"\n📁 Log file: {converter.conversation_log_file}")
    print(f"✅ Individual analysis reverted successfully!")

if __name__ == "__main__":
    test_individual_revert()
