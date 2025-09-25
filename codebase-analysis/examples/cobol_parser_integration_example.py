"""
COBOL Parser Integration Example
Shows how to integrate the fraud management ontology with the existing COBOL parser
"""

import sys
from pathlib import Path
from typing import List, Dict, Any

# Add the project root to the path
sys.path.append(str(Path(__file__).parent.parent))

from lang.cobol.parser.cobol_parser import COBOLParser, COBOLParserResult
from lang.base.ontology.base_models import BaseOntologyResult, RiskLevel
from examples.fraud_management_ontology_example import FraudManagementOntology


class EnhancedCOBOLParser(COBOLParser):
    """Enhanced COBOL parser that integrates business domain knowledge"""
    
    def __init__(self):
        """Initialize the enhanced parser"""
        super().__init__()
        self.fraud_ontology = FraudManagementOntology()
    
    def parse_with_ontology(self, file_path: Path) -> Dict[str, Any]:
        """Parse COBOL file and enhance with business ontology"""
        # Parse using the base COBOL parser
        parser_result = self.parse(file_path)
        
        # Get the fraud management ontology
        fraud_ontology = self.fraud_ontology.create_ontology_result()
        
        # Enhance the parser result with business context
        enhanced_result = self._enhance_with_business_context(parser_result, fraud_ontology)
        
        return enhanced_result
    
    def _enhance_with_business_context(self, parser_result: COBOLParserResult, 
                                     fraud_ontology: BaseOntologyResult) -> Dict[str, Any]:
        """Enhance parser result with business domain knowledge"""
        
        # Map parser sections to business sections
        business_section_mapping = self._map_sections_to_business_context(
            parser_result.sections, fraud_ontology.sections
        )
        
        # Map parser subsections to business rules
        business_rule_mapping = self._map_subsections_to_business_rules(
            parser_result.subsections, fraud_ontology.business_rules
        )
        
        # Calculate enhanced metrics
        enhanced_metrics = self._calculate_enhanced_metrics(
            parser_result, fraud_ontology
        )
        
        # Create comprehensive result
        result = {
            "parser_result": {
                "program_name": parser_result.program.name,
                "sections": [self._section_to_dict(s) for s in parser_result.sections],
                "subsections": [self._subsection_to_dict(s) for s in parser_result.subsections],
                "operations": [self._operation_to_dict(o) for o in parser_result.operations],
                "relationships": [self._relationship_to_dict(r) for r in parser_result.relationships]
            },
            "business_context": {
                "domain": "Financial Services - Fraud Prevention",
                "business_sections": business_section_mapping,
                "business_rules": business_rule_mapping,
                "data_items": [self._data_item_to_dict(d) for d in fraud_ontology.data_items],
                "risk_assessment": self._assess_business_risk(parser_result, fraud_ontology)
            },
            "enhanced_metrics": enhanced_metrics,
            "ontology_metrics": self.fraud_ontology.get_ontology_metrics(),
            "validation": {
                "parser_errors": parser_result.validate_structure(),
                "ontology_errors": self.fraud_ontology.validate_consistency(),
                "business_consistency": self._validate_business_consistency(parser_result, fraud_ontology)
            }
        }
        
        return result
    
    def _map_sections_to_business_context(self, parser_sections: List, 
                                        business_sections: List) -> Dict[str, Any]:
        """Map parser sections to business context"""
        mapping = {}
        
        for section in parser_sections:
            business_context = self._find_business_context_for_section(section, business_sections)
            mapping[section.name] = {
                "parser_section": self._section_to_dict(section),
                "business_context": business_context,
                "risk_level": section.risk_level.value if hasattr(section, 'risk_level') else "UNKNOWN",
                "complexity_score": section.complexity_score if hasattr(section, 'complexity_score') else 0.0
            }
        
        return mapping
    
    def _map_subsections_to_business_rules(self, parser_subsections: List, 
                                         business_rules: List) -> Dict[str, Any]:
        """Map parser subsections to business rules"""
        mapping = {}
        
        for subsection in parser_subsections:
            # Try to match subsection name to business rule
            matching_rule = self._find_matching_business_rule(subsection.name, business_rules)
            
            mapping[subsection.name] = {
                "parser_subsection": self._subsection_to_dict(subsection),
                "business_rule": matching_rule,
                "is_fraud_rule": matching_rule is not None,
                "rule_priority": matching_rule.priority if matching_rule else "UNKNOWN"
            }
        
        return mapping
    
    def _find_business_context_for_section(self, section, business_sections: List) -> Dict[str, Any]:
        """Find business context for a parser section"""
        # Simple name matching - in production, this would be more sophisticated
        for business_section in business_sections:
            if section.name.upper() in business_section.name.upper() or \
               business_section.name.upper() in section.name.upper():
                return {
                    "name": business_section.name,
                    "type": business_section.type,
                    "business_logic": business_section.business_logic,
                    "confidence": business_section.confidence
                }
        
        return {
            "name": "UNKNOWN",
            "type": "UNKNOWN",
            "business_logic": "No business context identified",
            "confidence": 0.0
        }
    
    def _find_matching_business_rule(self, subsection_name: str, business_rules: List) -> Any:
        """Find matching business rule for a subsection"""
        # Look for rule patterns in subsection names
        for rule in business_rules:
            rule_number = rule.rule_id.split('-')[1]  # Extract rule number
            if f"RULE-{rule_number}" in subsection_name.upper() or \
               f"0{rule_number}" in subsection_name:
                return rule
        
        return None
    
    def _calculate_enhanced_metrics(self, parser_result: COBOLParserResult, 
                                  fraud_ontology: BaseOntologyResult) -> Dict[str, Any]:
        """Calculate enhanced metrics combining parser and ontology data"""
        
        # Get parser metrics
        parser_metrics = parser_result.get_parsing_metrics()
        
        # Get ontology metrics
        ontology_metrics = self.fraud_ontology.get_ontology_metrics()
        
        # Calculate business-specific metrics
        fraud_rule_count = len([r for r in fraud_ontology.business_rules if "RULE" in r.rule_id])
        high_risk_sections = len([s for s in parser_result.sections 
                                if hasattr(s, 'risk_level') and s.risk_level == RiskLevel.HIGH])
        
        return {
            "parsing_metrics": parser_metrics,
            "ontology_metrics": ontology_metrics,
            "business_metrics": {
                "fraud_rule_count": fraud_rule_count,
                "high_risk_sections": high_risk_sections,
                "business_rule_coverage": f"{fraud_rule_count}/10" if fraud_rule_count <= 10 else "COMPREHENSIVE",
                "system_complexity": "HIGH" if parser_metrics.get("total_sections", 0) > 5 else "MEDIUM"
            },
            "overall_assessment": self._calculate_overall_assessment(parser_result, fraud_ontology)
        }
    
    def _calculate_overall_assessment(self, parser_result: COBOLParserResult, 
                                    fraud_ontology: BaseOntologyResult) -> Dict[str, Any]:
        """Calculate overall system assessment"""
        
        # Calculate average complexity
        complexities = [s.complexity_score for s in parser_result.sections 
                       if hasattr(s, 'complexity_score')]
        avg_complexity = sum(complexities) / len(complexities) if complexities else 0.0
        
        # Determine risk level
        risk_levels = [s.risk_level for s in parser_result.sections 
                      if hasattr(s, 'risk_level')]
        high_risk_count = len([r for r in risk_levels if r == RiskLevel.HIGH])
        
        if high_risk_count > 2:
            overall_risk = "HIGH"
        elif high_risk_count > 0:
            overall_risk = "MEDIUM"
        else:
            overall_risk = "LOW"
        
        # Determine modernization potential
        if avg_complexity > 0.7 and overall_risk == "HIGH":
            modernization_potential = "HIGH"
        elif avg_complexity > 0.5 or overall_risk == "MEDIUM":
            modernization_potential = "MEDIUM"
        else:
            modernization_potential = "LOW"
        
        return {
            "overall_risk": overall_risk,
            "average_complexity": avg_complexity,
            "modernization_potential": modernization_potential,
            "business_value": "HIGH",  # Fraud prevention is high value
            "maintenance_effort": "HIGH" if avg_complexity > 0.6 else "MEDIUM"
        }
    
    def _assess_business_risk(self, parser_result: COBOLParserResult, 
                            fraud_ontology: BaseOntologyResult) -> Dict[str, Any]:
        """Assess business risk of the fraud management system"""
        
        # Count high-risk components
        high_risk_sections = len([s for s in parser_result.sections 
                                if hasattr(s, 'risk_level') and s.risk_level == RiskLevel.HIGH])
        
        # Count fraud rules
        fraud_rules = len(fraud_ontology.business_rules)
        
        # Assess data handling risk
        data_operations = len([o for o in parser_result.operations 
                             if o.operation_type in ['READ', 'WRITE', 'REWRITE', 'DELETE']])
        
        return {
            "high_risk_components": high_risk_sections,
            "fraud_rule_coverage": fraud_rules,
            "data_operations": data_operations,
            "business_impact": "CRITICAL",  # Fraud prevention is critical
            "compliance_requirements": ["PCI-DSS", "SOX", "GDPR"],
            "risk_mitigation": "COMPREHENSIVE" if fraud_rules >= 8 else "PARTIAL"
        }
    
    def _validate_business_consistency(self, parser_result: COBOLParserResult, 
                                     fraud_ontology: BaseOntologyResult) -> List[str]:
        """Validate business consistency between parser and ontology"""
        errors = []
        
        # Check if fraud rules are properly implemented
        fraud_rule_subsections = [s for s in parser_result.subsections 
                                if "RULE" in s.name.upper()]
        
        if len(fraud_rule_subsections) < len(fraud_ontology.business_rules):
            errors.append(f"Missing fraud rule implementations: found {len(fraud_rule_subsections)}, expected {len(fraud_ontology.business_rules)}")
        
        # Check for high-risk sections without proper error handling
        high_risk_sections = [s for s in parser_result.sections 
                            if hasattr(s, 'risk_level') and s.risk_level == RiskLevel.HIGH]
        
        for section in high_risk_sections:
            # Look for error handling operations in this section
            section_operations = [o for o in parser_result.operations 
                                if (hasattr(o, 'parent_subsection') and 
                                    section.name in o.parent_subsection)]
            
            error_handling_ops = [o for o in section_operations 
                                if o.operation_type in ['DISPLAY', 'STOP', 'CALL']]
            
            if not error_handling_ops:
                errors.append(f"High-risk section {section.name} lacks error handling")
        
        return errors
    
    # Helper methods for converting objects to dictionaries
    def _section_to_dict(self, section) -> Dict[str, Any]:
        """Convert section to dictionary"""
        return {
            "name": section.name,
            "type": section.type,
            "line_range": section.line_range,
            "line_count": section.line_count,
            "business_logic": section.business_logic,
            "confidence": section.confidence,
            "complexity_score": getattr(section, 'complexity_score', 0.0),
            "risk_level": getattr(section, 'risk_level', 'UNKNOWN')
        }
    
    def _subsection_to_dict(self, subsection) -> Dict[str, Any]:
        """Convert subsection to dictionary"""
        return {
            "name": subsection.name,
            "parent_section": subsection.parent_section,
            "line_range": subsection.line_range,
            "line_count": subsection.line_count,
            "business_logic": subsection.business_logic,
            "confidence": subsection.confidence,
            "complexity_score": getattr(subsection, 'complexity_score', 0.0),
            "risk_level": getattr(subsection, 'risk_level', 'UNKNOWN')
        }
    
    def _operation_to_dict(self, operation) -> Dict[str, Any]:
        """Convert operation to dictionary"""
        return {
            "name": operation.name,
            "operation_type": operation.operation_type,
            "parent_subsection": operation.parent_subsection,
            "line_range": operation.line_range,
            "line_count": operation.line_count,
            "business_logic": operation.business_logic,
            "confidence": operation.confidence,
            "complexity_score": getattr(operation, 'complexity_score', 0.0),
            "risk_level": getattr(operation, 'risk_level', 'UNKNOWN'),
            "parameters": getattr(operation, 'parameters', [])
        }
    
    def _relationship_to_dict(self, relationship) -> Dict[str, Any]:
        """Convert relationship to dictionary"""
        return {
            "source": relationship.source,
            "target": relationship.target,
            "relationship_type": relationship.relationship_type,
            "confidence": relationship.confidence,
            "strength": getattr(relationship, 'strength', 1.0)
        }
    
    def _data_item_to_dict(self, data_item) -> Dict[str, Any]:
        """Convert data item to dictionary"""
        return {
            "name": data_item.name,
            "data_type": data_item.data_type,
            "scope_level": data_item.scope_level,
            "parent_scope": data_item.parent_scope,
            "description": data_item.description,
            "operations": data_item.operations
        }


def main():
    """Demonstrate the enhanced COBOL parser with fraud management ontology"""
    print("=== Enhanced COBOL Parser with Fraud Management Ontology ===\n")
    
    # Initialize the enhanced parser
    parser = EnhancedCOBOLParser()
    
    # Parse the fraud management COBOL file
    cobol_file = Path("data/fixtures/vasu_fraud_management_cobol_reformatted.cbl")
    
    if not cobol_file.exists():
        print(f"Error: COBOL file not found at {cobol_file}")
        return
    
    print(f"Parsing COBOL file: {cobol_file}")
    print("=" * 60)
    
    try:
        # Parse with ontology enhancement
        result = parser.parse_with_ontology(cobol_file)
        
        # Display results
        print(f"\nProgram: {result['parser_result']['program_name']}")
        print(f"Domain: {result['business_context']['domain']}")
        
        # Display sections
        print(f"\nSections ({len(result['parser_result']['sections'])}):")
        for section in result['parser_result']['sections']:
            business_context = result['business_context']['business_sections'].get(section['name'], {})
            print(f"  {section['name']} - Risk: {section['risk_level']} - Business: {business_context.get('business_context', {}).get('name', 'Unknown')}")
        
        # Display business rules
        print(f"\nBusiness Rules ({len(result['business_context']['business_rules'])}):")
        for subsection_name, rule_info in result['business_context']['business_rules'].items():
            if rule_info['is_fraud_rule']:
                rule = rule_info['business_rule']
                print(f"  {rule.rule_id}: {rule.description} - Priority: {rule.priority}")
        
        # Display enhanced metrics
        print(f"\nEnhanced Metrics:")
        for category, metrics in result['enhanced_metrics'].items():
            print(f"  {category}:")
            for metric, value in metrics.items():
                print(f"    {metric}: {value}")
        
        # Display validation results
        print(f"\nValidation Results:")
        parser_errors = result['validation']['parser_errors']
        ontology_errors = result['validation']['ontology_errors']
        business_errors = result['validation']['business_consistency']
        
        if parser_errors:
            print(f"  Parser Errors: {len(parser_errors)}")
            for error in parser_errors:
                print(f"    - {error}")
        else:
            print(f"  Parser Errors: None")
        
        if ontology_errors:
            print(f"  Ontology Errors: {len(ontology_errors)}")
            for error in ontology_errors:
                print(f"    - {error}")
        else:
            print(f"  Ontology Errors: None")
        
        if business_errors:
            print(f"  Business Consistency Errors: {len(business_errors)}")
            for error in business_errors:
                print(f"    - {error}")
        else:
            print(f"  Business Consistency Errors: None")
        
        # Display overall assessment
        assessment = result['enhanced_metrics']['overall_assessment']
        print(f"\nOverall Assessment:")
        print(f"  Risk Level: {assessment['overall_risk']}")
        print(f"  Complexity: {assessment['average_complexity']:.2f}")
        print(f"  Modernization Potential: {assessment['modernization_potential']}")
        print(f"  Business Value: {assessment['business_value']}")
        print(f"  Maintenance Effort: {assessment['maintenance_effort']}")
        
    except Exception as e:
        print(f"Error parsing COBOL file: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
