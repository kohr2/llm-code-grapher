"""
Fraud Management System Ontology Example
Demonstrates how to use base models to represent high-level logic of the COBOL fraud management system
"""

from typing import List, Dict, Any
import sys
from pathlib import Path

# Add the project root to the path
sys.path.append(str(Path(__file__).parent.parent))

from lang.base.ontology.base_models import (
    BaseProgram, BaseSection, BaseSubsection, BaseOperation, 
    BaseRelationship, BaseDataItem, BaseBusinessRule, BaseOntologyResult,
    RiskLevel, QualityLevel, ModernizationLevel
)


class FraudManagementProgram(BaseProgram):
    """Concrete implementation of BaseProgram for fraud management system"""
    
    def get_complexity_metrics(self) -> Dict[str, float]:
        """Get complexity metrics for the fraud management program"""
        return {
            "cyclomatic_complexity": 45.2,  # High due to multiple fraud rules
            "maintainability_index": 65.0,  # Medium due to COBOL legacy
            "technical_debt": 78.5,  # High due to legacy patterns
            "cognitive_complexity": 38.7,  # High due to nested conditions
            "business_rule_complexity": 85.0  # Very high due to 10+ fraud rules
        }
    
    def get_quality_indicators(self) -> Dict[str, Any]:
        """Get quality indicators for the fraud management program"""
        return {
            "code_coverage": "UNKNOWN",  # Legacy system, no test coverage
            "documentation_quality": "MEDIUM",  # Some comments present
            "test_coverage": "LOW",  # Manual testing only
            "business_rule_coverage": "HIGH",  # Comprehensive fraud rules
            "data_validation": "HIGH",  # Strong input validation
            "error_handling": "MEDIUM"  # Basic error handling
        }


class FraudManagementDataItem(BaseDataItem):
    """Concrete implementation of BaseDataItem for fraud management system"""
    
    def get_usage_metrics(self) -> Dict[str, Any]:
        """Get usage metrics for data items"""
        return {
            "transaction_volume": "HIGH",  # Processes many transactions
            "data_complexity": "HIGH",  # Complex data structures
            "update_frequency": "REAL_TIME",  # Real-time updates
            "storage_requirements": "MEDIUM"  # Moderate storage needs
        }


class FraudManagementBusinessRule(BaseBusinessRule):
    """Concrete implementation of BaseBusinessRule for fraud management system"""
    
    def get_rule_metadata(self) -> Dict[str, Any]:
        """Get rule-specific metadata"""
        return {
            "total_rules": 10,
            "rule_coverage": "COMPREHENSIVE",
            "update_frequency": "QUARTERLY",
            "business_impact": "CRITICAL",
            "false_positive_rate": "5-10%",
            "detection_accuracy": "85-90%"
        }


class FraudManagementOntologyResult(BaseOntologyResult):
    """Concrete implementation of BaseOntologyResult for fraud management system"""
    
    def get_ontology_metrics(self) -> Dict[str, Any]:
        """Get overall ontology metrics for the fraud management system"""
        return {
            "total_sections": len(self.sections),
            "total_subsections": len(self.subsections),
            "total_operations": len(getattr(self, 'operations', [])),
            "total_relationships": len(self.relationships),
            "total_data_items": len(self.data_items),
            "total_business_rules": len(self.business_rules),
            "average_complexity": sum(s.complexity_score for s in self.sections) / len(self.sections) if self.sections else 0.0,
            "high_risk_components": len([s for s in self.sections if s.risk_level == RiskLevel.HIGH]),
            "business_rule_coverage": "COMPREHENSIVE",
            "system_reliability": "HIGH"
        }
    
    def validate_consistency(self) -> List[str]:
        """Validate ontology consistency and return errors"""
        errors = []
        
        # Check for missing business rules
        if len(self.business_rules) < 10:
            errors.append("Missing business rules - expected 10 fraud detection rules")
        
        # Check for high-risk components without proper handling
        high_risk_sections = [s for s in self.sections if s.risk_level == RiskLevel.HIGH]
        for section in high_risk_sections:
            if not any(r.source == section.name or r.target == section.name for r in self.relationships):
                errors.append(f"High-risk section {section.name} has no relationships defined")
        
        # Check for data items without operations
        for data_item in self.data_items:
            if not data_item.operations:
                errors.append(f"Data item {data_item.name} has no operations defined")
        
        return errors


class FraudManagementOntology:
    """High-level ontology representation of the COBOL fraud management system"""
    
    def __init__(self):
        """Initialize the fraud management ontology"""
        self.program = self._create_program()
        self.sections = self._create_sections()
        self.subsections = self._create_subsections()
        self.operations = self._create_operations()
        self.relationships = self._create_relationships()
        self.data_items = self._create_data_items()
        self.business_rules = self._create_business_rules()
    
    def _create_program(self) -> FraudManagementProgram:
        """Create the main program representation"""
        return FraudManagementProgram(
            name="FRAUD-MGMT-SYSTEM",
            language="COBOL",
            metadata={
                "author": "FRAUD-DETECTION-TEAM",
                "date_written": "2025-08-06",
                "purpose": "Real-time fraud detection and risk assessment system",
                "business_domain": "Financial Services - Fraud Prevention",
                "complexity_level": "HIGH",
                "maintenance_frequency": "DAILY"
            }
        )
    
    def _create_sections(self) -> List[BaseSection]:
        """Create main sections of the fraud management system"""
        sections = []
        
        # Main Control Section
        sections.append(BaseSection(
            name="0000-MAIN-CONTROL",
            type="PROCEDURE_SECTION",
            line_range=(216, 222),
            line_count=7,
            business_logic="Main program control flow - initializes system, processes transactions, finalizes",
            confidence=0.95,
            complexity_score=0.3,
            risk_level=RiskLevel.LOW,
            metadata={"purpose": "Program entry point and main control flow"}
        ))
        
        # Transaction Processing Section
        sections.append(BaseSection(
            name="2000-PROCESS-TRANSACTIONS",
            type="PROCEDURE_SECTION", 
            line_range=(499, 507),
            line_count=9,
            business_logic="Core transaction processing loop - reads and analyzes each transaction",
            confidence=0.9,
            complexity_score=0.4,
            risk_level=RiskLevel.MEDIUM,
            metadata={"purpose": "Main transaction processing loop"}
        ))
        
        # Fraud Rules Execution Section
        sections.append(BaseSection(
            name="2600-EXECUTE-FRAUD-RULES",
            type="PROCEDURE_SECTION",
            line_range=(582, 594),
            line_count=13,
            business_logic="Executes all 10 fraud detection rules in sequence",
            confidence=0.95,
            complexity_score=0.8,
            risk_level=RiskLevel.HIGH,
            metadata={"purpose": "Core fraud detection logic", "rule_count": 10}
        ))
        
        # Advanced Analytics Section
        sections.append(BaseSection(
            name="4000-ADVANCED-ANALYTICS",
            type="PROCEDURE_SECTION",
            line_range=(269, 276),
            line_count=8,
            business_logic="Advanced pattern recognition, neural network scoring, behavioral analysis",
            confidence=0.85,
            complexity_score=0.9,
            risk_level=RiskLevel.HIGH,
            metadata={"purpose": "Advanced ML-based fraud detection"}
        ))
        
        # Real-time Scoring Section
        sections.append(BaseSection(
            name="5000-REAL-TIME-SCORING",
            type="PROCEDURE_SECTION",
            line_range=(373, 380),
            line_count=8,
            business_logic="Real-time risk scoring using multiple ML models ensemble",
            confidence=0.9,
            complexity_score=0.85,
            risk_level=RiskLevel.HIGH,
            metadata={"purpose": "Real-time risk scoring with ML models"}
        ))
        
        return sections
    
    def _create_subsections(self) -> List[BaseSubsection]:
        """Create subsections (paragraphs) of the fraud management system"""
        subsections = []
        
        # Rule 1: High Amount Transaction
        subsections.append(BaseSubsection(
            name="2610-RULE-HIGH-AMOUNT",
            parent_section="2600-EXECUTE-FRAUD-RULES",
            line_range=(595, 604),
            line_count=10,
            business_logic="Detects high-value transactions exceeding suspicious amount threshold",
            confidence=0.9,
            complexity_score=0.3,
            risk_level=RiskLevel.MEDIUM,
            metadata={"rule_id": "RULE-01", "threshold": "SUSPICIOUS-AMOUNT"}
        ))
        
        # Rule 2: Velocity Check
        subsections.append(BaseSubsection(
            name="2620-RULE-VELOCITY-CHECK",
            parent_section="2600-EXECUTE-FRAUD-RULES",
            line_range=(605, 616),
            line_count=12,
            business_logic="Analyzes transaction velocity - hourly and daily limits",
            confidence=0.9,
            complexity_score=0.4,
            risk_level=RiskLevel.MEDIUM,
            metadata={"rule_id": "RULE-02", "checks": ["hourly", "daily"]}
        ))
        
        # Rule 3: Location Variance
        subsections.append(BaseSubsection(
            name="2630-RULE-LOCATION-VARIANCE",
            parent_section="2600-EXECUTE-FRAUD-RULES",
            line_range=(632, 644),
            line_count=13,
            business_logic="Geographical analysis - detects unusual location patterns",
            confidence=0.85,
            complexity_score=0.5,
            risk_level=RiskLevel.MEDIUM,
            metadata={"rule_id": "RULE-03", "analysis": "geographical"}
        ))
        
        # Neural Network Scoring
        subsections.append(BaseSubsection(
            name="4100-NEURAL-NETWORK-SCORING",
            parent_section="4000-ADVANCED-ANALYTICS",
            line_range=(277, 288),
            line_count=12,
            business_logic="Simulates neural network scoring with non-linear transformations",
            confidence=0.8,
            complexity_score=0.7,
            risk_level=RiskLevel.HIGH,
            metadata={"algorithm": "neural_network", "transformation": "sigmoid"}
        ))
        
        # Pattern Recognition
        subsections.append(BaseSubsection(
            name="4200-PATTERN-RECOGNITION",
            parent_section="4000-ADVANCED-ANALYTICS",
            line_range=(289, 296),
            line_count=8,
            business_logic="Identifies suspicious patterns in transaction sequences",
            confidence=0.8,
            complexity_score=0.6,
            risk_level=RiskLevel.HIGH,
            metadata={"algorithm": "pattern_recognition", "patterns": ["round_dollar", "ascending", "test_transaction"]}
        ))
        
        return subsections
    
    def _create_operations(self) -> List[BaseOperation]:
        """Create granular operations within the fraud management system"""
        operations = []
        
        # High Amount Check Operation
        operations.append(BaseOperation(
            name="check-high-amount",
            operation_type="IF",
            parent_subsection="2610-RULE-HIGH-AMOUNT",
            line_range=(597, 603),
            line_count=7,
            business_logic="IF TRANS-AMOUNT > SUSPICIOUS-AMOUNT - checks if transaction exceeds threshold",
            confidence=0.95,
            complexity_score=0.2,
            risk_level=RiskLevel.LOW,
            parameters=["TRANS-AMOUNT", "SUSPICIOUS-AMOUNT"],
            metadata={"condition": "amount_comparison", "threshold_type": "suspicious"}
        ))
        
        # Velocity Limit Check Operation
        operations.append(BaseOperation(
            name="check-velocity-limits",
            operation_type="IF",
            parent_subsection="2620-RULE-VELOCITY-CHECK",
            line_range=(608, 615),
            line_count=8,
            business_logic="Checks if VELO-TRANS-COUNT-1H > MAX-HOURLY-VELOCITY",
            confidence=0.9,
            complexity_score=0.3,
            risk_level=RiskLevel.MEDIUM,
            parameters=["VELO-TRANS-COUNT-1H", "MAX-HOURLY-VELOCITY"],
            metadata={"condition": "velocity_check", "time_window": "hourly"}
        ))
        
        # Risk Score Calculation Operation
        operations.append(BaseOperation(
            name="calculate-risk-score",
            operation_type="COMPUTE",
            parent_subsection="4100-NEURAL-NETWORK-SCORING",
            line_range=(279, 287),
            line_count=9,
            business_logic="COMPUTE WS-WORK-AMOUNT = weighted sum of risk factors with non-linear transformation",
            confidence=0.85,
            complexity_score=0.6,
            risk_level=RiskLevel.MEDIUM,
            parameters=["WS-TRANSACTION-RISK", "WS-VELOCITY-RISK", "WS-LOCATION-RISK", "WS-MERCHANT-RISK", "WS-BEHAVIORAL-RISK"],
            metadata={"algorithm": "weighted_sum", "transformation": "non_linear"}
        ))
        
        # Pattern Detection Operation
        operations.append(BaseOperation(
            name="detect-round-dollar-pattern",
            operation_type="IF",
            parent_subsection="4210-CHECK-ROUND-DOLLAR-PATTERN",
            line_range=(299, 302),
            line_count=4,
            business_logic="Detects round dollar amounts (potential card testing)",
            confidence=0.9,
            complexity_score=0.2,
            risk_level=RiskLevel.LOW,
            parameters=["TRANS-AMOUNT", "100.00"],
            metadata={"pattern": "round_dollar", "risk_type": "card_testing"}
        ))
        
        return operations
    
    def _create_relationships(self) -> List[BaseRelationship]:
        """Create relationships between components"""
        relationships = []
        
        # Main control calls transaction processing
        relationships.append(BaseRelationship(
            source="0000-MAIN-CONTROL",
            target="2000-PROCESS-TRANSACTIONS",
            relationship_type="PERFORMS",
            confidence=0.95,
            strength=1.0
        ))
        
        # Transaction processing calls fraud rules
        relationships.append(BaseRelationship(
            source="2000-PROCESS-TRANSACTIONS",
            target="2600-EXECUTE-FRAUD-RULES",
            relationship_type="PERFORMS",
            confidence=0.95,
            strength=1.0
        ))
        
        # Fraud rules section contains individual rules
        relationships.append(BaseRelationship(
            source="2600-EXECUTE-FRAUD-RULES",
            target="2610-RULE-HIGH-AMOUNT",
            relationship_type="CONTAINS",
            confidence=0.9,
            strength=0.8
        ))
        
        # Advanced analytics uses neural network scoring
        relationships.append(BaseRelationship(
            source="4000-ADVANCED-ANALYTICS",
            target="4100-NEURAL-NETWORK-SCORING",
            relationship_type="PERFORMS",
            confidence=0.9,
            strength=0.9
        ))
        
        # Operations within subsections
        relationships.append(BaseRelationship(
            source="2610-RULE-HIGH-AMOUNT",
            target="check-high-amount",
            relationship_type="CONTAINS",
            confidence=0.95,
            strength=0.9
        ))
        
        return relationships
    
    def _create_data_items(self) -> List[FraudManagementDataItem]:
        """Create data items representing the fraud system's data structures"""
        data_items = []
        
        # Transaction Record
        data_items.append(FraudManagementDataItem(
            name="TRANSACTION-RECORD",
            data_type="RECORD",
            scope_level=1,
            parent_scope=None,
            description="Main transaction data structure containing all transaction details",
            operations=["READ", "WRITE", "VALIDATE"]
        ))
        
        # Customer Record
        data_items.append(FraudManagementDataItem(
            name="CUSTOMER-RECORD",
            data_type="RECORD",
            scope_level=1,
            parent_scope=None,
            description="Customer profile data including risk scores and spending patterns",
            operations=["READ", "REWRITE", "UPDATE"]
        ))
        
        # Risk Score Variables
        data_items.append(FraudManagementDataItem(
            name="WS-TOTAL-RISK-SCORE",
            data_type="PIC",
            scope_level=1,
            parent_scope="WORKING-STORAGE",
            description="Aggregated risk score from all fraud detection rules",
            operations=["ADD", "COMPUTE", "MOVE", "COMPARE"]
        ))
        
        # Velocity Data
        data_items.append(FraudManagementDataItem(
            name="VELOCITY-RECORD",
            data_type="RECORD",
            scope_level=1,
            parent_scope=None,
            description="Transaction velocity tracking data for fraud detection",
            operations=["READ", "WRITE", "UPDATE"]
        ))
        
        return data_items
    
    def _create_business_rules(self) -> List[FraudManagementBusinessRule]:
        """Create business rules dynamically analyzed from COBOL code sections"""
        rules = []
        
        # Note: Business rules are now dynamically generated through LLM analysis
        # This method provides example rule structure for demonstration purposes
        # In production, rules are extracted from actual COBOL code sections
        
        # Example dynamic rule structure
        rules.append(FraudManagementBusinessRule(
            rule_id="DYNAMIC_RULE_001",
            description="LLM-analyzed business rule from code section",
            scope="TRANSACTION_PROCESSING",
            confidence=0.9,
            risk_level=RiskLevel.MEDIUM,
            priority="HIGH"
        ))
        
        return rules
    
    def create_ontology_result(self) -> FraudManagementOntologyResult:
        """Create the complete ontology result"""
        return FraudManagementOntologyResult(
            program=self.program,
            sections=self.sections,
            subsections=self.subsections,
            relationships=self.relationships,
            data_items=self.data_items,
            business_rules=self.business_rules
        )
    
    def get_complexity_metrics(self) -> Dict[str, float]:
        """Get complexity metrics for the fraud management program"""
        return self.program.get_complexity_metrics()
    
    def get_quality_indicators(self) -> Dict[str, Any]:
        """Get quality indicators for the fraud management program"""
        return self.program.get_quality_indicators()
    
    def get_ontology_metrics(self) -> Dict[str, Any]:
        """Get overall ontology metrics for the fraud management system"""
        return {
            "total_sections": len(self.sections),
            "total_subsections": len(self.subsections),
            "total_operations": len(getattr(self, 'operations', [])),
            "total_relationships": len(self.relationships),
            "total_data_items": len(self.data_items),
            "total_business_rules": len(self.business_rules),
            "average_complexity": sum(s.complexity_score for s in self.sections) / len(self.sections) if self.sections else 0.0,
            "high_risk_components": len([s for s in self.sections if s.risk_level == RiskLevel.HIGH]),
            "business_rule_coverage": "COMPREHENSIVE",
            "system_reliability": "HIGH"
        }
    
    def validate_consistency(self) -> List[str]:
        """Validate ontology consistency and return errors"""
        errors = []
        
        # Check for missing business rules
        if len(self.business_rules) < 10:
            errors.append("Missing business rules - expected 10 fraud detection rules")
        
        # Check for high-risk components without proper handling
        high_risk_sections = [s for s in self.sections if s.risk_level == RiskLevel.HIGH]
        for section in high_risk_sections:
            if not any(r.source == section.name or r.target == section.name for r in self.relationships):
                errors.append(f"High-risk section {section.name} has no relationships defined")
        
        # Check for data items without operations
        for data_item in self.data_items:
            if not data_item.operations:
                errors.append(f"Data item {data_item.name} has no operations defined")
        
        return errors


def main():
    """Demonstrate the fraud management ontology"""
    print("=== Fraud Management System Ontology ===")
    
    # Create the ontology
    fraud_ontology = FraudManagementOntology()
    
    # Create the complete ontology result
    ontology_result = fraud_ontology.create_ontology_result()
    
    # Display program information
    print(f"\nProgram: {ontology_result.program.name}")
    print(f"Language: {ontology_result.program.language}")
    print(f"Purpose: {ontology_result.program.metadata['purpose']}")
    
    # Display complexity metrics
    complexity_metrics = fraud_ontology.get_complexity_metrics()
    print(f"\nComplexity Metrics:")
    for metric, value in complexity_metrics.items():
        print(f"  {metric}: {value}")
    
    # Display quality indicators
    quality_indicators = fraud_ontology.get_quality_indicators()
    print(f"\nQuality Indicators:")
    for indicator, value in quality_indicators.items():
        print(f"  {indicator}: {value}")
    
    # Display sections
    print(f"\nSections ({len(ontology_result.sections)}):")
    for section in ontology_result.sections:
        print(f"  {section.name} ({section.type}) - Risk: {section.risk_level.value}")
    
    # Display business rules
    print(f"\nBusiness Rules ({len(ontology_result.business_rules)}):")
    for rule in ontology_result.business_rules:
        print(f"  {rule.rule_id}: {rule.description} - Risk: {rule.risk_level.value}")
    
    # Display ontology metrics
    ontology_metrics = fraud_ontology.get_ontology_metrics()
    print(f"\nOntology Metrics:")
    for metric, value in ontology_metrics.items():
        print(f"  {metric}: {value}")
    
    # Validate consistency
    validation_errors = fraud_ontology.validate_consistency()
    if validation_errors:
        print(f"\nValidation Errors:")
        for error in validation_errors:
            print(f"  - {error}")
    else:
        print(f"\n✓ Ontology validation passed - no errors found")


if __name__ == "__main__":
    main()
