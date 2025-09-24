"""
Fraud Management Ontology Neo4j Converter
Converts the fraud management ontology to Neo4j graph format for storage and analysis
"""

import sys
from pathlib import Path
from typing import Dict, Any, List, Optional

# Add the project root to the path
sys.path.append(str(Path(__file__).parent.parent))

from src.neo4j_models import CodeNode, CodeRelationship, GraphData
from examples.fraud_management_ontology_example import FraudManagementOntology, FraudManagementOntologyResult


class FraudOntologyNeo4jConverter:
    """Converts fraud management ontology to Neo4j graph format"""
    
    def __init__(self):
        """Initialize the converter"""
        self.node_counter = 0
        self.relationship_counter = 0
    
    def _generate_node_id(self, prefix: str) -> str:
        """Generate a unique node ID"""
        self.node_counter += 1
        return f"{prefix}_{self.node_counter}"
    
    def _generate_relationship_id(self) -> str:
        """Generate a unique relationship ID"""
        self.relationship_counter += 1
        return f"rel_{self.relationship_counter}"
    
    def convert_ontology_to_neo4j(self, ontology: FraudManagementOntology) -> GraphData:
        """Convert fraud management ontology to Neo4j graph data"""
        graph_data = GraphData()
        
        # Store graph data for relationship mapping
        self._current_graph_data = graph_data
        
        # Add program node
        program_node = self._create_program_node(ontology.program)
        graph_data.add_node(program_node)
        
        # Add section nodes
        section_nodes = self._create_section_nodes(ontology.sections, program_node.node_id)
        for node in section_nodes:
            graph_data.add_node(node)
        
        # Add subsection nodes
        subsection_nodes = self._create_subsection_nodes(ontology.subsections, program_node.node_id)
        for node in subsection_nodes:
            graph_data.add_node(node)
        
        # Add operation nodes
        operation_nodes = self._create_operation_nodes(ontology.operations, program_node.node_id)
        for node in operation_nodes:
            graph_data.add_node(node)
        
        # Add data item nodes
        data_nodes = self._create_data_item_nodes(ontology.data_items, program_node.node_id)
        for node in data_nodes:
            graph_data.add_node(node)
        
        # Add business rule nodes
        rule_nodes = self._create_business_rule_nodes(ontology.business_rules, program_node.node_id)
        for node in rule_nodes:
            graph_data.add_node(node)
        
        # Add relationships
        relationships = self._create_ontology_relationships(ontology, program_node.node_id)
        for rel in relationships:
            graph_data.add_relationship(rel)
        
        return graph_data
    
    def _create_program_node(self, program) -> CodeNode:
        """Create a program node from the fraud management program"""
        properties = {
            "language": program.language,
            "author": program.metadata.get("author", ""),
            "date_written": program.metadata.get("date_written", ""),
            "purpose": program.metadata.get("purpose", ""),
            "business_domain": program.metadata.get("business_domain", ""),
            "complexity_level": program.metadata.get("complexity_level", ""),
            "maintenance_frequency": program.metadata.get("maintenance_frequency", ""),
            "cyclomatic_complexity": program.get_complexity_metrics().get("cyclomatic_complexity", 0.0),
            "maintainability_index": program.get_complexity_metrics().get("maintainability_index", 0.0),
            "technical_debt": program.get_complexity_metrics().get("technical_debt", 0.0),
            "business_rule_coverage": program.get_quality_indicators().get("business_rule_coverage", ""),
            "system_reliability": "HIGH"
        }
        
        # Remove None values
        properties = {k: v for k, v in properties.items() if v is not None}
        
        return CodeNode(
            node_id=self._generate_node_id("program"),
            node_type="Program",
            name=program.name,
            language=program.language,
            properties=properties
        )
    
    def _create_section_nodes(self, sections: List, program_id: str) -> List[CodeNode]:
        """Create section nodes from fraud management sections"""
        nodes = []
        
        for i, section in enumerate(sections):
            properties = {
                "section_type": section.type,
                "line_range": list(section.line_range),
                "line_count": section.line_count,
                "business_logic": section.business_logic,
                "complexity_score": section.complexity_score,
                "risk_level": section.risk_level.value if hasattr(section.risk_level, 'value') else str(section.risk_level),
                "confidence": section.confidence,
                "purpose": section.metadata.get("purpose", "") if section.metadata else ""
            }
            
            # Remove None values
            properties = {k: v for k, v in properties.items() if v is not None}
            
            section_id = f"section_{i + 1}"
            node = CodeNode(
                node_id=section_id,
                node_type="Section",
                name=section.name,
                language="COBOL",
                properties=properties
            )
            nodes.append(node)
        
        return nodes
    
    def _create_subsection_nodes(self, subsections: List, program_id: str) -> List[CodeNode]:
        """Create subsection nodes from fraud management subsections"""
        nodes = []
        
        for i, subsection in enumerate(subsections):
            properties = {
                "parent_section": subsection.parent_section,
                "line_range": list(subsection.line_range),
                "line_count": subsection.line_count,
                "business_logic": subsection.business_logic,
                "complexity_score": subsection.complexity_score,
                "risk_level": subsection.risk_level.value if hasattr(subsection.risk_level, 'value') else str(subsection.risk_level),
                "confidence": subsection.confidence,
                "rule_id": subsection.metadata.get("rule_id", "") if subsection.metadata else "",
                "checks": subsection.metadata.get("checks", []) if subsection.metadata else [],
                "analysis": subsection.metadata.get("analysis", "") if subsection.metadata else "",
                "algorithm": subsection.metadata.get("algorithm", "") if subsection.metadata else "",
                "patterns": subsection.metadata.get("patterns", []) if subsection.metadata else []
            }
            
            # Remove None values
            properties = {k: v for k, v in properties.items() if v is not None}
            
            subsection_id = f"subsection_{i + 1}"
            node = CodeNode(
                node_id=subsection_id,
                node_type="Subsection",
                name=subsection.name,
                language="COBOL",
                properties=properties
            )
            nodes.append(node)
        
        return nodes
    
    def _create_operation_nodes(self, operations: List, program_id: str) -> List[CodeNode]:
        """Create operation nodes from fraud management operations"""
        nodes = []
        
        for i, operation in enumerate(operations):
            properties = {
                "operation_type": operation.operation_type,
                "parent_subsection": operation.parent_subsection,
                "line_range": list(operation.line_range),
                "line_count": operation.line_count,
                "business_logic": operation.business_logic,
                "complexity_score": operation.complexity_score,
                "risk_level": operation.risk_level.value if hasattr(operation.risk_level, 'value') else str(operation.risk_level),
                "confidence": operation.confidence,
                "parameters": operation.parameters,
                "parameter_count": len(operation.parameters),
                "condition": operation.metadata.get("condition", "") if operation.metadata else "",
                "threshold_type": operation.metadata.get("threshold_type", "") if operation.metadata else "",
                "time_window": operation.metadata.get("time_window", "") if operation.metadata else "",
                "algorithm": operation.metadata.get("algorithm", "") if operation.metadata else "",
                "transformation": operation.metadata.get("transformation", "") if operation.metadata else "",
                "pattern": operation.metadata.get("pattern", "") if operation.metadata else "",
                "risk_type": operation.metadata.get("risk_type", "") if operation.metadata else ""
            }
            
            # Remove None values
            properties = {k: v for k, v in properties.items() if v is not None}
            
            operation_id = f"operation_{i + 1}"
            node = CodeNode(
                node_id=operation_id,
                node_type="Operation",
                name=operation.name,
                language="COBOL",
                properties=properties
            )
            nodes.append(node)
        
        return nodes
    
    def _create_data_item_nodes(self, data_items: List, program_id: str) -> List[CodeNode]:
        """Create data item nodes from fraud management data items"""
        nodes = []
        
        for i, data_item in enumerate(data_items):
            properties = {
                "data_type": data_item.data_type,
                "scope_level": data_item.scope_level,
                "parent_scope": data_item.parent_scope,
                "description": data_item.description,
                "operations": data_item.operations,
                "transaction_volume": data_item.get_usage_metrics().get("transaction_volume", ""),
                "data_complexity": data_item.get_usage_metrics().get("data_complexity", ""),
                "update_frequency": data_item.get_usage_metrics().get("update_frequency", ""),
                "storage_requirements": data_item.get_usage_metrics().get("storage_requirements", "")
            }
            
            # Remove None values
            properties = {k: v for k, v in properties.items() if v is not None}
            
            data_id = f"data_{i + 1}"
            node = CodeNode(
                node_id=data_id,
                node_type="DataItem",
                name=data_item.name,
                language="COBOL",
                properties=properties
            )
            nodes.append(node)
        
        return nodes
    
    def _create_business_rule_nodes(self, business_rules: List, program_id: str) -> List[CodeNode]:
        """Create business rule nodes from fraud management business rules"""
        nodes = []
        
        for i, rule in enumerate(business_rules):
            properties = {
                "rule_id": rule.rule_id,
                "description": rule.description,
                "scope": rule.scope,
                "confidence": rule.confidence,
                "risk_level": rule.risk_level.value if hasattr(rule.risk_level, 'value') else str(rule.risk_level),
                "priority": rule.priority,
                "total_rules": rule.get_rule_metadata().get("total_rules", 0),
                "rule_coverage": rule.get_rule_metadata().get("rule_coverage", ""),
                "update_frequency": rule.get_rule_metadata().get("update_frequency", ""),
                "business_impact": rule.get_rule_metadata().get("business_impact", ""),
                "false_positive_rate": rule.get_rule_metadata().get("false_positive_rate", ""),
                "detection_accuracy": rule.get_rule_metadata().get("detection_accuracy", "")
            }
            
            # Remove None values
            properties = {k: v for k, v in properties.items() if v is not None}
            
            rule_id = f"rule_{i + 1}"
            node = CodeNode(
                node_id=rule_id,
                node_type="BusinessRule",
                name=rule.rule_id,
                language="COBOL",
                properties=properties
            )
            nodes.append(node)
        
        return nodes
    
    def _create_ontology_relationships(self, ontology: FraudManagementOntology, program_id: str) -> List[CodeRelationship]:
        """Create relationships from fraud management ontology"""
        relationships = []
        
        # Program to sections relationships
        for i, section in enumerate(ontology.sections):
            section_id = f"section_{i + 1}"
            rel = CodeRelationship(
                source_id=program_id,
                target_id=section_id,
                relationship_type="CONTAINS",
                properties={
                    "description": f"Program contains section {section.name}",
                    "confidence": 1.0,
                    "relationship_type": "structural"
                }
            )
            relationships.append(rel)
        
        # Sections to subsections relationships
        for i, subsection in enumerate(ontology.subsections):
            subsection_id = f"subsection_{i + 1}"
            
            # Find parent section
            parent_section_id = None
            for j, section in enumerate(ontology.sections):
                if section.name == subsection.parent_section:
                    parent_section_id = f"section_{j + 1}"
                    break
            
            if parent_section_id:
                rel = CodeRelationship(
                    source_id=parent_section_id,
                    target_id=subsection_id,
                    relationship_type="CONTAINS",
                    properties={
                        "description": f"Section {subsection.parent_section} contains subsection {subsection.name}",
                        "confidence": 1.0,
                        "relationship_type": "structural"
                    }
                )
                relationships.append(rel)
        
        # Subsections to operations relationships
        for i, operation in enumerate(ontology.operations):
            operation_id = f"operation_{i + 1}"
            
            # Find parent subsection
            parent_subsection_id = None
            for j, subsection in enumerate(ontology.subsections):
                if subsection.name == operation.parent_subsection:
                    parent_subsection_id = f"subsection_{j + 1}"
                    break
            
            if parent_subsection_id:
                rel = CodeRelationship(
                    source_id=parent_subsection_id,
                    target_id=operation_id,
                    relationship_type="CONTAINS",
                    properties={
                        "description": f"Subsection {operation.parent_subsection} contains operation {operation.name}",
                        "confidence": 1.0,
                        "relationship_type": "structural"
                    }
                )
                relationships.append(rel)
        
        # Program to data items relationships
        for i, data_item in enumerate(ontology.data_items):
            data_id = f"data_{i + 1}"
            rel = CodeRelationship(
                source_id=program_id,
                target_id=data_id,
                relationship_type="USES",
                properties={
                    "description": f"Program uses data item {data_item.name}",
                    "confidence": 1.0,
                    "relationship_type": "data_flow"
                }
            )
            relationships.append(rel)
        
        # Program to business rules relationships
        for i, rule in enumerate(ontology.business_rules):
            rule_id = f"rule_{i + 1}"
            rel = CodeRelationship(
                source_id=program_id,
                target_id=rule_id,
                relationship_type="IMPLEMENTS",
                properties={
                    "description": f"Program implements business rule {rule.rule_id}",
                    "confidence": 1.0,
                    "relationship_type": "business_logic"
                }
            )
            relationships.append(rel)
        
        # Business rules to subsections relationships (rule implementation)
        for i, rule in enumerate(ontology.business_rules):
            rule_id = f"rule_{i + 1}"
            
            # Find matching subsection
            for j, subsection in enumerate(ontology.subsections):
                if rule.rule_id in subsection.name or subsection.name in rule.rule_id:
                    subsection_id = f"subsection_{j + 1}"
                    rel = CodeRelationship(
                        source_id=rule_id,
                        target_id=subsection_id,
                        relationship_type="IMPLEMENTS",
                        properties={
                            "description": f"Business rule {rule.rule_id} is implemented in subsection {subsection.name}",
                            "confidence": 0.9,
                            "relationship_type": "business_logic"
                        }
                    )
                    relationships.append(rel)
                    break
        
        # Data flow relationships between operations and data items
        for i, operation in enumerate(ontology.operations):
            operation_id = f"operation_{i + 1}"
            
            # Find related data items based on parameters
            for j, data_item in enumerate(ontology.data_items):
                if any(param in data_item.name for param in operation.parameters):
                    data_id = f"data_{j + 1}"
                    rel = CodeRelationship(
                        source_id=operation_id,
                        target_id=data_id,
                        relationship_type="USES",
                        properties={
                            "description": f"Operation {operation.name} uses data item {data_item.name}",
                            "confidence": 0.8,
                            "relationship_type": "data_flow"
                        }
                    )
                    relationships.append(rel)
        
        # Risk-based relationships
        high_risk_sections = [s for s in ontology.sections if s.risk_level.value == "HIGH"]
        for i, section in enumerate(high_risk_sections):
            section_id = f"section_{i + 1}"
            
            # Link to business rules that might be related
            for j, rule in enumerate(ontology.business_rules):
                if rule.risk_level.value == "HIGH":
                    rule_id = f"rule_{j + 1}"
                    rel = CodeRelationship(
                        source_id=section_id,
                        target_id=rule_id,
                        relationship_type="RELATED_TO",
                        properties={
                            "description": f"High-risk section {section.name} is related to high-risk rule {rule.rule_id}",
                            "confidence": 0.7,
                            "relationship_type": "risk_assessment"
                        }
                    )
                    relationships.append(rel)
        
        return relationships


def convert_fraud_ontology_to_neo4j(ontology: FraudManagementOntology) -> GraphData:
    """Convenience function to convert fraud ontology to Neo4j graph data"""
    converter = FraudOntologyNeo4jConverter()
    return converter.convert_ontology_to_neo4j(ontology)


def main():
    """Demonstrate converting fraud management ontology to Neo4j format"""
    print("=== Fraud Management Ontology Neo4j Converter ===")
    
    # Create the fraud management ontology
    fraud_ontology = FraudManagementOntology()
    
    # Convert to Neo4j format
    converter = FraudOntologyNeo4jConverter()
    graph_data = converter.convert_ontology_to_neo4j(fraud_ontology)
    
    print(f"\nGraph Data Summary:")
    print(f"  Total Nodes: {graph_data.node_count}")
    print(f"  Total Relationships: {graph_data.relationship_count}")
    
    print(f"\nNode Types:")
    node_types = {}
    for node in graph_data.nodes:
        node_type = node.node_type
        node_types[node_type] = node_types.get(node_type, 0) + 1
    
    for node_type, count in node_types.items():
        print(f"  {node_type}: {count}")
    
    print(f"\nRelationship Types:")
    rel_types = {}
    for rel in graph_data.relationships:
        rel_type = rel.relationship_type
        rel_types[rel_type] = rel_types.get(rel_type, 0) + 1
    
    for rel_type, count in rel_types.items():
        print(f"  {rel_type}: {count}")
    
    print(f"\nSample Nodes:")
    for i, node in enumerate(graph_data.nodes[:5]):  # Show first 5 nodes
        print(f"  {i+1}. {node.node_type}: {node.name}")
        print(f"     Properties: {list(node.properties.keys())}")
    
    print(f"\nSample Relationships:")
    for i, rel in enumerate(graph_data.relationships[:5]):  # Show first 5 relationships
        print(f"  {i+1}. {rel.source_id} --[{rel.relationship_type}]--> {rel.target_id}")
        print(f"     Description: {rel.properties.get('description', 'N/A')}")
    
    print(f"\n✓ Fraud management ontology successfully converted to Neo4j format!")
    print(f"   Ready for storage in Neo4j graph database.")


if __name__ == "__main__":
    main()
