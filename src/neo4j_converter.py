"""
Neo4j Converter

This module converts parser results to Neo4j graph data format.
"""

from typing import Dict, Any, List, Optional
import logging

try:
    from .neo4j_models import CodeNode, CodeRelationship, GraphData
except ImportError:
    from neo4j_models import CodeNode, CodeRelationship, GraphData

from lang.base.parser.base_parser import BaseParserResult

logger = logging.getLogger(__name__)


class ParserResultConverter:
    """Converts parser results to Neo4j graph data"""
    
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
    
    def convert_parser_result(self, result: BaseParserResult) -> GraphData:
        """Convert a parser result to GraphData"""
        graph_data = GraphData()
        
        # Store graph data for relationship mapping
        self._current_graph_data = graph_data
        
        # Add program node
        program_node = self._create_program_node(result)
        graph_data.add_node(program_node)
        
        # Add section nodes
        section_nodes = self._create_section_nodes(result, program_node.node_id)
        for node in section_nodes:
            graph_data.add_node(node)
        
        # Add subsection nodes
        subsection_nodes = self._create_subsection_nodes(result, program_node.node_id)
        for node in subsection_nodes:
            graph_data.add_node(node)
        
        # Add data nodes (if any)
        data_nodes = self._create_data_nodes(result, program_node.node_id)
        for node in data_nodes:
            graph_data.add_node(node)
        
        # Add operation nodes (if any)
        operation_nodes = self._create_operation_nodes(result, program_node.node_id)
        for node in operation_nodes:
            graph_data.add_node(node)
        
        # Add business rule nodes
        business_rule_nodes = self._create_business_rule_nodes(result, program_node.node_id)
        for node in business_rule_nodes:
            graph_data.add_node(node)
        
        # Add relationships (now with access to all nodes)
        relationships = self._create_relationships(result, program_node.node_id)
        for rel in relationships:
            graph_data.add_relationship(rel)
        
        logger.info(f"Converted parser result: {graph_data.node_count} nodes, {graph_data.relationship_count} relationships")
        return graph_data
    
    def _create_program_node(self, result: BaseParserResult) -> CodeNode:
        """Create a program node from parser result"""
        properties = {
            "line_count": getattr(result.program, 'line_count', 0),
            "file_path": getattr(result.program, 'file_path', ''),
            "created_at": getattr(result.program, 'created_at', None),
            "complexity_score": getattr(result.program, 'complexity_score', 0.0),
            "business_logic": getattr(result.program, 'business_logic', ''),
            "risk_level": getattr(result.program, 'risk_level', 'UNKNOWN'),
            "confidence": getattr(result.program, 'confidence', 0.0)
        }
        
        # Remove None values
        properties = {k: v for k, v in properties.items() if v is not None}
        
        return CodeNode(
            node_id=self._generate_node_id("program"),
            node_type="Program",
            name=result.program.name,
            language=result.program.language,
            properties=properties
        )
    
    def _create_section_nodes(self, result: BaseParserResult, program_id: str) -> List[CodeNode]:
        """Create section nodes from parser result"""
        nodes = []
        
        for i, section in enumerate(result.sections):
            properties = {
                "line_range": getattr(section, 'line_range', [0, 0]),
                "business_logic": getattr(section, 'business_logic', ''),
                "complexity_score": getattr(section, 'complexity_score', 0.0),
                "risk_level": getattr(section, 'risk_level', 'UNKNOWN'),
                "confidence": getattr(section, 'confidence', 0.0),
                "section_type": getattr(section, 'type', 'UNKNOWN')
            }
            
            # Remove None values
            properties = {k: v for k, v in properties.items() if v is not None}
            
            # Use enumeration-based ID to match relationship creation
            section_id = f"section_{i + 1}"
            node = CodeNode(
                node_id=section_id,
                node_type="Section",
                name=section.name,
                language=result.program.language,
                properties=properties
            )
            nodes.append(node)
        
        return nodes
    
    def _create_subsection_nodes(self, result: BaseParserResult, program_id: str) -> List[CodeNode]:
        """Create subsection nodes from parser result"""
        nodes = []
        
        for i, subsection in enumerate(result.subsections):
            properties = {
                "line_range": getattr(subsection, 'line_range', [0, 0]),
                "business_logic": getattr(subsection, 'business_logic', ''),
                "complexity_score": getattr(subsection, 'complexity_score', 0.0),
                "risk_level": getattr(subsection, 'risk_level', 'UNKNOWN'),
                "confidence": getattr(subsection, 'confidence', 0.0),
                "subsection_type": getattr(subsection, 'type', 'UNKNOWN'),
                "parent_section": getattr(subsection, 'parent_section', 'UNKNOWN')
            }
            
            # Remove None values
            properties = {k: v for k, v in properties.items() if v is not None}
            
            # Use enumeration-based ID to match relationship creation
            subsection_id = f"subsection_{len(nodes) + 1}"
            
            node = CodeNode(
                node_id=subsection_id,
                node_type="Subsection",
                name=subsection.name or f"UNNAMED_SUBSECTION_{i+1}",  # Fallback name
                language=result.program.language,
                properties=properties
            )
            
            nodes.append(node)
        
        return nodes
    
    def _create_data_nodes(self, result: BaseParserResult, program_id: str) -> List[CodeNode]:
        """Create data nodes from parser result (if any data structures are found)"""
        nodes = []
        
        # This would be implemented based on the specific parser result structure
        # For now, we'll create a placeholder
        if hasattr(result, 'data_structures') and result.data_structures:
            for i, data_struct in enumerate(result.data_structures):
                properties = {
                    "data_type": getattr(data_struct, 'data_type', 'UNKNOWN'),
                    "size": getattr(data_struct, 'size', 0),
                    "description": getattr(data_struct, 'description', ''),
                    "confidence": getattr(data_struct, 'confidence', 0.0)
                }
                
                # Remove None values
                properties = {k: v for k, v in properties.items() if v is not None}
                
                node = CodeNode(
                    node_id=self._generate_node_id("data"),
                    node_type="Data",
                    name=getattr(data_struct, 'name', f'Data_{i}'),
                    language=result.program.language,
                    properties=properties
                )
                nodes.append(node)
        
        return nodes
    
    def _create_business_rule_nodes(self, result: BaseParserResult, program_id: str) -> List[CodeNode]:
        """Create business rule nodes from fraud rule subsections in parser result"""
        nodes = []
        
        # Extract fraud rules from subsections and deduplicate by rule name
        fraud_rules = []
        seen_rules = set()
        
        for subsection in result.subsections:
            if subsection.name and 'RULE-' in subsection.name:
                # Extract the rule type (e.g., "RULE-HIGH-AMOUNT" from "2610-RULE-HIGH-AMOUNT")
                rule_type = None
                for rule_key in ["RULE-HIGH-AMOUNT", "RULE-VELOCITY-CHECK", "RULE-LOCATION-VARIANCE", 
                               "RULE-MERCHANT-RISK", "RULE-TIME-PATTERN", "RULE-CARD-NOT-PRESENT", 
                               "RULE-SUSPICIOUS-CATEGORY", "RULE-CUSTOMER-BEHAVIOR", "RULE-ACCOUNT-AGE", 
                               "RULE-CROSS-VALIDATION"]:
                    if rule_key in subsection.name:
                        rule_type = rule_key
                        break
                
                if rule_type and rule_type not in seen_rules:
                    seen_rules.add(rule_type)
                    fraud_rules.append(subsection)
        
        # Create business rule nodes
        for i, rule in enumerate(fraud_rules):
            # Extract rule ID from subsection name (e.g., "2610-RULE-HIGH-AMOUNT" -> "RULE-01")
            rule_id = f"RULE-{i+1:02d}"
            
            # Extract rule description from business logic
            description = self._extract_rule_description(rule.business_logic, rule.name)
            
            # Determine priority and risk level based on rule type
            priority, risk_level = self._determine_rule_priority_and_risk(rule.name)
            
            properties = {
                "description": description,
                "priority": priority,
                "risk_level": risk_level,
                "business_impact": "CRITICAL",
                "detection_accuracy": "85-90%",
                "line_range": getattr(rule, 'line_range', [0, 0]),
                "confidence": getattr(rule, 'confidence', 0.0),
                "parent_section": getattr(rule, 'parent_section', 'UNKNOWN')
            }
            
            # Remove None values
            properties = {k: v for k, v in properties.items() if v is not None}
            
            node = CodeNode(
                node_id=f"rule_{i + 1}",
                node_type="BusinessRule",
                name=rule_id,
                language=result.program.language,
                properties=properties
            )
            nodes.append(node)
        
        return nodes
    
    def _extract_rule_description(self, business_logic: str, rule_name: str) -> str:
        """Extract rule description from business logic"""
        # Map rule names to descriptions
        rule_descriptions = {
            "RULE-HIGH-AMOUNT": "High Amount Transaction - Detects transactions exceeding suspicious amount threshold",
            "RULE-VELOCITY-CHECK": "Transaction Velocity Analysis - Monitors hourly and daily transaction frequency",
            "RULE-LOCATION-VARIANCE": "Geographical Location Analysis - Detects unusual location patterns",
            "RULE-MERCHANT-RISK": "Merchant Risk Assessment - Evaluates merchant risk levels and categories",
            "RULE-TIME-PATTERN": "Unusual Time Pattern - Detects transactions at unusual times",
            "RULE-CARD-NOT-PRESENT": "Card Not Present Risk - Higher risk for online/telephone transactions",
            "RULE-SUSPICIOUS-CATEGORY": "Suspicious Category Combinations - Multiple merchant categories",
            "RULE-CUSTOMER-BEHAVIOR": "Customer Behavioral Analysis - Analyzes customer spending patterns",
            "RULE-ACCOUNT-AGE": "New Account Risk - Higher risk for new accounts",
            "RULE-CROSS-VALIDATION": "Cross-validation of multiple risk factors - Combines multiple rule triggers"
        }
        
        # Try to find matching rule name
        for rule_key, description in rule_descriptions.items():
            if rule_key in rule_name:
                return description
        
        # Fallback: extract from business logic comment
        if "Rule" in business_logic:
            lines = business_logic.split('\n')
            for line in lines:
                if "Rule" in line and ":" in line:
                    return line.split(":", 1)[1].strip()
        
        return f"Business rule: {rule_name}"
    
    def _determine_rule_priority_and_risk(self, rule_name: str) -> tuple[str, str]:
        """Determine priority and risk level for a business rule"""
        # High priority rules
        high_priority_rules = ["RULE-HIGH-AMOUNT", "RULE-VELOCITY-CHECK", "RULE-CARD-NOT-PRESENT", "RULE-CUSTOMER-BEHAVIOR"]
        
        # High risk rules
        high_risk_rules = ["RULE-CUSTOMER-BEHAVIOR", "RULE-CROSS-VALIDATION"]
        
        priority = "HIGH" if any(rule in rule_name for rule in high_priority_rules) else "MEDIUM"
        risk_level = "HIGH" if any(rule in rule_name for rule in high_risk_rules) else "MEDIUM"
        
        return priority, risk_level
    
    def _create_business_rule_relationships(self, result: BaseParserResult, program_id: str) -> List[CodeRelationship]:
        """Create relationships between sections and business rules"""
        relationships = []
        
        # Find the fraud rules section
        fraud_rules_section_id = None
        for i, section in enumerate(result.sections):
            if section.name and 'EXECUTE-FRAUD-RULES' in section.name:
                fraud_rules_section_id = f"section_{i + 1}"
                break
        
        if not fraud_rules_section_id:
            return relationships
        
        # Create IMPLEMENTS relationships between fraud rules section and business rules
        fraud_rules = []
        seen_rules = set()
        
        for subsection in result.subsections:
            if subsection.name and 'RULE-' in subsection.name:
                # Extract the rule type (e.g., "RULE-HIGH-AMOUNT" from "2610-RULE-HIGH-AMOUNT")
                rule_type = None
                for rule_key in ["RULE-HIGH-AMOUNT", "RULE-VELOCITY-CHECK", "RULE-LOCATION-VARIANCE", 
                               "RULE-MERCHANT-RISK", "RULE-TIME-PATTERN", "RULE-CARD-NOT-PRESENT", 
                               "RULE-SUSPICIOUS-CATEGORY", "RULE-CUSTOMER-BEHAVIOR", "RULE-ACCOUNT-AGE", 
                               "RULE-CROSS-VALIDATION"]:
                    if rule_key in subsection.name:
                        rule_type = rule_key
                        break
                
                if rule_type and rule_type not in seen_rules:
                    seen_rules.add(rule_type)
                    fraud_rules.append(subsection)
        
        for i, rule in enumerate(fraud_rules):
            rule_id = f"rule_{i + 1}"
            
            # Create IMPLEMENTS relationship
            rel = CodeRelationship(
                source_id=fraud_rules_section_id,
                target_id=rule_id,
                relationship_type="IMPLEMENTS",
                properties={
                    "description": f"Section implements {rule.name}",
                    "confidence": 1.0,
                    "rule_name": rule.name,
                    "parent_section": getattr(rule, 'parent_section', 'UNKNOWN')
                }
            )
            relationships.append(rel)
        
        return relationships
    
    def _create_operation_nodes(self, result: BaseParserResult, program_id: str) -> List[CodeNode]:
        """Create operation nodes from parser result"""
        nodes = []
        
        # Check if result has operations (COBOL-specific)
        if hasattr(result, 'operations') and result.operations:
            for i, operation in enumerate(result.operations):
                properties = {
                    "line_range": getattr(operation, 'line_range', [0, 0]),
                    "business_logic": getattr(operation, 'business_logic', ''),
                    "complexity_score": getattr(operation, 'complexity_score', 0.0),
                    "risk_level": getattr(operation, 'risk_level', 'UNKNOWN'),
                    "confidence": getattr(operation, 'confidence', 0.0),
                    "operation_type": getattr(operation, 'operation_type', 'UNKNOWN'),
                    "parent_subsection": getattr(operation, 'parent_subsection', 'UNKNOWN'),
                    "parameters": getattr(operation, 'parameters', []),
                    "parameter_count": len(getattr(operation, 'parameters', []))
                }
                
                # Remove None values
                properties = {k: v for k, v in properties.items() if v is not None}
                
                # Use enumeration-based ID to match relationship creation
                operation_id = f"operation_{i + 1}"
                node = CodeNode(
                    node_id=operation_id,
                    node_type="Operation",
                    name=operation.name,
                    language=result.program.language,
                    properties=properties
                )
                nodes.append(node)
        
        return nodes
    
    def _create_relationships(self, result: BaseParserResult, program_id: str) -> List[CodeRelationship]:
        """Create relationships from parser result"""
        relationships = []
        
        # Create proper hierarchical tree structure
        # First, separate divisions from regular sections
        divisions = []
        regular_sections = []
        
        for i, section in enumerate(result.sections):
            section_id = f"section_{i + 1}"
            if section.type in ['IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE']:
                # This is a division - link directly to program
                divisions.append((section_id, section))
                rel = CodeRelationship(
                    source_id=program_id,
                    target_id=section_id,
                    relationship_type="CONTAINS",
                    properties={
                        "description": f"Program contains division {section.name}",
                        "confidence": 1.0
                    }
                )
                relationships.append(rel)
            else:
                # This is a regular section
                regular_sections.append((section_id, section))
        
        # Link regular sections to their immediate parent (division or program)
        for section_id, section in regular_sections:
            # Find the immediate parent division based on line ranges
            parent_division_id = None
            for div_id, division in divisions:
                # Check if this section is contained within this division
                if (division.line_range[0] <= section.line_range[0] and 
                    section.line_range[1] <= division.line_range[1]):
                    parent_division_id = div_id
                    break
            
            if parent_division_id:
                rel = CodeRelationship(
                    source_id=parent_division_id,
                    target_id=section_id,
                    relationship_type="CONTAINS",
                    properties={
                        "description": f"Division contains section {section.name}",
                        "confidence": 1.0
                    }
                )
                relationships.append(rel)
            else:
                # Fallback: link directly to program
                rel = CodeRelationship(
                    source_id=program_id,
                    target_id=section_id,
                    relationship_type="CONTAINS",
                    properties={
                        "description": f"Program contains section {section.name}",
                        "confidence": 1.0
                    }
                )
                relationships.append(rel)
        
        # Add section to subsection relationships - ensure immediate parent only
        for i, subsection in enumerate(result.subsections):
            subsection_id = f"subsection_{i + 1}"  # This should match the ID generation in _create_subsection_nodes
            
            # Find the immediate parent section ID
            parent_section_id = None
            if hasattr(subsection, 'parent_section') and subsection.parent_section:
                # Find the section with matching name (only regular sections, not divisions)
                for j, section in enumerate(result.sections):
                    if (section.name == subsection.parent_section and 
                        section.type not in ['IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE']):
                        parent_section_id = f"section_{j + 1}"
                        break
            
            # If we found a parent section, link subsection to section
            # Otherwise, link to program as fallback
            if parent_section_id:
                rel = CodeRelationship(
                    source_id=parent_section_id,
                    target_id=subsection_id,
                    relationship_type="CONTAINS",
                    properties={
                        "description": f"Section {subsection.parent_section} contains subsection {subsection.name}",
                        "confidence": 1.0
                    }
                )
                relationships.append(rel)
            else:
                # Fallback: link to program if no parent section found
                rel = CodeRelationship(
                    source_id=program_id,
                    target_id=subsection_id,
                    relationship_type="CONTAINS",
                    properties={
                        "description": f"Program contains subsection {subsection.name}",
                        "confidence": 1.0
                    }
                )
                relationships.append(rel)
        
        # Add operation to subsection relationships
        if hasattr(result, 'operations') and result.operations:
            for i, operation in enumerate(result.operations):
                operation_id = f"operation_{i + 1}"
                
                # Find the parent subsection ID
                parent_subsection_id = None
                if hasattr(operation, 'parent_subsection') and operation.parent_subsection:
                    # Find the subsection with matching name
                    for j, subsection in enumerate(result.subsections):
                        if subsection.name == operation.parent_subsection:
                            parent_subsection_id = f"subsection_{j + 1}"
                            break
                
                # If we found a parent subsection, link operation to subsection
                # Otherwise, link to program as fallback
                if parent_subsection_id:
                    rel = CodeRelationship(
                        source_id=parent_subsection_id,
                        target_id=operation_id,
                        relationship_type="CONTAINS",
                        properties={
                            "description": f"Subsection {operation.parent_subsection} contains operation {operation.name}",
                            "confidence": 1.0
                        }
                    )
                    relationships.append(rel)
                else:
                    # Fallback: link to program if no parent subsection found
                    rel = CodeRelationship(
                        source_id=program_id,
                        target_id=operation_id,
                        relationship_type="CONTAINS",
                        properties={
                            "description": f"Program contains operation {operation.name}",
                            "confidence": 1.0
                        }
                    )
                    relationships.append(rel)
        
        # Add business rule relationships
        business_rule_relationships = self._create_business_rule_relationships(result, program_id)
        relationships.extend(business_rule_relationships)
        
        # Add relationships from parser result
        for i, rel in enumerate(result.relationships):
            # Skip relationships that reference non-existent sections
            # Since we're ignoring line numbers, we can't map numeric targets to actual sections
            if rel.target.isdigit():
                # Skip numeric targets that don't correspond to actual sections
                continue
            
            # Map source and target to actual node IDs
            source_id = self._map_to_node_id(rel.source, program_id)
            target_id = self._map_to_node_id(rel.target, program_id)
            
            # Skip relationships that map to the same node (avoid self-loops)
            if source_id == target_id:
                continue
            
            # Special handling for operation-to-operation relationships
            if (rel.relationship_type in ["NEXT", "DEPENDS_ON", "CALLS"] and 
                source_id.startswith("operation_") and target_id.startswith("operation_")):
                # This is an operation-to-operation relationship
                relationship = CodeRelationship(
                    source_id=source_id,
                    target_id=target_id,
                    relationship_type=rel.relationship_type,
                    properties={
                        "confidence": getattr(rel, 'confidence', 0.0),
                        "strength": getattr(rel, 'strength', 1.0),
                        "description": f"Operation {rel.source} {rel.relationship_type.lower()} {rel.target}",
                        "original_source": rel.source,
                        "original_target": rel.target,
                        "flow_type": "operation_flow"
                    }
                )
            else:
                # Regular relationship
                relationship = CodeRelationship(
                    source_id=source_id,
                    target_id=target_id,
                    relationship_type=rel.relationship_type,
                    properties={
                        "confidence": getattr(rel, 'confidence', 0.0),
                        "line_number": getattr(rel, 'line_number', 0),
                        "description": getattr(rel, 'description', ''),
                        "original_source": rel.source,
                        "original_target": rel.target
                    }
                )
            
            relationships.append(relationship)
        
        # Validate tree structure - ensure no circular or multiple parent relationships
        self._validate_tree_structure(relationships)
        
        return relationships
    
    def _validate_tree_structure(self, relationships: List[CodeRelationship]) -> None:
        """Validate that relationships form a proper tree structure"""
        # Check for multiple parents for the same child
        child_parents = {}
        for rel in relationships:
            if rel.relationship_type == "CONTAINS":
                if rel.target_id in child_parents:
                    logger.warning(f"Node {rel.target_id} has multiple parents: {child_parents[rel.target_id]} and {rel.source_id}")
                child_parents[rel.target_id] = rel.source_id
        
        # Check for circular references
        for rel in relationships:
            if rel.relationship_type == "CONTAINS":
                if self._has_circular_reference(rel.source_id, rel.target_id, relationships):
                    logger.warning(f"Circular reference detected: {rel.source_id} -> {rel.target_id}")
    
    def _has_circular_reference(self, start_id: str, target_id: str, relationships: List[CodeRelationship]) -> bool:
        """Check if adding start_id -> target_id would create a circular reference"""
        # A circular reference exists if target_id can reach start_id through existing relationships
        visited = set()
        return self._dfs_circular_check(target_id, start_id, relationships, visited)
    
    def _dfs_circular_check(self, current_id: str, target_id: str, relationships: List[CodeRelationship], visited: set) -> bool:
        """DFS to check if current_id can reach target_id through existing relationships"""
        if current_id == target_id:
            return True
        
        if current_id in visited:
            return False
        
        visited.add(current_id)
        
        # Find all children of current_id
        for rel in relationships:
            if rel.relationship_type == "CONTAINS" and rel.source_id == current_id:
                if self._dfs_circular_check(rel.target_id, target_id, relationships, visited):
                    return True
        
        return False
    
    def _map_to_node_id(self, name: str, program_id: str) -> str:
        """Map a name to a node ID"""
        # This is a simplified mapping - in practice, you'd want more sophisticated logic
        # to map relationship names to actual node IDs
        
        # If it's a number, it might be a section reference
        # Since we're ignoring line numbers, we'll map numbers to program for now
        # In a real implementation, you'd want more sophisticated mapping logic
        if name.isdigit():
            # For now, map all numeric references to the program node
            # This could be improved with better relationship analysis
            return program_id
        
        # Check if this is an operation name (contains operation type patterns)
        operation_patterns = ['-read-', '-add-', '-if-', '-display-', '-move-', '-perform-', 
                            '-call-', '-compute-', '-evaluate-', '-set-', '-open-', '-close-',
                            '-write-', '-delete-', '-rewrite-', '-start-', '-stop-', '-accept-',
                            '-initialize-', '-string-', '-unstring-', '-inspect-', '-search-',
                            '-sort-', '-merge-', '-release-', '-return-']
        
        if any(pattern in name.lower() for pattern in operation_patterns):
            # This looks like an operation name, try to find matching operation
            for i, node in enumerate(self._current_graph_data.nodes):
                if node.name == name and node.node_type == "Operation":
                    return f"operation_{i + 1}"
        
        # If it contains common section keywords, treat as section
        section_keywords = ['SECTION', 'PARA', 'PROCEDURE']
        if any(keyword in name.upper() for keyword in section_keywords):
            return f"section_{name.replace(' ', '_')}"
        
        # Default to program for now
        return program_id


def convert_parser_result_to_neo4j(result: BaseParserResult) -> GraphData:
    """Convenience function to convert parser result to Neo4j graph data"""
    converter = ParserResultConverter()
    return converter.convert_parser_result(result)
