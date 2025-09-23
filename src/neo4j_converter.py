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
        
        # Add relationships
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
            
            node = CodeNode(
                node_id=self._generate_node_id("section"),
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
                "subsection_type": getattr(subsection, 'type', 'UNKNOWN')
            }
            
            # Remove None values
            properties = {k: v for k, v in properties.items() if v is not None}
            
            node = CodeNode(
                node_id=self._generate_node_id("subsection"),
                node_type="Subsection",
                name=subsection.name,
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
    
    def _create_relationships(self, result: BaseParserResult, program_id: str) -> List[CodeRelationship]:
        """Create relationships from parser result"""
        relationships = []
        
        # Add program to section relationships
        for i, section in enumerate(result.sections):
            section_id = f"section_{i + 1}"  # This should match the ID generation in _create_section_nodes
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
        
        # Add program to subsection relationships
        for i, subsection in enumerate(result.subsections):
            subsection_id = f"subsection_{i + 1}"  # This should match the ID generation in _create_subsection_nodes
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
        
        # Add relationships from parser result
        for i, rel in enumerate(result.relationships):
            # Map source and target to actual node IDs
            source_id = self._map_to_node_id(rel.source, program_id)
            target_id = self._map_to_node_id(rel.target, program_id)
            
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
        
        return relationships
    
    def _map_to_node_id(self, name: str, program_id: str) -> str:
        """Map a name to a node ID"""
        # This is a simplified mapping - in practice, you'd want more sophisticated logic
        # to map relationship names to actual node IDs
        
        # If it's a number, it might be a section reference
        if name.isdigit():
            return f"section_{name}"
        
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
