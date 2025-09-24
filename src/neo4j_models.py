"""
Neo4j Models for Code Graph Database

This module defines the data models for storing code analysis results
in Neo4j graph database.
"""

from dataclasses import dataclass, field
from typing import Dict, Any, List, Optional
import json


@dataclass
class CodeNode:
    """Represents a node in the code graph (Program, Section, Subsection, etc.)"""
    
    node_id: str
    node_type: str  # Program, Section, Subsection, Data, etc.
    name: str
    language: str
    properties: Dict[str, Any] = field(default_factory=dict)
    
    def to_cypher(self) -> str:
        """Convert node to Cypher CREATE statement"""
        # Escape single quotes in string values
        escaped_name = self.name.replace("'", "\\'")
        
        # Build properties string
        props = {
            "node_id": f"'{self.node_id}'",
            "name": f"'{escaped_name}'",
            "language": f"'{self.language}'",
            **{k: self._format_property_value(v) for k, v in self.properties.items()}
        }
        
        props_str = ", ".join([f"{k}: {v}" for k, v in props.items()])
        
        return f"CREATE (n:{self.node_type} {{{props_str}}})"
    
    def _format_property_value(self, value: Any) -> str:
        """Format a property value for Cypher syntax"""
        if isinstance(value, dict):
            # For nested objects, create a proper Cypher map syntax
            inner_props = []
            for k, v in value.items():
                formatted_v = self._format_property_value(v)
                inner_props.append(f"{k}: {formatted_v}")
            return f"{{{', '.join(inner_props)}}}"
        elif isinstance(value, (list, tuple)):
            # For lists/arrays, create Cypher array syntax
            formatted_items = [self._format_property_value(item) for item in value]
            return f"[{', '.join(formatted_items)}]"
        elif isinstance(value, str):
            # Escape single quotes in strings
            escaped_value = value.replace("'", "\\'")
            return f"'{escaped_value}'"
        else:
            # For numbers, booleans, etc., use repr
            return repr(value)
    
    def validate(self) -> None:
        """Validate node data"""
        if not self.node_id:
            raise ValueError("Node ID cannot be empty")
        if not self.node_type:
            raise ValueError("Node type cannot be empty")
        if not self.name:
            raise ValueError("Node name cannot be empty")
        if not self.language:
            raise ValueError("Language cannot be empty")


@dataclass
class CodeRelationship:
    """Represents a relationship in the code graph (PERFORM, CALLS, CONTAINS, etc.)"""
    
    source_id: str
    target_id: str
    relationship_type: str
    properties: Dict[str, Any] = field(default_factory=dict)
    
    def to_cypher(self) -> str:
        """Convert relationship to Cypher CREATE statement"""
        # Build properties string
        props = {k: self._format_property_value(v) for k, v in self.properties.items()}
        
        props_str = ", ".join([f"{k}: {v}" for k, v in props.items()]) if props else ""
        props_clause = f" {{{props_str}}}" if props_str else ""
        
        return (
            f"MATCH (source {{node_id: '{self.source_id}'}}), "
            f"(target {{node_id: '{self.target_id}'}}) "
            f"CREATE (source)-[r:{self.relationship_type}{props_clause}]->(target)"
        )
    
    def _format_property_value(self, value: Any) -> str:
        """Format a property value for Cypher syntax"""
        if isinstance(value, dict):
            # For nested objects, create a proper Cypher map syntax
            inner_props = []
            for k, v in value.items():
                formatted_v = self._format_property_value(v)
                inner_props.append(f"{k}: {formatted_v}")
            return f"{{{', '.join(inner_props)}}}"
        elif isinstance(value, (list, tuple)):
            # For lists/arrays, create Cypher array syntax
            formatted_items = [self._format_property_value(item) for item in value]
            return f"[{', '.join(formatted_items)}]"
        elif isinstance(value, str):
            # Escape single quotes in strings
            escaped_value = value.replace("'", "\\'")
            return f"'{escaped_value}'"
        else:
            # For numbers, booleans, etc., use repr
            return repr(value)
    
    def validate(self) -> None:
        """Validate relationship data"""
        if not self.source_id:
            raise ValueError("Source ID cannot be empty")
        if not self.target_id:
            raise ValueError("Target ID cannot be empty")
        if not self.relationship_type:
            raise ValueError("Relationship type cannot be empty")


@dataclass
class GraphData:
    """Container for nodes and relationships to be inserted into Neo4j"""
    
    nodes: List[CodeNode] = field(default_factory=list)
    relationships: List[CodeRelationship] = field(default_factory=list)
    
    @property
    def node_count(self) -> int:
        """Number of nodes in the graph"""
        return len(self.nodes)
    
    @property
    def relationship_count(self) -> int:
        """Number of relationships in the graph"""
        return len(self.relationships)
    
    def add_node(self, node: CodeNode) -> None:
        """Add a node to the graph"""
        node.validate()
        self.nodes.append(node)
    
    def add_relationship(self, relationship: CodeRelationship) -> None:
        """Add a relationship to the graph"""
        relationship.validate()
        self.relationships.append(relationship)
    
    def validate(self) -> None:
        """Validate all nodes and relationships"""
        for node in self.nodes:
            if not isinstance(node, CodeNode):
                raise ValueError("Invalid node type")
            node.validate()
        
        for relationship in self.relationships:
            if not isinstance(relationship, CodeRelationship):
                raise ValueError("Invalid relationship type")
            relationship.validate()
    
    def get_node_by_id(self, node_id: str) -> Optional[CodeNode]:
        """Get a node by its ID"""
        for node in self.nodes:
            if node.node_id == node_id:
                return node
        return None
    
    def get_relationships_by_type(self, relationship_type: str) -> List[CodeRelationship]:
        """Get all relationships of a specific type"""
        return [rel for rel in self.relationships if rel.relationship_type == relationship_type]
    
    def get_relationships_from_node(self, source_id: str) -> List[CodeRelationship]:
        """Get all relationships from a specific node"""
        return [rel for rel in self.relationships if rel.source_id == source_id]
    
    def get_relationships_to_node(self, target_id: str) -> List[CodeRelationship]:
        """Get all relationships to a specific node"""
        return [rel for rel in self.relationships if rel.target_id == target_id]
    
    def to_cypher(self) -> List[str]:
        """Convert entire graph to Cypher statements"""
        cypher_statements = []
        
        # Add all nodes
        for node in self.nodes:
            cypher_statements.append(node.to_cypher())
        
        # Add all relationships
        for relationship in self.relationships:
            cypher_statements.append(relationship.to_cypher())
        
        return cypher_statements
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert graph data to dictionary for JSON serialization"""
        return {
            "nodes": [
                {
                    "node_id": node.node_id,
                    "node_type": node.node_type,
                    "name": node.name,
                    "language": node.language,
                    "properties": node.properties
                }
                for node in self.nodes
            ],
            "relationships": [
                {
                    "source_id": rel.source_id,
                    "target_id": rel.target_id,
                    "relationship_type": rel.relationship_type,
                    "properties": rel.properties
                }
                for rel in self.relationships
            ],
            "node_count": self.node_count,
            "relationship_count": self.relationship_count
        }

