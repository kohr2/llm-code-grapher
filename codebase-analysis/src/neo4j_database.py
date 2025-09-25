"""
Neo4j Database Integration

This module provides functionality to connect to and interact with Neo4j database
for storing code analysis results.
"""

import os
from typing import Dict, Any, List, Optional, Union
from dataclasses import dataclass
import logging

try:
    from neo4j import GraphDatabase
except ImportError:
    GraphDatabase = None

try:
    from dotenv import load_dotenv
except ImportError:
    load_dotenv = None

try:
    from .neo4j_models import CodeNode, CodeRelationship, GraphData
except ImportError:
    from neo4j_models import CodeNode, CodeRelationship, GraphData

logger = logging.getLogger(__name__)


@dataclass
class Neo4jConfig:
    """Neo4j database configuration"""
    
    uri: str
    username: str
    password: str
    database: str = "neo4j"
    
    @classmethod
    def from_environment(cls, env_file: Optional[str] = None) -> 'Neo4jConfig':
        """Create Neo4j config from environment variables and .env file"""
        # Load .env file if available
        if load_dotenv and env_file:
            load_dotenv(env_file)
        elif load_dotenv:
            # Try to load .env from current directory
            load_dotenv()
        
        return cls(
            uri=os.getenv('NEO4J_URI', 'bolt://localhost:7687'),
            username=os.getenv('NEO4J_USERNAME', 'neo4j'),
            password=os.getenv('NEO4J_PASSWORD', 'password'),
            database=os.getenv('NEO4J_DATABASE', 'neo4j')
        )
    
    def validate(self) -> None:
        """Validate Neo4j configuration"""
        if not self.uri:
            raise ValueError("Neo4j URI cannot be empty")
        if not self.uri.startswith(('bolt://', 'neo4j://', 'neo4j+s://', 'neo4j+ssc://')):
            raise ValueError("Invalid Neo4j URI format")
        if not self.username:
            raise ValueError("Neo4j username cannot be empty")
        if not self.password:
            raise ValueError("Neo4j password cannot be empty")


class Neo4jDatabase:
    """Neo4j database connection and operations"""
    
    def __init__(self, config: Neo4jConfig):
        """Initialize Neo4j database connection"""
        if GraphDatabase is None:
            raise ImportError("Neo4j driver not installed. Run: pip install neo4j")
        
        self.config = config
        self.config.validate()
        self.driver = None
        self._connected = False
    
    def connect(self) -> None:
        """Connect to Neo4j database"""
        try:
            self.driver = GraphDatabase.driver(
                self.config.uri,
                auth=(self.config.username, self.config.password)
            )
            
            # Test connection
            with self.driver.session(database=self.config.database) as session:
                session.run("RETURN 1")
            
            self._connected = True
            logger.info(f"Connected to Neo4j at {self.config.uri}")
            
        except Exception as e:
            logger.error(f"Failed to connect to Neo4j: {e}")
            raise
    
    def disconnect(self) -> None:
        """Disconnect from Neo4j database"""
        if self.driver:
            self.driver.close()
            self.driver = None
            self._connected = False
            logger.info("Disconnected from Neo4j")
    
    def is_connected(self) -> bool:
        """Check if connected to Neo4j"""
        return self._connected and self.driver is not None
    
    def clear_database(self) -> None:
        """Clear all data from the database"""
        if not self.is_connected():
            raise RuntimeError("Not connected to Neo4j database")
        
        with self.driver.session(database=self.config.database) as session:
            session.run("MATCH (n) DETACH DELETE n")
            logger.info("Cleared Neo4j database")
    
    def insert_node(self, node: CodeNode) -> None:
        """Insert a single node into Neo4j"""
        if not self.is_connected():
            raise RuntimeError("Not connected to Neo4j database")
        
        node.validate()
        cypher = node.to_cypher()
        
        with self.driver.session(database=self.config.database) as session:
            session.run(cypher)
            logger.debug(f"Inserted node: {node.node_id}")
    
    def insert_nodes(self, nodes: List[CodeNode]) -> None:
        """Insert multiple nodes into Neo4j"""
        if not self.is_connected():
            raise RuntimeError("Not connected to Neo4j database")
        
        for node in nodes:
            self.insert_node(node)
        
        logger.info(f"Inserted {len(nodes)} nodes")
    
    def insert_relationship(self, relationship: CodeRelationship) -> None:
        """Insert a single relationship into Neo4j"""
        if not self.is_connected():
            raise RuntimeError("Not connected to Neo4j database")
        
        relationship.validate()
        cypher = relationship.to_cypher()
        
        with self.driver.session(database=self.config.database) as session:
            session.run(cypher)
            logger.debug(f"Inserted relationship: {relationship.source_id} -> {relationship.target_id}")
    
    def insert_relationships(self, relationships: List[CodeRelationship]) -> None:
        """Insert multiple relationships into Neo4j"""
        if not self.is_connected():
            raise RuntimeError("Not connected to Neo4j database")
        
        for relationship in relationships:
            self.insert_relationship(relationship)
        
        logger.info(f"Inserted {len(relationships)} relationships")
    
    def insert_graph_data(self, graph_data: GraphData) -> None:
        """Insert complete graph data into Neo4j"""
        if not self.is_connected():
            raise RuntimeError("Not connected to Neo4j database")
        
        graph_data.validate()
        
        # Insert nodes first
        if graph_data.nodes:
            self.insert_nodes(graph_data.nodes)
        
        # Then insert relationships
        if graph_data.relationships:
            self.insert_relationships(graph_data.relationships)
        
        logger.info(f"Inserted graph data: {graph_data.node_count} nodes, {graph_data.relationship_count} relationships")
    
    def query(self, cypher_query: str, parameters: Optional[Dict[str, Any]] = None) -> List[Dict[str, Any]]:
        """Execute a Cypher query and return results"""
        if not self.is_connected():
            raise RuntimeError("Not connected to Neo4j database")
        
        with self.driver.session(database=self.config.database) as session:
            result = session.run(cypher_query, parameters or {})
            return [record.data() for record in result]
    
    def get_node_count(self) -> int:
        """Get total number of nodes in the database"""
        result = self.query("MATCH (n) RETURN count(n) as count")
        return result[0]['count'] if result else 0
    
    def get_relationship_count(self) -> int:
        """Get total number of relationships in the database"""
        result = self.query("MATCH ()-[r]->() RETURN count(r) as count")
        return result[0]['count'] if result else 0
    
    def get_program_nodes(self) -> List[Dict[str, Any]]:
        """Get all program nodes"""
        return self.query("MATCH (n:Program) RETURN n")
    
    def get_section_nodes(self) -> List[Dict[str, Any]]:
        """Get all section nodes"""
        return self.query("MATCH (n:Section) RETURN n")
    
    def get_relationships_by_type(self, relationship_type: str) -> List[Dict[str, Any]]:
        """Get all relationships of a specific type"""
        return self.query(f"MATCH ()-[r:{relationship_type}]->() RETURN r")
    
    def get_code_structure(self, program_name: Optional[str] = None) -> Dict[str, Any]:
        """Get complete code structure for analysis"""
        if program_name:
            # Get structure for specific program
            programs = self.query(
                "MATCH (p:Program {name: $program_name}) RETURN p",
                {"program_name": program_name}
            )
        else:
            # Get all programs
            programs = self.get_program_nodes()
        
        structure = {
            "programs": programs,
            "total_nodes": self.get_node_count(),
            "total_relationships": self.get_relationship_count()
        }
        
        return structure
    
    def __enter__(self):
        """Context manager entry"""
        self.connect()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        self.disconnect()


def create_neo4j_database(config: Optional[Neo4jConfig] = None, env_file: Optional[str] = None) -> Neo4jDatabase:
    """Create and return a Neo4j database instance"""
    if config is None:
        config = Neo4jConfig.from_environment(env_file)
    
    return Neo4jDatabase(config)
