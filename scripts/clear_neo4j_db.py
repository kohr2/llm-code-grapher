#!/usr/bin/env python3
"""
Clear Neo4j Database Script
Clears all nodes and relationships from the Neo4j database
"""

import sys
from pathlib import Path

# Add the codebase-analysis directory to the path
sys.path.append(str(Path(__file__).parent.parent / "codebase-analysis" / "src"))
sys.path.append(str(Path(__file__).parent.parent / "codebase-analysis"))

from neo4j_database import Neo4jDatabase, Neo4jConfig


def main():
    """Clear the Neo4j database"""
    print("Clearing Neo4j database...")
    
    try:
        # Create database connection
        config = Neo4jConfig.from_environment()
        db = Neo4jDatabase(config)
        db.connect()
        
        # Check current status
        node_count = db.get_node_count()
        rel_count = db.get_relationship_count()
        print(f"Current database status: {node_count} nodes, {rel_count} relationships")
        
        if node_count == 0 and rel_count == 0:
            print("Database is already empty")
            return
        
        # Confirm before clearing
        response = input("Are you sure you want to clear the database? (y/N): ")
        if response.lower() != 'y':
            print("Operation cancelled")
            return
        
        # Clear the database
        db.clear_database()
        print("✓ Database cleared successfully")
        
        # Verify it's empty
        node_count = db.get_node_count()
        rel_count = db.get_relationship_count()
        print(f"Database after clearing: {node_count} nodes, {rel_count} relationships")
        
        db.disconnect()
        print("✓ Disconnected from Neo4j")
        
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
