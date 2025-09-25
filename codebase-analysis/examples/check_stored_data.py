#!/usr/bin/env python3
"""
Check Stored Data in Neo4j
Shows exactly where the fraud management ontology data is stored
"""

import sys
from pathlib import Path

# Add the project root to the path
sys.path.append(str(Path(__file__).parent.parent))

from src.neo4j_database import Neo4jDatabase, Neo4jConfig


def main():
    """Check what data is stored in Neo4j"""
    print("=== Checking Stored Data in Neo4j ===\n")
    
    try:
        # Connect to Neo4j
        config = Neo4jConfig.from_environment()
        db = Neo4jDatabase(config)
        db.connect()
        
        print("✓ Connected to Neo4j database")
        
        # Get database statistics
        node_count = db.get_node_count()
        rel_count = db.get_relationship_count()
        print(f"Database Status: {node_count} nodes, {rel_count} relationships")
        
        if node_count == 0:
            print("No data found in database")
            return
        
        # Get all node types
        print("\n=== Node Types in Database ===")
        node_types = db.query("""
            MATCH (n)
            RETURN labels(n)[0] as node_type, count(n) as count
            ORDER BY count DESC
        """)
        
        for node_type in node_types:
            print(f"  {node_type['node_type']}: {node_type['count']} nodes")
        
        # Get all relationship types
        print("\n=== Relationship Types in Database ===")
        rel_types = db.query("""
            MATCH ()-[r]->()
            RETURN type(r) as relationship_type, count(r) as count
            ORDER BY count DESC
        """)
        
        for rel_type in rel_types:
            print(f"  {rel_type['relationship_type']}: {rel_type['count']} relationships")
        
        # Get program information
        print("\n=== Program Information ===")
        programs = db.query("MATCH (p:Program) RETURN p")
        for program in programs:
            p = program['p']
            print(f"Program: {p['name']}")
            print(f"  Language: {p['language']}")
            print(f"  Purpose: {p['purpose']}")
            print(f"  Business Domain: {p['business_domain']}")
            print(f"  Cyclomatic Complexity: {p['cyclomatic_complexity']}")
            print(f"  Technical Debt: {p['technical_debt']}")
        
        # Get business rules
        print("\n=== Business Rules ===")
        rules = db.query("""
            MATCH (r:BusinessRule)
            RETURN r.name as rule_id, r.description, r.priority, r.risk_level
            ORDER BY r.name
        """)
        
        for rule in rules:
            print(f"  {rule['rule_id']}: {rule['description']}")
            print(f"    Priority: {rule['priority']}, Risk: {rule['risk_level']}")
        
        # Get sections
        print("\n=== Sections ===")
        sections = db.query("""
            MATCH (s:Section)
            RETURN s.name, s.risk_level, s.complexity_score
            ORDER BY s.complexity_score DESC
        """)
        
        for section in sections:
            print(f"  {section['s.name']}: Risk={section['s.risk_level']}, Complexity={section['s.complexity_score']}")
        
        # Get data items
        print("\n=== Data Items ===")
        data_items = db.query("""
            MATCH (d:DataItem)
            RETURN d.name, d.data_type, d.transaction_volume
            ORDER BY d.name
        """)
        
        for item in data_items:
            print(f"  {item['d.name']}: {item['d.data_type']} (Volume: {item['d.transaction_volume']})")
        
        # Show some relationships
        print("\n=== Sample Relationships ===")
        relationships = db.query("""
            MATCH (a)-[r]->(b)
            RETURN labels(a)[0] as source_type, a.name as source_name, 
                   type(r) as relationship_type, 
                   labels(b)[0] as target_type, b.name as target_name
            LIMIT 10
        """)
        
        for rel in relationships:
            print(f"  {rel['source_type']}({rel['source_name']}) --[{rel['relationship_type']}]--> {rel['target_type']}({rel['target_name']})")
        
        # Show where data is physically stored
        print("\n=== Physical Storage Information ===")
        print("Data is stored in Neo4j graph database with the following structure:")
        print("  - Database URI: " + config.uri)
        print("  - Database Name: " + config.database)
        print("  - Username: " + config.username)
        print("  - All data is stored as nodes and relationships in the graph")
        print("  - Each node has properties containing the actual data")
        print("  - Relationships connect nodes and define their interactions")
        
        db.disconnect()
        print("\n✓ Disconnected from Neo4j")
        
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
