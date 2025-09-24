"""
Fraud Management Ontology Neo4j Demo
Demonstrates how to store and query the fraud management ontology in Neo4j
"""

import sys
from pathlib import Path
from typing import Dict, Any, List, Optional

# Add the project root to the path
sys.path.append(str(Path(__file__).parent.parent))

from src.neo4j_database import Neo4jDatabase, Neo4jConfig
from examples.fraud_management_ontology_example import FraudManagementOntology
from examples.fraud_ontology_neo4j_converter import convert_fraud_ontology_to_neo4j


class FraudOntologyNeo4jDemo:
    """Demo class for fraud management ontology in Neo4j"""
    
    def __init__(self, neo4j_config: Optional[Neo4jConfig] = None):
        """Initialize the demo"""
        self.neo4j_config = neo4j_config or Neo4jConfig.from_environment()
        self.db = None
        self.fraud_ontology = None
        self.graph_data = None
    
    def setup(self):
        """Setup the demo environment"""
        print("Setting up Fraud Management Ontology Neo4j Demo...")
        
        # Create fraud management ontology
        self.fraud_ontology = FraudManagementOntology()
        print("✓ Created fraud management ontology")
        
        # Convert to Neo4j format
        self.graph_data = convert_fraud_ontology_to_neo4j(self.fraud_ontology)
        print(f"✓ Converted to Neo4j format: {self.graph_data.node_count} nodes, {self.graph_data.relationship_count} relationships")
        
        # Connect to Neo4j
        self.db = Neo4jDatabase(self.neo4j_config)
        try:
            self.db.connect()
            print("✓ Connected to Neo4j database")
        except Exception as e:
            print(f"✗ Failed to connect to Neo4j: {e}")
            print("  Make sure Neo4j is running and credentials are correct")
            return False
        
        return True
    
    def store_ontology(self):
        """Store the fraud management ontology in Neo4j"""
        if not self.db or not self.db.is_connected():
            print("✗ Not connected to Neo4j database")
            return False
        
        try:
            # Clear existing data
            self.db.clear_database()
            print("✓ Cleared existing data")
            
            # Insert the fraud management ontology
            self.db.insert_graph_data(self.graph_data)
            print("✓ Stored fraud management ontology in Neo4j")
            
            return True
        except Exception as e:
            print(f"✗ Failed to store ontology: {e}")
            return False
    
    def query_business_rules(self):
        """Query business rules from the graph"""
        print("\n=== Business Rules Query ===")
        
        # Get all business rules
        rules = self.db.query("""
            MATCH (r:BusinessRule)
            RETURN r.rule_id as rule_id, r.description as description, 
                   r.priority as priority, r.risk_level as risk_level,
                   r.business_impact as business_impact
            ORDER BY r.priority DESC, r.risk_level DESC
        """)
        
        print(f"Found {len(rules)} business rules:")
        for rule in rules:
            print(f"  {rule['rule_id']}: {rule['description']}")
            print(f"    Priority: {rule['priority']}, Risk: {rule['risk_level']}, Impact: {rule['business_impact']}")
    
    def query_high_risk_components(self):
        """Query high-risk components from the graph"""
        print("\n=== High-Risk Components Query ===")
        
        # Get high-risk sections
        high_risk_sections = self.db.query("""
            MATCH (s:Section)
            WHERE s.risk_level = 'HIGH'
            RETURN s.name as name, s.business_logic as business_logic,
                   s.complexity_score as complexity_score
        """)
        
        print(f"Found {len(high_risk_sections)} high-risk sections:")
        for section in high_risk_sections:
            print(f"  {section['name']}: {section['business_logic']}")
            print(f"    Complexity: {section['complexity_score']}")
        
        # Get high-risk operations
        high_risk_operations = self.db.query("""
            MATCH (o:Operation)
            WHERE o.risk_level = 'HIGH'
            RETURN o.name as name, o.operation_type as operation_type,
                   o.business_logic as business_logic
        """)
        
        print(f"\nFound {len(high_risk_operations)} high-risk operations:")
        for operation in high_risk_operations:
            print(f"  {operation['name']} ({operation['operation_type']}): {operation['business_logic']}")
    
    def query_data_flow(self):
        """Query data flow relationships"""
        print("\n=== Data Flow Analysis ===")
        
        # Get data flow relationships
        data_flows = self.db.query("""
            MATCH (o:Operation)-[r:USES]->(d:DataItem)
            RETURN o.name as operation, d.name as data_item, 
                   r.description as description
            ORDER BY o.name
        """)
        
        print(f"Found {len(data_flows)} data flow relationships:")
        for flow in data_flows:
            print(f"  {flow['operation']} uses {flow['data_item']}")
            print(f"    Description: {flow['description']}")
    
    def query_rule_implementation(self):
        """Query business rule implementation"""
        print("\n=== Business Rule Implementation ===")
        
        # Get rule implementation relationships
        implementations = self.db.query("""
            MATCH (r:BusinessRule)-[rel:IMPLEMENTS]->(s:Subsection)
            RETURN r.rule_id as rule_id, s.name as subsection,
                   rel.description as description
            ORDER BY r.rule_id
        """)
        
        print(f"Found {len(implementations)} rule implementations:")
        for impl in implementations:
            print(f"  {impl['rule_id']} implemented in {impl['subsection']}")
            print(f"    Description: {impl['description']}")
    
    def query_risk_assessment(self):
        """Query risk assessment relationships"""
        print("\n=== Risk Assessment Analysis ===")
        
        # Get risk-related relationships
        risk_relationships = self.db.query("""
            MATCH (s:Section)-[r:RELATED_TO]->(br:BusinessRule)
            WHERE s.risk_level = 'HIGH' AND br.risk_level = 'HIGH'
            RETURN s.name as section, br.rule_id as rule_id,
                   r.description as description
            ORDER BY s.name
        """)
        
        print(f"Found {len(risk_relationships)} high-risk relationships:")
        for rel in risk_relationships:
            print(f"  {rel['section']} related to {rel['rule_id']}")
            print(f"    Description: {rel['description']}")
    
    def query_complexity_analysis(self):
        """Query complexity analysis"""
        print("\n=== Complexity Analysis ===")
        
        # Get complexity metrics
        complexity = self.db.query("""
            MATCH (p:Program)
            RETURN p.cyclomatic_complexity as cyclomatic_complexity,
                   p.maintainability_index as maintainability_index,
                   p.technical_debt as technical_debt,
                   p.business_rule_coverage as business_rule_coverage
        """)
        
        if complexity:
            metrics = complexity[0]
            print("Program Complexity Metrics:")
            print(f"  Cyclomatic Complexity: {metrics['cyclomatic_complexity']}")
            print(f"  Maintainability Index: {metrics['maintainability_index']}")
            print(f"  Technical Debt: {metrics['technical_debt']}")
            print(f"  Business Rule Coverage: {metrics['business_rule_coverage']}")
        
        # Get section complexity distribution
        section_complexity = self.db.query("""
            MATCH (s:Section)
            RETURN s.name as name, s.complexity_score as complexity_score,
                   s.risk_level as risk_level
            ORDER BY s.complexity_score DESC
        """)
        
        print(f"\nSection Complexity Distribution:")
        for section in section_complexity:
            print(f"  {section['name']}: {section['complexity_score']} (Risk: {section['risk_level']})")
    
    def query_business_impact(self):
        """Query business impact analysis"""
        print("\n=== Business Impact Analysis ===")
        
        # Get business rules by impact
        business_impact = self.db.query("""
            MATCH (r:BusinessRule)
            RETURN r.rule_id as rule_id, r.business_impact as business_impact,
                   r.priority as priority, r.detection_accuracy as detection_accuracy
            ORDER BY r.business_impact DESC, r.priority DESC
        """)
        
        print("Business Rules by Impact:")
        for rule in business_impact:
            print(f"  {rule['rule_id']}: {rule['business_impact']} impact")
            print(f"    Priority: {rule['priority']}, Accuracy: {rule['detection_accuracy']}")
    
    def query_system_architecture(self):
        """Query system architecture overview"""
        print("\n=== System Architecture Overview ===")
        
        # Get program structure
        structure = self.db.get_code_structure()
        print(f"Total Nodes: {structure['total_nodes']}")
        print(f"Total Relationships: {structure['total_relationships']}")
        
        # Get node type distribution
        node_distribution = self.db.query("""
            MATCH (n)
            RETURN labels(n)[0] as node_type, count(n) as count
            ORDER BY count DESC
        """)
        
        print("\nNode Type Distribution:")
        for node_type in node_distribution:
            print(f"  {node_type['node_type']}: {node_type['count']}")
        
        # Get relationship type distribution
        rel_distribution = self.db.query("""
            MATCH ()-[r]->()
            RETURN type(r) as relationship_type, count(r) as count
            ORDER BY count DESC
        """)
        
        print("\nRelationship Type Distribution:")
        for rel_type in rel_distribution:
            print(f"  {rel_type['relationship_type']}: {rel_type['count']}")
    
    def run_demo(self):
        """Run the complete demo"""
        print("=== Fraud Management Ontology Neo4j Demo ===\n")
        
        # Setup
        if not self.setup():
            return
        
        # Store ontology
        if not self.store_ontology():
            return
        
        # Run queries
        self.query_business_rules()
        self.query_high_risk_components()
        self.query_data_flow()
        self.query_rule_implementation()
        self.query_risk_assessment()
        self.query_complexity_analysis()
        self.query_business_impact()
        self.query_system_architecture()
        
        print("\n✓ Demo completed successfully!")
        print("  All fraud management ontology data is now stored in Neo4j")
        print("  You can now run complex graph queries to analyze the system")
    
    def cleanup(self):
        """Cleanup resources"""
        if self.db:
            self.db.disconnect()
            print("✓ Disconnected from Neo4j")


def main():
    """Main demo function"""
    demo = FraudOntologyNeo4jDemo()
    
    try:
        demo.run_demo()
    except KeyboardInterrupt:
        print("\n\nDemo interrupted by user")
    except Exception as e:
        print(f"\nDemo failed: {e}")
        import traceback
        traceback.print_exc()
    finally:
        demo.cleanup()


if __name__ == "__main__":
    main()
