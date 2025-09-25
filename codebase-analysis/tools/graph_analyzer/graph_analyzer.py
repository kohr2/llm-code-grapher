#!/usr/bin/env python3
"""
Neo4j Graph Structure Analyzer

Analyzes the hierarchical structure of a Neo4j graph starting from Program nodes.
Supports multiple database connections and provides detailed tree analysis.
"""

import argparse
import sys
from typing import Dict, List, Tuple, Any
from dataclasses import dataclass
from neo4j import GraphDatabase
import json
from collections import defaultdict, Counter


@dataclass
class GraphNode:
    """Represents a node in the graph analysis"""
    level: int
    node_type: str
    relationship_type: str
    count: int
    path: str = ""


class Neo4jGraphAnalyzer:
    """Analyzes Neo4j graph structure and hierarchy"""
    
    def __init__(self, uri: str, username: str, password: str, database: str = "neo4j"):
        """Initialize the analyzer with database connection details"""
        self.uri = uri
        self.username = username
        self.password = password
        self.database = database
        self.driver = None
    
    def connect(self):
        """Establish connection to Neo4j database"""
        try:
            self.driver = GraphDatabase.driver(self.uri, auth=(self.username, self.password))
            print(f"✅ Connected to Neo4j database: {self.database}")
        except Exception as e:
            print(f"❌ Failed to connect to Neo4j: {e}")
            sys.exit(1)
    
    def close(self):
        """Close the database connection"""
        if self.driver:
            self.driver.close()
    
    def execute_query(self, query: str, parameters: Dict = None) -> List[Dict]:
        """Execute a Cypher query and return results"""
        with self.driver.session(database=self.database) as session:
            try:
                result = session.run(query, parameters or {})
                return [record.data() for record in result]
            except Exception as e:
                print(f"❌ Query failed: {e}")
                return []
    
    def analyze_program_structure(self, max_depth: int = 4) -> Dict[str, Any]:
        """Analyze the complete graph structure starting from root nodes"""
        print(f"🔍 Analyzing graph structure (max depth: {max_depth})...")
        
        # Get basic database info
        db_info = self.get_database_info()
        
        # Find the root node type (Program, program, or most connected node)
        root_node_type = self.find_root_node_type()
        print(f"🎯 Using root node type: {root_node_type}")
        
        # Analyze each level starting from root
        level_analysis = {}
        for level in range(max_depth + 1):
            if level == 1:
                # Special logic for level 1: find node type with least direct but most indirect connections
                level_analysis[level] = self.analyze_level_1_optimal(root_node_type)
            else:
                level_analysis[level] = self.analyze_level(level, max_depth, root_node_type)
        
        # Get relationship analysis
        relationship_analysis = self.analyze_relationships(max_depth, root_node_type)
        
        # Get common paths
        common_paths = self.get_common_paths(max_depth, root_node_type)
        
        return {
            "database_info": db_info,
            "root_node_type": root_node_type,
            "level_analysis": level_analysis,
            "relationship_analysis": relationship_analysis,
            "common_paths": common_paths,
            "max_depth": max_depth
        }
    
    def get_database_info(self) -> Dict[str, Any]:
        """Get basic information about the database"""
        queries = {
            "total_nodes": "MATCH (n) RETURN count(n) as count",
            "total_relationships": "MATCH ()-[r]->() RETURN count(r) as count",
            "total_lines": "MATCH (n) WHERE n.line_count IS NOT NULL RETURN sum(n.line_count) as count",
            "node_types": """
                MATCH (n) 
                WHERE n.line_count IS NOT NULL
                RETURN DISTINCT labels(n) as labels, 
                       count(n) as count,
                       min(n.line_count) as min_lines,
                       max(n.line_count) as max_lines,
                       avg(n.line_count) as avg_lines
                ORDER BY count DESC
            """,
            "node_types_no_lines": """
                MATCH (n) 
                WHERE n.line_count IS NULL
                RETURN DISTINCT labels(n) as labels, 
                       count(n) as count,
                       0 as min_lines,
                       0 as max_lines,
                       0.0 as avg_lines
                ORDER BY count DESC
            """,
            "relationship_types": "MATCH ()-[r]->() RETURN DISTINCT type(r) as type, count(r) as count ORDER BY count DESC"
        }
        
        info = {}
        for key, query in queries.items():
            results = self.execute_query(query)
            if results:
                if key in ["total_nodes", "total_relationships", "total_lines"]:
                    info[key] = results[0]
                elif key == "node_types":
                    # Combine node types with and without line counts
                    nodes_with_lines = results
                    nodes_without_lines = self.execute_query(queries["node_types_no_lines"])
                    all_nodes = nodes_with_lines + (nodes_without_lines if nodes_without_lines else [])
                    info[key] = all_nodes
                else:
                    info[key] = results
        
        return info
    
    def find_root_node_type(self) -> str:
        """Find the root node type - prefer Program, then program, then most connected node"""
        # First try Program (uppercase)
        query = "MATCH (p:Program) RETURN count(p) as count"
        result = self.execute_query(query)
        if result and result[0]['count'] > 0:
            return "Program"
        
        # Then try program (lowercase)
        query = "MATCH (p:program) RETURN count(p) as count"
        result = self.execute_query(query)
        if result and result[0]['count'] > 0:
            return "program"
        
        # If neither exists, find the node type with least direct but most indirect connections
        return self.find_optimal_root_node_type()
    
    def find_optimal_root_node_type(self) -> str:
        """Find node type with least direct connections but most indirect connections"""
        # Get all node types and their connection patterns
        query = """
        MATCH (n)
        WITH labels(n)[0] as nodeType, n
        OPTIONAL MATCH (n)-[r1]->(connected)
        OPTIONAL MATCH (n)<-[r2]-(incoming)
        WITH nodeType, 
             count(DISTINCT n) as node_count,
             count(DISTINCT r1) as direct_outgoing,
             count(DISTINCT r2) as direct_incoming,
             count(DISTINCT connected) as direct_connections
        OPTIONAL MATCH (n)-[*2..3]-(indirect)
        WITH nodeType, node_count, direct_connections, count(DISTINCT indirect) as indirect_connections
        WHERE node_count > 0
        RETURN nodeType, node_count, direct_connections, indirect_connections,
               (indirect_connections * 1.0 / direct_connections) as indirect_ratio
        ORDER BY indirect_ratio DESC, indirect_connections DESC
        LIMIT 1
        """
        
        result = self.execute_query(query)
        if result:
            return result[0]['nodeType']
        
        # Fallback: return the most common node type
        query = "MATCH (n) RETURN labels(n)[0] as nodeType, count(n) as count ORDER BY count DESC LIMIT 1"
        result = self.execute_query(query)
        return result[0]['nodeType'] if result else "Unknown"
    
    def analyze_level(self, level: int, max_depth: int, root_node_type: str = "Program") -> List[Dict]:
        """Analyze nodes at a specific level from root nodes"""
        if level == 0:
            # Level 0: Root nodes
            query = f"""
            MATCH (p:{root_node_type})
            RETURN labels(p) as nodeType, count(p) as count, sum(p.line_count) as total_lines
            """
        else:
            # Level 1+: Children at specific depth
            # Build path pattern: (p:RootType)-[r0]->(level1)-[r1]->(level2)...
            path_parts = [f"(p:{root_node_type})"]
            for i in range(level):
                path_parts.append(f"-[r{i}]->(level{i+1})")
            
            path_pattern = "".join(path_parts)
            
            query = f"""
            MATCH {path_pattern}
            RETURN 
                labels(level{level}) as nodeType,
                type(r{level-1}) as relationshipType,
                count(level{level}) as count,
                sum(level{level}.line_count) as total_lines
            ORDER BY count DESC
            """
        
        return self.execute_query(query)
    
    def analyze_level_1_optimal(self, root_node_type: str) -> List[Dict]:
        """Analyze level 1 to find node type with least direct but most indirect connections"""
        query = f"""
        MATCH (p:{root_node_type})-[r]->(connected)
        WITH labels(connected)[0] as nodeType, 
             count(DISTINCT connected) as direct_count,
             count(DISTINCT r) as direct_relationships
        OPTIONAL MATCH (p:{root_node_type})-[*2..3]-(indirect)
        WHERE labels(indirect)[0] = nodeType
        WITH nodeType, direct_count, direct_relationships, count(DISTINCT indirect) as indirect_count
        WHERE direct_count > 0
        WITH nodeType, direct_count, direct_relationships, indirect_count,
             (indirect_count * 1.0 / direct_count) as indirect_ratio
        RETURN nodeType, direct_count, direct_relationships, indirect_count, indirect_ratio
        ORDER BY indirect_ratio DESC, indirect_count DESC
        LIMIT 1
        """
        
        result = self.execute_query(query)
        if not result:
            # Fallback to regular level 1 analysis
            return self.analyze_level(1, 2, root_node_type)
        
        optimal_node_type = result[0]['nodeType']
        print(f"🎯 Level 1 optimal node type: {optimal_node_type} (ratio: {result[0]['indirect_ratio']:.2f})")
        
        # Now get the actual level 1 data for this optimal node type
        query = f"""
        MATCH (p:{root_node_type})-[r]->(connected)
        WHERE labels(connected)[0] = '{optimal_node_type}'
        RETURN 
            labels(connected) as nodeType,
            type(r) as relationshipType,
            count(connected) as count,
            sum(connected.line_count) as total_lines
        ORDER BY count DESC
        """
        
        return self.execute_query(query)
    
    def analyze_relationships(self, max_depth: int, root_node_type: str = "Program") -> Dict[int, List[Dict]]:
        """Analyze relationship types at each level"""
        relationship_analysis = {}
        
        for level in range(1, max_depth + 1):
            # Build path pattern: (p:RootType)-[r0]->(level1)-[r1]->(level2)...
            path_parts = [f"(p:{root_node_type})"]
            for i in range(level):
                path_parts.append(f"-[r{i}]->(level{i+1})")
            
            path_pattern = "".join(path_parts)
            
            query = f"""
            MATCH {path_pattern}
            WITH level{level-1} as parent, level{level} as child, r{level-1} as rel
            RETURN 
                type(rel) as relationshipType,
                labels(parent) as parentType,
                labels(child) as childType,
                count(*) as count
            ORDER BY count DESC
            """
            
            results = self.execute_query(query)
            if results:
                relationship_analysis[level] = results
        
        return relationship_analysis
    
    def get_common_paths(self, max_depth: int, root_node_type: str = "Program", limit: int = 20) -> List[Dict]:
        """Get the most common paths from root nodes"""
        query = f"""
        MATCH path = (p:{root_node_type})-[*1..{max_depth}]-(end)
        WITH path,
             [node in nodes(path) | labels(node)[0]] as nodeTypes,
             [rel in relationships(path) | type(rel)] as relTypes
        WHERE size(nodeTypes) >= 2
        RETURN 
            nodeTypes[0] + " --[" + relTypes[0] + "]--> " + nodeTypes[1] as path,
            count(*) as count
        ORDER BY count DESC
        LIMIT {limit}
        """
        
        return self.execute_query(query)
    
    def print_analysis(self, analysis: Dict[str, Any], output_format: str = "table"):
        """Print the analysis results in the specified format"""
        if output_format == "json":
            print(json.dumps(analysis, indent=2))
            return
        
        print("\n" + "="*80)
        print("📊 NEO4J GRAPH STRUCTURE ANALYSIS")
        print("="*80)
        
        # Database info
        db_info = analysis["database_info"]
        print(f"\n🗄️  Database: {self.database}")
        print(f"📈 Total Nodes: {db_info.get('total_nodes', {}).get('count', 'N/A')}")
        print(f"🔗 Total Relationships: {db_info.get('total_relationships', {}).get('count', 'N/A')}")
        print(f"📝 Total Lines: {db_info.get('total_lines', {}).get('count', 'N/A')}")
        
        # Node types summary
        print(f"\n📋 Node Types Summary:")
        for node_info in db_info.get('node_types', []):
            labels = ', '.join(node_info['labels'])
            count = node_info['count']
            min_lines = node_info.get('min_lines', 0)
            max_lines = node_info.get('max_lines', 0)
            avg_lines = node_info.get('avg_lines', 0)
            
            if min_lines == max_lines == 0:
                print(f"   • {labels}: {count} (no line count data)")
            else:
                print(f"   • {labels}: {count} (lines: {min_lines}-{max_lines}, avg: {avg_lines:.1f})")
        
        # Relationship types summary
        print(f"\n🔗 Relationship Types Summary:")
        for rel_info in db_info.get('relationship_types', []):
            print(f"   • {rel_info['type']}: {rel_info['count']}")
        
        # Level-by-level analysis - Summary only
        print(f"\n🌳 Hierarchical Structure Analysis:")
        print("-" * 60)
        
        for level, level_data in analysis["level_analysis"].items():
            if not level_data:
                continue
                
            # Calculate total count and line count for this level
            total_count = sum(item['count'] for item in level_data)
            total_lines = sum(item.get('total_lines', 0) or 0 for item in level_data)
            node_types = set()
            rel_types = set()
            
            for item in level_data:
                node_type = ', '.join(item['nodeType']) if isinstance(item['nodeType'], list) else item['nodeType']
                rel_type = item.get('relationshipType', 'N/A')
                node_types.add(node_type)
                if rel_type != 'N/A':
                    rel_types.add(rel_type)
            
            # Format line count
            lines_str = f" ({total_lines} lines)" if total_lines > 0 else ""
            
            if level == 0:
                print(f"Level {level}: {total_count} nodes ({', '.join(sorted(node_types))}){lines_str}")
            else:
                rel_str = f" via {', '.join(sorted(rel_types))}" if rel_types else ""
                print(f"Level {level}: {total_count} nodes ({', '.join(sorted(node_types))}){rel_str}{lines_str}")
        
        # Common paths - Top 5 only
        print(f"\n🛤️  Most Common Paths (Top 5):")
        print("-" * 60)
        for path_info in analysis["common_paths"][:5]:
            print(f"   {path_info['path']}: {path_info['count']}")
        
        print("\n" + "="*80)
    
    def export_analysis(self, analysis: Dict[str, Any], filename: str):
        """Export analysis to JSON file"""
        try:
            with open(filename, 'w') as f:
                json.dump(analysis, f, indent=2)
            print(f"✅ Analysis exported to: {filename}")
        except Exception as e:
            print(f"❌ Failed to export analysis: {e}")


def main():
    """Main function with command line interface"""
    parser = argparse.ArgumentParser(
        description="Analyze Neo4j graph structure starting from Program nodes",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python graph_analyzer.py --uri bolt://localhost:7687 --user neo4j --password password
  python graph_analyzer.py --uri bolt://localhost:7687 --user neo4j --password password --database fraud_db
  python graph_analyzer.py --uri bolt://localhost:7687 --user neo4j --password password --max-depth 3 --output json
  python graph_analyzer.py --uri bolt://localhost:7687 --user neo4j --password password --export analysis.json
        """
    )
    
    parser.add_argument('--uri', required=True, help='Neo4j database URI (e.g., bolt://localhost:7687)')
    parser.add_argument('--user', required=True, help='Neo4j username')
    parser.add_argument('--password', required=True, help='Neo4j password')
    parser.add_argument('--database', default='neo4j', help='Database name (default: neo4j)')
    parser.add_argument('--max-depth', type=int, default=4, help='Maximum depth to analyze (default: 4)')
    parser.add_argument('--output', choices=['table', 'json'], default='table', help='Output format (default: table)')
    parser.add_argument('--export', help='Export analysis to JSON file')
    
    args = parser.parse_args()
    
    # Create analyzer
    analyzer = Neo4jGraphAnalyzer(args.uri, args.user, args.password, args.database)
    
    try:
        # Connect to database
        analyzer.connect()
        
        # Perform analysis
        analysis = analyzer.analyze_program_structure(args.max_depth)
        
        # Print results
        analyzer.print_analysis(analysis, args.output)
        
        # Export if requested
        if args.export:
            analyzer.export_analysis(analysis, args.export)
    
    except KeyboardInterrupt:
        print("\n⚠️  Analysis interrupted by user")
    except Exception as e:
        print(f"❌ Analysis failed: {e}")
        sys.exit(1)
    finally:
        analyzer.close()


if __name__ == "__main__":
    main()
