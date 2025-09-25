#!/usr/bin/env python3
"""
Graph Analyzer Launcher

Convenient launcher script for the Neo4j Graph Analyzer.
This script provides easy access to the graph analyzer from the main project directory.
Uses .env configuration with command line argument overrides.
"""

import sys
import os
import argparse
from pathlib import Path
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

# Add the tools directory to the Python path
tools_dir = Path(__file__).parent / "tools"
sys.path.insert(0, str(tools_dir))

# Import the graph analyzer
from graph_analyzer.graph_analyzer import Neo4jGraphAnalyzer


def get_config_from_env_and_args():
    """Get configuration from .env file and command line arguments"""
    parser = argparse.ArgumentParser(
        description="Analyze Neo4j graph structure with .env configuration",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python analyze_graph.py                                    # Use .env defaults
  python analyze_graph.py --database fraud_db               # Override database
  python analyze_graph.py --max-depth 3 --output json       # Override depth and output
  python analyze_graph.py --export analysis.json            # Export results
        """
    )
    
    # Get defaults from environment variables
    default_uri = os.getenv('NEO4J_URI', 'bolt://localhost:7687')
    default_user = os.getenv('NEO4J_USERNAME', 'neo4j')
    default_password = os.getenv('NEO4J_PASSWORD', 'password')
    default_database = os.getenv('NEO4J_DATABASE', 'neo4j')
    
    # Define arguments with .env defaults
    parser.add_argument('--uri', default=default_uri, 
                       help=f'Neo4j database URI (default from .env: {default_uri})')
    parser.add_argument('--user', default=default_user, 
                       help=f'Neo4j username (default from .env: {default_user})')
    parser.add_argument('--password', default=default_password, 
                       help=f'Neo4j password (default from .env: {default_password})')
    parser.add_argument('--database', default=default_database, 
                       help=f'Database name (default from .env: {default_database})')
    parser.add_argument('--max-depth', type=int, default=4, 
                       help='Maximum depth to analyze (default: 4)')
    parser.add_argument('--output', choices=['table', 'json'], default='table', 
                       help='Output format (default: table)')
    parser.add_argument('--export', help='Export analysis to JSON file')
    parser.add_argument('--show-config', action='store_true', 
                       help='Show configuration and exit')
    
    return parser.parse_args()


def show_configuration(config):
    """Display current configuration"""
    print("🔧 Graph Analyzer Configuration:")
    print("=" * 50)
    print(f"URI: {config.uri}")
    print(f"User: {config.user}")
    print(f"Password: {'*' * len(config.password)}")
    print(f"Database: {config.database}")
    print(f"Max Depth: {config.max_depth}")
    print(f"Output Format: {config.output}")
    print(f"Export File: {config.export or 'None'}")
    print("=" * 50)


def main():
    """Main function with .env configuration support"""
    try:
        # Parse configuration
        config = get_config_from_env_and_args()
        
        # Show configuration if requested
        if config.show_config:
            show_configuration(config)
            return
        
        # Create analyzer
        analyzer = Neo4jGraphAnalyzer(
            uri=config.uri,
            username=config.user,
            password=config.password,
            database=config.database
        )
        
        try:
            # Connect to database
            analyzer.connect()
            
            # Perform analysis
            analysis = analyzer.analyze_program_structure(config.max_depth)
            
            # Print results
            analyzer.print_analysis(analysis, config.output)
            
            # Export if requested
            if config.export:
                analyzer.export_analysis(analysis, config.export)
        
        except KeyboardInterrupt:
            print("\n⚠️  Analysis interrupted by user")
        except Exception as e:
            print(f"❌ Analysis failed: {e}")
            sys.exit(1)
        finally:
            analyzer.close()
    
    except Exception as e:
        print(f"❌ Configuration error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
