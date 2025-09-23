#!/usr/bin/env python3
"""
Neo4j .env File Example

This example shows how to use .env files for Neo4j credentials
instead of setting environment variables manually.
"""

import os
import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from neo4j_database import Neo4jConfig, Neo4jDatabase, create_neo4j_database


def example_with_env_file():
    """Example using .env file for Neo4j credentials"""
    
    print("🔧 Neo4j .env File Example")
    print("=" * 40)
    
    # Method 1: Using Neo4jConfig directly with .env file
    print("\n1️⃣ Using Neo4jConfig with .env file:")
    
    # Create a sample .env file (in real usage, you'd have this file)
    env_content = """
# Neo4j Configuration
NEO4J_URI=bolt://localhost:7687
NEO4J_USERNAME=neo4j
NEO4J_PASSWORD=dashboard-killer
NEO4J_DATABASE=codegrapher
"""
    
    # Write .env file for this example
    env_file = ".env.example"
    with open(env_file, 'w') as f:
        f.write(env_content)
    
    try:
        # Load config from .env file
        config = Neo4jConfig.from_environment(env_file)
        print(f"✅ Loaded config from {env_file}:")
        print(f"   - URI: {config.uri}")
        print(f"   - Username: {config.username}")
        print(f"   - Database: {config.database}")
        
        # Validate config
        config.validate()
        print("✅ Config validation passed")
        
    except Exception as e:
        print(f"❌ Error: {e}")
    finally:
        # Clean up example file
        if os.path.exists(env_file):
            os.unlink(env_file)
    
    # Method 2: Using create_neo4j_database with .env file
    print("\n2️⃣ Using create_neo4j_database with .env file:")
    
    # Create another .env file for this example
    env_content2 = """
NEO4J_URI=bolt://test-server:7687
NEO4J_USERNAME=testuser
NEO4J_PASSWORD=testpass
NEO4J_DATABASE=testdb
"""
    
    env_file2 = ".env.test"
    with open(env_file2, 'w') as f:
        f.write(env_content2)
    
    try:
        # Create database instance with .env file
        db = create_neo4j_database(env_file=env_file2)
        print(f"✅ Created database instance from {env_file2}:")
        print(f"   - URI: {db.config.uri}")
        print(f"   - Username: {db.config.username}")
        print(f"   - Database: {db.config.database}")
        
    except Exception as e:
        print(f"❌ Error: {e}")
    finally:
        # Clean up example file
        if os.path.exists(env_file2):
            os.unlink(env_file2)
    
    # Method 3: Environment variable override
    print("\n3️⃣ Environment variable override:")
    
    # Set environment variables
    os.environ['NEO4J_URI'] = 'bolt://env-override:7687'
    os.environ['NEO4J_USERNAME'] = 'envuser'
    os.environ['NEO4J_PASSWORD'] = 'envpass'
    os.environ['NEO4J_DATABASE'] = 'envdb'
    
    try:
        # Load config (should use environment variables)
        config = Neo4jConfig.from_environment()
        print("✅ Loaded config from environment variables:")
        print(f"   - URI: {config.uri}")
        print(f"   - Username: {config.username}")
        print(f"   - Database: {config.database}")
        
    except Exception as e:
        print(f"❌ Error: {e}")
    finally:
        # Clean up environment variables
        for key in ['NEO4J_URI', 'NEO4J_USERNAME', 'NEO4J_PASSWORD', 'NEO4J_DATABASE']:
            if key in os.environ:
                del os.environ[key]
    
    print("\n📝 How to use .env files in your project:")
    print("1. Create a .env file in your project root:")
    print("   NEO4J_URI=bolt://localhost:7687")
    print("   NEO4J_USERNAME=neo4j")
    print("   NEO4J_PASSWORD=your-password")
    print("   NEO4J_DATABASE=your-database")
    print()
    print("2. Use in your code:")
    print("   from src.neo4j_database import create_neo4j_database")
    print("   db = create_neo4j_database()  # Automatically loads .env")
    print()
    print("3. Or specify a custom .env file:")
    print("   db = create_neo4j_database(env_file='custom.env')")
    print()
    print("🎉 .env file integration is ready!")


if __name__ == "__main__":
    example_with_env_file()
