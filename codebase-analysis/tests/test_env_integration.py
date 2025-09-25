"""
Test .env file integration for Neo4j configuration

This test demonstrates how to use .env files for Neo4j credentials.
"""

import os
import tempfile
import pytest
from unittest.mock import patch

from src.neo4j_database import Neo4jConfig, Neo4jDatabase


class TestEnvIntegration:
    """Test .env file integration"""
    
    def test_neo4j_config_from_env_file(self):
        """Test creating Neo4j config from .env file"""
        # Create a temporary .env file
        env_content = """
# Neo4j Configuration
NEO4J_URI=bolt://test-server:7687
NEO4J_USERNAME=testuser
NEO4J_PASSWORD=testpass
NEO4J_DATABASE=testdb
"""
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.env', delete=False) as f:
            f.write(env_content)
            env_file = f.name
        
        try:
            # Test loading from .env file
            config = Neo4jConfig.from_environment(env_file)
            
            assert config.uri == "bolt://test-server:7687"
            assert config.username == "testuser"
            assert config.password == "testpass"
            assert config.database == "testdb"
            
        finally:
            # Clean up
            os.unlink(env_file)
    
    def test_neo4j_config_from_env_file_missing(self):
        """Test creating Neo4j config when .env file doesn't exist"""
        # Clear any existing environment variables
        with patch.dict(os.environ, {}, clear=True):
            # Should fall back to defaults
            config = Neo4jConfig.from_environment("nonexistent.env")
            
            # Should use defaults
            assert config.uri == "bolt://localhost:7687"
            assert config.username == "neo4j"
            assert config.password == "password"
            assert config.database == "neo4j"
    
    def test_neo4j_config_environment_override(self):
        """Test that environment variables override .env file"""
        env_content = """
NEO4J_URI=bolt://env-file:7687
NEO4J_USERNAME=envuser
NEO4J_PASSWORD=envpass
NEO4J_DATABASE=envdb
"""
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.env', delete=False) as f:
            f.write(env_content)
            env_file = f.name
        
        try:
            # Set environment variables that should override .env file
            with patch.dict(os.environ, {
                'NEO4J_URI': 'bolt://env-var:7687',
                'NEO4J_USERNAME': 'envvaruser',
                'NEO4J_PASSWORD': 'envvarpass',
                'NEO4J_DATABASE': 'envvardb'
            }):
                config = Neo4jConfig.from_environment(env_file)
                
                # Environment variables should take precedence
                assert config.uri == "bolt://env-var:7687"
                assert config.username == "envvaruser"
                assert config.password == "envvarpass"
                assert config.database == "envvardb"
                
        finally:
            os.unlink(env_file)
    
    def test_neo4j_database_with_env_file(self):
        """Test creating Neo4j database with .env file"""
        env_content = """
NEO4J_URI=bolt://test-server:7687
NEO4J_USERNAME=testuser
NEO4J_PASSWORD=testpass
NEO4J_DATABASE=testdb
"""
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.env', delete=False) as f:
            f.write(env_content)
            env_file = f.name
        
        try:
            # Test creating database with .env file
            from src.neo4j_database import create_neo4j_database
            
            with patch('neo4j.GraphDatabase.driver') as mock_driver:
                db = create_neo4j_database(env_file=env_file)
                
                assert db.config.uri == "bolt://test-server:7687"
                assert db.config.username == "testuser"
                assert db.config.password == "testpass"
                assert db.config.database == "testdb"
                
        finally:
            os.unlink(env_file)
    
    def test_neo4j_config_validation_with_env(self):
        """Test Neo4j config validation with .env values"""
        env_content = """
NEO4J_URI=bolt://localhost:7687
NEO4J_USERNAME=neo4j
NEO4J_PASSWORD=secure-password
NEO4J_DATABASE=codegrapher
"""
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.env', delete=False) as f:
            f.write(env_content)
            env_file = f.name
        
        try:
            # Clear environment variables to ensure .env file is used
            with patch.dict(os.environ, {}, clear=True):
                config = Neo4jConfig.from_environment(env_file)
                config.validate()  # Should not raise
                
                assert config.uri == "bolt://localhost:7687"
                assert config.username == "neo4j"
                assert config.password == "secure-password"
                assert config.database == "codegrapher"
            
        finally:
            os.unlink(env_file)
    
    def test_neo4j_config_invalid_uri_from_env(self):
        """Test Neo4j config validation with invalid URI from .env"""
        env_content = """
NEO4J_URI=invalid-uri
NEO4J_USERNAME=neo4j
NEO4J_PASSWORD=password
NEO4J_DATABASE=neo4j
"""
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.env', delete=False) as f:
            f.write(env_content)
            env_file = f.name
        
        try:
            # Clear environment variables to ensure .env file is used
            with patch.dict(os.environ, {}, clear=True):
                config = Neo4jConfig.from_environment(env_file)
                
                with pytest.raises(ValueError, match="Invalid Neo4j URI format"):
                    config.validate()
                    
        finally:
            os.unlink(env_file)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
