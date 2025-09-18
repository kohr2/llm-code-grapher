"""
Configuration Manager for COBOL Code Grapher

Handles loading and validation of configuration from YAML files and environment variables.
"""

import os
import yaml
from pathlib import Path
from typing import Dict, Any, Optional
from pydantic import BaseModel, Field


class LLMConfig(BaseModel):
    """LLM configuration settings"""
    provider: str = "openai"
    model: str = "gpt-4"
    max_tokens: int = 4000
    temperature: float = 0.1
    api_key: Optional[str] = None
    base_url: Optional[str] = None  # For Ollama and other local providers


class ProcessingConfig(BaseModel):
    """Processing configuration settings"""
    chunk_size: int = 2000
    overlap: int = 200
    max_retries: int = 3
    parallel_sections: bool = True
    confidence_threshold: float = 0.7


class COBOLConfig(BaseModel):
    """COBOL-specific configuration settings"""
    section_patterns: list[str] = Field(default_factory=lambda: [
        r"^\s*[A-Z0-9-]+\s+SECTION\s*\.",
        r"^\s*[A-Z0-9-]+\s+PARAGRAPH\s*\."
    ])
    subsection_patterns: list[str] = Field(default_factory=lambda: [
        r"^\s*[A-Z0-9-]+\s*\."
    ])
    data_patterns: list[str] = Field(default_factory=lambda: [
        r"^\s*[0-9]+\s+[A-Z0-9-]+\s+.*PIC\s+",
        r"^\s*[0-9]+\s+[A-Z0-9-]+\s+.*REDEFINES\s+"
    ])


class OutputConfig(BaseModel):
    """Output configuration settings"""
    format: str = "json"
    include_confidence: bool = True
    generate_visualization: bool = True
    output_dir: str = "./data/output"
    filename_template: str = "{input_name}_analysis_{timestamp}"


class LoggingConfig(BaseModel):
    """Logging configuration settings"""
    level: str = "INFO"
    file: str = "./logs/cobol_grapher.log"
    format: str = "%(asctime)s - %(name)s - %(levelname)s - %(message)s"


class Config(BaseModel):
    """Main configuration class"""
    llm: LLMConfig = Field(default_factory=LLMConfig)
    processing: ProcessingConfig = Field(default_factory=ProcessingConfig)
    cobol: COBOLConfig = Field(default_factory=COBOLConfig)
    output: OutputConfig = Field(default_factory=OutputConfig)
    logging: LoggingConfig = Field(default_factory=LoggingConfig)


class ConfigManager:
    """Manages configuration loading and validation"""
    
    def __init__(self, config_path: Optional[str] = None):
        self.config_path = config_path or "config.yaml"
        self._config: Optional[Config] = None
    
    def load_config(self) -> Config:
        """Load configuration from file and environment variables"""
        if self._config is not None:
            return self._config
        
        # Load from YAML file
        config_data = self._load_yaml_config()
        
        # Override with environment variables
        config_data = self._apply_env_overrides(config_data)
        
        # Create and validate config
        self._config = Config(**config_data)
        return self._config
    
    def _load_yaml_config(self) -> Dict[str, Any]:
        """Load configuration from YAML file"""
        config_path = Path(self.config_path)
        
        if not config_path.exists():
            print(f"Warning: Config file {self.config_path} not found, using defaults")
            return {}
        
        try:
            with open(config_path, 'r', encoding='utf-8') as f:
                return yaml.safe_load(f) or {}
        except Exception as e:
            print(f"Error loading config file: {e}")
            return {}
    
    def _apply_env_overrides(self, config_data: Dict[str, Any]) -> Dict[str, Any]:
        """Apply environment variable overrides to config"""
        # LLM configuration
        if "llm" not in config_data:
            config_data["llm"] = {}
            
        # API keys
        if "OPENAI_API_KEY" in os.environ:
            config_data["llm"]["api_key"] = os.environ["OPENAI_API_KEY"]
        
        # Provider-specific settings
        if "LLM_PROVIDER" in os.environ:
            config_data["llm"]["provider"] = os.environ["LLM_PROVIDER"]
        
        if "LLM_MODEL" in os.environ:
            config_data["llm"]["model"] = os.environ["LLM_MODEL"]
            
        if "LLM_BASE_URL" in os.environ:
            config_data["llm"]["base_url"] = os.environ["LLM_BASE_URL"]
        
        # Neo4j configuration
        neo4j_config = {}
        if "NEO4J_URI" in os.environ:
            neo4j_config["uri"] = os.environ["NEO4J_URI"]
        if "NEO4J_USERNAME" in os.environ:
            neo4j_config["username"] = os.environ["NEO4J_USERNAME"]
        if "NEO4J_PASSWORD" in os.environ:
            neo4j_config["password"] = os.environ["NEO4J_PASSWORD"]
        if "NEO4J_DATABASE" in os.environ:
            neo4j_config["database"] = os.environ["NEO4J_DATABASE"]
        
        if neo4j_config:
            config_data["neo4j"] = neo4j_config
        
        return config_data
    
    def get_config(self) -> Config:
        """Get the current configuration"""
        if self._config is None:
            return self.load_config()
        return self._config
    
    def reload_config(self) -> Config:
        """Reload configuration from file"""
        self._config = None
        return self.load_config()


# Global config manager instance
config_manager = ConfigManager()


def get_config() -> Config:
    """Get the global configuration instance"""
    return config_manager.get_config()


def reload_config() -> Config:
    """Reload the global configuration"""
    return config_manager.reload_config()
