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


class LanguageConfig(BaseModel):
    """Language-specific configuration settings"""
    # This will be populated dynamically based on the language being analyzed
    section_patterns: list[str] = Field(default_factory=list)
    subsection_patterns: list[str] = Field(default_factory=list)
    data_patterns: list[str] = Field(default_factory=list)


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
    file: str = "./logs/llm_code_grapher.log"
    format: str = "%(asctime)s - %(name)s - %(levelname)s - %(message)s"


class Config(BaseModel):
    """Main configuration class"""
    llm: LLMConfig = Field(default_factory=LLMConfig)
    processing: ProcessingConfig = Field(default_factory=ProcessingConfig)
    language: LanguageConfig = Field(default_factory=LanguageConfig)
    output: OutputConfig = Field(default_factory=OutputConfig)
    logging: LoggingConfig = Field(default_factory=LoggingConfig)


class ConfigManager:
    """Manages configuration loading and validation"""
    
    def __init__(self, config_path: Optional[str] = None):
        self.config_path = config_path or "config.yaml"
        self._config: Optional[Config] = None
        self.config: Optional[Dict[str, Any]] = None
    
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
            raise FileNotFoundError(f"Config file not found: {self.config_path}")
        
        try:
            with open(config_path, 'r', encoding='utf-8') as f:
                return yaml.safe_load(f) or {}
        except yaml.YAMLError as e:
            raise yaml.YAMLError(f"Invalid YAML in config file: {e}")
        except Exception as e:
            raise Exception(f"Error loading config file: {e}")
    
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
    
    def validate_config(self, config: Dict[str, Any]) -> None:
        """Validate configuration and raise ValueError for invalid values"""
        # Check required sections
        required_sections = ['llm', 'processing', 'output']
        missing_sections = [section for section in required_sections if section not in config]
        if missing_sections:
            raise ValueError(f"Missing required configuration sections: {', '.join(missing_sections)}")
        
        # Validate LLM configuration
        if 'llm' in config:
            self._validate_llm_config(config['llm'])
        
        # Validate processing configuration
        if 'processing' in config:
            self._validate_processing_config(config['processing'])
        
        # Validate output configuration
        if 'output' in config:
            self._validate_output_config(config['output'])
    
    def _validate_llm_config(self, llm_config: Dict[str, Any]) -> None:
        """Validate LLM configuration"""
        valid_providers = ['openai', 'anthropic', 'ollama']
        if 'provider' in llm_config:
            if llm_config['provider'] not in valid_providers:
                raise ValueError(f"Invalid LLM provider: {llm_config['provider']}")
        
        # Validate required fields
        required_fields = ['provider', 'model', 'max_tokens', 'temperature']
        for field in required_fields:
            if field not in llm_config:
                raise ValueError(f"Missing required field in LLM config: {field}")
        
        # Validate types
        if 'max_tokens' in llm_config:
            if not isinstance(llm_config['max_tokens'], int):
                raise ValueError("max_tokens must be an integer")
            if llm_config['max_tokens'] <= 0:
                raise ValueError("max_tokens must be positive")
        
        if 'temperature' in llm_config:
            if not isinstance(llm_config['temperature'], (int, float)):
                raise ValueError("temperature must be a number")
            if not 0 <= llm_config['temperature'] <= 2:
                raise ValueError("temperature must be between 0 and 2")
    
    def _validate_processing_config(self, processing_config: Dict[str, Any]) -> None:
        """Validate processing configuration"""
        # Validate required fields
        required_fields = ['chunk_size', 'overlap', 'max_retries', 'confidence_threshold']
        for field in required_fields:
            if field not in processing_config:
                raise ValueError(f"Missing required field in processing config: {field}")
        
        # Validate chunk_size
        if 'chunk_size' in processing_config:
            if not isinstance(processing_config['chunk_size'], int):
                raise ValueError("chunk_size must be an integer")
            if processing_config['chunk_size'] <= 0:
                raise ValueError("Chunk size must be positive")
        
        # Validate overlap
        if 'overlap' in processing_config:
            if not isinstance(processing_config['overlap'], int):
                raise ValueError("overlap must be an integer")
            if processing_config['overlap'] < 0:
                raise ValueError("Overlap must be non-negative")
            if 'chunk_size' in processing_config and processing_config['overlap'] >= processing_config['chunk_size']:
                raise ValueError("Overlap must be less than chunk size")
        
        # Validate confidence_threshold
        if 'confidence_threshold' in processing_config:
            if not isinstance(processing_config['confidence_threshold'], (int, float)):
                raise ValueError("confidence_threshold must be a number")
            if not 0 <= processing_config['confidence_threshold'] <= 1:
                raise ValueError("Confidence threshold must be between 0 and 1")
    
    def _validate_output_config(self, output_config: Dict[str, Any]) -> None:
        """Validate output configuration"""
        # Validate required fields
        required_fields = ['format', 'include_confidence', 'generate_visualization']
        for field in required_fields:
            if field not in output_config:
                raise ValueError(f"Missing required field in output config: {field}")
        
        # Validate output format
        if 'format' in output_config:
            valid_formats = ['json', 'text', 'yaml', 'all']
            if output_config['format'] not in valid_formats:
                raise ValueError(f"Invalid output format: {output_config['format']}")
    
    def validate(self) -> None:
        """Validate the current configuration"""
        if self.config is not None:
            # Use the manually set config
            self.validate_config(self.config)
        else:
            # Use the loaded config
            if self._config is None:
                self.load_config()
            config_dict = self._config.model_dump()
            self.validate_config(config_dict)


# Global config manager instance
config_manager = ConfigManager()


def get_config() -> Config:
    """Get the global configuration instance"""
    return config_manager.get_config()


def reload_config() -> Config:
    """Reload the global configuration"""
    return config_manager.reload_config()


def validate_config(config: Dict[str, Any]) -> None:
    """Validate a configuration dictionary"""
    config_manager.validate_config(config)


def load_config(config_path: Optional[str] = None) -> Config:
    """Load configuration from file"""
    if config_path:
        manager = ConfigManager(config_path)
        return manager.load_config()
    return config_manager.load_config()


def get_default_config() -> Dict[str, Any]:
    """Get default configuration as dictionary"""
    return Config().model_dump()
