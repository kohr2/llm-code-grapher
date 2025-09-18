"""
LLM Provider Factory
Handles initialization and management of different LLM providers (OpenAI, Ollama, etc.)
"""

from abc import ABC, abstractmethod
from typing import Dict, Any, Optional, Union
from dataclasses import dataclass


@dataclass
class LLMProviderConfig:
    """Configuration for LLM providers"""
    provider: str
    model: str
    api_key: Optional[str] = None
    base_url: Optional[str] = None
    max_tokens: int = 4000
    temperature: float = 0.1


class BaseLLMProvider(ABC):
    """Abstract base class for LLM providers"""
    
    def __init__(self, config: LLMProviderConfig):
        self.config = config
        self.client = self._initialize_client()
    
    @abstractmethod
    def _initialize_client(self):
        """Initialize the LLM client"""
        pass
    
    @abstractmethod
    def generate_response(self, messages: list, **kwargs) -> str:
        """Generate a response from the LLM"""
        pass
    
    @abstractmethod
    def is_available(self) -> bool:
        """Check if the provider is available and properly configured"""
        pass


class OpenAIProvider(BaseLLMProvider):
    """OpenAI provider implementation"""
    
    def _initialize_client(self):
        """Initialize OpenAI client"""
        try:
            import openai
            return openai.OpenAI(api_key=self.config.api_key)
        except ImportError:
            raise ImportError("OpenAI package not installed. Run: pip install openai")
    
    def generate_response(self, messages: list, **kwargs) -> str:
        """Generate response using OpenAI API"""
        try:
            response = self.client.chat.completions.create(
                model=self.config.model,
                messages=messages,
                max_tokens=kwargs.get('max_tokens', self.config.max_tokens),
                temperature=kwargs.get('temperature', self.config.temperature)
            )
            return response.choices[0].message.content
        except Exception as e:
            raise RuntimeError(f"OpenAI API error: {str(e)}")
    
    def is_available(self) -> bool:
        """Check if OpenAI is available"""
        try:
            return self.config.api_key is not None and self.client is not None
        except Exception:
            return False


class OllamaProvider(BaseLLMProvider):
    """Ollama provider implementation"""
    
    def _initialize_client(self):
        """Initialize Ollama client"""
        try:
            import requests
            return requests
        except ImportError:
            raise ImportError("Requests package not installed. Run: pip install requests")
    
    def generate_response(self, messages: list, **kwargs) -> str:
        """Generate response using Ollama API"""
        try:
            # Convert messages to Ollama format
            prompt = self._convert_messages_to_prompt(messages)
            
            response = self.client.post(
                f"{self.config.base_url or 'http://localhost:11434'}/api/generate",
                json={
                    "model": self.config.model,
                    "prompt": prompt,
                    "stream": False,
                    "options": {
                        "temperature": kwargs.get('temperature', self.config.temperature),
                        "num_predict": kwargs.get('max_tokens', self.config.max_tokens)
                    }
                }
            )
            response.raise_for_status()
            return response.json()["response"]
        except Exception as e:
            raise RuntimeError(f"Ollama API error: {str(e)}")
    
    def _convert_messages_to_prompt(self, messages: list) -> str:
        """Convert OpenAI-style messages to Ollama prompt format"""
        prompt_parts = []
        for message in messages:
            role = message["role"]
            content = message["content"]
            
            if role == "system":
                prompt_parts.append(f"System: {content}")
            elif role == "user":
                prompt_parts.append(f"Human: {content}")
            elif role == "assistant":
                prompt_parts.append(f"Assistant: {content}")
        
        return "\n\n".join(prompt_parts)
    
    def is_available(self) -> bool:
        """Check if Ollama is available"""
        try:
            if not self.client:
                return False
            
            # Test connection to Ollama
            response = self.client.get(
                f"{self.config.base_url or 'http://localhost:11434'}/api/tags",
                timeout=5
            )
            return response.status_code == 200
        except Exception:
            return False


class LLMProviderFactory:
    """Factory for creating LLM providers"""
    
    _providers = {
        "openai": OpenAIProvider,
        "ollama": OllamaProvider,
    }
    
    @classmethod
    def create_provider(cls, config: LLMProviderConfig) -> BaseLLMProvider:
        """Create an LLM provider based on configuration"""
        provider_class = cls._providers.get(config.provider.lower())
        if not provider_class:
            raise ValueError(f"Unsupported LLM provider: {config.provider}")
        
        return provider_class(config)
    
    @classmethod
    def get_available_providers(cls) -> list[str]:
        """Get list of available provider names"""
        return list(cls._providers.keys())
    
    @classmethod
    def register_provider(cls, name: str, provider_class: type[BaseLLMProvider]):
        """Register a new provider class"""
        cls._providers[name.lower()] = provider_class
