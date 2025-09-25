"""
Base Ontology Package
Contains abstract base classes and common ontology utilities
"""

from .base_ontology import BaseOntology, BaseOntologyValidator, ValidationResult
from .base_models import BaseProgram, BaseSection, BaseSubsection, BaseRelationship

__all__ = ['BaseOntology', 'BaseOntologyValidator', 'ValidationResult', 'BaseProgram', 'BaseSection', 'BaseSubsection', 'BaseRelationship']
