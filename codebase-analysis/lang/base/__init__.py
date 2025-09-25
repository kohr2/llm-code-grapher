"""
Base Language Package
Contains abstract base classes and common utilities for all languages
"""

from .ontology import BaseOntology, BaseOntologyValidator, BaseProgram, BaseSection, BaseSubsection, BaseRelationship

__all__ = ['BaseOntology', 'BaseOntologyValidator', 'BaseProgram', 'BaseSection', 'BaseSubsection', 'BaseRelationship']
