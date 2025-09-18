"""
Language-specific Packages
Contains language-specific parsers, analyzers, and ontologies
"""

from .base import BaseOntology, BaseOntologyValidator, BaseProgram, BaseSection, BaseSubsection, BaseRelationship
from .cobol import COBOLParser, COBOLAnalyzer, COBOLOntologyValidator, ValidationResult

__all__ = [
    'BaseOntology', 'BaseOntologyValidator', 'BaseProgram', 'BaseSection', 'BaseSubsection', 'BaseRelationship',
    'COBOLParser', 'COBOLAnalyzer', 'COBOLOntologyValidator', 'ValidationResult'
]
