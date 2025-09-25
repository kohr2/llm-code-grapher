"""
COBOL Ontology Package
Contains COBOL-specific ontology validation and utilities
"""

from .cobol_ontology import COBOLOntology
from .cobol_ontology_validator import COBOLOntologyValidator
from lang.base.ontology import ValidationResult

__all__ = ['COBOLOntology', 'COBOLOntologyValidator', 'ValidationResult']
