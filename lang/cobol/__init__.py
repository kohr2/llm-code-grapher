"""
COBOL Language Package
Contains COBOL-specific parsing, analysis, and ontology components
"""

from .parser import COBOLParser, COBOLAnalyzer
from .ontology.cobol_ontology_validator import COBOLOntologyValidator, ValidationResult

__all__ = ['COBOLParser', 'COBOLAnalyzer', 'COBOLOntologyValidator', 'ValidationResult']