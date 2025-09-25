"""
COBOL Program Ontology
Extends the base ontology with COBOL-specific concepts and validation rules
"""

from typing import List, Dict, Any

from lang.base.ontology import BaseOntology
from lang.base.ontology.base_models import RiskLevel, QualityLevel, ModernizationLevel


class COBOLOntology(BaseOntology):
    """COBOL-specific ontology extending the base ontology"""
    
    def __init__(self):
        """Initialize COBOL ontology"""
        super().__init__(None)
    
    def get_language(self) -> str:
        """Get the language this ontology supports"""
        return "COBOL"
    
    def get_ontology_version(self) -> str:
        """Get the ontology version"""
        return "1.0.0"
    
    def get_relationship_types(self) -> List[str]:
        """Get valid relationship types for COBOL"""
        return [
            "CALLS", "USES", "MODIFIES", "DEPENDS_ON",
            "DATA_FLOW", "CONTROL_FLOW", "IMPLEMENTS", "CONTAINS",
            "PERFORMS", "GOES_TO", "EXITS", "RETURNS"
        ]
    
    def get_section_types(self) -> List[str]:
        """Get valid section types for COBOL"""
        return [
            "IDENTIFICATION", "ENVIRONMENT", "DATA", "PROCEDURE",
            "FILE", "WORKING-STORAGE", "LINKAGE", "PROCEDURE_SECTION"
        ]
    
    def _load_ontology_schema(self, ontology_file) -> Dict[str, Any]:
        """Load COBOL-specific ontology schema - now returns static schema"""
        return {
            "name": "COBOL Program Ontology",
            "version": "1.0.0",
            "description": "COBOL-specific ontology extending base program ontology",
            "language": "COBOL",
            "concepts": self._get_cobol_concepts(),
            "relationships": self._get_cobol_relationships(),
            "validation": self._get_cobol_validation_rules()
        }
    
    def _get_cobol_concepts(self) -> Dict[str, Any]:
        """Get COBOL-specific concepts for schema"""
        return {
            "division": {
                "description": "COBOL division (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)",
                "properties": {
                    "name": "Division name",
                    "type": "Division type",
                    "line_range": "Source code line range",
                    "line_count": "Number of lines"
                }
            },
            "paragraph": {
                "description": "COBOL paragraph within PROCEDURE DIVISION",
                "properties": {
                    "name": "Paragraph name",
                    "parent_section": "Parent section",
                    "line_range": "Source code line range",
                    "line_count": "Number of lines"
                }
            },
            "copybook": {
                "description": "COBOL copybook (included file)",
                "properties": {
                    "name": "Copybook name",
                    "file_path": "Copybook file path",
                    "included_by": "Program that includes this copybook"
                }
            }
        }
    
    def _get_cobol_relationships(self) -> Dict[str, Any]:
        """Get COBOL-specific relationships"""
        return {
            "division_contains_paragraph": {
                "source": "division",
                "target": "paragraph",
                "type": "CONTAINS",
                "description": "Division contains paragraphs"
            },
            "program_includes_copybook": {
                "source": "program",
                "target": "copybook",
                "type": "INCLUDES",
                "description": "Program includes copybooks"
            }
        }
    
    def _get_cobol_validation_rules(self) -> Dict[str, Any]:
        """Get COBOL-specific validation rules"""
        return {
            "required_properties": {
                "division": ["name", "type", "line_range"],
                "paragraph": ["name", "parent_section", "line_range"],
                "copybook": ["name", "file_path"]
            },
            "value_constraints": {
                "division_type": "Must be one of: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE"
            }
        }
    
    def get_cobol_data_types(self) -> List[str]:
        """Get COBOL-specific data types"""
        return [
            "PIC", "PICTURE", "COMP", "COMP-1", "COMP-2", "COMP-3",
            "BINARY", "PACKED-DECIMAL", "FLOAT", "DOUBLE", "CHAR",
            "VARCHAR", "DATE", "TIME", "TIMESTAMP"
        ]
    
    def get_cobol_operations(self) -> List[str]:
        """Get COBOL-specific operations"""
        return [
            "MOVE", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "COMPUTE",
            "IF", "ELSE", "END-IF", "PERFORM", "CALL", "GO TO",
            "READ", "WRITE", "REWRITE", "DELETE", "START", "OPEN", "CLOSE"
        ]
