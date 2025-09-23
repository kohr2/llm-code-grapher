"""
COBOL Parser
Parses COBOL programs and extracts structural information
"""

from pathlib import Path
from typing import List, Dict, Any, Optional
import re
from dataclasses import dataclass

from lang.base.parser import BaseParser, BaseParserResult
from lang.base.ontology.base_models import BaseProgram, BaseSection, BaseSubsection, BaseRelationship, RiskLevel


@dataclass
class COBOLSection(BaseSection):
    """Represents a COBOL section"""
    pass  # Uses default implementations from BaseSection


@dataclass
class COBOLSubsection(BaseSubsection):
    """Represents a COBOL subsection"""
    pass  # Uses default implementations from BaseSubsection


@dataclass
class COBOLProgram(BaseProgram):
    """Represents a COBOL program"""
    def get_complexity_metrics(self) -> Dict[str, float]:
        """Get complexity metrics for the program"""
        return {
            "cyclomatic_complexity": 0.0,  # To be calculated
            "maintainability_index": 0.0,  # To be calculated
            "technical_debt": 0.0  # To be calculated
        }
    
    def get_quality_indicators(self) -> Dict[str, Any]:
        """Get quality indicators for the program"""
        return {
            "code_coverage": "UNKNOWN",
            "documentation_quality": "UNKNOWN",
            "test_coverage": "UNKNOWN"
        }


@dataclass
class COBOLRelationship(BaseRelationship):
    """Represents a COBOL relationship"""
    pass  # Uses default implementations from BaseRelationship


@dataclass
class COBOLParserResult(BaseParserResult):
    """Result of COBOL parsing"""
    def get_parsing_metrics(self) -> Dict[str, Any]:
        """Get parsing-specific metrics"""
        return {
            "total_sections": len(self.sections),
            "total_subsections": len(self.subsections),
            "total_relationships": len(self.relationships),
            "parsing_confidence": self._calculate_confidence()
        }
    
    def validate_structure(self) -> List[str]:
        """Validate parsed structure and return errors"""
        errors = []
        
        # Check for duplicate section names
        section_names = [s.name for s in self.sections]
        if len(section_names) != len(set(section_names)):
            errors.append("Duplicate section names found")
        
        # Check for duplicate subsection names
        subsection_names = [s.name for s in self.subsections]
        if len(subsection_names) != len(set(subsection_names)):
            errors.append("Duplicate subsection names found")
        
        return errors
    
    def _calculate_confidence(self) -> float:
        """Calculate overall parsing confidence"""
        if not self.sections:
            return 0.0
        
        confidences = []
        for section in self.sections:
            if hasattr(section, 'confidence'):
                confidences.append(section.confidence)
        
        for subsection in self.subsections:
            if hasattr(subsection, 'confidence'):
                confidences.append(subsection.confidence)
        
        return sum(confidences) / len(confidences) if confidences else 0.0


class COBOLParser(BaseParser):
    """Parses COBOL programs and extracts structural information"""
    
    def __init__(self):
        """Initialize the COBOL parser"""
        super().__init__("COBOL")
    
    def _get_section_patterns(self) -> Dict[str, str]:
        """Get COBOL-specific section patterns"""
        return {
            # COBOL Divisions (with line numbers)
            'IDENTIFICATION': r'^\d+\s+IDENTIFICATION\s+DIVISION',
            'ENVIRONMENT': r'^\d+\s+ENVIRONMENT\s+DIVISION',
            'DATA': r'^\d+\s+DATA\s+DIVISION',
            'PROCEDURE': r'^\d+\s+PROCEDURE\s+DIVISION',
            # COBOL Sections (numbered sections like 1000-INITIALIZE-PROGRAM SECTION)
            # Note: COBOL lines may have line numbers at the beginning
            'SECTION': r'^\d+\s+\d+-(\w+(?:-\w+)*)\s+SECTION'
        }
    
    def _get_subsection_patterns(self) -> Dict[str, str]:
        """Get COBOL-specific subsection patterns"""
        return {
            # COBOL Divisions
            'FILE': r'^FILE\s+SECTION',
            'WORKING-STORAGE': r'^WORKING-STORAGE\s+SECTION',
            'LINKAGE': r'^LINKAGE\s+SECTION',
            'PROCEDURE_SECTION': r'^PROCEDURE\s+SECTION',
            # COBOL Paragraphs (within sections)
            'PARAGRAPH': r'^\d+\s+\d+-(\w+(?:-\w+)*)\.'
        }
    
    def _get_relationship_patterns(self) -> Dict[str, str]:
        """Get COBOL-specific relationship patterns"""
        return {
            'CALL': r'CALL\s+[\'"]?(\w+)[\'"]?',
            'PERFORM': r'PERFORM\s+(\w+)',
            'GO_TO': r'GO\s+TO\s+(\w+)'
        }
    
    def parse(self, file_path: Path) -> COBOLParserResult:
        """Parse a COBOL file and return analysis results"""
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
        
        lines = content.split('\n')
        
        # Extract program name
        program_name = self._extract_program_name(lines)
        
        # Find sections and subsections
        sections = self._find_sections(lines)
        subsections = self._find_subsections(lines, sections)
        
        # Find relationships
        relationships = self._find_relationships(lines, sections, subsections)
        
        # Create metadata
        metadata = {
            'total_lines': len(lines),
            'file_path': str(file_path),
            'analysis_timestamp': None  # Will be set by caller
        }
        
        # Create COBOL program
        program = COBOLProgram(
            name=program_name,
            language="COBOL",
            metadata=metadata
        )
        
        return COBOLParserResult(
            program=program,
            sections=sections,
            subsections=subsections,
            relationships=relationships,
            metadata=metadata
        )
    
    def _extract_program_name(self, lines: List[str]) -> str:
        """Extract program name from IDENTIFICATION DIVISION"""
        for line in lines:
            if 'PROGRAM-ID' in line.upper():
                # Extract program name from PROGRAM-ID line
                match = re.search(r'PROGRAM-ID\.\s+(\w+)', line.upper())
                if match:
                    return match.group(1)
        return "UNKNOWN"
    
    def _find_sections(self, lines: List[str]) -> List[COBOLSection]:
        """Find all sections in the COBOL program"""
        sections = []
        matches = self._find_pattern_matches(lines, self.section_patterns)
        
        for line_index, section_type, match_text in matches:
            # Find the end of this section
            end_line = self._find_section_end(lines, line_index, section_type)
            
            # Extract business logic
            business_logic = self._extract_business_logic(lines, line_index, end_line)
            
            # Calculate complexity and risk
            complexity_score = self._calculate_complexity_score(lines, line_index, end_line)
            risk_level = self._assess_risk_level(lines, line_index, end_line)
            
            # Extract section name based on type
            original_line_number = None
            if section_type == 'SECTION':
                # Extract section name from match text (e.g., "022300 1000-INITIALIZE-PROGRAM SECTION")
                import re
                match = re.match(r'^\d+\s+\d+-(\w+(?:-\w+)*)\s+SECTION', match_text)
                if match:
                    # Only use the actual section name, ignore all line numbers
                    section_name = match.group(1)
                    name = section_name
                else:
                    name = "UNKNOWN-SECTION"
            else:
                # For divisions, use the division type as the name
                name = f"{section_type}-DIVISION"
            
            # Create section using base class method
            section = self._create_section(
                name=name,
                section_type=section_type,
                line_range=(line_index + 1, end_line + 1),
                business_logic=business_logic,
                confidence=0.9,  # High confidence for structure detection
                complexity_score=complexity_score,
                risk_level=risk_level,
                metadata=None
            )
            
            sections.append(COBOLSection(**section.__dict__))
        
        return sections
    
    def _find_subsections(self, lines: List[str], sections: List[COBOLSection]) -> List[COBOLSubsection]:
        """Find all subsections in the COBOL program"""
        subsections = []
        matches = self._find_pattern_matches(lines, self.subsection_patterns)
        
        for line_index, subsection_type, match_text in matches:
            # Find parent section
            parent_section = self._find_parent_section(sections, line_index)
            
            # Find the end of this subsection
            end_line = self._find_subsection_end(lines, line_index, subsection_type)
            
            # Extract business logic
            business_logic = self._extract_business_logic(lines, line_index, end_line)
            
            # Calculate complexity and risk
            complexity_score = self._calculate_complexity_score(lines, line_index, end_line)
            risk_level = self._assess_risk_level(lines, line_index, end_line)
            
            # Extract subsection name based on type
            if subsection_type == 'PARAGRAPH':
                # Extract paragraph name from match text (e.g., "021700 0000-MAIN-PROCESS.")
                import re
                match = re.match(r'^\d+\s+\d+-(\w+(?:-\w+)*)\.', match_text)
                if match:
                    paragraph_name = match.group(1)
                    name = paragraph_name
                else:
                    name = "UNKNOWN-PARAGRAPH"
            else:
                name = f"{subsection_type}-SECTION"
            
            # Create subsection using base class method
            subsection = self._create_subsection(
                name=name,
                parent_section=parent_section,
                line_range=(line_index + 1, end_line + 1),
                business_logic=business_logic,
                confidence=0.8,  # High confidence for structure detection
                complexity_score=complexity_score,
                risk_level=risk_level
            )
            subsections.append(COBOLSubsection(**subsection.__dict__))
        
        return subsections
    
    def _find_section_end(self, lines: List[str], start_line: int, section_type: str) -> int:
        """Find the end line of a section (COBOL-specific)"""
        return super()._find_section_end(lines, start_line, self.section_patterns)
    
    def _find_subsection_end(self, lines: List[str], start_line: int, subsection_type: str) -> int:
        """Find the end line of a subsection (COBOL-specific)"""
        return super()._find_subsection_end(lines, start_line, self.section_patterns, self.subsection_patterns)
    
    def _find_parent_section(self, sections: List[COBOLSection], line_number: int) -> str:
        """Find the parent section for a subsection (COBOL-specific)"""
        return super()._find_parent_section(sections, line_number)
    
    def _extract_business_logic(self, lines: List[str], start_line: int, end_line: int) -> str:
        """Extract business logic from a section (COBOL-specific)"""
        return super()._extract_business_logic(lines, start_line, end_line)
    
    def _find_relationships(self, lines: List[str], sections: List[COBOLSection], subsections: List[COBOLSubsection]) -> List[BaseRelationship]:
        """Find relationships between sections and subsections"""
        return self._extract_relationships_from_patterns(lines, self.relationship_patterns)
