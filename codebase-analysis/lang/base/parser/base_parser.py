"""
Base Parser
Abstract base class for language-specific parsers
"""

from abc import ABC, abstractmethod
from pathlib import Path
from typing import List, Dict, Any, Optional, Type
from dataclasses import dataclass

from ..ontology.base_models import BaseProgram, BaseSection, BaseSubsection, BaseRelationship


@dataclass
class BaseParserResult(ABC):
    """Abstract base class for parser results"""
    program: BaseProgram
    sections: List[BaseSection]
    subsections: List[BaseSubsection]
    relationships: List[BaseRelationship]
    metadata: Dict[str, Any]
    
    @abstractmethod
    def get_parsing_metrics(self) -> Dict[str, Any]:
        """Get parsing-specific metrics"""
        pass
    
    @abstractmethod
    def validate_structure(self) -> List[str]:
        """Validate parsed structure and return errors"""
        pass


class BaseParser(ABC):
    """Abstract base class for language-specific parsers"""
    
    def __init__(self, language: str):
        """Initialize base parser"""
        self.language = language
        self.section_patterns = self._get_section_patterns()
        self.subsection_patterns = self._get_subsection_patterns()
        self.relationship_patterns = self._get_relationship_patterns()
    
    @abstractmethod
    def _get_section_patterns(self) -> Dict[str, str]:
        """Get language-specific section patterns"""
        pass
    
    @abstractmethod
    def _get_subsection_patterns(self) -> Dict[str, str]:
        """Get language-specific subsection patterns"""
        pass
    
    @abstractmethod
    def _get_relationship_patterns(self) -> Dict[str, str]:
        """Get language-specific relationship patterns"""
        pass
    
    @abstractmethod
    def parse(self, file_path: Path) -> BaseParserResult:
        """Parse a file and return analysis results"""
        pass
    
    @abstractmethod
    def _extract_program_name(self, lines: List[str]) -> str:
        """Extract program name from source code"""
        pass
    
    @abstractmethod
    def _find_sections(self, lines: List[str]) -> List[BaseSection]:
        """Find all sections in the program"""
        pass
    
    @abstractmethod
    def _find_subsections(self, lines: List[str], sections: List[BaseSection]) -> List[BaseSubsection]:
        """Find all subsections in the program"""
        pass
    
    @abstractmethod
    def _find_relationships(self, lines: List[str], sections: List[BaseSection], subsections: List[BaseSubsection]) -> List[BaseRelationship]:
        """Find relationships between components"""
        pass
    
    @abstractmethod
    def _extract_business_logic(self, lines: List[str], start_line: int, end_line: int) -> str:
        """Extract business logic from code section"""
        pass
    
    def _read_file(self, file_path: Path) -> List[str]:
        """Read file and return lines with error handling"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                return f.read().split('\n')
        except Exception as e:
            raise FileNotFoundError(f"Could not read file {file_path}: {str(e)}")
    
    def _find_pattern_end(self, lines: List[str], start_line: int, patterns: Dict[str, str]) -> int:
        """Find the end line of a pattern match"""
        for i in range(start_line + 1, len(lines)):
            line_upper = lines[i].upper().strip()
            if any(self._match_pattern(pattern, line_upper) for pattern in patterns.values()):
                return i - 1
        return len(lines) - 1
    
    def _match_pattern(self, pattern: str, line: str) -> bool:
        """Match a pattern against a line (to be overridden by subclasses)"""
        import re
        return bool(re.match(pattern, line))
    
    def _calculate_complexity_score(self, lines: List[str], start_line: int, end_line: int) -> float:
        """Calculate basic complexity score (to be overridden by subclasses)"""
        # Basic complexity calculation - can be overridden
        code_lines = self._filter_code_lines(lines, start_line, end_line)
        return min(len(code_lines) / 100.0, 1.0)  # Normalize to 0-1
    
    def _assess_risk_level(self, lines: List[str], start_line: int, end_line: int) -> str:
        """Assess risk level (to be overridden by subclasses)"""
        # Basic risk assessment - can be overridden
        code_lines = self._filter_code_lines(lines, start_line, end_line)
        
        if len(code_lines) > 50:
            return "HIGH"
        elif len(code_lines) > 20:
            return "MEDIUM"
        else:
            return "LOW"
    
    def _filter_code_lines(self, lines: List[str], start_line: int, end_line: int) -> List[str]:
        """Filter out comments and empty lines from code section"""
        code_lines = []
        for i in range(start_line, min(end_line + 1, len(lines))):
            line = lines[i].strip()
            if line and not self._is_comment_line(line):
                code_lines.append(line)
        return code_lines
    
    def _is_comment_line(self, line: str) -> bool:
        """Check if line is a comment (to be overridden by subclasses)"""
        # Default comment detection - can be overridden
        return line.startswith('*') or line.startswith('/*') or line.startswith('//')
    
    def _create_section(self, name: str, section_type: str, line_range: tuple[int, int], 
                       business_logic: str, confidence: float = 0.9, 
                       complexity_score: float = 0.5, risk_level: str = "LOW",
                       metadata: Optional[Dict[str, Any]] = None) -> BaseSection:
        """Create a section with default values (to be overridden by subclasses)"""
        line_count = line_range[1] - line_range[0] + 1
        return BaseSection(
            name=name,
            type=section_type,
            line_range=line_range,
            line_count=line_count,
            business_logic=business_logic,
            confidence=confidence,
            complexity_score=complexity_score,
            risk_level=risk_level,
            metadata=metadata
        )
    
    def _create_subsection(self, name: str, parent_section: str, line_range: tuple[int, int],
                          business_logic: str, confidence: float = 0.8,
                          complexity_score: float = 0.4, risk_level: str = "LOW") -> BaseSubsection:
        """Create a subsection with default values (to be overridden by subclasses)"""
        line_count = line_range[1] - line_range[0] + 1
        return BaseSubsection(
            name=name,
            parent_section=parent_section,
            line_range=line_range,
            line_count=line_count,
            business_logic=business_logic,
            confidence=confidence,
            complexity_score=complexity_score,
            risk_level=risk_level
        )
    
    def _find_pattern_matches(self, lines: List[str], patterns: Dict[str, str]) -> List[tuple[int, str, str]]:
        """Find all pattern matches in lines, returns (line_index, pattern_name, match_text)"""
        matches = []
        for i, line in enumerate(lines):
            line_upper = line.upper().strip()
            for pattern_name, pattern in patterns.items():
                if self._match_pattern(pattern, line_upper):
                    matches.append((i, pattern_name, line_upper))
        return matches
    
    def _extract_relationships_from_patterns(self, lines: List[str], 
                                           relationship_patterns: Dict[str, str]) -> List[BaseRelationship]:
        """Extract relationships using pattern matching"""
        relationships = []
        for i, line in enumerate(lines):
            line_upper = line.upper()
            for rel_type, pattern in relationship_patterns.items():
                import re
                match = re.search(pattern, line_upper)
                if match:
                    target = match.group(1) if match.groups() else "UNKNOWN"
                    relationships.append(BaseRelationship(
                        source="CURRENT-PROGRAM",
                        target=target,
                        relationship_type=rel_type,
                        confidence=0.9,
                        strength=1.0
                    ))
        return relationships
    
    def _find_section_end(self, lines: List[str], start_line: int, patterns: Dict[str, str]) -> int:
        """Find the end line of a section by looking for next pattern match"""
        for i in range(start_line + 1, len(lines)):
            line_upper = lines[i].upper().strip()
            if any(self._match_pattern(pattern, line_upper) for pattern in patterns.values()):
                return i - 1
        return len(lines) - 1
    
    def _find_subsection_end(self, lines: List[str], start_line: int, 
                           section_patterns: Dict[str, str], subsection_patterns: Dict[str, str]) -> int:
        """Find the end line of a subsection by looking for next section or subsection"""
        for i in range(start_line + 1, len(lines)):
            line_upper = lines[i].upper().strip()
            if (any(self._match_pattern(pattern, line_upper) for pattern in section_patterns.values()) or
                any(self._match_pattern(pattern, line_upper) for pattern in subsection_patterns.values())):
                return i - 1
        return len(lines) - 1
    
    def _find_parent_section(self, sections: List[BaseSection], line_number: int) -> str:
        """Find the immediate parent section for a subsection based on line ranges"""
        # Find all sections that contain this line number
        containing_sections = []
        for section in sections:
            if section.line_range[0] <= line_number + 1 <= section.line_range[1]:
                containing_sections.append(section)
        
        if not containing_sections:
            return "UNKNOWN"
        
        # If only one section contains it, return that
        if len(containing_sections) == 1:
            return containing_sections[0].name
        
        # If multiple sections contain it, find the most specific (smallest range)
        # This ensures we get the immediate parent, not a higher-level division
        immediate_parent = min(containing_sections, key=lambda s: s.line_range[1] - s.line_range[0])
        return immediate_parent.name
    
    def _extract_business_logic(self, lines: List[str], start_line: int, end_line: int) -> str:
        """Extract business logic from a code section (generic implementation)"""
        logic_lines = self._filter_code_lines(lines, start_line, end_line)
        return ' '.join(logic_lines[:5])  # First 5 non-comment lines
    
    def _get_default_metrics(self, line_count: int, complexity_score: float, risk_level: str) -> Dict[str, Any]:
        """Get default metrics for sections/subsections"""
        return {
            "line_count": line_count,
            "complexity_score": complexity_score,
            "risk_level": risk_level
        }
    
    def get_parsing_statistics(self, result: BaseParserResult) -> Dict[str, Any]:
        """Get parsing statistics for the result"""
        return {
            "language": self.language,
            "total_sections": len(result.sections),
            "total_subsections": len(result.subsections),
            "total_relationships": len(result.relationships),
            "total_lines": result.metadata.get("total_lines", 0),
            "parsing_confidence": self._calculate_overall_confidence(result)
        }
    
    def _calculate_overall_confidence(self, result: BaseParserResult) -> float:
        """Calculate overall parsing confidence"""
        if not result.sections:
            return 0.0
        
        confidences = []
        for section in result.sections:
            if hasattr(section, 'confidence'):
                confidences.append(section.confidence)
        
        for subsection in result.subsections:
            if hasattr(subsection, 'confidence'):
                confidences.append(subsection.confidence)
        
        return sum(confidences) / len(confidences) if confidences else 0.0
