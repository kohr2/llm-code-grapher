"""
Base Parser Tests
Abstract base class for language-specific parser tests
"""

import pytest
from abc import ABC, abstractmethod
from pathlib import Path
from typing import List, Dict, Any
from unittest.mock import Mock

from ..parser import BaseParser, BaseParserResult
from ..ontology.base_models import BaseProgram, BaseSection, BaseSubsection, BaseRelationship


class BaseParserTests(ABC):
    """Abstract base class for parser tests"""
    
    @abstractmethod
    def get_parser_class(self):
        """Get the parser class to test"""
        pass
    
    @abstractmethod
    def get_language(self) -> str:
        """Get the language being tested"""
        pass
    
    @abstractmethod
    def get_sample_code(self) -> str:
        """Get sample code for testing"""
        pass
    
    @abstractmethod
    def get_expected_sections(self) -> List[Dict[str, Any]]:
        """Get expected sections for sample code"""
        pass
    
    @abstractmethod
    def get_expected_subsections(self) -> List[Dict[str, Any]]:
        """Get expected subsections for sample code"""
        pass
    
    @pytest.fixture
    def parser(self):
        """Create parser instance for testing"""
        return self.get_parser_class()()
    
    @pytest.fixture
    def sample_code(self):
        """Get sample code for testing"""
        return self.get_sample_code()
    
    def test_parser_initialization(self, parser):
        """Test parser initializes correctly"""
        assert parser is not None
        assert parser.language == self.get_language()
        assert hasattr(parser, 'section_patterns')
        assert hasattr(parser, 'subsection_patterns')
        assert hasattr(parser, 'relationship_patterns')
    
    def test_section_patterns_defined(self, parser):
        """Test that section patterns are defined"""
        assert parser.section_patterns is not None
        assert len(parser.section_patterns) > 0
        for pattern_name, pattern in parser.section_patterns.items():
            assert isinstance(pattern_name, str)
            assert isinstance(pattern, str)
            assert len(pattern) > 0
    
    def test_subsection_patterns_defined(self, parser):
        """Test that subsection patterns are defined"""
        assert parser.subsection_patterns is not None
        assert len(parser.subsection_patterns) > 0
        for pattern_name, pattern in parser.subsection_patterns.items():
            assert isinstance(pattern_name, str)
            assert isinstance(pattern, str)
            assert len(pattern) > 0
    
    def test_relationship_patterns_defined(self, parser):
        """Test that relationship patterns are defined"""
        assert parser.relationship_patterns is not None
        assert len(parser.relationship_patterns) > 0
        for pattern_name, pattern in parser.relationship_patterns.items():
            assert isinstance(pattern_name, str)
            assert isinstance(pattern, str)
            assert len(pattern) > 0
    
    def test_file_reading(self, parser, sample_code, tmp_path):
        """Test file reading functionality"""
        # Create temporary file
        test_file = tmp_path / f"test.{self.get_language().lower()}"
        test_file.write_text(sample_code)
        
        # Test file reading
        lines = parser._read_file(test_file)
        assert isinstance(lines, list)
        assert len(lines) > 0
        assert lines[0].strip() == sample_code.split('\n')[0].strip()
    
    def test_pattern_matching(self, parser):
        """Test pattern matching functionality"""
        test_line = "TEST LINE"
        
        # Test that patterns can be matched
        for pattern_name, pattern in parser.section_patterns.items():
            # This is a basic test - subclasses should override with specific tests
            assert isinstance(parser._match_pattern(pattern, test_line), bool)
    
    def test_comment_line_detection(self, parser):
        """Test comment line detection"""
        # Test various comment formats
        comment_lines = [
            "* This is a comment",
            "/* This is a comment */",
            "// This is a comment",
            "    * Indented comment",
            "    /* Indented comment */"
        ]
        
        non_comment_lines = [
            "PROGRAM-ID. TEST-PROGRAM.",
            "    MOVE 1 TO WS-COUNTER",
            "    PERFORM 1000-PROCESS",
            "    IF WS-FLAG = 'Y'"
        ]
        
        for line in comment_lines:
            assert parser._is_comment_line(line), f"Should detect as comment: {line}"
        
        for line in non_comment_lines:
            assert not parser._is_comment_line(line), f"Should not detect as comment: {line}"
    
    def test_code_line_filtering(self, parser):
        """Test code line filtering"""
        lines = [
            "* This is a comment",
            "PROGRAM-ID. TEST-PROGRAM.",
            "    * Indented comment",
            "    MOVE 1 TO WS-COUNTER",
            "",
            "    PERFORM 1000-PROCESS"
        ]
        
        filtered_lines = parser._filter_code_lines(lines, 0, len(lines) - 1)
        
        # Should filter out comments and empty lines
        assert len(filtered_lines) == 2
        assert "PROGRAM-ID. TEST-PROGRAM." in filtered_lines
        assert "    MOVE 1 TO WS-COUNTER" in filtered_lines
    
    def test_complexity_calculation(self, parser):
        """Test complexity score calculation"""
        lines = ["line1", "line2", "line3", "* comment", "line4"]
        
        complexity = parser._calculate_complexity_score(lines, 0, 4)
        
        assert isinstance(complexity, float)
        assert 0.0 <= complexity <= 1.0
    
    def test_risk_assessment(self, parser):
        """Test risk level assessment"""
        # Test different line counts
        short_lines = ["line1", "line2", "line3"]
        medium_lines = ["line" + str(i) for i in range(25)]
        long_lines = ["line" + str(i) for i in range(75)]
        
        short_risk = parser._assess_risk_level(short_lines, 0, 2)
        medium_risk = parser._assess_risk_level(medium_lines, 0, 24)
        long_risk = parser._assess_risk_level(long_lines, 0, 74)
        
        assert short_risk in ["LOW", "MEDIUM", "HIGH", "CRITICAL"]
        assert medium_risk in ["LOW", "MEDIUM", "HIGH", "CRITICAL"]
        assert long_risk in ["LOW", "MEDIUM", "HIGH", "CRITICAL"]
        
        # Risk should generally increase with line count
        risk_levels = {"LOW": 1, "MEDIUM": 2, "HIGH": 3, "CRITICAL": 4}
        assert risk_levels[short_risk] <= risk_levels[medium_risk] <= risk_levels[long_risk]
    
    def test_section_creation(self, parser):
        """Test section creation with default values"""
        section = parser._create_section(
            name="TEST-SECTION",
            section_type="PROCEDURE",
            line_range=(1, 10),
            business_logic="Test logic"
        )
        
        assert isinstance(section, BaseSection)
        assert section.name == "TEST-SECTION"
        assert section.type == "PROCEDURE"
        assert section.line_range == (1, 10)
        assert section.line_count == 10
        assert section.business_logic == "Test logic"
        assert section.confidence == 0.9
        assert section.complexity_score == 0.5
        assert section.risk_level == "LOW"
    
    def test_subsection_creation(self, parser):
        """Test subsection creation with default values"""
        subsection = parser._create_subsection(
            name="TEST-SUBSECTION",
            parent_section="TEST-SECTION",
            line_range=(5, 8),
            business_logic="Test logic"
        )
        
        assert isinstance(subsection, BaseSubsection)
        assert subsection.name == "TEST-SUBSECTION"
        assert subsection.parent_section == "TEST-SECTION"
        assert subsection.line_range == (5, 8)
        assert subsection.line_count == 4
        assert subsection.business_logic == "Test logic"
        assert subsection.confidence == 0.8
        assert subsection.complexity_score == 0.4
        assert subsection.risk_level == "LOW"
    
    def test_parsing_statistics(self, parser, sample_code, tmp_path):
        """Test parsing statistics calculation"""
        # Create temporary file
        test_file = tmp_path / f"test.{self.get_language().lower()}"
        test_file.write_text(sample_code)
        
        # Parse file
        result = parser.parse(test_file)
        
        # Test statistics
        stats = parser.get_parsing_statistics(result)
        
        assert isinstance(stats, dict)
        assert "language" in stats
        assert "total_sections" in stats
        assert "total_subsections" in stats
        assert "total_relationships" in stats
        assert "total_lines" in stats
        assert "parsing_confidence" in stats
        
        assert stats["language"] == self.get_language()
        assert stats["total_sections"] >= 0
        assert stats["total_subsections"] >= 0
        assert stats["total_relationships"] >= 0
        assert stats["total_lines"] > 0
        assert 0.0 <= stats["parsing_confidence"] <= 1.0
