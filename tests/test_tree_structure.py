"""
Test Tree Structure Validation

This module tests that the code graph maintains proper tree structure
with immediate parent relationships only.
"""

import pytest
from unittest.mock import Mock
from src.neo4j_converter import ParserResultConverter
from lang.base.parser.base_parser import BaseParserResult
from lang.base.ontology.base_models import BaseProgram, BaseSection, BaseSubsection


class TestTreeStructure:
    """Test cases for tree structure validation"""
    
    def test_no_multiple_parents(self):
        """Test that no node has multiple parents"""
        converter = ParserResultConverter()
        
        # Create mock parser result with proper tree structure
        program = Mock(spec=BaseProgram)
        program.name = "TEST-PROGRAM"
        program.language = "COBOL"
        
        sections = [
            Mock(spec=BaseSection, name="DATA-DIVISION", type="DATA", line_range=(1, 10)),
            Mock(spec=BaseSection, name="FILE-SECTION", type="SECTION", line_range=(2, 8)),
            Mock(spec=BaseSection, name="WORKING-STORAGE", type="SECTION", line_range=(9, 10))
        ]
        
        subsections = [
            Mock(spec=BaseSubsection, name="FILE-DEF", parent_section="FILE-SECTION", line_range=(3, 5)),
            Mock(spec=BaseSubsection, name="WORKING-DEF", parent_section="WORKING-STORAGE", line_range=(9, 10))
        ]
        
        result = Mock(spec=BaseParserResult)
        result.program = program
        result.sections = sections
        result.subsections = subsections
        result.relationships = []
        
        # Convert to graph data
        graph_data = converter.convert_parser_result(result)
        
        # Check that each node has only one parent
        child_parents = {}
        for rel in graph_data.relationships:
            if rel.relationship_type == "CONTAINS":
                if rel.target_id in child_parents:
                    pytest.fail(f"Node {rel.target_id} has multiple parents: {child_parents[rel.target_id]} and {rel.source_id}")
                child_parents[rel.target_id] = rel.source_id
    
    def test_immediate_parent_assignment(self):
        """Test that subsections are assigned to their immediate parent sections"""
        converter = ParserResultConverter()
        
        # Create mock parser result with nested structure
        program = Mock(spec=BaseProgram)
        program.name = "TEST-PROGRAM"
        program.language = "COBOL"
        
        sections = [
            Mock(spec=BaseSection, name="DATA-DIVISION", type="DATA", line_range=(1, 20)),
            Mock(spec=BaseSection, name="FILE-SECTION", type="SECTION", line_range=(2, 15)),
            Mock(spec=BaseSection, name="WORKING-STORAGE", type="SECTION", line_range=(16, 20))
        ]
        
        subsections = [
            Mock(spec=BaseSubsection, name="FILE-DEF", parent_section="FILE-SECTION", line_range=(3, 5)),
            Mock(spec=BaseSubsection, name="WORKING-DEF", parent_section="WORKING-STORAGE", line_range=(17, 19))
        ]
        
        result = Mock(spec=BaseParserResult)
        result.program = program
        result.sections = sections
        result.subsections = subsections
        result.relationships = []
        
        # Convert to graph data
        graph_data = converter.convert_parser_result(result)
        
        # Check that subsections are linked to their immediate parent sections, not divisions
        subsection_relationships = [rel for rel in graph_data.relationships 
                                  if rel.relationship_type == "CONTAINS" and rel.target_id.startswith("subsection_")]
        
        for rel in subsection_relationships:
            # Find the source section name
            source_section = None
            for section in sections:
                if f"section_{sections.index(section) + 1}" == rel.source_id:
                    source_section = section
                    break
            
            # Find the target subsection name
            target_subsection = None
            for subsection in subsections:
                if f"subsection_{subsections.index(subsection) + 1}" == rel.target_id:
                    target_subsection = subsection
                    break
            
            if source_section and target_subsection:
                # The source should be the immediate parent section, not a division
                assert source_section.name == target_subsection.parent_section
                assert source_section.type != "DATA"  # Should not be a division
    
    def test_no_circular_references(self):
        """Test that no circular references exist in the graph"""
        converter = ParserResultConverter()
        
        # Create mock parser result
        program = Mock(spec=BaseProgram)
        program.name = "TEST-PROGRAM"
        program.language = "COBOL"
        
        sections = [
            Mock(spec=BaseSection, name="DATA-DIVISION", type="DATA", line_range=(1, 10)),
            Mock(spec=BaseSection, name="FILE-SECTION", type="SECTION", line_range=(2, 8))
        ]
        
        subsections = [
            Mock(spec=BaseSubsection, name="FILE-DEF", parent_section="FILE-SECTION", line_range=(3, 5))
        ]
        
        result = Mock(spec=BaseParserResult)
        result.program = program
        result.sections = sections
        result.subsections = subsections
        result.relationships = []
        
        # Convert to graph data
        graph_data = converter.convert_parser_result(result)
        
        # Check for circular references
        for rel in graph_data.relationships:
            if rel.relationship_type == "CONTAINS":
                # Use the converter's method to check for circular references
                has_circular = converter._has_circular_reference(rel.source_id, rel.target_id, graph_data.relationships)
                assert not has_circular, f"Circular reference detected: {rel.source_id} -> {rel.target_id}"
    
    def test_proper_hierarchy_levels(self):
        """Test that the hierarchy has proper levels: Program -> Division -> Section -> Subsection"""
        converter = ParserResultConverter()
        
        # Create mock parser result with full hierarchy
        program = Mock(spec=BaseProgram)
        program.name = "TEST-PROGRAM"
        program.language = "COBOL"
        
        sections = [
            Mock(spec=BaseSection, name="DATA-DIVISION", type="DATA", line_range=(1, 20)),
            Mock(spec=BaseSection, name="FILE-SECTION", type="SECTION", line_range=(2, 15)),
            Mock(spec=BaseSection, name="WORKING-STORAGE", type="SECTION", line_range=(16, 20))
        ]
        
        subsections = [
            Mock(spec=BaseSubsection, name="FILE-DEF", parent_section="FILE-SECTION", line_range=(3, 5)),
            Mock(spec=BaseSubsection, name="WORKING-DEF", parent_section="WORKING-STORAGE", line_range=(17, 19))
        ]
        
        result = Mock(spec=BaseParserResult)
        result.program = program
        result.sections = sections
        result.subsections = subsections
        result.relationships = []
        
        # Convert to graph data
        graph_data = converter.convert_parser_result(result)
        
        # Check hierarchy levels
        program_id = None
        division_ids = []
        section_ids = []
        subsection_ids = []
        
        for node in graph_data.nodes:
            if node.node_type == "Program":
                program_id = node.node_id
            elif node.node_type == "Section" and node.name.endswith("-DIVISION"):
                division_ids.append(node.node_id)
            elif node.node_type == "Section":
                section_ids.append(node.node_id)
            elif node.node_type == "Subsection":
                subsection_ids.append(node.node_id)
        
        # Verify hierarchy
        assert program_id is not None, "Program node should exist"
        assert len(division_ids) > 0, "Division nodes should exist"
        assert len(section_ids) > 0, "Section nodes should exist"
        assert len(subsection_ids) > 0, "Subsection nodes should exist"
        
        # Check that divisions are linked to program
        division_relationships = [rel for rel in graph_data.relationships 
                                if rel.relationship_type == "CONTAINS" and rel.source_id == program_id]
        assert len(division_relationships) > 0, "Divisions should be linked to program"
        
        # Check that sections are linked to divisions or program
        section_relationships = [rel for rel in graph_data.relationships 
                               if rel.relationship_type == "CONTAINS" and rel.target_id in section_ids]
        assert len(section_relationships) > 0, "Sections should be linked to their parents"
        
        # Check that subsections are linked to sections
        subsection_relationships = [rel for rel in graph_data.relationships 
                                  if rel.relationship_type == "CONTAINS" and rel.target_id in subsection_ids]
        assert len(subsection_relationships) > 0, "Subsections should be linked to their parent sections"
