"""
COBOL Parser
Parses COBOL programs and extracts structural information
"""

from pathlib import Path
from typing import List, Dict, Any, Optional
import re
from dataclasses import dataclass, field

from lang.base.parser import BaseParser, BaseParserResult
from lang.base.ontology.base_models import BaseProgram, BaseSection, BaseSubsection, BaseOperation, BaseRelationship, RiskLevel


@dataclass
class COBOLSection(BaseSection):
    """Represents a COBOL section"""
    pass  # Uses default implementations from BaseSection


@dataclass
class COBOLSubsection(BaseSubsection):
    """Represents a COBOL subsection"""
    pass  # Uses default implementations from BaseSubsection


@dataclass
class COBOLOperation(BaseOperation):
    """Represents a COBOL operation"""
    def get_operation_metrics(self) -> Dict[str, Any]:
        """Get COBOL operation-specific metrics"""
        base_metrics = super().get_operation_metrics()
        base_metrics.update({
            "cobol_operation_type": self.operation_type,
            "has_parameters": len(self.parameters) > 0,
            "is_conditional": self.operation_type in ['IF', 'EVALUATE', 'PERFORM'],
            "is_io_operation": self.operation_type in ['READ', 'WRITE', 'OPEN', 'CLOSE', 'DELETE', 'REWRITE'],
            "is_arithmetic": self.operation_type in ['ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'COMPUTE']
        })
        return base_metrics


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
    operations: List[COBOLOperation] = field(default_factory=list)
    
    def get_parsing_metrics(self) -> Dict[str, Any]:
        """Get parsing-specific metrics"""
        return {
            "total_sections": len(self.sections),
            "total_subsections": len(self.subsections),
            "total_operations": len(self.operations),
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
            'PARAGRAPH': r'^\d+\s+\d+-(\w+(?:-\w+)*)\.',
            # Granular logical operations within paragraphs - using single regex with capture group
            'COBOL_OPERATION': r'^\s*(READ|ADD|IF|DISPLAY|MOVE|PERFORM|CALL|COMPUTE|EVALUATE|SET|OPEN|CLOSE|WRITE|DELETE|REWRITE|START|STOP|ACCEPT|INITIALIZE|STRING|UNSTRING|INSPECT|SEARCH|SORT|MERGE|RELEASE|RETURN)\s+'
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
        
        # Get operations from temporary storage
        operations = getattr(self, '_temp_operations', [])
        
        # Find operation flow relationships
        operation_relationships = self._find_operation_relationships(operations)
        relationships.extend(operation_relationships)
        
        return COBOLParserResult(
            program=program,
            sections=sections,
            subsections=subsections,
            operations=operations,
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
        """Find all subsections in the COBOL program with granular operation detection"""
        subsections = []
        
        # First, find all paragraph-level subsections
        paragraph_matches = self._find_pattern_matches(lines, {'PARAGRAPH': self.subsection_patterns['PARAGRAPH']})
        
        for line_index, subsection_type, match_text in paragraph_matches:
            # Find parent section
            parent_section = self._find_parent_section(sections, line_index)
            
            # Find the end of this paragraph
            end_line = self._find_subsection_end(lines, line_index, subsection_type)
            
            # Extract paragraph name
            import re
            match = re.match(r'^\d+\s+\d+-(\w+(?:-\w+)*)\.', match_text)
            if match:
                paragraph_name = match.group(1)
            else:
                paragraph_name = "UNKNOWN-PARAGRAPH"
            
            # Check if this paragraph contains multiple logical operations
            paragraph_lines = lines[line_index:end_line + 1]
            operation_count = self._count_logical_operations(paragraph_lines)
            
            if operation_count > 1:
                # Break down the paragraph into granular operations
                # First create a subsection for the paragraph
                paragraph_subsection = self._create_subsection(
                    name=paragraph_name,
                    parent_section=parent_section,
                    line_range=(line_index + 1, end_line + 1),
                    business_logic=self._extract_business_logic(lines, line_index, end_line),
                    confidence=0.8,
                    complexity_score=self._calculate_complexity_score(lines, line_index, end_line),
                    risk_level=self._assess_risk_level(lines, line_index, end_line)
                )
                subsections.append(COBOLSubsection(**paragraph_subsection.__dict__))
                
                # Then create operations within this subsection
                operations = self._create_granular_operations(
                    paragraph_lines, line_index, paragraph_name, paragraph_name
                )
                # Store operations for later addition to result
                if not hasattr(self, '_temp_operations'):
                    self._temp_operations = []
                self._temp_operations.extend(operations)
            else:
                # Create single subsection for simple paragraphs
                business_logic = self._extract_business_logic(lines, line_index, end_line)
                complexity_score = self._calculate_complexity_score(lines, line_index, end_line)
                risk_level = self._assess_risk_level(lines, line_index, end_line)
                
                subsection = self._create_subsection(
                    name=paragraph_name,
                    parent_section=parent_section,
                    line_range=(line_index + 1, end_line + 1),
                    business_logic=business_logic,
                    confidence=0.8,
                    complexity_score=complexity_score,
                    risk_level=risk_level
                )
                subsections.append(COBOLSubsection(**subsection.__dict__))
        
        # Also find section-level subsections (FILE, WORKING-STORAGE, etc.)
        section_matches = self._find_pattern_matches(lines, {
            'FILE': self.subsection_patterns['FILE'],
            'WORKING-STORAGE': self.subsection_patterns['WORKING-STORAGE'],
            'LINKAGE': self.subsection_patterns['LINKAGE'],
            'PROCEDURE_SECTION': self.subsection_patterns['PROCEDURE_SECTION']
        })
        
        for line_index, subsection_type, match_text in section_matches:
            parent_section = self._find_parent_section(sections, line_index)
            end_line = self._find_subsection_end(lines, line_index, subsection_type)
            business_logic = self._extract_business_logic(lines, line_index, end_line)
            complexity_score = self._calculate_complexity_score(lines, line_index, end_line)
            risk_level = self._assess_risk_level(lines, line_index, end_line)
            
            subsection = self._create_subsection(
                name=f"{subsection_type}-SECTION",
                parent_section=parent_section,
                line_range=(line_index + 1, end_line + 1),
                business_logic=business_logic,
                confidence=0.8,
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
        """Find the immediate parent section for a subsection (COBOL-specific)"""
        # First, try to find the most specific (immediate) parent
        immediate_parent = super()._find_parent_section(sections, line_number)
        
        # If we found a division as parent, try to find a more specific section
        if immediate_parent and any(immediate_parent.endswith(div_type) for div_type in ['IDENTIFICATION-DIVISION', 'ENVIRONMENT-DIVISION', 'DATA-DIVISION', 'PROCEDURE-DIVISION']):
            # Look for a more specific section within this division
            for section in sections:
                if (section.type not in ['IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE'] and
                    section.line_range[0] <= line_number + 1 <= section.line_range[1]):
                    return section.name
        
        return immediate_parent
    
    def _extract_business_logic(self, lines: List[str], start_line: int, end_line: int) -> str:
        """Extract business logic from a section (COBOL-specific)"""
        return super()._extract_business_logic(lines, start_line, end_line)
    
    def _count_logical_operations(self, paragraph_lines: List[str]) -> int:
        """Count the number of logical operations in a paragraph"""
        operation_count = 0
        operation_pattern = r'^\d+\s+(READ|ADD|IF|DISPLAY|MOVE|PERFORM|CALL|COMPUTE|EVALUATE|SET|OPEN|CLOSE|WRITE|DELETE|REWRITE|START|STOP|ACCEPT|INITIALIZE|STRING|UNSTRING|INSPECT|SEARCH|SORT|MERGE|RELEASE|RETURN)'
        
        for line in paragraph_lines:
            line_upper = line.upper().strip()
            if re.match(operation_pattern, line_upper):
                operation_count += 1
        
        return operation_count
    
    def _create_granular_operations(self, paragraph_lines: List[str], start_line_index: int, 
                                   paragraph_name: str, parent_subsection: str) -> List[COBOLOperation]:
        """Create granular operations for complex paragraphs"""
        operations = []
        operation_pattern = r'^\d+\s+(READ|ADD|IF|DISPLAY|MOVE|PERFORM|CALL|COMPUTE|EVALUATE|SET|OPEN|CLOSE|WRITE|DELETE|REWRITE|START|STOP|ACCEPT|INITIALIZE|STRING|UNSTRING|INSPECT|SEARCH|SORT|MERGE|RELEASE|RETURN)'
        
        operation_counter = 1
        
        for i, line in enumerate(paragraph_lines):
            line_upper = line.upper().strip()
            if not line_upper or line_upper.startswith('*'):  # Skip empty lines and comments
                continue
                
            # Check if this line contains a logical operation
            match = re.match(operation_pattern, line_upper)
            if match:
                operation_type = match.group(1)
                
                # Find the end of this operation (next operation or end of paragraph)
                end_line_in_paragraph = i
                for j in range(i + 1, len(paragraph_lines)):
                    next_line = paragraph_lines[j].upper().strip()
                    if next_line and not next_line.startswith('*'):
                        # Check if next line is another operation
                        next_match = re.match(operation_pattern, next_line)
                        if next_match:
                            end_line_in_paragraph = j - 1
                            break
                        end_line_in_paragraph = j
                
                # Create operation
                operation_name = f"{paragraph_name}-{operation_type.lower()}-{operation_counter}"
                operation_counter += 1
                
                # Calculate line range in the original file
                actual_start_line = start_line_index + i + 1
                actual_end_line = start_line_index + end_line_in_paragraph + 1
                
                # Extract business logic for this operation
                operation_lines = paragraph_lines[i:end_line_in_paragraph + 1]
                business_logic = ' '.join(operation_lines).strip()
                
                # Extract parameters from the operation
                parameters = self._extract_operation_parameters(operation_lines, operation_type)
                
                # Calculate complexity and risk
                complexity_score = self._calculate_operation_complexity(operation_lines)
                risk_level = self._assess_operation_risk(operation_lines)
                
                operation = COBOLOperation(
                    name=operation_name,
                    operation_type=operation_type,
                    parent_subsection=parent_subsection,
                    line_range=(actual_start_line, actual_end_line),
                    line_count=actual_end_line - actual_start_line + 1,
                    business_logic=business_logic,
                    confidence=0.9,  # High confidence for operation detection
                    complexity_score=complexity_score,
                    risk_level=risk_level,
                    parameters=parameters
                )
                operations.append(operation)
        
        return operations
    
    def _calculate_operation_complexity(self, operation_lines: List[str]) -> float:
        """Calculate complexity score for a specific operation"""
        # Base complexity
        complexity = 0.1
        
        # Add complexity for different operation types
        for line in operation_lines:
            line_upper = line.upper().strip()
            if re.match(r'^\s*IF\s+', line_upper):
                complexity += 0.2  # IF statements add complexity
            elif re.match(r'^\s*EVALUATE\s+', line_upper):
                complexity += 0.3  # EVALUATE statements are more complex
            elif re.match(r'^\s*PERFORM\s+', line_upper):
                complexity += 0.15  # PERFORM statements add complexity
            elif re.match(r'^\s*CALL\s+', line_upper):
                complexity += 0.1  # CALL statements add some complexity
            else:
                complexity += 0.05  # Other operations add minimal complexity
        
        return min(complexity, 1.0)  # Cap at 1.0
    
    def _assess_operation_risk(self, operation_lines: List[str]) -> str:
        """Assess risk level for a specific operation"""
        risk_indicators = {
            'HIGH': ['DELETE', 'REWRITE', 'CALL', 'STOP'],
            'MEDIUM': ['READ', 'WRITE', 'IF', 'EVALUATE', 'PERFORM'],
            'LOW': ['MOVE', 'ADD', 'DISPLAY', 'SET', 'ACCEPT']
        }
        
        for line in operation_lines:
            line_upper = line.upper().strip()
            for risk_level, keywords in risk_indicators.items():
                if any(keyword in line_upper for keyword in keywords):
                    return risk_level
        
        return 'LOW'  # Default to low risk
    
    def _extract_operation_parameters(self, operation_lines: List[str], operation_type: str) -> List[str]:
        """Extract parameters from a COBOL operation"""
        parameters = []
        
        # Join all lines and clean up
        full_operation = ' '.join(operation_lines).strip()
        
        # Extract parameters based on operation type
        if operation_type == 'READ':
            # Extract file name and options
            match = re.search(r'READ\s+(\w+)', full_operation, re.IGNORECASE)
            if match:
                parameters.append(match.group(1))
        elif operation_type == 'ADD':
            # Extract operands
            match = re.search(r'ADD\s+([^TO]+)\s+TO\s+(\w+)', full_operation, re.IGNORECASE)
            if match:
                parameters.extend([match.group(1).strip(), match.group(2)])
        elif operation_type == 'MOVE':
            # Extract source and destination
            match = re.search(r'MOVE\s+([^TO]+)\s+TO\s+(\w+)', full_operation, re.IGNORECASE)
            if match:
                parameters.extend([match.group(1).strip(), match.group(2)])
        elif operation_type == 'DISPLAY':
            # Extract display items
            match = re.search(r'DISPLAY\s+([^.]*)', full_operation, re.IGNORECASE)
            if match:
                display_items = match.group(1).strip()
                # Split by common delimiters
                items = re.split(r'[\s,]+', display_items)
                parameters.extend([item for item in items if item])
        elif operation_type == 'IF':
            # Extract condition
            match = re.search(r'IF\s+([^THEN]*)', full_operation, re.IGNORECASE)
            if match:
                parameters.append(match.group(1).strip())
        elif operation_type == 'PERFORM':
            # Extract procedure name
            match = re.search(r'PERFORM\s+(\w+)', full_operation, re.IGNORECASE)
            if match:
                parameters.append(match.group(1))
        elif operation_type == 'CALL':
            # Extract program name
            match = re.search(r'CALL\s+[\'"]?(\w+)[\'"]?', full_operation, re.IGNORECASE)
            if match:
                parameters.append(match.group(1))
        
        return parameters
    
    def _find_relationships(self, lines: List[str], sections: List[COBOLSection], subsections: List[COBOLSubsection]) -> List[BaseRelationship]:
        """Find relationships between sections and subsections"""
        return self._extract_relationships_from_patterns(lines, self.relationship_patterns)
    
    def _find_operation_relationships(self, operations: List[COBOLOperation]) -> List[BaseRelationship]:
        """Find relationships between operations (flow, dependencies, calls)"""
        relationships = []
        
        if not operations:
            return relationships
        
        # Sort operations by line range for sequential analysis
        sorted_operations = sorted(operations, key=lambda op: op.line_range[0])
        
        for i, current_op in enumerate(sorted_operations):
            # Find next operation in sequence
            if i + 1 < len(sorted_operations):
                next_op = sorted_operations[i + 1]
                
                # Check if operations are in the same subsection (sequential flow)
                if current_op.parent_subsection == next_op.parent_subsection:
                    # Create NEXT relationship
                    rel = COBOLRelationship(
                        source=current_op.name,
                        target=next_op.name,
                        relationship_type="NEXT",
                        confidence=0.9,
                        strength=1.0
                    )
                    relationships.append(rel)
            
            # Find dependent operations based on operation type and parameters
            dependencies = self._find_operation_dependencies(current_op, sorted_operations)
            for dep_op in dependencies:
                rel = COBOLRelationship(
                    source=current_op.name,
                    target=dep_op.name,
                    relationship_type="DEPENDS_ON",
                    confidence=0.8,
                    strength=0.7
                )
                relationships.append(rel)
            
            # Find call relationships
            calls = self._find_operation_calls(current_op, sorted_operations)
            for called_op in calls:
                rel = COBOLRelationship(
                    source=current_op.name,
                    target=called_op.name,
                    relationship_type="CALLS",
                    confidence=0.9,
                    strength=1.0
                )
                relationships.append(rel)
        
        return relationships
    
    def _find_operation_dependencies(self, current_op: COBOLOperation, all_operations: List[COBOLOperation]) -> List[COBOLOperation]:
        """Find operations that current operation depends on"""
        dependencies = []
        
        # Check for data dependencies based on parameters
        current_params = set(current_op.parameters)
        
        for other_op in all_operations:
            if other_op == current_op:
                continue
                
            # Skip if other operation comes after current operation
            if other_op.line_range[0] > current_op.line_range[0]:
                continue
                
            other_params = set(other_op.parameters)
            
            # Check for parameter overlap (data dependency)
            if current_params & other_params:
                dependencies.append(other_op)
            
            # Check for specific operation type dependencies
            if self._has_operation_dependency(current_op, other_op):
                dependencies.append(other_op)
        
        return dependencies
    
    def _has_operation_dependency(self, current_op: COBOLOperation, other_op: COBOLOperation) -> bool:
        """Check if current operation has a specific dependency on other operation"""
        # READ operations depend on OPEN operations
        if current_op.operation_type == "READ" and other_op.operation_type == "OPEN":
            return True
        
        # WRITE operations depend on OPEN operations
        if current_op.operation_type == "WRITE" and other_op.operation_type == "OPEN":
            return True
        
        # Operations that use data depend on operations that set/modify that data
        if (current_op.operation_type in ["ADD", "MOVE", "COMPUTE"] and 
            other_op.operation_type in ["MOVE", "INITIALIZE", "ACCEPT"]):
            return True
        
        # IF operations depend on operations that set the condition variables
        if (current_op.operation_type == "IF" and 
            other_op.operation_type in ["MOVE", "ADD", "COMPUTE", "READ"]):
            return True
        
        return False
    
    def _find_operation_calls(self, current_op: COBOLOperation, all_operations: List[COBOLOperation]) -> List[COBOLOperation]:
        """Find operations that current operation calls"""
        calls = []
        
        # Check if current operation is a PERFORM or CALL
        if current_op.operation_type in ["PERFORM", "CALL"]:
            # Extract the target name from parameters
            if current_op.parameters:
                target_name = current_op.parameters[0]
                
                # Find operations with matching names
                for other_op in all_operations:
                    if (other_op != current_op and 
                        target_name.lower() in other_op.name.lower()):
                        calls.append(other_op)
        
        return calls
