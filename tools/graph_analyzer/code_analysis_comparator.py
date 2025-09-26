#!/usr/bin/env python3
"""
Code Analysis Comparator

This tool compares a code file with two different Neo4j database analyses
to identify discrepancies and validate analysis accuracy.

Usage:
    python code_analysis_comparator.py --file code.cbl --db1 database1 --db2 database2
"""

import sys
import os
import argparse
import re
from pathlib import Path
from typing import Dict, List, Any, Tuple
from dotenv import load_dotenv
import json
from datetime import datetime

# Add the current directory to Python path
current_dir = Path(__file__).parent
sys.path.insert(0, str(current_dir))

from graph_analyzer import Neo4jGraphAnalyzer


class CodeAnalysisComparator:
    """Compare code file analysis between two databases"""
    
    def __init__(self, db1_uri: str, db1_user: str, db1_password: str, db1_database: str,
                 db2_uri: str, db2_user: str, db2_password: str, db2_database: str):
        """Initialize comparator with two database connections"""
        self.analyzer1 = Neo4jGraphAnalyzer(db1_uri, db1_user, db1_password, db1_database)
        self.analyzer2 = Neo4jGraphAnalyzer(db2_uri, db2_user, db2_password, db2_database)
        
        # Test connections
        try:
            self.analyzer1.connect()
            print(f"✅ Connected to database 1: {db1_database}")
        except Exception as e:
            print(f"❌ Failed to connect to database 1: {e}")
            
        try:
            self.analyzer2.connect()
            print(f"✅ Connected to database 2: {db2_database}")
        except Exception as e:
            print(f"❌ Failed to connect to database 2: {e}")
        
    def analyze_code_file(self, file_path: str) -> Dict[str, Any]:
        """Analyze the actual code file structure with detailed structural mapping"""
        print(f"📁 Analyzing code file: {file_path}")
        
        if not os.path.exists(file_path):
            raise FileNotFoundError(f"File not found: {file_path}")
        
        # Read file content
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        lines = content.split('\n')
        
        # Basic file analysis
        analysis = {
            'file_path': file_path,
            'file_name': Path(file_path).name,
            'total_lines': len(lines),
            'total_characters': len(content),
            'non_empty_lines': len([line for line in lines if line.strip()]),
            'file_size_kb': len(content.encode('utf-8')) / 1024,
            'analysis_timestamp': datetime.now().isoformat()
        }
        
        # Detect language
        file_ext = Path(file_path).suffix.lower()
        language_map = {
            '.cbl': 'COBOL',
            '.cob': 'COBOL', 
            '.py': 'Python',
            '.java': 'Java',
            '.js': 'JavaScript',
            '.ts': 'TypeScript'
        }
        analysis['detected_language'] = language_map.get(file_ext, 'Unknown')
        
        # Perform detailed structural analysis based on language
        if analysis['detected_language'] == 'COBOL':
            analysis['structural_analysis'] = self._analyze_cobol_structure(lines)
        elif analysis['detected_language'] == 'Python':
            analysis['structural_analysis'] = self._analyze_python_structure(lines)
        else:
            analysis['structural_analysis'] = self._analyze_generic_structure(lines)
        
        return analysis
    
    def _analyze_cobol_structure(self, lines: List[str]) -> Dict[str, Any]:
        """Analyze COBOL file structure"""
        structure = {
            'divisions': [],
            'sections': [],
            'paragraphs': [],
            'data_items': [],
            'files': [],
            'programs': [],
            'procedures': [],
            'statements': [],
            'comments': []
        }
        
        current_division = None
        current_section = None
        current_paragraph = None
        
        for i, line in enumerate(lines, 1):
            line_stripped = line.strip()
            if not line_stripped:
                continue
                
            # COBOL line number pattern (first 6 characters)
            line_content = line[6:].strip() if len(line) >= 6 else line_stripped
            
            # Detect divisions
            if re.match(r'^\s*(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION', line_content, re.IGNORECASE):
                division_name = re.match(r'^\s*(\w+)\s+DIVISION', line_content, re.IGNORECASE).group(1).upper()
                structure['divisions'].append({
                    'name': division_name,
                    'line_number': i,
                    'line_content': line_content
                })
                current_division = division_name
                current_section = None
                current_paragraph = None
            
            # Detect sections
            elif re.match(r'^\s*\w+.*\s+SECTION', line_content, re.IGNORECASE):
                section_name = line_content.split()[0].upper()
                structure['sections'].append({
                    'name': section_name,
                    'line_number': i,
                    'line_content': line_content,
                    'division': current_division
                })
                current_section = section_name
                current_paragraph = None
            
            # Detect paragraphs (COBOL paragraphs start at column 8+)
            elif re.match(r'^\s*\d+\s+\w+', line_content) and not re.match(r'^\s*\d+\s+(IF|WHEN|EVALUATE|PERFORM|MOVE|ADD|SUBTRACT|MULTIPLY|DIVIDE|COMPUTE|STRING|UNSTRING|ACCEPT|DISPLAY|OPEN|CLOSE|READ|WRITE|REWRITE|DELETE|START)', line_content, re.IGNORECASE):
                paragraph_name = line_content.split()[1].upper()
                structure['paragraphs'].append({
                    'name': paragraph_name,
                    'line_number': i,
                    'line_content': line_content,
                    'division': current_division,
                    'section': current_section
                })
                current_paragraph = paragraph_name
            
            # Detect data items (01, 05, 10, etc. level numbers)
            elif re.match(r'^\s*\d{2}\s+', line_content):
                level_number = line_content.split()[0]
                if level_number in ['01', '05', '10', '15', '20', '25', '30', '35', '40', '45', '50', '55', '60', '65', '70', '75', '80', '85', '90', '95']:
                    data_name = line_content.split()[1] if len(line_content.split()) > 1 else 'UNNAMED'
                    structure['data_items'].append({
                        'name': data_name,
                        'level': level_number,
                        'line_number': i,
                        'line_content': line_content,
                        'division': current_division,
                        'section': current_section
                    })
            
            # Detect file definitions
            elif re.match(r'^\s*FD\s+', line_content, re.IGNORECASE):
                file_name = line_content.split()[1] if len(line_content.split()) > 1 else 'UNNAMED'
                structure['files'].append({
                    'name': file_name,
                    'line_number': i,
                    'line_content': line_content,
                    'division': current_division,
                    'section': current_section
                })
            
            # Detect program ID
            elif re.match(r'^\s*PROGRAM-ID', line_content, re.IGNORECASE):
                program_name = line_content.split('.')[0].split()[-1] if '.' in line_content else 'UNNAMED'
                structure['programs'].append({
                    'name': program_name,
                    'line_number': i,
                    'line_content': line_content,
                    'division': current_division
                })
            
            # Detect procedure statements
            elif current_division == 'PROCEDURE' and line_content:
                statement_type = self._classify_cobol_statement(line_content)
                structure['statements'].append({
                    'type': statement_type,
                    'line_number': i,
                    'line_content': line_content,
                    'paragraph': current_paragraph,
                    'section': current_section
                })
            
            # Detect comments
            elif line_content.startswith('*') or line_content.startswith('/'):
                structure['comments'].append({
                    'line_number': i,
                    'line_content': line_content
                })
        
        # Calculate structural metrics
        structure['metrics'] = {
            'total_divisions': len(structure['divisions']),
            'total_sections': len(structure['sections']),
            'total_paragraphs': len(structure['paragraphs']),
            'total_data_items': len(structure['data_items']),
            'total_files': len(structure['files']),
            'total_programs': len(structure['programs']),
            'total_statements': len(structure['statements']),
            'total_comments': len(structure['comments']),
            'avg_paragraph_length': self._calculate_avg_paragraph_length(structure['paragraphs'], lines),
            'data_item_levels': self._get_data_item_level_distribution(structure['data_items'])
        }
        
        return structure
    
    def _classify_cobol_statement(self, line_content: str) -> str:
        """Classify COBOL statement type"""
        line_upper = line_content.upper()
        
        if re.match(r'^\s*(IF|WHEN|EVALUATE)', line_upper):
            return 'CONDITIONAL'
        elif re.match(r'^\s*(PERFORM)', line_upper):
            return 'PERFORM'
        elif re.match(r'^\s*(MOVE|ADD|SUBTRACT|MULTIPLY|DIVIDE|COMPUTE)', line_upper):
            return 'ARITHMETIC'
        elif re.match(r'^\s*(STRING|UNSTRING)', line_upper):
            return 'STRING_MANIPULATION'
        elif re.match(r'^\s*(ACCEPT|DISPLAY)', line_upper):
            return 'I_O'
        elif re.match(r'^\s*(OPEN|CLOSE)', line_upper):
            return 'FILE_CONTROL'
        elif re.match(r'^\s*(READ|WRITE|REWRITE|DELETE|START)', line_upper):
            return 'FILE_ACCESS'
        elif re.match(r'^\s*(INITIALIZE|SET)', line_upper):
            return 'INITIALIZATION'
        elif re.match(r'^\s*(STOP|EXIT)', line_upper):
            return 'CONTROL_FLOW'
        else:
            return 'OTHER'
    
    def _calculate_avg_paragraph_length(self, paragraphs: List[Dict], lines: List[str]) -> float:
        """Calculate average paragraph length"""
        if not paragraphs:
            return 0.0
        
        total_length = 0
        for i, para in enumerate(paragraphs):
            start_line = para['line_number']
            end_line = paragraphs[i + 1]['line_number'] - 1 if i + 1 < len(paragraphs) else len(lines)
            total_length += end_line - start_line + 1
        
        return total_length / len(paragraphs)
    
    def _get_data_item_level_distribution(self, data_items: List[Dict]) -> Dict[str, int]:
        """Get distribution of data item levels"""
        level_dist = {}
        for item in data_items:
            level = item['level']
            level_dist[level] = level_dist.get(level, 0) + 1
        return level_dist
    
    def _analyze_python_structure(self, lines: List[str]) -> Dict[str, Any]:
        """Analyze Python file structure"""
        structure = {
            'classes': [],
            'functions': [],
            'imports': [],
            'variables': [],
            'comments': []
        }
        
        for i, line in enumerate(lines, 1):
            line_stripped = line.strip()
            if not line_stripped or line_stripped.startswith('#'):
                if line_stripped.startswith('#'):
                    structure['comments'].append({
                        'line_number': i,
                        'line_content': line_stripped
                    })
                continue
            
            # Detect imports
            if line_stripped.startswith(('import ', 'from ')):
                structure['imports'].append({
                    'line_number': i,
                    'line_content': line_stripped
                })
            
            # Detect class definitions
            elif line_stripped.startswith('class '):
                class_name = line_stripped.split()[1].split('(')[0]
                structure['classes'].append({
                    'name': class_name,
                    'line_number': i,
                    'line_content': line_stripped
                })
            
            # Detect function definitions
            elif line_stripped.startswith('def '):
                func_name = line_stripped.split()[1].split('(')[0]
                structure['functions'].append({
                    'name': func_name,
                    'line_number': i,
                    'line_content': line_stripped
                })
        
        structure['metrics'] = {
            'total_classes': len(structure['classes']),
            'total_functions': len(structure['functions']),
            'total_imports': len(structure['imports']),
            'total_comments': len(structure['comments'])
        }
        
        return structure
    
    def _analyze_generic_structure(self, lines: List[str]) -> Dict[str, Any]:
        """Analyze generic file structure"""
        structure = {
            'functions': [],
            'classes': [],
            'comments': []
        }
        
        for i, line in enumerate(lines, 1):
            line_stripped = line.strip()
            if not line_stripped:
                continue
            
            # Detect comments
            if line_stripped.startswith(('//', '/*', '*', '#')):
                structure['comments'].append({
                    'line_number': i,
                    'line_content': line_stripped
                })
        
        structure['metrics'] = {
            'total_comments': len(structure['comments'])
        }
        
        return structure
    
    def get_database_analysis(self, analyzer: Neo4jGraphAnalyzer, db_name: str) -> Dict[str, Any]:
        """Get comprehensive analysis from a database"""
        print(f"🔍 Analyzing database: {db_name}")
        
        try:
            # Get basic database info
            db_info = analyzer.get_database_info()
            
            # Get hierarchical analysis
            hierarchical = analyzer.analyze_program_structure(max_depth=4)
            
            # Get node type distribution
            node_types = {}
            for node_info in db_info.get('node_types', []):
                labels = ', '.join(node_info['labels'])
                node_types[labels] = {
                    'count': node_info['count'],
                    'min_lines': node_info.get('min_lines', 0),
                    'max_lines': node_info.get('max_lines', 0),
                    'avg_lines': node_info.get('avg_lines', 0)
                }
            
            return {
                'database_name': db_name,
                'total_nodes': db_info.get('total_nodes', {}).get('count', 0),
                'total_relationships': db_info.get('total_relationships', {}).get('count', 0),
                'total_lines': db_info.get('total_lines', {}).get('count', 0),
                'node_types': node_types,
                'hierarchical_analysis': hierarchical,
                'analysis_timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            print(f"❌ Error analyzing database {db_name}: {e}")
            return {
                'database_name': db_name,
                'error': str(e),
                'analysis_timestamp': datetime.now().isoformat()
            }
    
    def compare_analyses(self, file_analysis: Dict[str, Any], 
                        db1_analysis: Dict[str, Any], 
                        db2_analysis: Dict[str, Any]) -> Dict[str, Any]:
        """Compare the three analyses and identify discrepancies using file structure as reference"""
        print("🔍 Comparing analyses...")
        
        comparison = {
            'file_analysis': file_analysis,
            'database1_analysis': db1_analysis,
            'database2_analysis': db2_analysis,
            'discrepancies': [],
            'structural_comparison': {},
            'summary': {},
            'comparison_timestamp': datetime.now().isoformat()
        }
        
        # Compare basic metrics
        discrepancies = []
        
        # Line count comparison
        file_lines = file_analysis['total_lines']
        db1_lines = db1_analysis.get('total_lines', 0)
        db2_lines = db2_analysis.get('total_lines', 0)
        
        if db1_lines != file_lines:
            discrepancies.append({
                'type': 'line_count_mismatch',
                'description': f'Database 1 line count ({db1_lines}) differs from file ({file_lines})',
                'severity': 'high' if abs(db1_lines - file_lines) > file_lines * 0.1 else 'medium',
                'file_value': file_lines,
                'db1_value': db1_lines,
                'db2_value': db2_lines
            })
        
        if db2_lines != file_lines:
            discrepancies.append({
                'type': 'line_count_mismatch',
                'description': f'Database 2 line count ({db2_lines}) differs from file ({file_lines})',
                'severity': 'high' if abs(db2_lines - file_lines) > file_lines * 0.1 else 'medium',
                'file_value': file_lines,
                'db1_value': db1_lines,
                'db2_value': db2_lines
            })
        
        # Structural comparison using file analysis as reference
        if 'structural_analysis' in file_analysis:
            structural_comparison = self._compare_structural_elements(
                file_analysis['structural_analysis'], 
                db1_analysis, 
                db2_analysis
            )
            comparison['structural_comparison'] = structural_comparison
            discrepancies.extend(structural_comparison.get('discrepancies', []))
        
        # Node count comparison
        db1_nodes = db1_analysis.get('total_nodes', 0)
        db2_nodes = db2_analysis.get('total_nodes', 0)
        
        if db1_nodes != db2_nodes:
            discrepancies.append({
                'type': 'node_count_difference',
                'description': f'Database 1 has {db1_nodes} nodes, Database 2 has {db2_nodes} nodes',
                'severity': 'medium',
                'db1_value': db1_nodes,
                'db2_value': db2_nodes
            })
        
        # Relationship count comparison
        db1_rels = db1_analysis.get('total_relationships', 0)
        db2_rels = db2_analysis.get('total_relationships', 0)
        
        if db1_rels != db2_rels:
            discrepancies.append({
                'type': 'relationship_count_difference',
                'description': f'Database 1 has {db1_rels} relationships, Database 2 has {db2_rels} relationships',
                'severity': 'medium',
                'db1_value': db1_rels,
                'db2_value': db2_rels
            })
        
        # Node type comparison
        db1_types = set(db1_analysis.get('node_types', {}).keys())
        db2_types = set(db2_analysis.get('node_types', {}).keys())
        
        if db1_types != db2_types:
            only_in_db1 = db1_types - db2_types
            only_in_db2 = db2_types - db1_types
            
            if only_in_db1:
                discrepancies.append({
                    'type': 'node_type_difference',
                    'description': f'Node types only in Database 1: {list(only_in_db1)}',
                    'severity': 'low',
                    'db1_types': list(only_in_db1),
                    'db2_types': []
                })
            
            if only_in_db2:
                discrepancies.append({
                    'type': 'node_type_difference',
                    'description': f'Node types only in Database 2: {list(only_in_db2)}',
                    'severity': 'low',
                    'db1_types': [],
                    'db2_types': list(only_in_db2)
                })
        
        # Hierarchical structure comparison
        db1_hier = db1_analysis.get('hierarchical_analysis', {}).get('level_analysis', {})
        db2_hier = db2_analysis.get('hierarchical_analysis', {}).get('level_analysis', {})
        
        for level in range(5):  # Compare levels 0-4
            level1_data = db1_hier.get(level, [])
            level2_data = db2_hier.get(level, [])
            
            if level1_data and level2_data:
                level1_count = sum(item.get('count', 0) for item in level1_data)
                level2_count = sum(item.get('count', 0) for item in level2_data)
                
                if level1_count != level2_count:
                    discrepancies.append({
                        'type': 'hierarchical_difference',
                        'description': f'Level {level}: Database 1 has {level1_count} nodes, Database 2 has {level2_count} nodes',
                        'severity': 'medium',
                        'level': level,
                        'db1_count': level1_count,
                        'db2_count': level2_count
                    })
        
        comparison['discrepancies'] = discrepancies
        
        # Generate summary
        comparison['summary'] = {
            'total_discrepancies': len(discrepancies),
            'high_severity': len([d for d in discrepancies if d.get('severity') == 'high']),
            'medium_severity': len([d for d in discrepancies if d.get('severity') == 'medium']),
            'low_severity': len([d for d in discrepancies if d.get('severity') == 'low']),
            'file_lines': file_lines,
            'db1_lines': db1_lines,
            'db2_lines': db2_lines,
            'db1_nodes': db1_nodes,
            'db2_nodes': db2_nodes,
            'db1_relationships': db1_rels,
            'db2_relationships': db2_rels
        }
        
        return comparison
    
    def _compare_structural_elements(self, file_structure: Dict[str, Any], 
                                   db1_analysis: Dict[str, Any], 
                                   db2_analysis: Dict[str, Any]) -> Dict[str, Any]:
        """Compare structural elements between file analysis and database analyses"""
        comparison = {
            'file_structure': file_structure,
            'db1_mapping': {},
            'db2_mapping': {},
            'discrepancies': []
        }
        
        # Map file structure elements to database node types
        file_metrics = file_structure.get('metrics', {})
        
        # COBOL-specific structural comparison
        if 'total_paragraphs' in file_metrics:
            # Map paragraphs to database nodes
            file_paragraphs = file_metrics.get('total_paragraphs', 0)
            file_data_items = file_metrics.get('total_data_items', 0)
            file_files = file_metrics.get('total_files', 0)
            file_programs = file_metrics.get('total_programs', 0)
            
            # Try to map to database node types
            db1_types = db1_analysis.get('node_types', {})
            db2_types = db2_analysis.get('node_types', {})
            
            # Map paragraphs
            db1_paragraphs = self._estimate_paragraph_count(db1_types)
            db2_paragraphs = self._estimate_paragraph_count(db2_types)
            
            if abs(db1_paragraphs - file_paragraphs) > file_paragraphs * 0.2:
                comparison['discrepancies'].append({
                    'type': 'paragraph_count_mismatch',
                    'description': f'Database 1 paragraph count ({db1_paragraphs}) differs significantly from file ({file_paragraphs})',
                    'severity': 'high' if abs(db1_paragraphs - file_paragraphs) > file_paragraphs * 0.5 else 'medium',
                    'file_value': file_paragraphs,
                    'db1_value': db1_paragraphs,
                    'db2_value': db2_paragraphs
                })
            
            if abs(db2_paragraphs - file_paragraphs) > file_paragraphs * 0.2:
                comparison['discrepancies'].append({
                    'type': 'paragraph_count_mismatch',
                    'description': f'Database 2 paragraph count ({db2_paragraphs}) differs significantly from file ({file_paragraphs})',
                    'severity': 'high' if abs(db2_paragraphs - file_paragraphs) > file_paragraphs * 0.5 else 'medium',
                    'file_value': file_paragraphs,
                    'db1_value': db1_paragraphs,
                    'db2_value': db2_paragraphs
                })
            
            # Map data items
            db1_data_items = self._estimate_data_item_count(db1_types)
            db2_data_items = self._estimate_data_item_count(db2_types)
            
            if abs(db1_data_items - file_data_items) > file_data_items * 0.2:
                comparison['discrepancies'].append({
                    'type': 'data_item_count_mismatch',
                    'description': f'Database 1 data item count ({db1_data_items}) differs significantly from file ({file_data_items})',
                    'severity': 'high' if abs(db1_data_items - file_data_items) > file_data_items * 0.5 else 'medium',
                    'file_value': file_data_items,
                    'db1_value': db1_data_items,
                    'db2_value': db2_data_items
                })
            
            if abs(db2_data_items - file_data_items) > file_data_items * 0.2:
                comparison['discrepancies'].append({
                    'type': 'data_item_count_mismatch',
                    'description': f'Database 2 data item count ({db2_data_items}) differs significantly from file ({file_data_items})',
                    'severity': 'high' if abs(db2_data_items - file_data_items) > file_data_items * 0.5 else 'medium',
                    'file_value': file_data_items,
                    'db1_value': db1_data_items,
                    'db2_value': db2_data_items
                })
            
            comparison['db1_mapping'] = {
                'estimated_paragraphs': db1_paragraphs,
                'estimated_data_items': db1_data_items,
                'estimated_files': self._estimate_file_count(db1_types),
                'estimated_programs': self._estimate_program_count(db1_types)
            }
            
            comparison['db2_mapping'] = {
                'estimated_paragraphs': db2_paragraphs,
                'estimated_data_items': db2_data_items,
                'estimated_files': self._estimate_file_count(db2_types),
                'estimated_programs': self._estimate_program_count(db2_types)
            }
        
        return comparison
    
    def _estimate_paragraph_count(self, node_types: Dict[str, Any]) -> int:
        """Estimate paragraph count from database node types"""
        # Map common node type names to paragraph equivalents
        paragraph_mappings = {
            'paragraph': 0,
            'Subsection': 0,
            'Operation': 0,
            'BusinessRule': 0
        }
        
        for node_type, info in node_types.items():
            if node_type.lower() in paragraph_mappings:
                paragraph_mappings[node_type.lower()] = info.get('count', 0)
        
        # Return the highest count that might represent paragraphs
        return max(paragraph_mappings.values())
    
    def _estimate_data_item_count(self, node_types: Dict[str, Any]) -> int:
        """Estimate data item count from database node types"""
        data_item_mappings = {
            'data_item': 0,
            'data': 0,
            'variable': 0
        }
        
        for node_type, info in node_types.items():
            if node_type.lower() in data_item_mappings:
                data_item_mappings[node_type.lower()] = info.get('count', 0)
        
        return max(data_item_mappings.values())
    
    def _estimate_file_count(self, node_types: Dict[str, Any]) -> int:
        """Estimate file count from database node types"""
        file_mappings = {
            'file': 0,
            'File': 0
        }
        
        for node_type, info in node_types.items():
            if node_type.lower() in file_mappings:
                file_mappings[node_type.lower()] = info.get('count', 0)
        
        return max(file_mappings.values())
    
    def _estimate_program_count(self, node_types: Dict[str, Any]) -> int:
        """Estimate program count from database node types"""
        program_mappings = {
            'program': 0,
            'Program': 0,
            'compilation_unit': 0
        }
        
        for node_type, info in node_types.items():
            if node_type.lower() in program_mappings:
                program_mappings[node_type.lower()] = info.get('count', 0)
        
        return max(program_mappings.values())
    
    def print_comparison_report(self, comparison: Dict[str, Any]):
        """Print a detailed comparison report with structured tables"""
        print("\n" + "="*80)
        print("📊 CODE ANALYSIS COMPARISON REPORT")
        print("="*80)
        
        # File analysis
        file_analysis = comparison['file_analysis']
        print(f"\n📁 FILE ANALYSIS:")
        print(f"   File: {file_analysis['file_name']}")
        print(f"   Language: {file_analysis['detected_language']}")
        print(f"   Lines: {file_analysis['total_lines']}")
        print(f"   Characters: {file_analysis['total_characters']}")
        print(f"   Size: {file_analysis['file_size_kb']:.1f} KB")
        
        # Print structured tables
        self._print_basic_metrics_table(comparison)
        self._print_node_types_table(comparison)
        self._print_hierarchical_structure_table(comparison)
        self._print_relationships_table(comparison)
        self._print_structural_mapping_table(comparison)
        self._print_discrepancies_summary(comparison)
        
        print("\n" + "="*80)
    
    def _print_basic_metrics_table(self, comparison: Dict[str, Any]):
        """Print basic metrics comparison table"""
        print(f"\n📊 BASIC METRICS COMPARISON")
        print("-" * 60)
        print(f"{'Metric':<25} {'Reference':<12} {'DB1':<12} {'DB2':<12}")
        print("-" * 60)
        
        file_analysis = comparison['file_analysis']
        db1 = comparison['database1_analysis']
        db2 = comparison['database2_analysis']
        
        # Basic file metrics
        print(f"{'Total Lines':<25} {file_analysis['total_lines']:<12} {db1.get('total_lines', 0):<12} {db2.get('total_lines', 0):<12}")
        print(f"{'Total Characters':<25} {file_analysis['total_characters']:<12} {'-':<12} {'-':<12}")
        print(f"{'File Size (KB)':<25} {file_analysis['file_size_kb']:<12.1f} {'-':<12} {'-':<12}")
        
        # Database metrics
        print(f"{'Total Nodes':<25} {'-':<12} {db1.get('total_nodes', 0):<12} {db2.get('total_nodes', 0):<12}")
        print(f"{'Total Relationships':<25} {'-':<12} {db1.get('total_relationships', 0):<12} {db2.get('total_relationships', 0):<12}")
        print(f"{'Node Types Count':<25} {'-':<12} {len(db1.get('node_types', {})):<12} {len(db2.get('node_types', {})):<12}")
        
        # COBOL structural metrics from file (primary reference)
        if 'structural_analysis' in file_analysis:
            struct = file_analysis['structural_analysis']
            metrics = struct.get('metrics', {})
            if 'total_divisions' in metrics:
                print(f"{'COBOL STRUCTURE':<25} {'Reference':<12} {'DB1':<12} {'DB2':<12}")
                print("-" * 60)
                print(f"{'Divisions':<25} {metrics.get('total_divisions', 0):<12} {'-':<12} {'-':<12}")
                print(f"{'Sections':<25} {metrics.get('total_sections', 0):<12} {'-':<12} {'-':<12}")
                print(f"{'Paragraphs':<25} {metrics.get('total_paragraphs', 0):<12} {'-':<12} {'-':<12}")
                print(f"{'Data Items':<25} {metrics.get('total_data_items', 0):<12} {'-':<12} {'-':<12}")
                print(f"{'Files':<25} {metrics.get('total_files', 0):<12} {'-':<12} {'-':<12}")
                print(f"{'Programs':<25} {metrics.get('total_programs', 0):<12} {'-':<12} {'-':<12}")
                print(f"{'Statements':<25} {metrics.get('total_statements', 0):<12} {'-':<12} {'-':<12}")
                print(f"{'Comments':<25} {metrics.get('total_comments', 0):<12} {'-':<12} {'-':<12}")
    
    def _print_node_types_table(self, comparison: Dict[str, Any]):
        """Print node types comparison table"""
        print(f"\n🏷️  NODE TYPES COMPARISON")
        print("-" * 80)
        
        db1 = comparison['database1_analysis']
        db2 = comparison['database2_analysis']
        
        # Get all unique node types
        db1_types = set(db1.get('node_types', {}).keys())
        db2_types = set(db2.get('node_types', {}).keys())
        all_types = db1_types.union(db2_types)
        
        if all_types:
            print(f"{'Node Type':<25} {'DB1 Count':<15} {'DB2 Count':<15} {'Status':<20}")
            print("-" * 80)
            
            for node_type in sorted(all_types):
                db1_count = db1.get('node_types', {}).get(node_type, {}).get('count', 0)
                db2_count = db2.get('node_types', {}).get(node_type, {}).get('count', 0)
                
                if node_type in db1_types and node_type in db2_types:
                    status = "Both"
                elif node_type in db1_types:
                    status = "DB1 Only"
                else:
                    status = "DB2 Only"
                
                print(f"{node_type:<25} {db1_count:<15} {db2_count:<15} {status:<20}")
        else:
            print("No node types found in either database.")
    
    def _print_hierarchical_structure_table(self, comparison: Dict[str, Any]):
        """Print hierarchical structure comparison table"""
        print(f"\n🌳 HIERARCHICAL STRUCTURE COMPARISON")
        print("-" * 60)
        print(f"{'Level':<8} {'DB1 Count':<15} {'DB2 Count':<15} {'Difference':<15}")
        print("-" * 60)
        
        db1 = comparison['database1_analysis']
        db2 = comparison['database2_analysis']
        
        db1_hier = db1.get('hierarchical_analysis', {}).get('level_analysis', {})
        db2_hier = db2.get('hierarchical_analysis', {}).get('level_analysis', {})
        
        for level in range(5):  # Levels 0-4
            level1_data = db1_hier.get(level, [])
            level2_data = db2_hier.get(level, [])
            
            level1_count = sum(item.get('count', 0) for item in level1_data) if level1_data else 0
            level2_count = sum(item.get('count', 0) for item in level2_data) if level2_data else 0
            difference = level1_count - level2_count
            
            print(f"{level:<8} {level1_count:<15} {level2_count:<15} {difference:<15}")
    
    def _print_relationships_table(self, comparison: Dict[str, Any]):
        """Print relationships comparison table"""
        print(f"\n🔗 RELATIONSHIPS COMPARISON")
        print("-" * 80)
        
        db1 = comparison['database1_analysis']
        db2 = comparison['database2_analysis']
        
        # Get relationship types from hierarchical analysis
        db1_rels = db1.get('hierarchical_analysis', {}).get('database_info', {}).get('relationship_types', [])
        db2_rels = db2.get('hierarchical_analysis', {}).get('database_info', {}).get('relationship_types', [])
        
        # Create relationship type mapping
        db1_rel_map = {rel['type']: rel['count'] for rel in db1_rels}
        db2_rel_map = {rel['type']: rel['count'] for rel in db2_rels}
        
        all_rel_types = set(db1_rel_map.keys()).union(set(db2_rel_map.keys()))
        
        if all_rel_types:
            print(f"{'Relationship Type':<25} {'DB1 Count':<15} {'DB2 Count':<15} {'Status':<20}")
            print("-" * 80)
            
            for rel_type in sorted(all_rel_types):
                db1_count = db1_rel_map.get(rel_type, 0)
                db2_count = db2_rel_map.get(rel_type, 0)
                
                if rel_type in db1_rel_map and rel_type in db2_rel_map:
                    status = "Both"
                elif rel_type in db1_rel_map:
                    status = "DB1 Only"
                else:
                    status = "DB2 Only"
                
                print(f"{rel_type:<25} {db1_count:<15} {db2_count:<15} {status:<20}")
        else:
            print("No relationship types found in either database.")
    
    def _print_structural_mapping_table(self, comparison: Dict[str, Any]):
        """Print structural mapping comparison table using COBOL structure as reference"""
        if 'structural_comparison' not in comparison or not comparison['structural_comparison']:
            return
            
        print(f"\n🗺️  COBOL STRUCTURAL MAPPING COMPARISON")
        print("-" * 60)
        print(f"{'COBOL Element':<20} {'Reference':<12} {'DB1 Est.':<12} {'DB2 Est.':<12}")
        print("-" * 60)
        
        struct_comp = comparison['structural_comparison']
        file_metrics = struct_comp.get('file_structure', {}).get('metrics', {})
        db1_map = struct_comp.get('db1_mapping', {})
        db2_map = struct_comp.get('db2_mapping', {})
        
        if 'total_divisions' in file_metrics:
            # COBOL structural elements as primary reference
            print(f"{'Divisions':<20} {file_metrics.get('total_divisions', 0):<12} {'-':<12} {'-':<12}")
            print(f"{'Sections':<20} {file_metrics.get('total_sections', 0):<12} {'-':<12} {'-':<12}")
            print(f"{'Paragraphs':<20} {file_metrics.get('total_paragraphs', 0):<12} {db1_map.get('estimated_paragraphs', 0):<12} {db2_map.get('estimated_paragraphs', 0):<12}")
            print(f"{'Data Items':<20} {file_metrics.get('total_data_items', 0):<12} {db1_map.get('estimated_data_items', 0):<12} {db2_map.get('estimated_data_items', 0):<12}")
            print(f"{'Files':<20} {file_metrics.get('total_files', 0):<12} {db1_map.get('estimated_files', 0):<12} {db2_map.get('estimated_files', 0):<12}")
            print(f"{'Programs':<20} {file_metrics.get('total_programs', 0):<12} {db1_map.get('estimated_programs', 0):<12} {db2_map.get('estimated_programs', 0):<12}")
            print(f"{'Statements':<20} {file_metrics.get('total_statements', 0):<12} {'-':<12} {'-':<12}")
            print(f"{'Comments':<20} {file_metrics.get('total_comments', 0):<12} {'-':<12} {'-':<12}")
    
    def _print_discrepancies_summary(self, comparison: Dict[str, Any]):
        """Print discrepancies summary"""
        summary = comparison['summary']
        print(f"\n⚠️  DISCREPANCY SUMMARY")
        print("-" * 40)
        print(f"Total Discrepancies: {summary['total_discrepancies']}")
        print(f"High Severity: {summary['high_severity']}")
        print(f"Medium Severity: {summary['medium_severity']}")
        print(f"Low Severity: {summary['low_severity']}")
        
        if comparison['discrepancies']:
            print(f"\n📋 DETAILED DISCREPANCIES:")
            for i, disc in enumerate(comparison['discrepancies'], 1):
                severity_icon = "🔴" if disc['severity'] == 'high' else "🟡" if disc['severity'] == 'medium' else "🟢"
                print(f"   {i}. {severity_icon} {disc['description']}")
        else:
            print(f"\n✅ No discrepancies found!")
    
    def export_comparison(self, comparison: Dict[str, Any], output_file: str):
        """Export comparison results to JSON file"""
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(comparison, f, indent=2, ensure_ascii=False)
        print(f"📄 Comparison exported to: {output_file}")


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="Compare code file analysis between two Neo4j databases",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python code_analysis_comparator.py --file code.cbl --db1 db1 --db2 db2
  python code_analysis_comparator.py --file code.cbl --db1 db1 --db2 db2 --export comparison.json
        """
    )
    
    # Load environment variables
    load_dotenv()
    
    # Database 1 arguments
    parser.add_argument('--db1', required=True, help='First database name')
    parser.add_argument('--db1-uri', default=os.getenv('NEO4J_URI', 'bolt://localhost:7687'),
                       help='Database 1 URI (default from .env)')
    parser.add_argument('--db1-user', default=os.getenv('NEO4J_USERNAME', 'neo4j'),
                       help='Database 1 username (default from .env)')
    parser.add_argument('--db1-password', default=os.getenv('NEO4J_PASSWORD', 'password'),
                       help='Database 1 password (default from .env)')
    
    # Database 2 arguments
    parser.add_argument('--db2', required=True, help='Second database name')
    parser.add_argument('--db2-uri', default=os.getenv('NEO4J_URI', 'bolt://localhost:7687'),
                       help='Database 2 URI (default from .env)')
    parser.add_argument('--db2-user', default=os.getenv('NEO4J_USERNAME', 'neo4j'),
                       help='Database 2 username (default from .env)')
    parser.add_argument('--db2-password', default=os.getenv('NEO4J_PASSWORD', 'password'),
                       help='Database 2 password (default from .env)')
    
    # File and output arguments
    parser.add_argument('--file', required=True, help='Code file to analyze')
    parser.add_argument('--export', help='Export comparison to JSON file')
    
    args = parser.parse_args()
    
    try:
        # Initialize comparator
        comparator = CodeAnalysisComparator(
            args.db1_uri, args.db1_user, args.db1_password, args.db1,
            args.db2_uri, args.db2_user, args.db2_password, args.db2
        )
        
        # Analyze code file
        file_analysis = comparator.analyze_code_file(args.file)
        
        # Analyze both databases
        db1_analysis = comparator.get_database_analysis(comparator.analyzer1, args.db1)
        db2_analysis = comparator.get_database_analysis(comparator.analyzer2, args.db2)
        
        # Compare analyses
        comparison = comparator.compare_analyses(file_analysis, db1_analysis, db2_analysis)
        
        # Print report
        comparator.print_comparison_report(comparison)
        
        # Export if requested
        if args.export:
            comparator.export_comparison(comparison, args.export)
        
    except Exception as e:
        print(f"❌ Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
