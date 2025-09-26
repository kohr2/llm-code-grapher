"""
COBOL Reference Structure Analyzer

Provides COBOL-specific structure analysis functionality.
"""

import re
from typing import Dict, List, Any


def analyze_cobol_structure_reference(code_content: str) -> Dict[str, Any]:
    """Analyze COBOL file structure using reference analysis"""
    lines = code_content.split('\n')
    
    structure = {
        'divisions': [],
        'sections': [],
        'paragraphs': [],
        'data_items': [],
        'files': [],
        'programs': [],
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
            statement_type = classify_cobol_statement(line_content)
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
        'total_comments': len(structure['comments'])
    }
    
    return structure


def classify_cobol_statement(line_content: str) -> str:
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
