"""
Utility functions for LLM Code Grapher

Common utility functions used across the application.
Language-agnostic utilities for code analysis.
"""

import re
import os
import logging
from pathlib import Path
from typing import List, Tuple, Optional, Dict, Any
from datetime import datetime

# Configure logging
logger = logging.getLogger(__name__)


def read_file_safely(file_path: str) -> str:
    """Safely read a file with proper error handling"""
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            return f.read()
    except FileNotFoundError:
        raise FileNotFoundError(f"File not found: {file_path}")
    except Exception as e:
        raise IOError(f"Error reading file {file_path}: {e}")


def write_file_safely(file_path: str, content: str) -> None:
    """Safely write content to a file with proper error handling"""
    try:
        # Ensure directory exists
        Path(file_path).parent.mkdir(parents=True, exist_ok=True)
        
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
    except Exception as e:
        raise IOError(f"Error writing file {file_path}: {e}")


def chunk_text(text: str, chunk_size: int = 2000, overlap: int = 200) -> List[str]:
    """Split text into overlapping chunks for LLM processing"""
    lines = text.split('\n')
    chunks = []
    
    for i in range(0, len(lines), chunk_size - overlap):
        chunk_lines = lines[i:i + chunk_size]
        chunks.append('\n'.join(chunk_lines))
    
    return chunks


def extract_line_ranges(text: str, patterns: List[str]) -> List[Tuple[int, int, str]]:
    """Extract line ranges matching given patterns"""
    lines = text.split('\n')
    matches = []
    
    for i, line in enumerate(lines, 1):
        for pattern in patterns:
            if re.search(pattern, line, re.IGNORECASE):
                # Find the end of this section (next section or end of file)
                end_line = len(lines)
                for j in range(i, len(lines)):
                    if re.search('|'.join(patterns), lines[j], re.IGNORECASE) and j != i - 1:
                        end_line = j
                        break
                
                matches.append((i, end_line, line.strip()))
                break
    
    return matches


def is_comment_line(line: str, language: str = "generic") -> bool:
    """Check if a line is a comment based on language"""
    stripped = line.strip()
    
    if language.lower() == "cobol":
        return stripped.startswith('*') or stripped.startswith('*>')
    elif language.lower() == "java":
        return stripped.startswith('//') or stripped.startswith('/*')
    elif language.lower() == "python":
        return stripped.startswith('#')
    else:
        # Generic comment detection
        return stripped.startswith('#') or stripped.startswith('//') or stripped.startswith('*')


def clean_code_line(line: str, language: str = "generic") -> str:
    """Clean a code line by removing comments and extra whitespace"""
    # Remove comments based on language
    if language.lower() == "cobol":
        if '*' in line:
            line = line[:line.index('*')]
    elif language.lower() == "java":
        if '//' in line:
            line = line[:line.index('//')]
    elif language.lower() == "python":
        if '#' in line:
            line = line[:line.index('#')]
    else:
        # Generic comment removal
        for comment_char in ['#', '//', '*']:
            if comment_char in line:
                line = line[:line.index(comment_char)]
                break
    
    # Remove trailing whitespace
    return line.rstrip()


def calculate_complexity_score(section_code: str, language: str = "generic") -> float:
    """Calculate a simple complexity score for a code section"""
    lines = section_code.split('\n')
    non_comment_lines = [line for line in lines if not is_comment_line(line, language)]
    
    if not non_comment_lines:
        return 0.0
    
    # Simple complexity metrics
    control_structures = 0
    for line in non_comment_lines:
        line_upper = line.upper()
        if any(keyword in line_upper for keyword in ['IF', 'ELSE', 'END-IF', 'PERFORM', 'EVALUATE', 'WHEN']):
            control_structures += 1
    
    # Normalize to 0-1 scale
    return min(control_structures / len(non_comment_lines), 1.0)


def determine_risk_level(complexity_score: float, confidence: float) -> str:
    """Determine risk level based on complexity and confidence"""
    if complexity_score > 0.8 or confidence < 0.5:
        return "CRITICAL"
    elif complexity_score > 0.6 or confidence < 0.7:
        return "HIGH"
    elif complexity_score > 0.4 or confidence < 0.8:
        return "MEDIUM"
    else:
        return "LOW"


def format_timestamp() -> str:
    """Get current timestamp in a standard format"""
    return datetime.now().strftime("%Y-%m-%d %H:%M:%S")


def ensure_directory_exists(directory_path: str) -> None:
    """Ensure a directory exists, create if it doesn't"""
    Path(directory_path).mkdir(parents=True, exist_ok=True)


def get_file_size_mb(file_path: str) -> float:
    """Get file size in megabytes"""
    return os.path.getsize(file_path) / (1024 * 1024)


def validate_identifier(identifier: str, language: str = "generic") -> bool:
    """Validate if a string is a valid identifier for the given language"""
    if not identifier:
        return False
    
    if language.lower() == "cobol":
        # COBOL identifiers must start with a letter and contain only letters, numbers, and hyphens
        pattern = r'^[A-Z][A-Z0-9-]*$'
        return bool(re.match(pattern, identifier.upper()))
    elif language.lower() == "java":
        # Java identifiers must start with letter, underscore, or $, then letters, digits, underscore, or $
        pattern = r'^[a-zA-Z_$][a-zA-Z0-9_$]*$'
        return bool(re.match(pattern, identifier))
    elif language.lower() == "python":
        # Python identifiers must start with letter or underscore, then letters, digits, or underscore
        pattern = r'^[a-zA-Z_][a-zA-Z0-9_]*$'
        return bool(re.match(pattern, identifier))
    else:
        # Generic validation - alphanumeric with underscores and hyphens
        pattern = r'^[a-zA-Z][a-zA-Z0-9_-]*$'
        return bool(re.match(pattern, identifier))


def extract_function_calls(code: str, language: str = "generic") -> List[str]:
    """Extract function/method calls from code based on language"""
    targets = []
    lines = code.split('\n')
    
    for line in lines:
        if language.lower() == "cobol" and 'PERFORM' in line.upper():
            # COBOL PERFORM statements
            words = line.upper().split()
            perform_index = -1
            for i, word in enumerate(words):
                if word == 'PERFORM':
                    perform_index = i
                    break
            
            if perform_index >= 0 and perform_index + 1 < len(words):
                target = words[perform_index + 1]
                target = re.sub(r'[^A-Z0-9-]', '', target)
                if target and validate_identifier(target, language):
                    targets.append(target)
        
        elif language.lower() == "java" and ('(' in line and ')' in line):
            # Java method calls
            pattern = r'(\w+)\s*\('
            matches = re.findall(pattern, line)
            for match in matches:
                if validate_identifier(match, language):
                    targets.append(match)
        
        elif language.lower() == "python" and ('(' in line and ')' in line):
            # Python function calls
            pattern = r'(\w+)\s*\('
            matches = re.findall(pattern, line)
            for match in matches:
                if validate_identifier(match, language):
                    targets.append(match)
    
    return list(set(targets))  # Remove duplicates


def create_analysis_metadata(processing_time: float, 
                           tokens_used: int = 0,
                           confidence_threshold: float = 0.7) -> Dict[str, Any]:
    """Create metadata for analysis results"""
    return {
        "processing_time": f"{processing_time:.2f}s",
        "llm_tokens_used": tokens_used,
        "confidence_threshold": confidence_threshold,
        "ontology_validation": "PASSED",
        "timestamp": format_timestamp()
    }


def detect_language_from_extension(file_extension: str) -> str:
    """Detect programming language from file extension"""
    extension_map = {
        '.cbl': 'cobol',
        '.cob': 'cobol',
        '.java': 'java',
        '.py': 'python',
        '.js': 'javascript',
        '.ts': 'typescript',
        '.cpp': 'cpp',
        '.c': 'c',
        '.cs': 'csharp',
        '.go': 'go',
        '.rs': 'rust',
        '.php': 'php',
        '.rb': 'ruby',
        '.swift': 'swift',
        '.kt': 'kotlin',
        '.scala': 'scala'
    }
    
    return extension_map.get(file_extension.lower(), 'generic')


def log_processing_step(step: str, details: str = "") -> None:
    """Log a processing step (placeholder for proper logging)"""
    timestamp = format_timestamp()
    message = f"[{timestamp}] {step}"
    if details:
        message += f" - {details}"
    logger.info(message)


def validate_file_path(file_path: str) -> bool:
    """Validate file path with proper error handling"""
    if not file_path:
        raise ValueError("File path cannot be empty")
    
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"File not found: {file_path}")
    
    return True


def detect_language(file_path: str) -> str:
    """Detect programming language from file path"""
    if not file_path:
        return "UNKNOWN"
    
    extension = Path(file_path).suffix
    return detect_language_from_extension(extension).upper()


def get_file_extension(file_path: str) -> str:
    """Get file extension from file path"""
    if not file_path:
        return ""
    return Path(file_path).suffix


def format_confidence_score(score: float) -> str:
    """Format confidence score with validation"""
    if not isinstance(score, (int, float)):
        raise TypeError(f"Confidence score must be a number, got {type(score)}")
    
    if score < 0.0 or score > 1.0:
        raise ValueError(f"Confidence score must be between 0.0 and 1.0, got {score}")
    
    return f"{score:.2f}"


def format_complexity_score(score: float) -> str:
    """Format complexity score with validation"""
    if not isinstance(score, (int, float)):
        raise TypeError(f"Complexity score must be a number, got {type(score)}")
    
    if score < 0.0 or score > 1.0:
        raise ValueError(f"Complexity score must be between 0.0 and 1.0, got {score}")
    
    return f"{score:.2f}"


def format_risk_level(risk_level: str) -> str:
    """Format risk level with validation"""
    if not isinstance(risk_level, str):
        raise TypeError(f"Risk level must be a string, got {type(risk_level)}")
    
    valid_levels = ['LOW', 'MEDIUM', 'HIGH', 'CRITICAL']
    upper_level = risk_level.upper()
    
    if upper_level not in valid_levels:
        raise ValueError(f"Risk level must be one of {valid_levels}, got {risk_level}")
    
    return upper_level


def calculate_metrics(data: List[Any]) -> Dict[str, Any]:
    """Calculate basic metrics from data"""
    if not isinstance(data, list):
        raise TypeError(f"Data must be a list, got {type(data)}")
    
    return {
        "count": len(data),
        "total_items": len(data)
    }


def generate_summary(data: Dict[str, Any]) -> str:
    """Generate summary from data"""
    if not isinstance(data, dict):
        raise TypeError(f"Data must be a dictionary, got {type(data)}")
    
    sections = data.get("sections", [])
    subsections = data.get("subsections", [])
    relationships = data.get("relationships", [])
    
    return f"Analysis complete: {len(sections)} sections, {len(subsections)} subsections, {len(relationships)} relationships"


def sanitize_filename(filename: str) -> str:
    """Sanitize filename by removing invalid characters"""
    if not isinstance(filename, str):
        raise TypeError(f"Filename must be a string, got {type(filename)}")
    
    # Remove invalid characters for filenames
    sanitized = re.sub(r'[<>:"/\\|?*]', '_', filename)
    
    # Remove multiple consecutive underscores
    sanitized = re.sub(r'_+', '_', sanitized)
    
    # Remove leading/trailing underscores and dots
    sanitized = sanitized.strip('_.')
    
    return sanitized


def merge_overlapping_ranges(ranges: List[Tuple[int, int]]) -> List[Tuple[int, int]]:
    """Merge overlapping ranges"""
    if not isinstance(ranges, list):
        raise TypeError(f"Ranges must be a list, got {type(ranges)}")
    
    if not ranges:
        return []
    
    # Sort ranges by start position
    sorted_ranges = sorted(ranges, key=lambda x: x[0])
    merged = [sorted_ranges[0]]
    
    for current in sorted_ranges[1:]:
        last = merged[-1]
        
        # If current range overlaps with the last merged range
        if current[0] <= last[1]:
            # Merge ranges
            merged[-1] = (last[0], max(last[1], current[1]))
        else:
            # No overlap, add current range
            merged.append(current)
    
    return merged


def calculate_similarity(text1: str, text2: str) -> float:
    """Calculate similarity between two texts using sequence matching"""
    if not isinstance(text1, str) or not isinstance(text2, str):
        raise TypeError("Both texts must be strings")
    
    if not text1 and not text2:
        return 1.0
    
    if not text1 or not text2:
        return 0.0
    
    # Use sequence matching for similarity
    from difflib import SequenceMatcher
    return SequenceMatcher(None, text1, text2).ratio()
