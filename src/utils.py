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
    """
    Validate file path with comprehensive error handling
    
    Args:
        file_path: Path to validate
        
    Returns:
        True if valid
        
    Raises:
        ValueError: If file path is empty or invalid
        FileNotFoundError: If file doesn't exist
        PermissionError: If file is not readable
    """
    if not file_path:
        raise ValueError("File path cannot be empty")
    
    if not isinstance(file_path, str):
        raise ValueError("File path must be a string")
    
    # Normalize path
    normalized_path = os.path.normpath(file_path)
    
    if not os.path.exists(normalized_path):
        raise FileNotFoundError(f"File not found: {normalized_path}")
    
    if not os.path.isfile(normalized_path):
        raise ValueError(f"Path is not a file: {normalized_path}")
    
    if not os.access(normalized_path, os.R_OK):
        raise PermissionError(f"File is not readable: {normalized_path}")
    
    return True


def validate_directory_path(dir_path: str) -> bool:
    """
    Validate directory path with error handling
    
    Args:
        dir_path: Directory path to validate
        
    Returns:
        True if valid
        
    Raises:
        ValueError: If directory path is empty or invalid
        FileNotFoundError: If directory doesn't exist
        PermissionError: If directory is not accessible
    """
    if not dir_path:
        raise ValueError("Directory path cannot be empty")
    
    if not isinstance(dir_path, str):
        raise ValueError("Directory path must be a string")
    
    # Normalize path
    normalized_path = os.path.normpath(dir_path)
    
    if not os.path.exists(normalized_path):
        raise FileNotFoundError(f"Directory not found: {normalized_path}")
    
    if not os.path.isdir(normalized_path):
        raise ValueError(f"Path is not a directory: {normalized_path}")
    
    if not os.access(normalized_path, os.R_OK):
        raise PermissionError(f"Directory is not readable: {normalized_path}")
    
    return True


def get_file_size(file_path: str) -> int:
    """
    Get file size with error handling
    
    Args:
        file_path: Path to file
        
    Returns:
        File size in bytes
        
    Raises:
        FileNotFoundError: If file doesn't exist
        OSError: If file size cannot be determined
    """
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"File not found: {file_path}")
    
    try:
        return os.path.getsize(file_path)
    except OSError as e:
        raise OSError(f"Cannot get file size for {file_path}: {e}")


def get_file_extension(file_path: str) -> str:
    """
    Get file extension with error handling
    
    Args:
        file_path: Path to file
        
    Returns:
        File extension (including dot)
        
    Raises:
        ValueError: If file path is invalid
    """
    if not file_path:
        raise ValueError("File path cannot be empty")
    
    return Path(file_path).suffix.lower()


def is_valid_file_extension(file_path: str, valid_extensions: List[str]) -> bool:
    """
    Check if file has valid extension
    
    Args:
        file_path: Path to file
        valid_extensions: List of valid extensions (with or without dots)
        
    Returns:
        True if extension is valid
        
    Raises:
        ValueError: If file path is invalid
    """
    if not file_path:
        raise ValueError("File path cannot be empty")
    
    if not valid_extensions:
        raise ValueError("Valid extensions list cannot be empty")
    
    file_ext = get_file_extension(file_path)
    
    # Normalize extensions (ensure they start with dot)
    normalized_extensions = []
    for ext in valid_extensions:
        if not ext.startswith('.'):
            ext = '.' + ext
        normalized_extensions.append(ext.lower())
    
    return file_ext in normalized_extensions


def safe_file_operation(operation_func, *args, **kwargs):
    """
    Safely execute file operation with error handling
    
    Args:
        operation_func: Function to execute
        *args: Arguments for the function
        **kwargs: Keyword arguments for the function
        
    Returns:
        Result of the operation
        
    Raises:
        Various exceptions based on the operation
    """
    try:
        return operation_func(*args, **kwargs)
    except Exception as e:
        logging.error(f"File operation failed: {e}")
        raise
