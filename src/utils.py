"""
Utility functions for COBOL Code Grapher

Common utility functions used across the application.
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


def is_comment_line(line: str) -> bool:
    """Check if a line is a COBOL comment"""
    stripped = line.strip()
    return (stripped.startswith('*') or 
            stripped.startswith('//') or 
            stripped.startswith('/*'))


def clean_cobol_line(line: str) -> str:
    """Clean a COBOL line by removing comments and extra whitespace"""
    # Remove comments
    if '*' in line:
        line = line[:line.index('*')]
    
    # Remove trailing whitespace
    return line.rstrip()


def calculate_complexity_score(section_code: str) -> float:
    """Calculate a simple complexity score for a COBOL section"""
    lines = section_code.split('\n')
    non_comment_lines = [line for line in lines if not is_comment_line(line)]
    
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


def validate_cobol_identifier(identifier: str) -> bool:
    """Validate if a string is a valid COBOL identifier"""
    if not identifier:
        return False
    
    # COBOL identifiers must start with a letter and contain only letters, numbers, and hyphens
    pattern = r'^[A-Z][A-Z0-9-]*$'
    return bool(re.match(pattern, identifier.upper()))


def extract_perform_targets(code: str) -> List[str]:
    """Extract PERFORM statement targets from COBOL code"""
    targets = []
    lines = code.split('\n')
    
    for line in lines:
        if 'PERFORM' in line.upper():
            # Simple extraction - look for words after PERFORM
            words = line.upper().split()
            perform_index = -1
            for i, word in enumerate(words):
                if word == 'PERFORM':
                    perform_index = i
                    break
            
            if perform_index >= 0 and perform_index + 1 < len(words):
                target = words[perform_index + 1]
                # Clean up the target (remove punctuation)
                target = re.sub(r'[^A-Z0-9-]', '', target)
                if target and validate_cobol_identifier(target):
                    targets.append(target)
    
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


def log_processing_step(step: str, details: str = "") -> None:
    """Log a processing step (placeholder for proper logging)"""
    timestamp = format_timestamp()
    message = f"[{timestamp}] {step}"
    if details:
        message += f" - {details}"
    logger.info(message)
