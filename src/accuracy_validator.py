"""
Accuracy validation module for LLM Code Grapher
Validates analysis results against ground truth data
"""

import json
import os
from typing import Dict, List, Any, Optional
from dataclasses import dataclass
from enum import Enum

class ValidationResult(Enum):
    """Validation result types"""
    EXACT_MATCH = "exact_match"
    PARTIAL_MATCH = "partial_match"
    NO_MATCH = "no_match"

@dataclass
class AccuracyMetrics:
    """Accuracy metrics container"""
    precision: float
    recall: float
    f1_score: float
    total_sections: int
    matched_sections: int
    false_positives: int
    false_negatives: int

class AccuracyValidator:
    """Validates accuracy of code analysis results against ground truth"""
    
    def __init__(self, ground_truth_path: Optional[str] = None):
        """
        Initialize accuracy validator
        
        Args:
            ground_truth_path: Path to ground truth JSON file
        """
        self.ground_truth_path = ground_truth_path
        self.ground_truth = {}
    
    def _load_ground_truth(self) -> Dict[str, Any]:
        """Load ground truth data from file"""
        if not self.ground_truth_path:
            return {}
        
        if not os.path.exists(self.ground_truth_path):
            raise FileNotFoundError(f"Ground truth file not found: {self.ground_truth_path}")
        
        try:
            with open(self.ground_truth_path, 'r') as f:
                return json.load(f)
        except json.JSONDecodeError as e:
            raise e
    
    def load_ground_truth(self) -> Dict[str, Any]:
        """Public method to load ground truth data from file"""
        return self._load_ground_truth()
    
    def validate_section_accuracy(self, predicted_sections: List[Dict], 
                                ground_truth_sections: List[Dict]) -> Dict[str, Any]:
        """
        Validate section accuracy against ground truth
        
        Args:
            predicted_sections: List of predicted sections from analysis
            ground_truth_sections: List of ground truth sections
            
        Returns:
            Dictionary containing accuracy metrics and detailed results
        """
        if not predicted_sections or not ground_truth_sections:
            return self._create_empty_result()
        
        # Normalize section data for comparison
        predicted_normalized = self._normalize_sections(predicted_sections)
        ground_truth_normalized = self._normalize_sections(ground_truth_sections)
        
        # Calculate accuracy metrics
        metrics = self._calculate_accuracy_metrics(
            predicted_normalized, ground_truth_normalized
        )
        
        # Find detailed matches
        detailed_matches = self._find_detailed_matches(
            predicted_normalized, ground_truth_normalized
        )
        
        return {
            'metrics': metrics,
            'detailed_matches': detailed_matches,
            'validation_summary': self._create_validation_summary(metrics)
        }
    
    def _normalize_sections(self, sections: List[Dict]) -> List[Dict]:
        """Normalize section data for comparison"""
        normalized = []
        for section in sections:
            normalized_section = {
                'name': section.get('name', '').lower().strip(),
                'start_line': int(section.get('start_line', 0)),
                'end_line': int(section.get('end_line', 0)),
                'type': section.get('type', '').lower().strip()
            }
            normalized.append(normalized_section)
        return normalized
    
    def _calculate_accuracy_metrics(self, predicted: List[Dict], 
                                  ground_truth: List[Dict]) -> AccuracyMetrics:
        """Calculate accuracy metrics"""
        matched = 0
        false_positives = 0
        false_negatives = 0
        
        # Find matches
        for pred_section in predicted:
            if self._find_matching_section(pred_section, ground_truth):
                matched += 1
            else:
                false_positives += 1
        
        # Count false negatives
        for gt_section in ground_truth:
            if not self._find_matching_section(gt_section, predicted):
                false_negatives += 1
        
        # Calculate metrics
        precision = matched / len(predicted) if predicted else 0.0
        recall = matched / len(ground_truth) if ground_truth else 0.0
        f1_score = 2 * (precision * recall) / (precision + recall) if (precision + recall) > 0 else 0.0
        
        return AccuracyMetrics(
            precision=precision,
            recall=recall,
            f1_score=f1_score,
            total_sections=len(ground_truth),
            matched_sections=matched,
            false_positives=false_positives,
            false_negatives=false_negatives
        )
    
    def _find_matching_section(self, section: Dict, section_list: List[Dict]) -> bool:
        """Find if a section has a match in the list"""
        for other_section in section_list:
            if (section['name'] == other_section['name'] and
                abs(section['start_line'] - other_section['start_line']) <= 2 and
                section['type'] == other_section['type']):
                return True
        return False
    
    def _find_detailed_matches(self, predicted: List[Dict], 
                             ground_truth: List[Dict]) -> List[Dict]:
        """Find detailed match information"""
        matches = []
        for pred_section in predicted:
            match_info = {
                'predicted': pred_section,
                'match_type': ValidationResult.NO_MATCH.value,
                'matched_section': None
            }
            
            for gt_section in ground_truth:
                if self._find_matching_section(pred_section, [gt_section]):
                    match_info['match_type'] = ValidationResult.EXACT_MATCH.value
                    match_info['matched_section'] = gt_section
                    break
            
            matches.append(match_info)
        
        return matches
    
    def _create_validation_summary(self, metrics: AccuracyMetrics) -> Dict[str, str]:
        """Create human-readable validation summary"""
        return {
            'overall_accuracy': f"{metrics.f1_score:.2%}",
            'precision': f"{metrics.precision:.2%}",
            'recall': f"{metrics.recall:.2%}",
            'status': self._get_accuracy_status(metrics.f1_score)
        }
    
    def _get_accuracy_status(self, f1_score: float) -> str:
        """Get accuracy status based on F1 score"""
        if f1_score >= 0.9:
            return "Excellent"
        elif f1_score >= 0.8:
            return "Good"
        elif f1_score >= 0.7:
            return "Fair"
        else:
            return "Poor"
    
    def _create_empty_result(self) -> Dict[str, Any]:
        """Create empty result for edge cases"""
        return {
            'metrics': AccuracyMetrics(0.0, 0.0, 0.0, 0, 0, 0, 0),
            'detailed_matches': [],
            'validation_summary': {
                'overall_accuracy': "0.00%",
                'precision': "0.00%",
                'recall': "0.00%",
                'status': "No Data"
            }
        }

# Convenience functions for external use
def validate_analysis_accuracy(predicted_results: Dict[str, Any], 
                             ground_truth_path: str) -> Dict[str, Any]:
    """
    Convenience function to validate analysis accuracy
    
    Args:
        predicted_results: Results from code analysis
        ground_truth_path: Path to ground truth file
        
    Returns:
        Validation results
    """
    validator = AccuracyValidator(ground_truth_path)
    
    predicted_sections = predicted_results.get('sections', [])
    ground_truth_sections = validator.ground_truth.get('sections', [])
    
    return validator.validate_section_accuracy(predicted_sections, ground_truth_sections)

def create_ground_truth_template() -> Dict[str, Any]:
    """Create a template for ground truth files"""
    return {
        "metadata": {
            "created_date": "",
            "source_file": "",
            "validator": "",
            "version": "1.0"
        },
        "sections": [
            {
                "name": "SECTION_NAME",
                "type": "SECTION_TYPE",
                "start_line": 1,
                "end_line": 10,
                "description": "Section description"
            }
        ]
    }