"""
Output Generator for COBOL Code Grapher

Handles generation of various output formats including JSON, text summaries,
and graph visualizations.
"""

import json
import os
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, List, Optional
from dataclasses import asdict

from .config_manager import get_config


class OutputGenerator:
    """Generates various output formats for COBOL analysis results"""
    
    def __init__(self, output_dir: Optional[str] = None):
        self.config = get_config()
        self.output_dir = Path(output_dir or self.config.output.output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
    
    def generate_json_output(self, analysis_result: Dict[str, Any], 
                           input_filename: str) -> str:
        """Generate JSON output file"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = self.config.output.filename_template.format(
            input_name=Path(input_filename).stem,
            timestamp=timestamp
        ) + ".json"
        
        output_path = self.output_dir / "json" / filename
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(analysis_result, f, indent=2, ensure_ascii=False)
        
        return str(output_path)
    
    def generate_text_summary(self, analysis_result: Dict[str, Any], 
                            input_filename: str) -> str:
        """Generate human-readable text summary"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = self.config.output.filename_template.format(
            input_name=Path(input_filename).stem,
            timestamp=timestamp
        ) + ".txt"
        
        output_path = self.output_dir / "text" / filename
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        summary = self._create_text_summary(analysis_result)
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(summary)
        
        return str(output_path)
    
    def _create_text_summary(self, analysis_result: Dict[str, Any]) -> str:
        """Create human-readable text summary"""
        lines = []
        lines.append("=" * 80)
        lines.append("COBOL CODE ANALYSIS SUMMARY")
        lines.append("=" * 80)
        lines.append("")
        
        # Program information
        if "program_name" in analysis_result:
            lines.append(f"Program: {analysis_result['program_name']}")
        
        if "total_sections" in analysis_result:
            lines.append(f"Total Sections: {analysis_result['total_sections']}")
        
        if "total_subsections" in analysis_result:
            lines.append(f"Total Subsections: {analysis_result['total_subsections']}")
        
        lines.append("")
        
        # Sections summary
        if "sections" in analysis_result:
            lines.append("SECTIONS:")
            lines.append("-" * 40)
            for section in analysis_result["sections"]:
                lines.append(f"• {section.get('name', 'Unknown')} "
                           f"({section.get('type', 'Unknown')})")
                if "business_logic" in section:
                    lines.append(f"  Purpose: {section['business_logic']}")
                if "confidence" in section:
                    lines.append(f"  Confidence: {section['confidence']:.2f}")
                lines.append("")
        
        # Relationships summary
        if "relationships" in analysis_result and analysis_result["relationships"]:
            lines.append("RELATIONSHIPS:")
            lines.append("-" * 40)
            for rel in analysis_result["relationships"]:
                lines.append(f"• {rel.get('source', 'Unknown')} → "
                           f"{rel.get('target', 'Unknown')} "
                           f"({rel.get('relationship_type', 'Unknown')})")
                if "confidence" in rel:
                    lines.append(f"  Confidence: {rel['confidence']:.2f}")
                lines.append("")
        
        # Quality metrics
        if "ontology_metrics" in analysis_result:
            metrics = analysis_result["ontology_metrics"]
            lines.append("QUALITY METRICS:")
            lines.append("-" * 40)
            
            if "complexity_metrics" in metrics:
                comp = metrics["complexity_metrics"]
                lines.append(f"Cyclomatic Complexity: {comp.get('cyclomatic_complexity', 'N/A')}")
                lines.append(f"Maintainability Index: {comp.get('maintainability_index', 'N/A')}")
                lines.append(f"Technical Debt Ratio: {comp.get('technical_debt_ratio', 'N/A')}")
                lines.append("")
            
            if "maintenance_risks" in metrics and metrics["maintenance_risks"]:
                lines.append("MAINTENANCE RISKS:")
                for risk in metrics["maintenance_risks"]:
                    lines.append(f"• {risk}")
                lines.append("")
            
            if "modernization_potential" in metrics:
                lines.append(f"Modernization Potential: {metrics['modernization_potential']}")
        
        # Analysis metadata
        if "analysis_metadata" in analysis_result:
            meta = analysis_result["analysis_metadata"]
            lines.append("")
            lines.append("ANALYSIS METADATA:")
            lines.append("-" * 40)
            for key, value in meta.items():
                lines.append(f"{key.replace('_', ' ').title()}: {value}")
        
        lines.append("")
        lines.append("=" * 80)
        lines.append(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        lines.append("=" * 80)
        
        return "\n".join(lines)
    
    def generate_all_outputs(self, analysis_result: Dict[str, Any], 
                           input_filename: str) -> Dict[str, str]:
        """Generate all configured output formats"""
        outputs = {}
        
        # Generate JSON output
        if self.config.output.format in ["json", "all"]:
            outputs["json"] = self.generate_json_output(analysis_result, input_filename)
        
        # Generate text summary
        outputs["text"] = self.generate_text_summary(analysis_result, input_filename)
        
        # TODO: Add graph visualization generation
        if self.config.output.generate_visualization:
            # Placeholder for future graph generation
            pass
        
        return outputs


def generate_output(analysis_result: Dict[str, Any], 
                   input_filename: str,
                   output_dir: Optional[str] = None) -> Dict[str, str]:
    """Convenience function to generate all outputs"""
    generator = OutputGenerator(output_dir)
    return generator.generate_all_outputs(analysis_result, input_filename)
