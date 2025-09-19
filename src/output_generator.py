"""
Output Generator for LLM Code Grapher

Handles generation of various output formats including JSON, text summaries,
and graph visualizations. Language-agnostic output generation.
"""

import json
import os
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, List, Optional
from dataclasses import asdict

from config_manager import get_config


class OutputGenerator:
    """Generates various output formats for COBOL analysis results"""
    
    def __init__(self, output_format: Optional[str] = None, output_dir: Optional[str] = None):
        self.config = get_config()
        self.output_dir = Path(output_dir or self.config.output.output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Validate output format if provided
        if output_format is not None:
            valid_formats = ['json', 'text', 'yaml', 'all']
            if output_format not in valid_formats:
                raise ValueError(f"Invalid output format: {output_format}")
            self.output_format = output_format
        else:
            self.output_format = self.config.output.format
    
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
        
        try:
            with open(output_path, 'w', encoding='utf-8') as f:
                json.dump(analysis_result, f, indent=2, ensure_ascii=False)
        except PermissionError as e:
            raise PermissionError(f"Cannot create output file: {e}")
        except Exception as e:
            raise Exception(f"Error generating JSON output: {e}")
        
        return str(output_path)
    
    def generate_yaml_output(self, analysis_result: Dict[str, Any], 
                           input_filename: str) -> str:
        """Generate YAML output file"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = self.config.output.filename_template.format(
            input_name=Path(input_filename).stem,
            timestamp=timestamp
        ) + ".yaml"
        
        output_path = self.output_dir / "yaml" / filename
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        try:
            with open(output_path, 'w', encoding='utf-8') as f:
                yaml.dump(analysis_result, f, default_flow_style=False, allow_unicode=True)
        except PermissionError as e:
            raise PermissionError(f"Cannot create output file: {e}")
        except Exception as e:
            raise Exception(f"Error generating YAML output: {e}")
        
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
        language = analysis_result.get("language", "UNKNOWN")
        lines.append(f"{language.upper()} CODE ANALYSIS SUMMARY")
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
        
        # Graph visualization generation (future feature)
        if self.config.output.generate_visualization:
            # Note: Graph visualization will be implemented in Phase 2
            self.logger.info("Graph visualization requested but not yet implemented")
            pass
        
        return outputs


def generate_json_output(analysis_result: Dict[str, Any], 
                        output_path: str) -> str:
    """Convenience function to generate JSON output"""
    generator = OutputGenerator()
    return generator.generate_json_output(analysis_result, Path(output_path).stem)


def generate_text_output(analysis_result: Dict[str, Any], 
                        output_path: str) -> str:
    """Convenience function to generate text output"""
    generator = OutputGenerator()
    return generator.generate_text_summary(analysis_result, Path(output_path).stem)


def generate_yaml_output(analysis_result: Dict[str, Any], 
                        output_path: str) -> str:
    """Convenience function to generate YAML output"""
    generator = OutputGenerator()
    return generator.generate_yaml_output(analysis_result, Path(output_path).stem)


def generate_output(analysis_result: Dict[str, Any], 
                   input_filename: str,
                   output_dir: Optional[str] = None) -> Dict[str, str]:
    """Convenience function to generate all outputs"""
    generator = OutputGenerator(output_dir)
    return generator.generate_all_outputs(analysis_result, input_filename)
