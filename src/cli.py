"""
Command Line Interface for LLM Code Grapher

Provides CLI functionality for analyzing code structure and generating outputs.
Supports multiple programming languages through language-agnostic architecture.
"""

import click
import sys
from pathlib import Path
from typing import Optional

from .config_manager import get_config, reload_config
from .utils import read_file_safely, log_processing_step, detect_language_from_extension
from .output_generator import generate_output


@click.group()
@click.option('--config', '-c', help='Path to configuration file')
@click.option('--verbose', '-v', is_flag=True, help='Enable verbose output')
@click.pass_context
def cli(ctx, config: Optional[str], verbose: bool):
    """LLM Code Grapher - Analyze code structure using LLMs"""
    ctx.ensure_object(dict)
    ctx.obj['verbose'] = verbose
    
    if config:
        # Load custom config file
        try:
            reload_config(config)
            if verbose:
                click.echo(f"Loaded custom config from: {config}")
        except Exception as e:
            click.echo(f"Error loading config file {config}: {e}", err=True)
            sys.exit(1)


@cli.command()
@click.argument('input_file', type=click.Path(exists=True))
@click.option('--language', '-l', 
              type=click.Choice(['cobol', 'java', 'python']), 
              help='Programming language (auto-detected if not specified)')
@click.option('--output-dir', '-o', help='Output directory for results')
@click.option('--format', 'output_format', 
              type=click.Choice(['json', 'text', 'all']), 
              default='all', help='Output format')
@click.option('--confidence-threshold', type=float, default=0.7,
              help='Minimum confidence threshold for analysis')
@click.option('--provider', 
              type=click.Choice(['openai', 'ollama']), 
              help='LLM provider to use (overrides config)')
@click.option('--model', help='Model to use (overrides config)')
@click.option('--base-url', help='Base URL for local providers like Ollama')
@click.pass_context
def analyze(ctx, input_file: str, language: Optional[str], output_dir: Optional[str], 
           output_format: str, confidence_threshold: float,
           provider: Optional[str], model: Optional[str], base_url: Optional[str]):
    """Analyze a code file and generate structured output"""
    
    verbose = ctx.obj.get('verbose', False)
    
    try:
        log_processing_step("Starting code analysis", f"File: {input_file}")
        
        # Read input file
        log_processing_step("Reading input file")
        code_content = read_file_safely(input_file)
        
        if verbose:
            click.echo(f"Read {len(code_content)} characters from {input_file}")
        
        # Detect language if not specified
        if not language:
            file_extension = Path(input_file).suffix.lower()
            language = detect_language_from_extension(file_extension)
        
        if verbose:
            click.echo(f"Using language: {language}")
        
        # Perform analysis using the language framework
        log_processing_step(f"Performing {language.upper()} analysis")
        
        # Import language-specific components dynamically
        try:
            parser_module = __import__(f"lang.{language}.parser.{language}_parser", fromlist=[f"{language.upper()}Parser"])
            analyzer_module = __import__(f"lang.{language}.parser.llm_analyzer", fromlist=[f"{language.upper()}Analyzer"])
            ontology_module = __import__(f"lang.{language}.ontology.{language}_ontology", fromlist=[f"{language.upper()}Ontology"])
        except ImportError as e:
            click.echo(f"Error: Language '{language}' is not supported. {e}", err=True)
            sys.exit(1)
        
        # Initialize components
        ParserClass = getattr(parser_module, f"{language.upper()}Parser")
        AnalyzerClass = getattr(analyzer_module, f"{language.upper()}Analyzer")
        OntologyClass = getattr(ontology_module, f"{language.upper()}Ontology")
        
        # Get configuration and apply CLI overrides
        config = get_config()
        
        # Override config with CLI arguments
        if provider:
            config.llm.provider = provider
        if model:
            config.llm.model = model
        if base_url:
            config.llm.base_url = base_url
        
        parser = ParserClass()
        analyzer = AnalyzerClass(config.llm)
        ontology = OntologyClass()
        
        # Parse the code file
        log_processing_step(f"Parsing {language.upper()} structure")
        sections = parser.parse_sections(code_content)
        
        # Analyze with LLM
        log_processing_step("Analyzing with LLM")
        analyzed_sections = []
        for section in sections:
            analysis = analyzer.analyze_section(
                section.code, 
                section.name, 
                section.type
            )
            section.business_logic = analysis.get('description', '')
            section.confidence = analysis.get('confidence', 0.0)
            analyzed_sections.append(section)
        
        # Create ontology
        log_processing_step(f"Creating {language.upper()} ontology")
        program_ontology = ontology.create_program_ontology(
            program_name=Path(input_file).stem,
            sections=analyzed_sections
        )
        
        # Create analysis result from ontology
        analysis_result = {
            "program_name": program_ontology.program_name,
            "language": language.upper(),
            "total_sections": len(program_ontology.sections),
            "total_subsections": len(program_ontology.subsections),
            "sections": [
                {
                    "name": section.name,
                    "type": section.type,
                    "line_range": section.line_range,
                    "line_count": section.line_count,
                    "business_logic": section.business_logic,
                    "confidence": section.confidence,
                    "complexity_score": getattr(section, 'complexity_score', 0.0),
                    "risk_level": getattr(section, 'risk_level', 'LOW')
                }
                for section in program_ontology.sections
            ],
            "relationships": [
                {
                    "source": rel.source,
                    "target": rel.target,
                    "relationship_type": rel.relationship_type,
                    "confidence": rel.confidence,
                    "strength": rel.strength
                }
                for rel in program_ontology.relationships
            ],
            "ontology_metrics": program_ontology.complexity_metrics,
            "quality_indicators": program_ontology.quality_indicators,
            "maintenance_risks": program_ontology.maintenance_risks,
            "modernization_potential": program_ontology.modernization_potential,
            "confidence_threshold": confidence_threshold
        }
        
        # Generate outputs
        log_processing_step("Generating outputs")
        output_generator = generate_output(analysis_result, input_file, output_dir, output_format)
        
        if verbose:
            click.echo(f"Analysis complete. Generated {len(output_generator)} output files.")
        
        click.echo("✅ Analysis completed successfully!")
        
    except Exception as e:
        log_processing_step(f"Error during analysis: {e}")
        click.echo(f"❌ Analysis failed: {e}", err=True)
        if verbose:
            import traceback
            click.echo(traceback.format_exc(), err=True)
        sys.exit(1)


@cli.command()
@click.argument('input_file', type=click.Path(exists=True))
@click.option('--language', '-l', 
              type=click.Choice(['cobol', 'java', 'python']), 
              help='Programming language (auto-detected if not specified)')
@click.pass_context
def preview(ctx, input_file: str, language: Optional[str]):
    """Preview code file structure without full analysis"""
    
    verbose = ctx.obj.get('verbose', False)
    
    try:
        log_processing_step("Previewing code file", f"File: {input_file}")
        
        # Read input file
        code_content = read_file_safely(input_file)
        lines = code_content.split('\n')
        
        # Detect language if not specified
        if not language:
            file_extension = Path(input_file).suffix.lower()
            language = detect_language_from_extension(file_extension)
        
        click.echo(f"📁 File: {input_file}")
        click.echo(f"🔤 Language: {language.upper()}")
        click.echo(f"📏 Lines: {len(lines)}")
        click.echo(f"📊 Characters: {len(code_content)}")
        
        # Show basic file structure
        click.echo(f"\n📋 File Structure Preview:")
        click.echo(f"   Language: {language.upper()}")
        click.echo(f"   Lines: {len(lines)}")
        click.echo(f"   Characters: {len(code_content)}")
        
        if verbose:
            # Show first few lines
            click.echo(f"\n📄 First 10 lines:")
            for i, line in enumerate(lines[:10], 1):
                click.echo(f"   {i:3d}: {line}")
            
            if len(lines) > 10:
                click.echo(f"   ... and {len(lines) - 10} more lines")
        
        click.echo("✅ Preview completed successfully!")
        
    except Exception as e:
        click.echo(f"❌ Preview failed: {e}", err=True)
        sys.exit(1)


@cli.command()
def list_languages():
    """List supported programming languages"""
    click.echo("🔤 Supported Programming Languages:")
    click.echo("   • COBOL (.cbl, .cob)")
    click.echo("   • Java (.java)")
    click.echo("   • Python (.py)")
    click.echo("\n💡 Language is auto-detected from file extension, or use --language option")


@cli.command()
def version():
    """Show version information"""
    click.echo("LLM Code Grapher v1.0.0")
    click.echo("Language-agnostic code analysis using LLMs")


def main():
    """Main entry point for the CLI"""
    cli()


if __name__ == "__main__":
    main()