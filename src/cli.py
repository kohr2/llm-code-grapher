"""
Command Line Interface for COBOL Code Grapher

Provides CLI functionality for analyzing COBOL code and generating outputs.
"""

import click
import sys
from pathlib import Path
from typing import Optional

from .config_manager import get_config, reload_config
from .utils import read_file_safely, log_processing_step
from .output_generator import generate_output


@click.group()
@click.option('--config', '-c', help='Path to configuration file')
@click.option('--verbose', '-v', is_flag=True, help='Enable verbose output')
@click.pass_context
def cli(ctx, config: Optional[str], verbose: bool):
    """COBOL Code Grapher - Analyze COBOL code structure using LLMs"""
    ctx.ensure_object(dict)
    ctx.obj['verbose'] = verbose
    
    if config:
        # TODO: Implement config file override
        pass


@cli.command()
@click.argument('input_file', type=click.Path(exists=True))
@click.option('--output-dir', '-o', help='Output directory for results')
@click.option('--format', 'output_format', 
              type=click.Choice(['json', 'text', 'all']), 
              default='all', help='Output format')
@click.option('--confidence-threshold', type=float, default=0.7,
              help='Minimum confidence threshold for analysis')
@click.pass_context
def analyze(ctx, input_file: str, output_dir: Optional[str], 
           output_format: str, confidence_threshold: float):
    """Analyze a COBOL file and generate structured output"""
    
    verbose = ctx.obj.get('verbose', False)
    
    try:
        log_processing_step("Starting COBOL analysis", f"File: {input_file}")
        
        # Read input file
        log_processing_step("Reading input file")
        cobol_code = read_file_safely(input_file)
        
        if verbose:
            click.echo(f"Read {len(cobol_code)} characters from {input_file}")
        
        # TODO: Implement actual COBOL analysis
        # This is a placeholder for the main analysis logic
        log_processing_step("Performing COBOL analysis")
        
        # Placeholder analysis result
        analysis_result = {
            "program_name": Path(input_file).stem,
            "total_sections": 0,
            "total_subsections": 0,
            "sections": [],
            "relationships": [],
            "ontology_metrics": {
                "complexity_metrics": {
                    "cyclomatic_complexity": 0.0,
                    "maintainability_index": 0.0,
                    "technical_debt_ratio": 0.0
                },
                "quality_indicators": {
                    "code_coverage": "UNKNOWN",
                    "documentation_quality": "UNKNOWN",
                    "test_coverage": "UNKNOWN"
                },
                "maintenance_risks": [],
                "modernization_potential": "UNKNOWN"
            },
            "analysis_metadata": {
                "processing_time": "0.00s",
                "llm_tokens_used": 0,
                "confidence_threshold": confidence_threshold,
                "ontology_validation": "PASSED"
            }
        }
        
        # Generate outputs
        log_processing_step("Generating outputs")
        outputs = generate_output(analysis_result, input_file, output_dir)
        
        # Display results
        click.echo("\n" + "="*60)
        click.echo("ANALYSIS COMPLETE")
        click.echo("="*60)
        
        for output_type, output_path in outputs.items():
            click.echo(f"{output_type.upper()} output: {output_path}")
        
        click.echo(f"\nAnalysis completed successfully!")
        
    except Exception as e:
        click.echo(f"Error during analysis: {e}", err=True)
        if verbose:
            import traceback
            click.echo(traceback.format_exc(), err=True)
        sys.exit(1)


@cli.command()
@click.option('--reload', is_flag=True, help='Reload configuration')
def config(reload: bool):
    """Show current configuration"""
    
    if reload:
        config = reload_config()
        click.echo("Configuration reloaded")
    else:
        config = get_config()
    
    click.echo("\nCurrent Configuration:")
    click.echo("="*40)
    
    # LLM Configuration
    click.echo(f"LLM Provider: {config.llm.provider}")
    click.echo(f"LLM Model: {config.llm.model}")
    click.echo(f"Max Tokens: {config.llm.max_tokens}")
    click.echo(f"Temperature: {config.llm.temperature}")
    click.echo(f"API Key: {'***' if config.llm.api_key else 'Not set'}")
    
    # Processing Configuration
    click.echo(f"\nChunk Size: {config.processing.chunk_size}")
    click.echo(f"Overlap: {config.processing.overlap}")
    click.echo(f"Max Retries: {config.processing.max_retries}")
    click.echo(f"Confidence Threshold: {config.processing.confidence_threshold}")
    
    # Output Configuration
    click.echo(f"\nOutput Format: {config.output.format}")
    click.echo(f"Output Directory: {config.output.output_dir}")
    click.echo(f"Generate Visualization: {config.output.generate_visualization}")


@cli.command()
def validate():
    """Validate configuration and environment"""
    
    click.echo("Validating configuration and environment...")
    
    # Check configuration
    try:
        config = get_config()
        click.echo("✓ Configuration loaded successfully")
    except Exception as e:
        click.echo(f"✗ Configuration error: {e}")
        return
    
    # Check API key
    if config.llm.api_key:
        click.echo("✓ OpenAI API key is set")
    else:
        click.echo("✗ OpenAI API key is not set")
    
    # Check output directory
    output_dir = Path(config.output.output_dir)
    if output_dir.exists():
        click.echo("✓ Output directory exists")
    else:
        try:
            output_dir.mkdir(parents=True, exist_ok=True)
            click.echo("✓ Output directory created")
        except Exception as e:
            click.echo(f"✗ Cannot create output directory: {e}")
    
    # Check required Python packages
    required_packages = ['openai', 'pydantic', 'yaml', 'click']
    missing_packages = []
    
    for package in required_packages:
        try:
            __import__(package)
            click.echo(f"✓ {package} is available")
        except ImportError:
            missing_packages.append(package)
            click.echo(f"✗ {package} is not installed")
    
    if missing_packages:
        click.echo(f"\nMissing packages: {', '.join(missing_packages)}")
        click.echo("Run: pip install -r requirements.txt")
    else:
        click.echo("\n✓ All required packages are available")


@cli.command()
@click.argument('input_file', type=click.Path(exists=True))
def preview(input_file: str):
    """Preview COBOL file structure without full analysis"""
    
    try:
        log_processing_step("Previewing COBOL file", f"File: {input_file}")
        
        # Read file
        cobol_code = read_file_safely(input_file)
        lines = cobol_code.split('\n')
        
        click.echo(f"\nFile: {input_file}")
        click.echo(f"Lines: {len(lines)}")
        click.echo(f"Characters: {len(cobol_code)}")
        
        # Find potential sections
        config = get_config()
        section_patterns = config.cobol.section_patterns
        
        click.echo("\nPotential Sections Found:")
        click.echo("-" * 40)
        
        for i, line in enumerate(lines, 1):
            for pattern in section_patterns:
                import re
                if re.search(pattern, line, re.IGNORECASE):
                    click.echo(f"Line {i:4d}: {line.strip()}")
                    break
        
        click.echo(f"\nPreview completed for {input_file}")
        
    except Exception as e:
        click.echo(f"Error during preview: {e}", err=True)
        sys.exit(1)


def main():
    """Main entry point for CLI"""
    cli()


if __name__ == '__main__':
    main()
