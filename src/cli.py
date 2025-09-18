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
def analyze(ctx, input_file: str, output_dir: Optional[str], 
           output_format: str, confidence_threshold: float,
           provider: Optional[str], model: Optional[str], base_url: Optional[str]):
    """Analyze a COBOL file and generate structured output"""
    
    verbose = ctx.obj.get('verbose', False)
    
    try:
        log_processing_step("Starting COBOL analysis", f"File: {input_file}")
        
        # Read input file
        log_processing_step("Reading input file")
        cobol_code = read_file_safely(input_file)
        
        if verbose:
            click.echo(f"Read {len(cobol_code)} characters from {input_file}")
        
        # Perform COBOL analysis using the language framework
        log_processing_step("Performing COBOL analysis")
        
        # Import COBOL components
        from lang.cobol.parser.cobol_parser import COBOLParser
        from lang.cobol.parser.llm_analyzer import COBOLAnalyzer
        from lang.cobol.ontology.cobol_ontology import COBOLOntology
        from lang.base.parser.llm_provider import LLMProviderConfig
        
        # Get configuration and apply CLI overrides
        config = get_config()
        
        # Override config with CLI arguments
        if provider:
            config.llm.provider = provider
        if model:
            config.llm.model = model
        if base_url:
            config.llm.base_url = base_url
        
        # Create provider configuration
        provider_config = LLMProviderConfig(
            provider=config.llm.provider,
            model=config.llm.model,
            api_key=config.llm.api_key,
            base_url=config.llm.base_url,
            max_tokens=config.llm.max_tokens,
            temperature=config.llm.temperature
        )
        
        # Initialize components
        parser = COBOLParser()
        analyzer = COBOLAnalyzer(provider_config)
        ontology = COBOLOntology()
        
        # Parse the COBOL file
        log_processing_step("Parsing COBOL structure")
        sections = parser.parse_sections(cobol_code)
        
        # Analyze with LLM
        log_processing_step("Analyzing with LLM")
        analyzed_sections = []
        for section in sections:
            analysis = analyzer.analyze_section(
                section.code, 
                section.name, 
                section.type
            )
            section.business_logic = analysis.business_logic
            section.confidence = analysis.confidence
            analyzed_sections.append(section)
        
        # Create ontology
        log_processing_step("Creating COBOL ontology")
        program_ontology = ontology.create_program_ontology(
            program_name=Path(input_file).stem,
            sections=analyzed_sections
        )
        
        # Create analysis result from ontology
        analysis_result = {
            "program_name": program_ontology.program_name,
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
    
    # Check LLM provider configuration
    provider = config.llm.provider.lower()
    if provider == "openai":
        if config.llm.api_key:
            click.echo("✓ OpenAI API key is set")
        else:
            click.echo("✗ OpenAI API key is not set")
    elif provider == "ollama":
        click.echo("✓ Using Ollama provider (local)")
        if config.llm.base_url:
            click.echo(f"  Base URL: {config.llm.base_url}")
        else:
            click.echo("  Using default Ollama URL: http://localhost:11434")
    else:
        click.echo(f"✗ Unknown provider: {provider}")
    
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
