"""
Command Line Interface for LLM Code Grapher

Provides CLI functionality for analyzing code structure and generating outputs.
Supports multiple programming languages through language-agnostic architecture.
"""

import click
import sys
import logging
from pathlib import Path
from typing import Optional

from .config_manager import get_config, reload_config
from .utils import read_file_safely, log_processing_step, detect_language_from_extension
from .output_generator import generate_output
from .neo4j_converter import convert_parser_result_to_neo4j
from .neo4j_database import create_neo4j_database, Neo4jConfig


@click.group()
@click.option('--config', '-c', help='Path to configuration file')
@click.option('--verbose', '-v', is_flag=True, help='Enable verbose output')
@click.pass_context
def cli(ctx, config: Optional[str], verbose: bool):
    """LLM Code Grapher - Analyze code structure using LLMs"""
    ctx.ensure_object(dict)
    ctx.obj['verbose'] = verbose
    
    # Configure logging based on verbose flag
    if verbose:
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s',
            handlers=[logging.StreamHandler(sys.stdout)]
        )
    else:
        logging.basicConfig(
            level=logging.WARNING,
            format='%(levelname)s - %(message)s',
            handlers=[logging.StreamHandler(sys.stdout)]
        )
    
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
@click.option('--neo4j', is_flag=True, help='Insert results into Neo4j database (reads credentials from .env)')
@click.option('--clear-db', is_flag=True, help='Clear Neo4j database before inserting')
@click.option('--skip-llm', is_flag=True, help='Skip LLM analysis in Neo4j conversion (faster, less detailed)')
@click.option('--structured-analysis', is_flag=True, help='Use structured LLM analysis for complete file (requires API key)')
@click.pass_context
def analyze(ctx, input_file: str, language: Optional[str], output_dir: Optional[str], 
           output_format: str, confidence_threshold: float,
           provider: Optional[str], model: Optional[str], base_url: Optional[str],
           neo4j: bool, clear_db: bool, skip_llm: bool, structured_analysis: bool):
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
        
        parser = ParserClass(config.__dict__)
        analyzer = AnalyzerClass(config.llm)
        ontology = OntologyClass()
        
        # Parse the code file
        log_processing_step(f"Parsing {language.upper()} structure")
        result = parser.parse(Path(input_file))
        sections = result.sections
        
        # Analyze with LLM based on strategy
        if structured_analysis:
            log_processing_step("Performing structured LLM analysis on complete file")
            
            # Validate LLM configuration for structured analysis
            if not config.llm.api_key:
                click.echo("Error: LLM API key is required for structured analysis", err=True)
                click.echo("Please set OPENAI_API_KEY environment variable or configure in config.yaml", err=True)
                sys.exit(1)
            
            try:
                # Perform structured analysis on the complete file
                structured_result = analyzer.analyze_complete_file_structured(code_content, result.program.name)
                
                # Convert structured result to parser result format
                analyzed_sections = []
                for section_data in structured_result.get("sections", []):
                    # Create section object from structured data
                    from lang.cobol.parser.cobol_parser import COBOLSection
                    section = COBOLSection(
                        name=section_data.get("name", "UNKNOWN"),
                        type=section_data.get("type", "UNKNOWN"),
                        line_range=section_data.get("line_range", (0, 0)),
                        business_logic=section_data.get("business_logic", ""),
                        confidence=section_data.get("confidence", 0.5),
                        complexity_score=section_data.get("complexity_score", 0.5),
                        risk_level=section_data.get("risk_level", "LOW")
                    )
                    analyzed_sections.append(section)
                
                # Update result with structured analysis
                result.sections = analyzed_sections
                
                # Store structured result for output generation
                result.structured_analysis = structured_result
                
                log_processing_step(f"Structured analysis completed: {len(structured_result.get('sections', []))} sections, {len(structured_result.get('business_rules', []))} business rules")
                
            except Exception as e:
                log_processing_step(f"Structured analysis failed: {e}")
                click.echo(f"Warning: Structured analysis failed, falling back to basic analysis: {e}", err=True)
                
                # Fallback to basic analysis
                analyzed_sections = []
                for section in sections:
                    analyzed_sections.append(section)
                result.sections = analyzed_sections
        else:
            log_processing_step("Using basic parser analysis (no LLM)")
            analyzed_sections = []
            for section in sections:
                # Use the existing business_logic from parser
                # The section already has basic business logic extracted
                analyzed_sections.append(section)
            
            # Update result with analyzed sections
            result.sections = analyzed_sections
        
        # Create analysis result from parser result
        analysis_result = {
            "program_name": result.program.name,
            "language": language.upper(),
            "total_sections": len(result.sections),
            "total_subsections": len(result.subsections),
            "sections": [
                {
                    "name": section.name,
                    "type": getattr(section, 'type', 'UNKNOWN'),
                    "line_range": getattr(section, 'line_range', [0, 0]),
                    "line_count": getattr(section, 'line_count', 0),
                    "business_logic": getattr(section, 'business_logic', ''),
                    "confidence": getattr(section, 'confidence', 0.0),
                    "complexity_score": getattr(section, 'complexity_score', 0.0),
                    "risk_level": getattr(section, 'risk_level', 'LOW')
                }
                for section in result.sections
            ],
            "relationships": [
                {
                    "source": rel.source,
                    "target": rel.target,
                    "relationship_type": rel.relationship_type,
                    "confidence": getattr(rel, 'confidence', 0.0),
                    "strength": getattr(rel, 'strength', 0.0)
                }
                for rel in result.relationships
            ],
            "confidence_threshold": confidence_threshold
        }
        
        # Add structured analysis data if available
        if hasattr(result, 'structured_analysis') and result.structured_analysis:
            analysis_result["structured_analysis"] = result.structured_analysis
            analysis_result["analysis_strategy"] = "structured_llm"
            analysis_result["business_rules"] = result.structured_analysis.get("business_rules", [])
            analysis_result["data_elements"] = result.structured_analysis.get("data_elements", [])
            analysis_result["overview"] = result.structured_analysis.get("overview", "")
        else:
            analysis_result["analysis_strategy"] = "parser_only"
        
        # Generate outputs
        log_processing_step("Generating outputs")
        output_generator = generate_output(analysis_result, Path(input_file).stem, output_dir)
        
        if verbose:
            click.echo(f"Analysis complete. Generated {len(output_generator)} output files.")
        
        # Neo4j integration
        if neo4j:
            try:
                log_processing_step("Converting to Neo4j format")
                
                # Use the actual parser result for Neo4j conversion
                # Update result with additional data
                result.program.name = analysis_result["program_name"]
                result.program.language = language.upper()
                result.program.line_count = len(code_content.split('\n'))
                
                # Convert to Neo4j format with LLM configuration
                from lang.base.parser.llm_provider import LLMProviderConfig
                
                if skip_llm:
                    # Skip LLM analysis - use None config for faster conversion
                    llm_config = None
                    log_processing_step("Skipping LLM analysis for faster Neo4j conversion")
                else:
                    llm_config = LLMProviderConfig(
                        provider=config.llm.provider,
                        model=config.llm.model,
                        api_key=config.llm.api_key,
                        base_url=config.llm.base_url,
                        max_tokens=config.llm.max_tokens,
                        temperature=config.llm.temperature
                    )
                    
                    # Validate LLM configuration
                    if not llm_config.api_key:
                        click.echo("Error: LLM API key is required for Neo4j conversion with business rule analysis", err=True)
                        click.echo("Please set OPENAI_API_KEY environment variable or configure in config.yaml", err=True)
                        click.echo("Alternatively, use --skip-llm flag for faster conversion without LLM analysis", err=True)
                        sys.exit(1)
                
                graph_data = convert_parser_result_to_neo4j(result, llm_config, language)
                
                log_processing_step("Connecting to Neo4j database")
                db = create_neo4j_database()
                
                with db:
                    if clear_db:
                        log_processing_step("Clearing Neo4j database")
                        db.clear_database()
                    
                    log_processing_step(f"Inserting {graph_data.node_count} nodes and {graph_data.relationship_count} relationships into Neo4j")
                    db.insert_graph_data(graph_data)
                    
                    if verbose:
                        click.echo(f"Neo4j insertion complete:")
                        click.echo(f"  - Nodes: {graph_data.node_count}")
                        click.echo(f"  - Relationships: {graph_data.relationship_count}")
                
                click.echo("✅ Neo4j insertion completed successfully!")
                
            except Exception as neo4j_error:
                log_processing_step(f"Neo4j insertion failed: {neo4j_error}")
                click.echo(f"⚠️  Neo4j insertion failed: {neo4j_error}", err=True)
                if verbose:
                    import traceback
                    click.echo(traceback.format_exc(), err=True)
                # Don't exit on Neo4j failure, just warn
        
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