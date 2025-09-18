# COBOL Program Ontology

This directory contains the formal ontology definition for COBOL program analysis and modernization.

**Note**: This is part of an extensible ontology framework. The parent `ontology/` directory is designed to support multiple programming languages and domains, with each domain having its own subdirectory (e.g., `ontology/java/`, `ontology/python/`, `ontology/legacy/`).

## Files

- `cobol_ontology.py` - Main ontology definition using Python interfaces
- `cobol_ontology_validator.py` - Python validator for ontology compliance
- `README.md` - This documentation file

## Overview

The COBOL Program Ontology provides a structured framework for describing:

- **Program Structure**: Divisions, sections, subsections, and their relationships
- **Data Elements**: Data items, variables, and their business meanings
- **Business Rules**: Rules implemented in the code and their priorities
- **Quality Metrics**: Complexity, maintainability, and risk assessments
- **Relationships**: How different program components interact

## Key Concepts

### Core Components
- **Program**: Complete COBOL program with metadata
- **Division**: Main COBOL divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- **Section**: Sections within divisions (FILE, WORKING-STORAGE, PROCEDURE_SECTION)
- **Subsection**: Paragraphs within sections
- **DataItem**: Data variables and structures
- **BusinessRule**: Business logic and rules
- **Relationship**: Connections between components

### Quality Assessment
- **Complexity Metrics**: Cyclomatic complexity, maintainability index
- **Risk Levels**: LOW, MEDIUM, HIGH, CRITICAL
- **Quality Indicators**: Code coverage, documentation quality
- **Modernization Potential**: LOW, MEDIUM, HIGH

## Usage

The ontology is used by the COBOL Code Grapher to:

1. **Structure Analysis**: Parse COBOL code into ontology concepts
2. **Relationship Mapping**: Identify connections between components
3. **Quality Assessment**: Calculate complexity and risk metrics
4. **Business Logic Extraction**: Understand what each component does
5. **Modernization Planning**: Identify areas for improvement

## Validation

The ontology includes validation rules to ensure:

- **Consistency**: All references are valid
- **Completeness**: All required properties are present
- **Accuracy**: Values are within valid ranges
- **Relationships**: All connections are meaningful

## Examples

See the `cobol_ontology.py` file for detailed examples of each concept type.

## Integration

This ontology integrates with:

- **Phase 1 MVP**: Basic structure and relationships
- **Phase 2**: Advanced analysis and visualization
- **Phase 3**: Modernization recommendations and migration planning

## Version History

- **v1.0**: Initial ontology definition for Phase 1 MVP
