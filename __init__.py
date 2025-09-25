"""
Neo4j Graph Analyzer Tool

A comprehensive tool for analyzing Neo4j graph structures starting from Program nodes.
Provides hierarchical analysis, relationship mapping, and database switching capabilities.
"""

from .graph_analyzer import Neo4jGraphAnalyzer, GraphNode

__version__ = "1.0.0"
__author__ = "LLM Code Grapher Team"

__all__ = ['Neo4jGraphAnalyzer', 'GraphNode']
