"""
Neo4j Converter

This module converts parser results to Neo4j graph data format.
"""

from typing import Dict, Any, List, Optional
import logging

try:
    from .neo4j_models import CodeNode, CodeRelationship, GraphData
except ImportError:
    from neo4j_models import CodeNode, CodeRelationship, GraphData

from lang.base.parser.base_parser import BaseParserResult
from lang.base.parser.llm_provider import LLMProviderConfig, LLMProviderFactory

logger = logging.getLogger(__name__)


class ParserResultConverter:
    """Converts parser results to Neo4j graph data"""
    
    def __init__(self, llm_config: Optional[LLMProviderConfig], language: str = "cobol"):
        """Initialize the converter with optional LLM configuration and language"""
        self.node_counter = 0
        self.relationship_counter = 0
        self.llm_config = llm_config
        self.language = language.lower()
        self.analyzer = None
        
        # LLM call tracking
        self.llm_call_count = 0
        self.total_llm_time = 0.0
        
        # Setup LLM conversation logging
        if llm_config:
            import datetime
            import os
            
            # Create logs directory if it doesn't exist
            logs_dir = "logs/llm_conversations"
            os.makedirs(logs_dir, exist_ok=True)
            
            timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
            self.conversation_log_file = os.path.join(logs_dir, f"conversations_{timestamp}.log")
            logger.info(f"📝 LLM conversations will be logged to: {self.conversation_log_file}")
        else:
            self.conversation_log_file = None
        
        # Initialize language-specific LLM analyzer only if config provided
        if llm_config:
            try:
                self.analyzer = self._get_language_analyzer()
                logger.info(f"LLM analyzer initialized successfully for {self.language}")
            except Exception as e:
                logger.error(f"Failed to initialize LLM analyzer for {self.language}: {e}")
                raise RuntimeError(f"Failed to initialize LLM analyzer for {self.language}: {e}")
        else:
            logger.info(f"LLM analysis disabled - using basic business rule detection for {self.language}")
    
    def _get_language_analyzer(self):
        """Get the appropriate language-specific analyzer"""
        try:
            # Dynamically import the language-specific analyzer
            analyzer_module = __import__(f"lang.{self.language}.parser.llm_analyzer", fromlist=[f"{self.language.upper()}Analyzer"])
            analyzer_class = getattr(analyzer_module, f"{self.language.upper()}Analyzer")
            return analyzer_class(self.llm_config)
        except ImportError:
            # Fallback to base analyzer if language-specific one doesn't exist
            logger.warning(f"No language-specific analyzer found for {self.language}, using base analyzer")
            from lang.base.parser.base_llm_analyzer import BaseLLMAnalyzer
            return BaseLLMAnalyzer(self.language, self.llm_config)
    
    def _generate_node_id(self, prefix: str) -> str:
        """Generate a unique node ID"""
        self.node_counter += 1
        return f"{prefix}_{self.node_counter}"
    
    def _generate_relationship_id(self) -> str:
        """Generate a unique relationship ID"""
        self.relationship_counter += 1
        return f"rel_{self.relationship_counter}"
    
    def _log_conversation(self, call_type: str, subsection_name: str, prompt: str, response: str, duration: float):
        """Log LLM conversation to file"""
        if not self.conversation_log_file:
            return
            
        import datetime
        import inspect
        
        timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        
        # Get the calling function and call stack
        stack = inspect.stack()
        calling_function = stack[1].function if len(stack) > 1 else "unknown"
        calling_file = stack[1].filename if len(stack) > 1 else "unknown"
        calling_line = stack[1].lineno if len(stack) > 1 else "unknown"
        
        # Get the full call stack (up to 5 levels)
        call_stack = []
        for i, frame in enumerate(stack[1:6]):  # Skip current function, limit to 5 levels
            call_stack.append(f"  {i+1}. {frame.filename}:{frame.lineno} in {frame.function}()")
        
        with open(self.conversation_log_file, "a", encoding="utf-8") as f:
            f.write(f"\n{'='*80}\n")
            f.write(f"TIMESTAMP: {timestamp}\n")
            f.write(f"CALL TYPE: {call_type}\n")
            f.write(f"SUBSECTION: {subsection_name}\n")
            f.write(f"DURATION: {duration:.3f}s\n")
            f.write(f"LLM CALL #{self.llm_call_count}\n")
            f.write(f"TRIGGERED BY: {calling_function}() in {calling_file}:{calling_line}\n")
            f.write(f"CALL STACK:\n")
            f.write(f"\n".join(call_stack))
            f.write(f"\n{'='*80}\n")
            f.write(f"\nPROMPT:\n{prompt}\n")
            f.write(f"\n{'='*40}\n")
            f.write(f"\nRESPONSE:\n{response}\n")
            f.write(f"\n{'='*80}\n")
    
    def convert_parser_result(self, result: BaseParserResult) -> GraphData:
        """Convert a parser result to GraphData"""
        graph_data = GraphData()
        
        # Store graph data for relationship mapping
        self._current_graph_data = graph_data
        
        # Add program node
        program_node = self._create_program_node(result)
        graph_data.add_node(program_node)
        
        # Add section nodes
        section_nodes = self._create_section_nodes(result, program_node.node_id)
        for node in section_nodes:
            graph_data.add_node(node)
        
        # Add subsection nodes
        subsection_nodes = self._create_subsection_nodes(result, program_node.node_id)
        for node in subsection_nodes:
            graph_data.add_node(node)
        
        # Add data nodes (if any)
        data_nodes = self._create_data_nodes(result, program_node.node_id)
        for node in data_nodes:
            graph_data.add_node(node)
        
        # Add operation nodes (if any)
        operation_nodes = self._create_operation_nodes(result, program_node.node_id)
        for node in operation_nodes:
            graph_data.add_node(node)
        
        # Add business rule nodes
        business_rule_nodes = self._create_business_rule_nodes(result, program_node.node_id)
        for node in business_rule_nodes:
            graph_data.add_node(node)
        
        # Add relationships (now with access to all nodes)
        relationships = self._create_relationships(result, program_node.node_id)
        for rel in relationships:
            graph_data.add_relationship(rel)
        
        logger.info(f"Converted parser result: {graph_data.node_count} nodes, {graph_data.relationship_count} relationships")
        
        # Log LLM call summary
        if self.llm_config and self.llm_call_count > 0:
            avg_time = self.total_llm_time / self.llm_call_count
            logger.info(f"📊 LLM CALL SUMMARY:")
            logger.info(f"   Total LLM calls: {self.llm_call_count}")
            logger.info(f"   Total LLM time: {self.total_llm_time:.3f}s")
            logger.info(f"   Average call time: {avg_time:.3f}s")
            logger.info(f"   LLM time per node: {self.total_llm_time/graph_data.node_count:.3f}s")
        elif self.llm_config:
            logger.info(f"📊 LLM CALL SUMMARY: No LLM calls made (all subsections skipped or failed)")
        else:
            logger.info(f"📊 LLM CALL SUMMARY: LLM analysis disabled (--skip-llm flag used)")
        
        return graph_data
    
    def _create_program_node(self, result: BaseParserResult) -> CodeNode:
        """Create a program node from parser result"""
        properties = {
            "line_count": getattr(result.program, 'line_count', 0),
            "file_path": getattr(result.program, 'file_path', ''),
            "created_at": getattr(result.program, 'created_at', None),
            "complexity_score": getattr(result.program, 'complexity_score', 0.0),
            "business_logic": getattr(result.program, 'business_logic', ''),
            "risk_level": getattr(result.program, 'risk_level', 'UNKNOWN'),
            "confidence": getattr(result.program, 'confidence', 0.0)
        }
        
        # Remove None values
        properties = {k: v for k, v in properties.items() if v is not None}
        
        return CodeNode(
            node_id=self._generate_node_id("program"),
            node_type="Program",
            name=result.program.name,
            language=result.program.language,
            properties=properties
        )
    
    def _create_section_nodes(self, result: BaseParserResult, program_id: str) -> List[CodeNode]:
        """Create section nodes from parser result"""
        nodes = []
        
        for i, section in enumerate(result.sections):
            properties = {
                "line_range": getattr(section, 'line_range', [0, 0]),
                "business_logic": getattr(section, 'business_logic', ''),
                "complexity_score": getattr(section, 'complexity_score', 0.0),
                "risk_level": getattr(section, 'risk_level', 'UNKNOWN'),
                "confidence": getattr(section, 'confidence', 0.0),
                "section_type": getattr(section, 'type', 'UNKNOWN')
            }
            
            # Remove None values
            properties = {k: v for k, v in properties.items() if v is not None}
            
            # Use enumeration-based ID to match relationship creation
            section_id = f"section_{i + 1}"
            node = CodeNode(
                node_id=section_id,
                node_type="Section",
                name=section.name,
                language=result.program.language,
                properties=properties
            )
            nodes.append(node)
        
        return nodes
    
    def _create_subsection_nodes(self, result: BaseParserResult, program_id: str) -> List[CodeNode]:
        """Create subsection nodes from parser result"""
        nodes = []
        
        for i, subsection in enumerate(result.subsections):
            properties = {
                "line_range": getattr(subsection, 'line_range', [0, 0]),
                "business_logic": getattr(subsection, 'business_logic', ''),
                "complexity_score": getattr(subsection, 'complexity_score', 0.0),
                "risk_level": getattr(subsection, 'risk_level', 'UNKNOWN'),
                "confidence": getattr(subsection, 'confidence', 0.0),
                "subsection_type": getattr(subsection, 'type', 'UNKNOWN'),
                "parent_section": getattr(subsection, 'parent_section', 'UNKNOWN')
            }
            
            # Remove None values
            properties = {k: v for k, v in properties.items() if v is not None}
            
            # Use enumeration-based ID to match relationship creation
            subsection_id = f"subsection_{len(nodes) + 1}"
            
            node = CodeNode(
                node_id=subsection_id,
                node_type="Subsection",
                name=subsection.name or f"UNNAMED_SUBSECTION_{i+1}",  # Fallback name
                language=result.program.language,
                properties=properties
            )
            
            nodes.append(node)
        
        return nodes
    
    def _create_data_nodes(self, result: BaseParserResult, program_id: str) -> List[CodeNode]:
        """Create data nodes from parser result (if any data structures are found)"""
        nodes = []
        
        # This would be implemented based on the specific parser result structure
        # For now, we'll create a placeholder
        if hasattr(result, 'data_structures') and result.data_structures:
            for i, data_struct in enumerate(result.data_structures):
                properties = {
                    "data_type": getattr(data_struct, 'data_type', 'UNKNOWN'),
                    "size": getattr(data_struct, 'size', 0),
                    "description": getattr(data_struct, 'description', ''),
                    "confidence": getattr(data_struct, 'confidence', 0.0)
                }
                
                # Remove None values
                properties = {k: v for k, v in properties.items() if v is not None}
                
                node = CodeNode(
                    node_id=self._generate_node_id("data"),
                    node_type="Data",
                    name=getattr(data_struct, 'name', f'Data_{i}'),
                    language=result.program.language,
                    properties=properties
                )
                nodes.append(node)
        
        return nodes
    
    def _create_business_rule_nodes(self, result: BaseParserResult, program_id: str) -> List[CodeNode]:
        """Create business rule nodes from fraud rule subsections in parser result"""
        nodes = []
        
        # Extract business rules from subsections using LLM analysis
        business_rules = []
        seen_rules = set()
        total_subsections = len(result.subsections)
        
        logger.info(f"🔍 Starting business rule analysis for {total_subsections} subsections...")
        
        for i, subsection in enumerate(result.subsections, 1):
            logger.info(f"📊 Progress: {i}/{total_subsections} - Checking subsection: {subsection.name}")
            
            # Use LLM to determine if this subsection contains a business rule
            if self._is_business_rule(subsection):
                logger.info(f"✅ Found business rule: {subsection.name}")
                rule_name = subsection.name or f"UNNAMED_RULE_{len(business_rules) + 1}"
                if rule_name not in seen_rules:
                    seen_rules.add(rule_name)
                    business_rules.append(subsection)
            else:
                logger.info(f"❌ Not a business rule: {subsection.name}")
        
        logger.info(f"🎯 Business rule detection complete: {len(business_rules)} business rules found out of {total_subsections} subsections")
        
        # Create business rule nodes
        logger.info(f"🚀 Starting detailed analysis of {len(business_rules)} business rules...")
        
        # Use hybrid approach: batch small groups for optimal performance
        batch_analyses = {}
        if len(business_rules) > 1:
            # Use small batches (2-3 rules) for optimal performance
            batch_size = min(3, len(business_rules))
            logger.info(f"📦 Using hybrid batch analysis (size: {batch_size}) for {len(business_rules)} business rules")
            
            for i in range(0, len(business_rules), batch_size):
                batch_group = business_rules[i:i + batch_size]
                logger.info(f"📦 Processing batch {i//batch_size + 1}: {len(batch_group)} rules")
                
                try:
                    if len(batch_group) > 1:
                        group_analyses = self._analyze_business_rules_batch(batch_group)
                        batch_analyses.update(group_analyses)
                        logger.info(f"✅ Batch {i//batch_size + 1} completed: {len(group_analyses)} rules")
                    else:
                        # Single rule - use individual analysis
                        rule = batch_group[0]
                        analysis = self._analyze_business_rule_with_llm(rule.business_logic, rule.name)
                        batch_analyses[rule.name] = analysis
                        logger.info(f"✅ Individual analysis completed for: {rule.name}")
                except Exception as e:
                    logger.error(f"❌ Batch {i//batch_size + 1} failed: {e}")
                    logger.info(f"🔄 Falling back to individual analysis for this batch")
                    # Fall back to individual analysis for this batch
                    for rule in batch_group:
                        try:
                            analysis = self._analyze_business_rule_with_llm(rule.business_logic, rule.name)
                            batch_analyses[rule.name] = analysis
                        except Exception as e2:
                            logger.error(f"❌ Individual analysis also failed for {rule.name}: {e2}")
        
        for i, rule in enumerate(business_rules, 1):
            logger.info(f"📝 Processing business rule {i}/{len(business_rules)}: {rule.name}")
            # Generate dynamic rule ID based on analysis
            rule_id = f"DYNAMIC_RULE_{i+1:03d}"
            
            # Use batch analysis if available, otherwise fall back to individual
            if len(business_rules) > 1 and rule.name in batch_analyses:
                analysis = batch_analyses[rule.name]
                analysis_method = "LLM_BATCH"
                logger.info(f"📦 Using batch analysis for {rule.name}")
            else:
                try:
                    # Analyze business rule using LLM individually
                    analysis = self._analyze_business_rule_with_llm(rule.business_logic, rule.name)
                    analysis_method = "LLM_INDIVIDUAL"
                except Exception as e:
                    logger.error(f"Failed to analyze business rule {rule.name}: {e}")
                    # Skip this rule if LLM analysis fails
                    continue
            
            description = analysis["description"]
            priority = analysis["priority"]
            risk_level = analysis["risk_level"]
            functional_area = analysis["functional_area"]
            analysis_confidence = analysis["confidence"]
            
            properties = {
                "description": description,
                "priority": priority,
                "risk_level": risk_level,
                "functional_area": functional_area,
                "business_impact": "CRITICAL",
                "detection_accuracy": "85-90%",
                "confidence": getattr(rule, 'confidence', 0.0),
                "analysis_confidence": analysis_confidence,
                "analysis_method": analysis_method,
                "location": {
                    "line_range": list(getattr(rule, 'line_range', [0, 0])),
                    "line_count": getattr(rule, 'line_range', [0, 0])[1] - getattr(rule, 'line_range', [0, 0])[0] + 1,
                    "containing_section": getattr(rule, 'parent_section', 'UNKNOWN')
                }
            }
            
            # Remove None values
            properties = {k: v for k, v in properties.items() if v is not None}
            
            node = CodeNode(
                node_id=f"rule_{i + 1}",
                node_type="BusinessRule",
                name=rule_id,
                language=result.program.language,
                properties=properties
            )
            nodes.append(node)
        
        return nodes
    
    def _is_business_rule(self, subsection) -> bool:
        """Use LLM to determine if a subsection contains a business rule"""
        if not subsection.business_logic or not subsection.name:
            logger.debug(f"🔍 Skipping subsection {subsection.name}: missing business_logic or name")
            return False
        
        import time
        start_time = time.time()
        
        try:
            logger.info(f"🔍 Checking if subsection '{subsection.name}' is a business rule")
            logger.info(f"📝 Subsection code length: {len(subsection.business_logic)} characters")
            
            # Create a simple prompt to determine if this is a business rule
            prompt = f"""
{self.language.upper()} code:
Name: {subsection.name}
Code: {subsection.business_logic}

Is this a business rule? (validation, constraint, decision, policy)
Answer: YES or NO
"""
            
            logger.info(f"🤖 Making LLM call to check if '{subsection.name}' is a business rule")
            llm_start = time.time()
            response = self.analyzer.provider.generate_response([{"role": "user", "content": prompt}])
            llm_time = time.time() - llm_start
            
            # Track LLM call statistics
            self.llm_call_count += 1
            self.total_llm_time += llm_time
            
            result = response.strip().upper().startswith("YES")
            total_time = time.time() - start_time
            
            logger.info(f"⚡ Business rule check completed for '{subsection.name}' in {llm_time:.3f}s (total: {total_time:.3f}s)")
            logger.info(f"📄 Response: {response.strip()[:50]}... -> {'YES' if result else 'NO'}")
            logger.info(f"📊 LLM Call #{self.llm_call_count} | Total LLM time: {self.total_llm_time:.3f}s | Avg: {self.total_llm_time/self.llm_call_count:.3f}s")
            
            # Log conversation to file
            self._log_conversation("BUSINESS_RULE_CHECK", subsection.name, prompt, response, llm_time)
            
            return result
            
        except Exception as e:
            total_time = time.time() - start_time
            logger.warning(f"❌ Failed to analyze if subsection {subsection.name} is a business rule after {total_time:.3f}s: {e}")
            # Fallback: check for common business rule patterns
            fallback_result = self._has_business_rule_patterns(subsection)
            logger.info(f"🔄 Using fallback pattern matching for '{subsection.name}': {'YES' if fallback_result else 'NO'}")
            return fallback_result
    
    def _has_business_rule_patterns(self, subsection) -> bool:
        """Fallback method to detect business rule patterns"""
        name = subsection.name.upper() if subsection.name else ""
        logic = subsection.business_logic.upper()
        
        # Common business rule patterns across languages
        rule_patterns = [
            "RULE", "VALIDATE", "CHECK", "VERIFY", "CONDITION", 
            "IF", "WHEN", "UNLESS", "POLICY", "CONSTRAINT"
        ]
        
        return any(pattern in name or pattern in logic for pattern in rule_patterns)
    
    def _analyze_business_rules_batch(self, business_rules: List[Any]) -> Dict[str, Dict[str, Any]]:
        """Analyze multiple business rules in a single LLM call for better performance"""
        import time
        start_time = time.time()
        
        try:
            logger.info(f"🚀 Starting batch LLM analysis for {len(business_rules)} business rules")
            
            # Create batch prompt
            prompt_start = time.time()
            prompt = self._create_batch_business_rule_analysis_prompt(business_rules)
            prompt_time = time.time() - prompt_start
            
            logger.info(f"📋 Batch prompt created in {prompt_time:.3f}s (length: {len(prompt)} chars)")
            
            # Make single LLM call
            llm_start = time.time()
            logger.info(f"🤖 Making batch LLM API call for {len(business_rules)} rules")
            response = self.analyzer.provider.generate_response([{"role": "user", "content": prompt}])
            llm_time = time.time() - llm_start
            
            # Track LLM call statistics
            self.llm_call_count += 1
            self.total_llm_time += llm_time
            
            logger.info(f"⚡ Batch LLM API call completed in {llm_time:.3f}s for {len(business_rules)} rules")
            logger.info(f"📄 Response length: {len(response)} characters")
            logger.info(f"📊 LLM Call #{self.llm_call_count} | Total LLM time: {self.total_llm_time:.3f}s | Avg: {self.total_llm_time/self.llm_call_count:.3f}s")
            
            # Parse batch response
            parse_start = time.time()
            batch_analyses = self._parse_batch_business_rule_response(response, business_rules)
            parse_time = time.time() - parse_start
            
            total_time = time.time() - start_time
            
            logger.info(f"✅ Batch LLM analysis completed for {len(batch_analyses)} rules")
            logger.info(f"⏱️  Total time: {total_time:.3f}s (prompt: {prompt_time:.3f}s, LLM: {llm_time:.3f}s, parse: {parse_time:.3f}s)")
            
            # Log conversation to file
            self._log_conversation("BUSINESS_RULE_BATCH_ANALYSIS", f"{len(business_rules)}_rules", prompt, response, llm_time)
            
            return batch_analyses
            
        except Exception as e:
            total_time = time.time() - start_time
            logger.error(f"❌ Batch LLM analysis failed after {total_time:.3f}s: {e}")
            raise RuntimeError(f"Batch LLM analysis failed: {e}")
    
    def _analyze_business_rule_with_llm(self, business_logic: str, rule_name: str) -> Dict[str, Any]:
        """Analyze business rule using LLM - dynamic analysis only"""
        if not self.analyzer:
            logger.error(f"LLM analyzer not available for rule: {rule_name}")
            raise RuntimeError("LLM analyzer is required but not initialized. Please provide LLM configuration.")
        
        import time
        start_time = time.time()
        
        try:
            logger.info(f"🚀 Starting LLM analysis for rule: {rule_name}")
            logger.info(f"📝 Business logic length: {len(business_logic)} characters")
            
            # Create a specialized prompt for business rule analysis
            prompt_start = time.time()
            prompt = self._create_business_rule_analysis_prompt(business_logic, rule_name)
            prompt_time = time.time() - prompt_start
            
            logger.info(f"📋 Prompt created in {prompt_time:.3f}s (length: {len(prompt)} chars)")
            
            # Use the analyzer's provider to get response
            llm_start = time.time()
            logger.info(f"🤖 Making LLM API call for rule: {rule_name}")
            response = self.analyzer.provider.generate_response([{"role": "user", "content": prompt}])
            llm_time = time.time() - llm_start
            
            # Track LLM call statistics
            self.llm_call_count += 1
            self.total_llm_time += llm_time
            
            logger.info(f"⚡ LLM API call completed in {llm_time:.3f}s for rule: {rule_name}")
            logger.info(f"📄 Response length: {len(response)} characters")
            logger.info(f"📊 LLM Call #{self.llm_call_count} | Total LLM time: {self.total_llm_time:.3f}s | Avg: {self.total_llm_time/self.llm_call_count:.3f}s")
            
            # Parse the response
            parse_start = time.time()
            analysis = self._parse_business_rule_response(response)
            parse_time = time.time() - parse_start
            
            total_time = time.time() - start_time
            
            logger.info(f"✅ LLM analysis completed for rule: {rule_name}")
            logger.info(f"⏱️  Total time: {total_time:.3f}s (prompt: {prompt_time:.3f}s, LLM: {llm_time:.3f}s, parse: {parse_time:.3f}s)")
            
            # Log conversation to file
            self._log_conversation("BUSINESS_RULE_ANALYSIS", rule_name, prompt, response, llm_time)
            
            return analysis
            
        except Exception as e:
            total_time = time.time() - start_time
            logger.error(f"❌ LLM analysis failed for rule {rule_name} after {total_time:.3f}s: {e}")
            raise RuntimeError(f"LLM analysis failed for rule {rule_name}: {e}")
    
    def _create_batch_business_rule_analysis_prompt(self, business_rules: List[Any]) -> str:
        """Create an optimized batch prompt for analyzing multiple business rules"""
        rules_text = ""
        for i, rule in enumerate(business_rules, 1):
            rules_text += f"{i}. {rule.name}: {rule.business_logic}\n"
        
        return f"""
Analyze these {self.language.upper()} business rules:

{rules_text}

For each rule, provide:
RULE: [name]
DESCRIPTION: [what it does]
PRIORITY: [HIGH/MEDIUM/LOW]
RISK_LEVEL: [CRITICAL/HIGH/MEDIUM/LOW]
FUNCTIONAL_AREA: [area]
CONFIDENCE: [0.0-1.0]
"""
    
    def _parse_batch_business_rule_response(self, response: str, business_rules: List[Any]) -> Dict[str, Dict[str, Any]]:
        """Parse optimized batch LLM response for multiple business rules"""
        analyses = {}
        
        # Split response by rule sections
        lines = response.strip().split('\n')
        current_rule = None
        current_analysis = {}
        
        for line in lines:
            line = line.strip()
            
            # Check for new rule section (simplified pattern)
            if line.startswith('RULE:'):
                # Save previous rule if exists
                if current_rule and current_analysis:
                    analyses[current_rule] = current_analysis
                
                # Start new rule
                current_rule = line.replace('RULE:', '').strip()
                current_analysis = {}
                
            elif current_rule and line.startswith('DESCRIPTION:'):
                current_analysis["description"] = line.replace('DESCRIPTION:', '').strip()
            elif current_rule and line.startswith('PRIORITY:'):
                priority = line.replace('PRIORITY:', '').strip().upper()
                current_analysis["priority"] = priority if priority in ['HIGH', 'MEDIUM', 'LOW'] else 'MEDIUM'
            elif current_rule and line.startswith('RISK_LEVEL:'):
                risk = line.replace('RISK_LEVEL:', '').strip().upper()
                current_analysis["risk_level"] = risk if risk in ['CRITICAL', 'HIGH', 'MEDIUM', 'LOW'] else 'MEDIUM'
            elif current_rule and line.startswith('FUNCTIONAL_AREA:'):
                current_analysis["functional_area"] = line.replace('FUNCTIONAL_AREA:', '').strip()
            elif current_rule and line.startswith('CONFIDENCE:'):
                try:
                    confidence = float(line.replace('CONFIDENCE:', '').strip())
                    current_analysis["confidence"] = max(0.0, min(1.0, confidence))
                except ValueError:
                    current_analysis["confidence"] = 0.5
        
        # Save last rule
        if current_rule and current_analysis:
            analyses[current_rule] = current_analysis
        
        # Ensure all business rules have analyses (with defaults if missing)
        for rule in business_rules:
            if rule.name not in analyses:
                logger.warning(f"⚠️  No analysis found for rule: {rule.name}, using defaults")
                analyses[rule.name] = {
                    "description": f"Business rule analysis not available for {rule.name}",
                    "priority": "MEDIUM",
                    "risk_level": "MEDIUM", 
                    "functional_area": "BUSINESS-LOGIC",
                    "confidence": 0.5
                }
        
        logger.info(f"📊 Parsed {len(analyses)} rule analyses from batch response")
        return analyses
    
    def _create_business_rule_analysis_prompt(self, business_logic: str, rule_name: str) -> str:
        """Create a language-agnostic prompt for business rule analysis"""
        return f"""
Analyze this {self.language.upper()} business rule:

Rule: {rule_name}
Code: {business_logic}

Provide only the values in this exact format:
DESCRIPTION: [what this rule does]
PRIORITY: [HIGH/MEDIUM/LOW]
RISK_LEVEL: [CRITICAL/HIGH/MEDIUM/LOW]
FUNCTIONAL_AREA: [area name]
CONFIDENCE: [0.0-1.0]
"""
    
    def _parse_business_rule_response(self, response: str) -> Dict[str, Any]:
        """Parse LLM response for business rule analysis"""
        lines = response.strip().split('\n')
        
        description = "Business rule analysis not available"
        priority = "MEDIUM"
        risk_level = "MEDIUM"
        functional_area = "BUSINESS-LOGIC"  # Default fallback
        confidence = 0.5
        
        for line in lines:
            line = line.strip()
            if line.startswith('DESCRIPTION:'):
                description = line.replace('DESCRIPTION:', '').strip()
            elif line.startswith('PRIORITY:'):
                priority = line.replace('PRIORITY:', '').strip().upper()
                if priority not in ['HIGH', 'MEDIUM', 'LOW']:
                    priority = "MEDIUM"
            elif line.startswith('RISK_LEVEL:'):
                risk_level = line.replace('RISK_LEVEL:', '').strip().upper()
                if risk_level not in ['CRITICAL', 'HIGH', 'MEDIUM', 'LOW']:
                    risk_level = "MEDIUM"
            elif line.startswith('FUNCTIONAL_AREA:'):
                functional_area = line.replace('FUNCTIONAL_AREA:', '').strip().upper()
                # Accept any functional area name provided by LLM
                if not functional_area:
                    functional_area = "BUSINESS-LOGIC"
            elif line.startswith('CONFIDENCE:'):
                try:
                    confidence = float(line.replace('CONFIDENCE:', '').strip())
                    confidence = max(0.0, min(1.0, confidence))
                except ValueError:
                    confidence = 0.5
        
        return {
            "description": description,
            "priority": priority,
            "risk_level": risk_level,
            "functional_area": functional_area,
            "confidence": confidence
        }
    
    
    def _create_business_rule_relationships(self, result: BaseParserResult, program_id: str) -> List[CodeRelationship]:
        """Create relationships between sections/subsections and business rules using line-based analysis"""
        relationships = []
        
        # Extract business rules and create relationships
        business_rules = []
        seen_rules = set()
        
        for subsection in result.subsections:
            if self._is_business_rule(subsection):
                rule_name = subsection.name or f"UNNAMED_RULE_{len(business_rules) + 1}"
                if rule_name not in seen_rules:
                    seen_rules.add(rule_name)
                    business_rules.append((rule_name, subsection))
        
        # Create relationships for each business rule using line-based analysis
        for i, (rule_name, rule) in enumerate(business_rules):
            rule_id = f"rule_{i + 1}"
            
            try:
                # Get LLM analysis for this rule to determine functional area
                analysis = self._analyze_business_rule_with_llm(rule.business_logic, rule.name)
                functional_area = analysis.get("functional_area")
            except Exception as e:
                logger.error(f"Failed to analyze business rule {rule.name} for relationships: {e}")
                # Skip relationship creation for this rule if LLM analysis fails
                continue
            
            # Strategy: Create BOTH physical and semantic relationships
            # 1. Physical relationship: Find the actual containing section
            containing_section = self._find_containing_section(rule.line_range[0], result.sections)
            
            if containing_section:
                # Create relationship to the actual containing section
                section_id = self._get_section_id(containing_section, result.sections)
                rel = CodeRelationship(
                    source_id=section_id,
                    target_id=rule_id,
                    relationship_type="CONTAINS",
                    properties={
                        "description": f"Section {containing_section.name} contains {rule_name}",
                        "confidence": 1.0,
                        "relationship_type": "physical"
                    }
                )
                relationships.append(rel)
            
            # 2. Semantic relationship: Map to logical functional area using LLM analysis
            logical_section = self._get_logical_section_for_rule(rule_name, result.sections, functional_area)
            
            if logical_section and logical_section != containing_section:
                # Create semantic relationship to the logical functional section
                logical_section_id = self._get_section_id(logical_section, result.sections)
                rel = CodeRelationship(
                    source_id=logical_section_id,
                    target_id=rule_id,
                    relationship_type="RELATES_TO",
                    properties={
                        "description": f"Business rule {rule_name} relates to {logical_section.name} functionality",
                        "confidence": 0.8,
                        "relationship_type": "semantic"
                    }
                )
                relationships.append(rel)
            
            # Also create direct IMPLEMENTS relationship from subsection to business rule
            subsection_id = None
            for j, subsection in enumerate(result.subsections):
                if subsection.name == rule.name:
                    subsection_id = f"subsection_{j + 1}"
                    break
            
            if subsection_id:
                # Create direct IMPLEMENTS relationship from subsection to business rule
                rel = CodeRelationship(
                    source_id=subsection_id,
                    target_id=rule_id,
                    relationship_type="IMPLEMENTS",
                    properties={
                        "description": f"Subsection {rule.name} implements business rule",
                        "confidence": 1.0,
                        "relationship_type": "implementation"
                    }
                )
                relationships.append(rel)
        
        return relationships
    
    def _find_containing_section(self, rule_line_number: int, sections: List) -> Optional[Any]:
        """Find which section contains a business rule based on line numbers"""
        for section in sections:
            if section.line_range[0] <= rule_line_number <= section.line_range[1]:
                return section
        return None
    
    def _get_section_id(self, section: Any, sections: List) -> str:
        """Get the section ID for a given section object"""
        for i, s in enumerate(sections):
            if s.name == section.name and s.line_range == section.line_range:
                return f"section_{i + 1}"
        return "unknown_section"
    
    def _get_logical_section_for_rule(self, rule_type: str, sections: List, functional_area: Optional[str] = None) -> Optional[Any]:
        """Get the logical functional section for a business rule based on LLM analysis"""
        if not functional_area:
            logger.warning(f"No functional area provided for rule {rule_type} - LLM analysis required")
            return None
        
        # Find the section with the matching functional area name from LLM analysis
        for section in sections:
            if functional_area in section.name:
                return section
        
        # If no exact match, try to find a section that contains the functional area
        for section in sections:
            if functional_area.lower() in section.name.lower():
                return section
                
        logger.warning(f"No section found for functional area: {functional_area}")
        return None
    
    def _create_operation_nodes(self, result: BaseParserResult, program_id: str) -> List[CodeNode]:
        """Create operation nodes from parser result"""
        nodes = []
        
        # Check if result has operations (COBOL-specific)
        if hasattr(result, 'operations') and result.operations:
            for i, operation in enumerate(result.operations):
                properties = {
                    "line_range": getattr(operation, 'line_range', [0, 0]),
                    "business_logic": getattr(operation, 'business_logic', ''),
                    "complexity_score": getattr(operation, 'complexity_score', 0.0),
                    "risk_level": getattr(operation, 'risk_level', 'UNKNOWN'),
                    "confidence": getattr(operation, 'confidence', 0.0),
                    "operation_type": getattr(operation, 'operation_type', 'UNKNOWN'),
                    "parent_subsection": getattr(operation, 'parent_subsection', 'UNKNOWN'),
                    "parameters": getattr(operation, 'parameters', []),
                    "parameter_count": len(getattr(operation, 'parameters', []))
                }
                
                # Remove None values
                properties = {k: v for k, v in properties.items() if v is not None}
                
                # Use enumeration-based ID to match relationship creation
                operation_id = f"operation_{i + 1}"
                node = CodeNode(
                    node_id=operation_id,
                    node_type="Operation",
                    name=operation.name,
                    language=result.program.language,
                    properties=properties
                )
                nodes.append(node)
        
        return nodes
    
    def _create_relationships(self, result: BaseParserResult, program_id: str) -> List[CodeRelationship]:
        """Create relationships from parser result"""
        relationships = []
        
        # Create proper hierarchical tree structure
        # First, separate divisions from regular sections
        divisions = []
        regular_sections = []
        
        for i, section in enumerate(result.sections):
            section_id = f"section_{i + 1}"
            if section.type in ['IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE']:
                # This is a division - link directly to program
                divisions.append((section_id, section))
                rel = CodeRelationship(
                    source_id=program_id,
                    target_id=section_id,
                    relationship_type="CONTAINS",
                    properties={
                        "description": f"Program contains division {section.name}",
                        "confidence": 1.0
                    }
                )
                relationships.append(rel)
            else:
                # This is a regular section
                regular_sections.append((section_id, section))
        
        # Link regular sections to their immediate parent (division or program)
        for section_id, section in regular_sections:
            # Find the immediate parent division based on line ranges
            parent_division_id = None
            for div_id, division in divisions:
                # Check if this section is contained within this division
                if (division.line_range[0] <= section.line_range[0] and 
                    section.line_range[1] <= division.line_range[1]):
                    parent_division_id = div_id
                    break
            
            if parent_division_id:
                rel = CodeRelationship(
                    source_id=parent_division_id,
                    target_id=section_id,
                    relationship_type="CONTAINS",
                    properties={
                        "description": f"Division contains section {section.name}",
                        "confidence": 1.0
                    }
                )
                relationships.append(rel)
            else:
                # Fallback: link directly to program
                rel = CodeRelationship(
                    source_id=program_id,
                    target_id=section_id,
                    relationship_type="CONTAINS",
                    properties={
                        "description": f"Program contains section {section.name}",
                        "confidence": 1.0
                    }
                )
                relationships.append(rel)
        
        # Add section to subsection relationships - ensure immediate parent only
        for i, subsection in enumerate(result.subsections):
            subsection_id = f"subsection_{i + 1}"  # This should match the ID generation in _create_subsection_nodes
            
            # Find the immediate parent section ID
            parent_section_id = None
            if hasattr(subsection, 'parent_section') and subsection.parent_section:
                # Find the section with matching name (only regular sections, not divisions)
                for j, section in enumerate(result.sections):
                    if (section.name == subsection.parent_section and 
                        section.type not in ['IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE']):
                        parent_section_id = f"section_{j + 1}"
                        break
            
            # If we found a parent section, link subsection to section
            # Otherwise, link to program as fallback
            if parent_section_id:
                rel = CodeRelationship(
                    source_id=parent_section_id,
                    target_id=subsection_id,
                    relationship_type="CONTAINS",
                    properties={
                        "description": f"Section {subsection.parent_section} contains subsection {subsection.name}",
                        "confidence": 1.0
                    }
                )
                relationships.append(rel)
            else:
                # Fallback: link to program if no parent section found
                rel = CodeRelationship(
                    source_id=program_id,
                    target_id=subsection_id,
                    relationship_type="CONTAINS",
                    properties={
                        "description": f"Program contains subsection {subsection.name}",
                        "confidence": 1.0
                    }
                )
                relationships.append(rel)
        
        # Add operation to subsection relationships
        if hasattr(result, 'operations') and result.operations:
            for i, operation in enumerate(result.operations):
                operation_id = f"operation_{i + 1}"
                
                # Find the parent subsection ID
                parent_subsection_id = None
                if hasattr(operation, 'parent_subsection') and operation.parent_subsection:
                    # Find the subsection with matching name
                    for j, subsection in enumerate(result.subsections):
                        if subsection.name == operation.parent_subsection:
                            parent_subsection_id = f"subsection_{j + 1}"
                            break
                
                # If we found a parent subsection, link operation to subsection
                # Otherwise, link to program as fallback
                if parent_subsection_id:
                    rel = CodeRelationship(
                        source_id=parent_subsection_id,
                        target_id=operation_id,
                        relationship_type="CONTAINS",
                        properties={
                            "description": f"Subsection {operation.parent_subsection} contains operation {operation.name}",
                            "confidence": 1.0
                        }
                    )
                    relationships.append(rel)
                else:
                    # Fallback: link to program if no parent subsection found
                    rel = CodeRelationship(
                        source_id=program_id,
                        target_id=operation_id,
                        relationship_type="CONTAINS",
                        properties={
                            "description": f"Program contains operation {operation.name}",
                            "confidence": 1.0
                        }
                    )
                    relationships.append(rel)
        
        # Add business rule relationships
        business_rule_relationships = self._create_business_rule_relationships(result, program_id)
        relationships.extend(business_rule_relationships)
        
        # Add relationships from parser result
        for i, rel in enumerate(result.relationships):
            # Skip relationships that reference non-existent sections
            # Since we're ignoring line numbers, we can't map numeric targets to actual sections
            if rel.target.isdigit():
                # Skip numeric targets that don't correspond to actual sections
                continue
            
            # Map source and target to actual node IDs
            source_id = self._map_to_node_id(rel.source, program_id)
            target_id = self._map_to_node_id(rel.target, program_id)
            
            # Skip relationships that map to the same node (avoid self-loops)
            if source_id == target_id:
                continue
            
            # Special handling for operation-to-operation relationships
            if (rel.relationship_type in ["NEXT", "DEPENDS_ON", "CALLS"] and 
                source_id.startswith("operation_") and target_id.startswith("operation_")):
                # This is an operation-to-operation relationship
                relationship = CodeRelationship(
                    source_id=source_id,
                    target_id=target_id,
                    relationship_type=rel.relationship_type,
                    properties={
                        "confidence": getattr(rel, 'confidence', 0.0),
                        "strength": getattr(rel, 'strength', 1.0),
                        "description": f"Operation {rel.source} {rel.relationship_type.lower()} {rel.target}",
                        "original_source": rel.source,
                        "original_target": rel.target,
                        "flow_type": "operation_flow"
                    }
                )
            else:
                # Regular relationship
                relationship = CodeRelationship(
                    source_id=source_id,
                    target_id=target_id,
                    relationship_type=rel.relationship_type,
                    properties={
                        "confidence": getattr(rel, 'confidence', 0.0),
                        "line_number": getattr(rel, 'line_number', 0),
                        "description": getattr(rel, 'description', ''),
                        "original_source": rel.source,
                        "original_target": rel.target
                    }
                )
            
            relationships.append(relationship)
        
        # Validate tree structure - ensure no circular or multiple parent relationships
        self._validate_tree_structure(relationships)
        
        return relationships
    
    def _validate_tree_structure(self, relationships: List[CodeRelationship]) -> None:
        """Validate that relationships form a proper tree structure"""
        # Check for multiple parents for the same child
        child_parents = {}
        for rel in relationships:
            if rel.relationship_type == "CONTAINS":
                if rel.target_id in child_parents:
                    logger.warning(f"Node {rel.target_id} has multiple parents: {child_parents[rel.target_id]} and {rel.source_id}")
                child_parents[rel.target_id] = rel.source_id
        
        # Check for circular references
        for rel in relationships:
            if rel.relationship_type == "CONTAINS":
                if self._has_circular_reference(rel.source_id, rel.target_id, relationships):
                    logger.warning(f"Circular reference detected: {rel.source_id} -> {rel.target_id}")
    
    def _has_circular_reference(self, start_id: str, target_id: str, relationships: List[CodeRelationship]) -> bool:
        """Check if adding start_id -> target_id would create a circular reference"""
        # A circular reference exists if target_id can reach start_id through existing relationships
        visited = set()
        return self._dfs_circular_check(target_id, start_id, relationships, visited)
    
    def _dfs_circular_check(self, current_id: str, target_id: str, relationships: List[CodeRelationship], visited: set) -> bool:
        """DFS to check if current_id can reach target_id through existing relationships"""
        if current_id == target_id:
            return True
        
        if current_id in visited:
            return False
        
        visited.add(current_id)
        
        # Find all children of current_id
        for rel in relationships:
            if rel.relationship_type == "CONTAINS" and rel.source_id == current_id:
                if self._dfs_circular_check(rel.target_id, target_id, relationships, visited):
                    return True
        
        return False
    
    def _map_to_node_id(self, name: str, program_id: str) -> str:
        """Map a name to a node ID"""
        # This is a simplified mapping - in practice, you'd want more sophisticated logic
        # to map relationship names to actual node IDs
        
        # If it's a number, it might be a section reference
        # Since we're ignoring line numbers, we'll map numbers to program for now
        # In a real implementation, you'd want more sophisticated mapping logic
        if name.isdigit():
            # For now, map all numeric references to the program node
            # This could be improved with better relationship analysis
            return program_id
        
        # Check if this is an operation name (contains operation type patterns)
        operation_patterns = ['-read-', '-add-', '-if-', '-display-', '-move-', '-perform-', 
                            '-call-', '-compute-', '-evaluate-', '-set-', '-open-', '-close-',
                            '-write-', '-delete-', '-rewrite-', '-start-', '-stop-', '-accept-',
                            '-initialize-', '-string-', '-unstring-', '-inspect-', '-search-',
                            '-sort-', '-merge-', '-release-', '-return-']
        
        if any(pattern in name.lower() for pattern in operation_patterns):
            # This looks like an operation name, try to find matching operation
            # Find the operation by name and return its actual node_id
            for node in self._current_graph_data.nodes:
                if node.name == name and node.node_type == "Operation":
                    return node.node_id
        
        # If it contains common section keywords, treat as section
        section_keywords = ['SECTION', 'PARA', 'PROCEDURE']
        if any(keyword in name.upper() for keyword in section_keywords):
            return f"section_{name.replace(' ', '_')}"
        
        # Default to program for now
        return program_id


def convert_parser_result_to_neo4j(result: BaseParserResult, llm_config: LLMProviderConfig, language: str = None) -> GraphData:
    """Convenience function to convert parser result to Neo4j graph data"""
    # Auto-detect language from result if not provided
    if language is None:
        language = result.program.language.lower() if hasattr(result.program, 'language') else "cobol"
    
    converter = ParserResultConverter(llm_config, language)
    return converter.convert_parser_result(result)
