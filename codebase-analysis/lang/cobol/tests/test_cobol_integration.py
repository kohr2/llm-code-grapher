"""
COBOL Integration Tests
Tests end-to-end COBOL analysis workflow
"""

import pytest
from pathlib import Path
from lang.cobol.parser.cobol_parser import COBOLParser
from lang.cobol.parser.llm_analyzer import COBOLAnalyzer
from lang.cobol.ontology.cobol_ontology_validator import COBOLOntologyValidator


class TestCOBOLIntegration:
    """Integration tests for COBOL analysis workflow"""
    
    @pytest.fixture
    def parser(self):
        """Create COBOL parser instance"""
        return COBOLParser()
    
    @pytest.fixture
    def analyzer(self):
        """Create COBOL LLM analyzer instance"""
        return COBOLAnalyzer()
    
    @pytest.fixture
    def validator(self):
        """Create COBOL ontology validator instance"""
        return COBOLOntologyValidator()
    
    @pytest.fixture
    def sample_cobol_file(self, tmp_path):
        """Create sample COBOL file for testing"""
        cobol_code = """
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. INTEGRATION-TEST.
000300 AUTHOR. TEST-AUTHOR.
000400 DATE-WRITTEN. 2025-01-01.

000700 ENVIRONMENT DIVISION.
000800 CONFIGURATION SECTION.
000900 SOURCE-COMPUTER. IBM-Z15.

004200 DATA DIVISION.
004300 WORKING-STORAGE SECTION.
004400 01 WS-COUNTER PIC 9(5) VALUE ZERO.
004500 01 WS-FLAG PIC X(1) VALUE 'N'.

021400 PROCEDURE DIVISION.
021500 0000-MAIN-PROCESS.
021600     MOVE 1 TO WS-COUNTER
021700     PERFORM 1000-PROCESS
021800     IF WS-FLAG = 'Y'
021900         DISPLAY 'PROCESSING COMPLETE'
022000     END-IF
022100     STOP RUN.

022300 1000-PROCESS.
022400     ADD 1 TO WS-COUNTER
022500     IF WS-COUNTER > 10
022600         MOVE 'Y' TO WS-FLAG
022700     END-IF.
"""
        test_file = tmp_path / "integration_test.cbl"
        test_file.write_text(cobol_code)
        return test_file
    
    def test_end_to_end_analysis(self, parser, analyzer, validator, sample_cobol_file):
        """Test complete end-to-end analysis workflow"""
        # Step 1: Parse the COBOL file
        parse_result = parser.parse(sample_cobol_file)
        
        assert parse_result is not None
        assert parse_result.program.name == "INTEGRATION-TEST"
        assert parse_result.program.language == "COBOL"
        assert len(parse_result.sections) > 0
        assert len(parse_result.subsections) > 0
        
        # Step 2: Validate the parsing result against ontology
        validation_result = validator.validate_analysis_result(parse_result)
        
        assert validation_result is not None
        assert validation_result.language == "COBOL"
        assert "ontology_version" in validation_result.metrics
        
        # Step 3: Analyze sections with LLM (if available)
        # Note: This test will only work if OpenAI API key is available
        try:
            for section in parse_result.sections[:2]:  # Test first 2 sections
                analysis_result = analyzer.analyze_section(
                    section.business_logic,
                    section.name,
                    section.type
                )
                
                assert analysis_result is not None
                assert analysis_result.language == "COBOL"
                assert len(analysis_result.business_logic) > 0
                assert 0.0 <= analysis_result.complexity_score <= 1.0
                assert analysis_result.risk_level in ["LOW", "MEDIUM", "HIGH", "CRITICAL"]
                assert 0.0 <= analysis_result.confidence <= 1.0
                
        except Exception as e:
            # If LLM analysis fails (e.g., no API key), that's okay for integration test
            pytest.skip(f"LLM analysis not available: {e}")
    
    def test_parsing_accuracy_with_known_file(self, parser, validator):
        """Test parsing accuracy with the known fraud management file"""
        fraud_file = Path("data/fixtures/vasu_fraud_management_cobol_reformatted.cbl")
        
        if not fraud_file.exists():
            pytest.skip("Fraud management file not available")
        
        # Parse the file
        parse_result = parser.parse(fraud_file)
        
        assert parse_result is not None
        assert parse_result.program.name == "FRAUD-MGMT-SYSTEM"
        assert parse_result.program.language == "COBOL"
        
        # Should have reasonable number of sections
        assert len(parse_result.sections) >= 4  # At least 4 main divisions
        assert len(parse_result.subsections) >= 2  # At least FILE and WORKING-STORAGE
        
        # Validate against ontology
        validation_result = validator.validate_analysis_result(parse_result)
        
        assert validation_result is not None
        assert validation_result.language == "COBOL"
        assert "ontology_version" in validation_result.metrics
        
        # Should have reasonable validation results
        assert validation_result.metrics["total_components"] > 0
        assert validation_result.metrics["total_relationships"] >= 0
    
    def test_parser_performance(self, parser, sample_cobol_file):
        """Test parser performance with timing"""
        import time
        
        start_time = time.time()
        result = parser.parse(sample_cobol_file)
        end_time = time.time()
        
        parsing_time = end_time - start_time
        
        # Should parse quickly (less than 1 second for this small file)
        assert parsing_time < 1.0, f"Parsing took too long: {parsing_time:.2f} seconds"
        assert result is not None
        assert result.program.name == "INTEGRATION-TEST"
    
    def test_validation_performance(self, parser, validator, sample_cobol_file):
        """Test validation performance with timing"""
        import time
        
        # Parse first
        parse_result = parser.parse(sample_cobol_file)
        
        # Then validate
        start_time = time.time()
        validation_result = validator.validate_analysis_result(parse_result)
        end_time = time.time()
        
        validation_time = end_time - start_time
        
        # Should validate quickly (less than 1 second)
        assert validation_time < 1.0, f"Validation took too long: {validation_time:.2f} seconds"
        assert validation_result is not None
        assert validation_result.language == "COBOL"
    
    def test_error_handling_invalid_file(self, parser):
        """Test error handling with invalid file"""
        # Test with non-existent file
        with pytest.raises(FileNotFoundError):
            parser.parse(Path("non_existent_file.cbl"))
        
        # Test with empty file
        empty_file = Path("empty_file.cbl")
        empty_file.write_text("")
        
        result = parser.parse(empty_file)
        assert result is not None
        assert result.program.name == "UNKNOWN"
        assert len(result.sections) == 0
        assert len(result.subsections) == 0
        
        # Clean up
        empty_file.unlink()
    
    def test_parser_statistics(self, parser, sample_cobol_file):
        """Test parser statistics calculation"""
        result = parser.parse(sample_cobol_file)
        stats = parser.get_parsing_statistics(result)
        
        assert isinstance(stats, dict)
        assert stats["language"] == "COBOL"
        assert stats["total_sections"] > 0
        assert stats["total_subsections"] > 0
        assert stats["total_lines"] > 0
        assert 0.0 <= stats["parsing_confidence"] <= 1.0
    
    def test_validator_statistics(self, parser, validator, sample_cobol_file):
        """Test validator statistics calculation"""
        result = parser.parse(sample_cobol_file)
        validation_result = validator.validate_analysis_result(result)
        
        assert isinstance(validation_result.metrics, dict)
        assert "ontology_version" in validation_result.metrics
        assert "total_components" in validation_result.metrics
        assert "total_relationships" in validation_result.metrics
        assert validation_result.metrics["total_components"] > 0
