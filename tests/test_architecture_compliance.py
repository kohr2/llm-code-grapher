"""
Architecture Compliance Tests

Tests to ensure the language-agnostic architecture is properly maintained.
No language-specific code should exist outside the lang/ directory.
"""

import pytest
import ast
import os
from pathlib import Path
from typing import List, Set, Tuple


class ArchitectureComplianceTest:
    """Test class for architecture compliance checks"""
    
    def __init__(self):
        self.project_root = Path(__file__).parent.parent
        self.lang_dir = self.project_root / "lang"
        self.src_dir = self.project_root / "src"
        self.tests_dir = self.project_root / "tests"
        
        # Define language-specific keywords that should only exist in lang/ directory
        self.language_keywords = {
            'cobol': ['cobol', 'COBOL', 'Cobol', 'cbl', 'CBL'],
            'java': ['java', 'Java', 'JAVA', 'class', 'Class', 'method', 'Method'],
            'python': ['python', 'Python', 'PYTHON', 'def', 'class', 'import'],
            'general': ['section', 'Section', 'subsection', 'Subsection', 'paragraph', 'Paragraph']
        }
        
        # Files that are allowed to contain language-specific references
        self.allowed_files = {
            'README.md',
            'docs/prd.md', 
            'docs/phase1.md',
            'docs/setup.md',
            'config.yaml',
            'requirements.txt',
            'setup.py',
            'main.py',  # Entry point can reference languages
            'tests/test_architecture_compliance.py',  # This test file
        }
        
        # Directories to exclude from scanning
        self.excluded_dirs = {
            'venv',
            '__pycache__',
            '.git',
            'node_modules',
            '.pytest_cache',
            'htmlcov',
            '.coverage'
        }

    def get_python_files(self, directory: Path) -> List[Path]:
        """Get all Python files in a directory, excluding excluded dirs"""
        python_files = []
        
        for root, dirs, files in os.walk(directory):
            # Remove excluded directories from dirs list to prevent walking into them
            dirs[:] = [d for d in dirs if d not in self.excluded_dirs]
            
            for file in files:
                if file.endswith('.py'):
                    python_files.append(Path(root) / file)
        
        return python_files

    def get_all_files(self, directory: Path) -> List[Path]:
        """Get all files in a directory, excluding excluded dirs"""
        all_files = []
        
        for root, dirs, files in os.walk(directory):
            # Remove excluded directories from dirs list to prevent walking into them
            dirs[:] = [d for d in dirs if d not in self.excluded_dirs]
            
            for file in files:
                all_files.append(Path(root) / file)
        
        return all_files

    def check_file_for_language_keywords(self, file_path: Path) -> List[Tuple[str, int, str]]:
        """Check a file for language-specific keywords and return violations"""
        violations = []
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')
                
                for line_num, line in enumerate(lines, 1):
                    for language, keywords in self.language_keywords.items():
                        for keyword in keywords:
                            if keyword in line:
                                # Check if this is a comment or string (less strict)
                                stripped_line = line.strip()
                                if (stripped_line.startswith('#') or 
                                    stripped_line.startswith('"""') or 
                                    stripped_line.startswith("'''") or
                                    '"""' in line or "'''" in line):
                                    continue
                                
                                # Check if it's in a docstring or comment
                                if ('"""' in line and line.find(keyword) > line.find('"""')) or \
                                   ("'''" in line and line.find(keyword) > line.find("'''")):
                                    continue
                                
                                violations.append((keyword, line_num, line.strip()))
        
        except Exception as e:
            violations.append((f"Error reading file: {e}", 0, ""))
        
        return violations

    def check_imports_for_language_specific_modules(self, file_path: Path) -> List[Tuple[str, int, str]]:
        """Check for imports of language-specific modules outside lang/ directory"""
        violations = []
        
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
                tree = ast.parse(content)
                
                for node in ast.walk(tree):
                    if isinstance(node, ast.Import):
                        for alias in node.names:
                            if self._is_language_specific_import(alias.name):
                                violations.append((
                                    f"Language-specific import: {alias.name}", 
                                    node.lineno, 
                                    f"import {alias.name}"
                                ))
                    
                    elif isinstance(node, ast.ImportFrom):
                        if node.module and self._is_language_specific_import(node.module):
                            violations.append((
                                f"Language-specific import: {node.module}", 
                                node.lineno, 
                                f"from {node.module} import ..."
                            ))
        
        except SyntaxError:
            # Skip files with syntax errors
            pass
        except Exception as e:
            violations.append((f"Error parsing file: {e}", 0, ""))
        
        return violations

    def _is_language_specific_import(self, module_name: str) -> bool:
        """Check if an import is language-specific"""
        language_specific_patterns = [
            'lang.cobol',
            'lang.java', 
            'lang.python',
            'cobol_parser',
            'cobol_analyzer',
            'cobol_ontology',
            'java_parser',
            'java_analyzer',
            'java_ontology',
            'python_parser',
            'python_analyzer',
            'python_ontology'
        ]
        
        return any(pattern in module_name for pattern in language_specific_patterns)

    def is_file_allowed(self, file_path: Path) -> bool:
        """Check if a file is allowed to contain language-specific references"""
        relative_path = file_path.relative_to(self.project_root)
        return str(relative_path) in self.allowed_files

    def is_in_lang_directory(self, file_path: Path) -> bool:
        """Check if a file is in the lang/ directory"""
        try:
            file_path.relative_to(self.lang_dir)
            return True
        except ValueError:
            return False


@pytest.fixture
def compliance_checker():
    """Fixture providing the architecture compliance checker"""
    return ArchitectureComplianceTest()


class TestArchitectureCompliance:
    """Test cases for architecture compliance"""
    
    def test_no_language_keywords_in_src_directory(self, compliance_checker):
        """Test that no language-specific keywords exist in src/ directory"""
        violations = []
        
        python_files = compliance_checker.get_python_files(compliance_checker.src_dir)
        
        for file_path in python_files:
            if compliance_checker.is_file_allowed(file_path):
                continue
                
            file_violations = compliance_checker.check_file_for_language_keywords(file_path)
            for keyword, line_num, line_content in file_violations:
                violations.append(f"{file_path}:{line_num} - '{keyword}' in: {line_content}")
        
        assert not violations, f"Language-specific keywords found in src/ directory:\n" + "\n".join(violations)

    def test_no_language_keywords_in_tests_directory(self, compliance_checker):
        """Test that no language-specific keywords exist in tests/ directory (except allowed files)"""
        violations = []
        
        python_files = compliance_checker.get_python_files(compliance_checker.tests_dir)
        
        for file_path in python_files:
            if compliance_checker.is_file_allowed(file_path):
                continue
                
            file_violations = compliance_checker.check_file_for_language_keywords(file_path)
            for keyword, line_num, line_content in file_violations:
                violations.append(f"{file_path}:{line_num} - '{keyword}' in: {line_content}")
        
        assert not violations, f"Language-specific keywords found in tests/ directory:\n" + "\n".join(violations)

    def test_no_language_specific_imports_in_src_directory(self, compliance_checker):
        """Test that no language-specific imports exist in src/ directory"""
        violations = []
        
        python_files = compliance_checker.get_python_files(compliance_checker.src_dir)
        
        for file_path in python_files:
            if compliance_checker.is_file_allowed(file_path):
                continue
                
            file_violations = compliance_checker.check_imports_for_language_specific_modules(file_path)
            for violation, line_num, line_content in file_violations:
                violations.append(f"{file_path}:{line_num} - {violation}: {line_content}")
        
        assert not violations, f"Language-specific imports found in src/ directory:\n" + "\n".join(violations)

    def test_no_language_specific_imports_in_tests_directory(self, compliance_checker):
        """Test that no language-specific imports exist in tests/ directory (except allowed files)"""
        violations = []
        
        python_files = compliance_checker.get_python_files(compliance_checker.tests_dir)
        
        for file_path in python_files:
            if compliance_checker.is_file_allowed(file_path):
                continue
                
            file_violations = compliance_checker.check_imports_for_language_specific_modules(file_path)
            for violation, line_num, line_content in file_violations:
                violations.append(f"{file_path}:{line_num} - {violation}: {line_content}")
        
        assert not violations, f"Language-specific imports found in tests/ directory:\n" + "\n".join(violations)

    def test_language_specific_code_only_in_lang_directory(self, compliance_checker):
        """Test that language-specific code only exists in lang/ directory"""
        violations = []
        
        # Check all Python files outside lang/ directory
        all_python_files = []
        all_python_files.extend(compliance_checker.get_python_files(compliance_checker.src_dir))
        all_python_files.extend(compliance_checker.get_python_files(compliance_checker.tests_dir))
        
        for file_path in all_python_files:
            if compliance_checker.is_file_allowed(file_path):
                continue
                
            if compliance_checker.is_in_lang_directory(file_path):
                continue
                
            # Check for language-specific keywords
            file_violations = compliance_checker.check_file_for_language_keywords(file_path)
            for keyword, line_num, line_content in file_violations:
                violations.append(f"{file_path}:{line_num} - Language-specific keyword '{keyword}': {line_content}")
            
            # Check for language-specific imports
            import_violations = compliance_checker.check_imports_for_language_specific_modules(file_path)
            for violation, line_num, line_content in import_violations:
                violations.append(f"{file_path}:{line_num} - {violation}: {line_content}")
        
        assert not violations, f"Language-specific code found outside lang/ directory:\n" + "\n".join(violations)

    def test_src_directory_is_language_agnostic(self, compliance_checker):
        """Test that src/ directory contains only language-agnostic code"""
        violations = []
        
        python_files = compliance_checker.get_python_files(compliance_checker.src_dir)
        
        for file_path in python_files:
            if compliance_checker.is_file_allowed(file_path):
                continue
                
            # Check for any language-specific references
            file_violations = compliance_checker.check_file_for_language_keywords(file_path)
            for keyword, line_num, line_content in file_violations:
                violations.append(f"{file_path}:{line_num} - Language-specific keyword '{keyword}': {line_content}")
            
            import_violations = compliance_checker.check_imports_for_language_specific_modules(file_path)
            for violation, line_num, line_content in import_violations:
                violations.append(f"{file_path}:{line_num} - {violation}: {line_content}")
        
        assert not violations, f"Language-specific code found in language-agnostic src/ directory:\n" + "\n".join(violations)

    def test_lang_directory_structure_compliance(self, compliance_checker):
        """Test that lang/ directory follows the expected structure"""
        violations = []
        
        # Check that lang/ directory exists
        assert compliance_checker.lang_dir.exists(), "lang/ directory does not exist"
        
        # Check for base/ directory
        base_dir = compliance_checker.lang_dir / "base"
        assert base_dir.exists(), "lang/base/ directory does not exist"
        
        # Check for language-specific directories
        expected_languages = ['cobol']  # Add more as they are implemented
        for lang in expected_languages:
            lang_dir = compliance_checker.lang_dir / lang
            assert lang_dir.exists(), f"lang/{lang}/ directory does not exist"
            
            # Check for required subdirectories
            required_subdirs = ['parser', 'ontology', 'tests']
            for subdir in required_subdirs:
                subdir_path = lang_dir / subdir
                assert subdir_path.exists(), f"lang/{lang}/{subdir}/ directory does not exist"

    def test_no_hardcoded_language_references(self, compliance_checker):
        """Test that there are no hardcoded language references in the codebase"""
        violations = []
        
        # Check all Python files
        all_python_files = []
        all_python_files.extend(compliance_checker.get_python_files(compliance_checker.src_dir))
        all_python_files.extend(compliance_checker.get_python_files(compliance_checker.tests_dir))
        
        # Hardcoded language references that should not exist
        hardcoded_patterns = [
            'COBOLParser',
            'COBOLAnalyzer', 
            'COBOLOntology',
            'JavaParser',
            'JavaAnalyzer',
            'JavaOntology',
            'PythonParser',
            'PythonAnalyzer',
            'PythonOntology'
        ]
        
        for file_path in all_python_files:
            if compliance_checker.is_file_allowed(file_path):
                continue
                
            if compliance_checker.is_in_lang_directory(file_path):
                continue
                
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    content = f.read()
                    lines = content.split('\n')
                    
                    for line_num, line in enumerate(lines, 1):
                        for pattern in hardcoded_patterns:
                            if pattern in line and not line.strip().startswith('#'):
                                violations.append(f"{file_path}:{line_num} - Hardcoded language reference '{pattern}': {line.strip()}")
            
            except Exception as e:
                violations.append(f"{file_path} - Error reading file: {e}")
        
        assert not violations, f"Hardcoded language references found:\n" + "\n".join(violations)


if __name__ == "__main__":
    # Run the tests directly
    pytest.main([__file__, "-v"])
