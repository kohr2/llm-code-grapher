#!/usr/bin/env python3
"""
Architecture Compliance Checker

Checks that no language-specific code exists outside the lang/ directory.
This ensures the language-agnostic architecture is properly maintained.
"""

import ast
import os
from pathlib import Path
from typing import List, Set, Tuple


class ArchitectureComplianceChecker:
    """Checker for architecture compliance"""
    
    def __init__(self):
        self.project_root = Path(__file__).parent
        self.lang_dir = self.project_root / "lang"
        self.src_dir = self.project_root / "src"
        self.tests_dir = self.project_root / "tests"
        
        # Define language-specific keywords that should only exist in lang/ directory
        # Exclude generic Python keywords that are not language-specific
        self.language_keywords = {
            'cobol': ['cobol', 'COBOL', 'Cobol', 'cbl', 'CBL', 'PERFORM', 'SECTION', 'PARAGRAPH'],
            'java': ['java', 'Java', 'JAVA', 'public class', 'private class', 'protected class'],
            'python': ['python', 'Python', 'PYTHON', 'def main', 'if __name__'],
            'general': ['section', 'Section', 'subsection', 'Subsection', 'paragraph', 'Paragraph']
        }
        
        # Generic Python keywords that should be ignored
        self.ignored_keywords = {
            'import', 'class', 'def', 'method', 'function', 'return', 'if', 'else', 'for', 'while',
            'try', 'except', 'finally', 'with', 'as', 'from', 'in', 'is', 'and', 'or', 'not',
            'True', 'False', 'None', 'self', 'super', 'lambda', 'yield', 'pass', 'break', 'continue'
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
            'check_architecture.py',  # This checker file
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
                                # Skip if it's a generic Python keyword
                                if keyword.lower() in self.ignored_keywords:
                                    continue
                                    
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

    def run_compliance_check(self) -> bool:
        """Run the complete compliance check and return True if compliant"""
        print("🔍 Running Architecture Compliance Check...")
        print("=" * 60)
        
        all_violations = []
        
        # Check src/ directory
        print("\n📁 Checking src/ directory...")
        python_files = self.get_python_files(self.src_dir)
        src_violations = []
        
        for file_path in python_files:
            if self.is_file_allowed(file_path):
                continue
                
            file_violations = self.check_file_for_language_keywords(file_path)
            for keyword, line_num, line_content in file_violations:
                violation = f"  ❌ {file_path}:{line_num} - '{keyword}' in: {line_content}"
                src_violations.append(violation)
                all_violations.append(violation)
            
            import_violations = self.check_imports_for_language_specific_modules(file_path)
            for violation, line_num, line_content in import_violations:
                violation_msg = f"  ❌ {file_path}:{line_num} - {violation}: {line_content}"
                src_violations.append(violation_msg)
                all_violations.append(violation_msg)
        
        if src_violations:
            print(f"Found {len(src_violations)} violations in src/ directory:")
            for violation in src_violations:
                print(violation)
        else:
            print("  ✅ src/ directory is compliant")
        
        # Check tests/ directory
        print("\n📁 Checking tests/ directory...")
        python_files = self.get_python_files(self.tests_dir)
        test_violations = []
        
        for file_path in python_files:
            if self.is_file_allowed(file_path):
                continue
                
            file_violations = self.check_file_for_language_keywords(file_path)
            for keyword, line_num, line_content in file_violations:
                violation = f"  ❌ {file_path}:{line_num} - '{keyword}' in: {line_content}"
                test_violations.append(violation)
                all_violations.append(violation)
            
            import_violations = self.check_imports_for_language_specific_modules(file_path)
            for violation, line_num, line_content in import_violations:
                violation_msg = f"  ❌ {file_path}:{line_num} - {violation}: {line_content}"
                test_violations.append(violation_msg)
                all_violations.append(violation_msg)
        
        if test_violations:
            print(f"Found {len(test_violations)} violations in tests/ directory:")
            for violation in test_violations:
                print(violation)
        else:
            print("  ✅ tests/ directory is compliant")
        
        # Summary
        print("\n" + "=" * 60)
        if all_violations:
            print(f"❌ Architecture compliance check FAILED")
            print(f"Found {len(all_violations)} violations total")
            print("\n💡 All language-specific code should be in the lang/ directory")
            print("💡 The src/ directory should contain only language-agnostic code")
            return False
        else:
            print("✅ Architecture compliance check PASSED")
            print("🎉 All language-specific code is properly contained in lang/ directory")
            return True


def main():
    """Main entry point"""
    checker = ArchitectureComplianceChecker()
    success = checker.run_compliance_check()
    
    if not success:
        exit(1)
    else:
        exit(0)


if __name__ == "__main__":
    main()
