#!/bin/bash

# Test runner script for COBOL Code Grapher
# This script runs all tests with proper configuration

set -e  # Exit on any error

echo "🧪 Running COBOL Code Grapher tests..."

# Check if virtual environment exists
if [ ! -d "venv" ]; then
    echo "❌ Virtual environment not found. Run setup_env.sh first."
    exit 1
fi

# Activate virtual environment
echo "🔧 Activating virtual environment..."
source venv/bin/activate

# Check if pytest is installed
if ! command -v pytest &> /dev/null; then
    echo "❌ pytest is not installed. Installing..."
    pip install pytest
fi

# Set environment variables for testing
export PYTHONPATH="${PYTHONPATH}:$(pwd)/src"
export LOG_LEVEL=DEBUG

# Run tests with coverage if available
if command -v pytest-cov &> /dev/null; then
    echo "📊 Running tests with coverage..."
    pytest tests/ --cov=src --cov-report=html --cov-report=term
else
    echo "📝 Running tests..."
    pytest tests/ -v
fi

# Check test results
if [ $? -eq 0 ]; then
    echo "✅ All tests passed!"
else
    echo "❌ Some tests failed. Check the output above."
    exit 1
fi

echo ""
echo "🎉 Test run complete!"
