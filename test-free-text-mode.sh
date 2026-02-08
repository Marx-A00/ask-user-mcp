#!/bin/bash

# Test suite for free-text mode (05-02)

echo "=== Free-text mode test suite ==="
echo ""

# Test helper: check emacsclient is available
if ! command -v emacsclient &> /dev/null; then
    echo "ERROR: emacsclient not found"
    exit 1
fi

# Load the module
echo "Loading ask-user-popup.el..."
emacsclient --eval '(load-file "emacs/ask-user-popup.el")' > /dev/null 2>&1

echo "Tests require manual interaction."
echo "Follow the instructions for each test."
echo ""
echo "Press Enter to continue..."
read

# Test 1: Options with text fallback
echo "Test 1: Options with text fallback"
echo "  1. Tab to text field"
echo "  2. Type 'Purple'"
echo "  3. Press RET"
echo "  Expected: Returns 'Purple'"
echo ""
echo "Press Enter to start test 1..."
read

RESULT=$(emacsclient --eval '(mr-x/ask-user-popup "Choose color" "Pick your favorite" (quote ("Red" "Green" "Blue")))')
echo "Result: $RESULT"
echo ""

# Test 2: Option selection ignores text
echo "Test 2: Option selection ignores text"
echo "  1. Tab to text field"
echo "  2. Type 'custom'"
echo "  3. Tab back to options"
echo "  4. Press RET on 'B'"
echo "  Expected: Returns 'B' (not 'custom')"
echo ""
echo "Press Enter to start test 2..."
read

RESULT=$(emacsclient --eval '(mr-x/ask-user-popup "Choose" nil (quote ("A" "B")))')
echo "Result: $RESULT"
echo ""

# Test 3: Pure text mode (no options)
echo "Test 3: Pure text mode (no options)"
echo "  1. Type a response"
echo "  2. Press C-c C-c"
echo "  Expected: Returns typed text"
echo ""
echo "Press Enter to start test 3..."
read

RESULT=$(emacsclient --eval '(mr-x/ask-user-popup "Describe issue" "Be detailed" nil)')
echo "Result: $RESULT"
echo ""

# Test 4: Multi-line text
echo "Test 4: Multi-line text"
echo "  1. Type 'Line 1'"
echo "  2. Press C-j"
echo "  3. Type 'Line 2'"
echo "  4. Press C-c C-c"
echo "  Expected: Returns text with newline preserved"
echo ""
echo "Press Enter to start test 4..."
read

RESULT=$(emacsclient --eval '(mr-x/ask-user-popup "Multi-line test" "Use C-j for newlines" nil)')
echo "Result: $RESULT"
echo ""

# Test 5: Cancel behavior
echo "Test 5: Cancel behavior"
echo "  1. Press C-g at any point"
echo "  Expected: Non-zero exit code"
echo ""
echo "Press Enter to start test 5..."
read

emacsclient --eval '(mr-x/ask-user-popup "Cancel test" "Press C-g" nil)'
EXIT_CODE=$?
echo "Exit code: $EXIT_CODE"
if [ $EXIT_CODE -ne 0 ]; then
    echo "✓ Cancel works correctly (non-zero exit)"
else
    echo "✗ Cancel failed (expected non-zero exit)"
fi
echo ""

echo "=== Test suite complete ==="
