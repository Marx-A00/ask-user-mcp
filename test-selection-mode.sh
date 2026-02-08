#!/bin/bash
# Test script for selection mode end-to-end (Phase 5 Task 3)

set -e

cd "$(dirname "$0")"

echo "=========================================="
echo "Task 3: Selection Mode End-to-End Test"
echo "=========================================="
echo ""

# Test 1: Basic selection
echo "Test 1: Basic selection with 3 options"
echo "----------------------------------------"
echo "Instructions:"
echo "  1. Verify popup shows 3 numbered options (Red, Green, Blue)"
echo "  2. Press C-n to navigate to Green"
echo "  3. Press C-n again to navigate to Blue"
echo "  4. Press RET to confirm"
echo "Expected: Script should print 'Blue'"
echo ""
read -p "Press Enter to start test 1..."

RESULT=$(emacsclient --eval '(progn 
  (load-file "emacs/ask-user-popup.el")
  (mr-x/ask-user-popup "Choose color" "Pick your favorite" (quote ("Red" "Green" "Blue"))))')

echo ""
echo "Result: $RESULT"
echo ""

# Test 2: Wrap-around navigation
echo "Test 2: Wrap-around navigation"
echo "----------------------------------------"
echo "Instructions:"
echo "  1. Verify popup shows 2 options (A, B)"
echo "  2. Press C-p to navigate backwards (should wrap to B)"
echo "  3. Press C-n to navigate forward (should wrap to A)"
echo "  4. Press RET to confirm A"
echo "Expected: Script should print 'A'"
echo ""
read -p "Press Enter to start test 2..."

RESULT=$(emacsclient --eval '(mr-x/ask-user-popup "Choose letter" nil (quote ("A" "B")))')

echo ""
echo "Result: $RESULT"
echo ""

# Test 3: Cancel behavior
echo "Test 3: Cancel behavior"
echo "----------------------------------------"
echo "Instructions:"
echo "  1. Verify popup shows 2 options"
echo "  2. Press q to cancel"
echo "Expected: Script should show error and non-zero exit code"
echo ""
read -p "Press Enter to start test 3..."

if emacsclient --eval '(mr-x/ask-user-popup "Choose" nil (quote ("Option 1" "Option 2")))' 2>&1; then
  echo "ERROR: Should have returned non-zero exit code"
  exit 1
else
  echo "✓ Correctly returned non-zero exit code on cancel"
fi

echo ""
echo "=========================================="
echo "All tests complete!"
echo "=========================================="
