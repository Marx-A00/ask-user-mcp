#!/bin/bash
# Test script for selection mode (Phase 5 Task 1)

echo "Testing selection mode with options..."
echo "This will display a popup with 3 color options."
echo "Please verify:"
echo "  1. Options display as numbered list (1. Red, 2. Green, 3. Blue)"
echo "  2. First option is highlighted with inverse colors"
echo "  3. Press q to close and verify"
echo ""
echo "Starting test..."

cd "$(dirname "$0")"

# Load the popup file and display test
emacsclient --eval '(progn 
  (load-file "emacs/ask-user-popup.el")
  (mr-x/ask-user-popup "Choose color" "Pick your favorite" (quote ("Red" "Green" "Blue"))))'

echo ""
echo "Test complete."
