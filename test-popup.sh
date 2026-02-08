#!/bin/bash
# Test popup via emacsclient
# Purpose: Verify popup works when called from terminal (how MCP server will use it)

set -e

PROJECT_DIR="/Users/marcosandrade/roaming/projects/MCP servers/ask-user-mcp"

echo "=== Popup emacsclient Integration Test ==="
echo ""

# First, ensure Emacs has loaded the popup module
echo "Loading popup module..."
emacsclient --eval "(load-file \"$PROJECT_DIR/emacs/ask-user-popup.el\")" 2>/dev/null || {
  echo "ERROR: Failed to load popup module. Is Emacs server running?"
  exit 1
}

echo "Loaded successfully."
echo ""

# Test 1: Basic popup (user should press q to cancel)
echo "Test 1: Basic popup - press q or C-g in Emacs to cancel"
echo "Command: emacsclient --eval '(mr-x/ask-user-popup \"What is your favorite color?\" \"Choose wisely\")'"
echo ""

result=$(emacsclient --eval "(mr-x/ask-user-popup \"What is your favorite color?\" \"Choose wisely\")" 2>&1) || exit_code=$?

echo "Result: $result"
echo "Exit code: ${exit_code:-0}"
echo ""

if [ "${exit_code:-0}" -eq 1 ]; then
  echo "✓ Test passed: Cancel returned error (exit code 1)"
else
  echo "✗ Test failed: Expected exit code 1, got ${exit_code:-0}"
fi

echo ""
echo "=== Test Complete ==="
echo "Expected behavior:"
echo "- Popup appears in Emacs at bottom of frame"
echo "- Terminal blocks until popup dismissed"
echo "- C-g or q cancels and returns exit code 1"
