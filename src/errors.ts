/**
 * Error classification for emacsclient failures.
 * Parses stderr and exit codes to produce helpful, actionable error messages.
 */

/**
 * Classify an emacsclient error and return a helpful message.
 * @param stderr - The stderr output from emacsclient
 * @param code - The exit code from emacsclient
 * @returns A user-friendly error message with suggested fixes
 */
export function classifyEmacsError(stderr: string, code: number): string {
  const lowerStderr = stderr.toLowerCase();

  // Check for server not running patterns
  if (
    lowerStderr.includes("can't find socket") ||
    lowerStderr.includes("cannot find socket") ||
    lowerStderr.includes("server-start") ||
    lowerStderr.includes("no socket")
  ) {
    return "Emacs server isn't running. Start it with M-x server-start or run: emacsclient -e '(server-start)'";
  }

  // Check for permission issues
  if (lowerStderr.includes("permission denied")) {
    return "Cannot connect to Emacs server socket. Check permissions on ~/.emacs.d/server/ directory.";
  }

  // Check for authentication failures
  if (lowerStderr.includes("authentication failed")) {
    return "Emacs server authentication failed. Check your server-auth-dir configuration.";
  }

  // Check for command not found
  if (
    code === 127 ||
    lowerStderr.includes("command not found") ||
    lowerStderr.includes("not found")
  ) {
    return "emacsclient command not found. Install Emacs or ensure emacsclient is in your PATH.";
  }

  // Check for user cancellation (C-g)
  const stderrTrimmed = stderr.trim();
  if (lowerStderr.includes("quit") || (code === 1 && stderrTrimmed === "")) {
    return "User cancelled the question.";
  }

  // Fallback with context
  if (stderrTrimmed) {
    return `emacsclient failed (exit code ${code}): ${stderrTrimmed}`;
  }

  return `emacsclient failed with exit code ${code}. Check that Emacs is running and accessible.`;
}
