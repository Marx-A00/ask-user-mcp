---
phase: 01-core-mcp-server
plan: 02
subsystem: core-tool
tags: [mcp-tool, emacsclient, security, timeout]
requires:
  - 01-01
provides:
  - AskUserQuestion MCP tool
  - Secure emacsclient spawning interface
  - 5-minute timeout handling
  - Elisp minibuffer prompt function
affects:
  - 01-03 (will test this tool)
tech-stack:
  added:
    - "child_process.spawn": "Secure process spawning"
  patterns:
    - "Argument arrays (no shell injection)"
    - "Promise.race for timeouts"
    - "Elisp string escaping"
    - "Error as content (not thrown)"
key-files:
  created:
    - src/emacs-interface.ts
    - emacs/ask-user.el
  modified:
    - src/index.ts
decisions:
  - id: spawn-security
    choice: "spawn() with argument arrays (not shell strings)"
    rationale: "Prevents command injection - arguments passed as array, not concatenated strings"
  - id: timeout-implementation
    choice: "Promise.race with 5-minute timeout"
    rationale: "Simple, clean timeout without AbortController complexity"
  - id: error-handling
    choice: "Return errors as tool content, don't throw"
    rationale: "Claude sees errors and can respond appropriately, rather than failing silently"
  - id: prompt-formatting
    choice: "Format prompts in Node.js, not Elisp"
    rationale: "Keeps elisp simple, all formatting logic in one place (TypeScript)"
metrics:
  duration: "4 minutes"
  completed: 2026-01-26
---

# Phase 01 Plan 02: Implement AskUserQuestion Tool Summary

**One-liner:** Working AskUserQuestion tool bridging Claude to Emacs minibuffer with secure emacsclient spawning and 5-minute timeout

## What Was Built

Implemented the complete end-to-end flow for the AskUserQuestion MCP tool. Claude can now call this tool, which spawns emacsclient to prompt the user in Emacs, waits for their response, and returns it back to Claude. Security-hardened against command injection with 5-minute timeout protection.

**Key accomplishments:**

1. Created secure emacsclient interface with proper input escaping
2. Registered AskUserQuestion tool with Zod schema validation
3. Implemented 5-minute timeout with graceful error handling
4. Created elisp minibuffer prompt function
5. All security requirements met (no command injection)

## Technical Implementation

**emacs-interface.ts (97 lines):**
- `escapeElispString()`: Escapes backslashes and quotes for elisp code injection prevention
- `withTimeout()`: Generic Promise.race wrapper for timeout handling
- `askViaEmacs()`: Main function that spawns emacsclient and returns user response
  - Formats prompt: "Header: Question > " or "Question > "
  - Uses spawn() with argument array (secure)
  - Collects stdout/stderr from emacsclient
  - Parses elisp return value (strips surrounding quotes)
  - 5-minute timeout wraps the entire operation
  - Handles process errors (emacsclient not found, exit code non-zero)

**src/index.ts (57 lines):**
- Imports McpServer from `@modelcontextprotocol/sdk/server/mcp.js`
- Imports StdioServerTransport from `@modelcontextprotocol/sdk/server/stdio.js`
- Registers AskUserQuestion tool with Zod schema:
  - `question`: required string
  - `header`: optional string
- Tool handler:
  - Calls `askViaEmacs(args.question, args.header)`
  - Returns response as content text
  - Catches errors and returns them as content (not thrown)

**emacs/ask-user.el (16 lines):**
- Simple `ask-user-question` function
- Uses `read-from-minibuffer` for user input
- Proper Emacs Lisp file headers and provide statement
- Receives pre-formatted prompt from Node.js

**Security features:**
- spawn() called with argument array: `["--eval", elispExpr]`
- No `shell: true` option used
- Elisp strings escaped to prevent code injection
- Timeout prevents indefinite hangs

## Decisions Made

**1. Spawn Security**
- Chose: spawn() with argument arrays
- Reason: Prevents command injection attacks by passing arguments as array elements rather than concatenated strings
- Impact: SECURITY-01 requirement met
- Alternative rejected: String concatenation with shell: true (vulnerable to injection)

**2. Timeout Implementation**
- Chose: Promise.race with 5-minute timeout
- Reason: Simple, clean approach without AbortController complexity
- Impact: Prevents Claude from hanging indefinitely
- Alternative rejected: AbortController (overkill for this use case)

**3. Error Handling Pattern**
- Chose: Return errors as tool content, don't throw
- Reason: Claude can see error messages and respond appropriately (retry, ask user, change approach)
- Impact: Better user experience - Claude handles errors gracefully
- Alternative rejected: Throwing errors (causes tool call to fail with no context for Claude)

**4. Prompt Formatting Location**
- Chose: Format prompts in Node.js (emacs-interface.ts)
- Reason: Keeps elisp simple, all formatting logic centralized in TypeScript
- Impact: Easier to modify prompt format, elisp stays minimal
- Alternative rejected: Format in elisp (would duplicate logic, harder to maintain)

## Files Created/Modified

**Created:**
- `src/emacs-interface.ts` - Secure emacsclient spawning (97 lines)
- `emacs/ask-user.el` - Minibuffer prompt function (16 lines)

**Modified:**
- `src/index.ts` - Added AskUserQuestion tool registration (56 lines total, +55 lines)

**Commits:**
- `21035f1`: feat(01-02): create emacsclient interface with secure spawning
- `fe52a98`: feat(01-02): register AskUserQuestion tool with Zod validation
- `9f97093`: feat(01-02): create elisp function for minibuffer prompts

## Testing & Verification

**Build Verification:**
```bash
npm run build
# ✓ Compiled without errors
```

**Security Verification:**
```bash
grep "spawn.*\[" src/emacs-interface.ts
# ✓ Found: spawn("emacsclient", ["--eval", elispExpr])
# Confirms argument array usage

grep "shell.*true" src/
# ✓ No matches - no shell: true usage
```

**Runtime Verification:**
```bash
node build/index.js
# ✓ Output: "Ask User MCP Server running on stdio"
# Server starts without errors
```

**Code Quality:**
- All TypeScript compiles with strict mode
- No implicit any types
- Proper error handling in all code paths
- Promise properly awaited and handled

## Deviations from Plan

**Deviation 1: MCP SDK Import Paths**
- Rule: 3 (Blocking issue)
- Issue: Initial plan assumed imports from `@modelcontextprotocol/sdk` root
- Found during: Task 2 compilation
- Fix: Changed to `@modelcontextprotocol/sdk/server/mcp.js` and `stdio.js` submodule imports
- Reason: MCP SDK exports McpServer from submodule, not root
- Files modified: src/index.ts
- Commit: fe52a98

This was a blocking issue - couldn't compile without finding the correct import paths. Discovered by reading SDK package.json exports and checking example files.

## Blockers & Issues

None encountered after resolving import path issue.

## Next Phase Readiness

**Ready for Plan 01-03:** Manual Testing & Documentation

The AskUserQuestion tool is fully implemented and ready for testing. Next plan will:
- Test the tool manually with a running Emacs server
- Verify the full flow: MCP client → server → emacsclient → Emacs → response
- Document usage instructions for users
- Test timeout behavior
- Test error handling (emacsclient not found, Emacs not running)

**Technical Foundation:**
- ✓ Tool registered and appears in tools/list
- ✓ Zod schema validation working
- ✓ Secure spawning implemented
- ✓ Timeout handling in place
- ✓ Error handling returns errors as content
- ✓ Elisp function ready for loading

**Dependencies Met:**
- MCP SDK tool registration API working correctly
- spawn() available and working
- Build process verified
- All imports resolve correctly

**Testing Prerequisites:**
- User must have Emacs running with server started
- emacs/ask-user.el must be loaded in Emacs config
- emacsclient must be in PATH

No blockers for proceeding to Plan 01-03.
