---
phase: "02"
plan: "02"
subsystem: "error-handling"
tags: ["typescript", "emacsclient", "mcp", "errors", "timeouts"]

# Dependency graph
requires:
  - "02-01" # Logging infrastructure
provides:
  - "Error classification module"
  - "Configurable timeout support"
  - "MCP-compliant error responses"
affects:
  - "03-*" # Testing phase will test these error paths

# Tech tracking
tech-stack:
  added: []
  patterns:
    - "Error classification by pattern matching"
    - "Manual timeout tracking with process kill"
    - "Options object for function parameters"

# File tracking
key-files:
  created:
    - "src/errors.ts"
  modified:
    - "src/emacs-interface.ts"
    - "src/index.ts"

# Decisions log
decisions:
  - id: "ERR-CLASSIFY"
    choice: "Pattern-match stderr for error classification"
    reason: "emacsclient exit codes not sufficient, stderr patterns are distinctive"
  - id: "TIMEOUT-MANUAL"
    choice: "Manual setTimeout with SIGTERM instead of AbortController"
    reason: "Node.js signal option unreliable, manual tracking gives control"
  - id: "TIMEOUT-BOUNDS"
    choice: "30s-30min bounds for timeout_ms"
    reason: "Minimum prevents accidental instant timeout, max prevents indefinite hangs"

# Metrics
metrics:
  duration: "1 minute 46 seconds"
  completed: "2026-01-27"
---

# Phase 02 Plan 02: Error Handling & Configurable Timeouts Summary

**One-liner:** Error classification with actionable messages, configurable timeouts (30s-30min), and MCP-spec isError responses.

## What Was Built

### Error Classification Module (src/errors.ts)

The `classifyEmacsError(stderr, code)` function parses emacsclient errors to produce helpful, actionable messages:

- **Server not running:** Detects socket/server-start patterns, suggests `M-x server-start`
- **Permission denied:** Points to `~/.emacs.d/server/` directory permissions
- **Authentication failed:** Suggests checking `server-auth-dir` config
- **Command not found:** Exit code 127 or "not found" pattern, suggests PATH check
- **User cancelled:** Empty stderr with exit code 1 or "quit" pattern
- **Fallback:** Includes exit code and stderr for debugging

### Configurable Timeout (src/emacs-interface.ts)

- **AskOptions interface:** Optional `header` and `timeout_ms` parameters
- **Bounds validation:** 30 seconds minimum, 30 minutes maximum
- **Manual timeout tracking:** Uses `setTimeout` + `SIGTERM` (more reliable than AbortController signal)
- **Error integration:** Uses `classifyEmacsError` for all failure cases
- **Logging:** Debug logs at start/success, error logs on failure

### Tool Schema Updates (src/index.ts)

- **timeout_ms parameter:** Optional, validated with Zod (30000-1800000)
- **Empty question validation:** `z.string().min(1)` with custom message
- **isError flag:** Returns `isError: true` on failures per MCP specification
- **Options object:** Passes structured options to `askViaEmacs`

## Key Code Patterns

**Error classification (src/errors.ts:12-57):**
```typescript
export function classifyEmacsError(stderr: string, code: number): string {
  const lowerStderr = stderr.toLowerCase();
  
  if (lowerStderr.includes("can't find socket") || ...) {
    return "Emacs server isn't running. Start it with M-x server-start...";
  }
  // ... pattern matching continues
}
```

**Timeout handling (src/emacs-interface.ts:66-82):**
```typescript
let timedOut = false;
const timeoutTimer = setTimeout(() => {
  timedOut = true;
  proc.kill("SIGTERM");
}, timeout);
```

**MCP error response (src/index.ts:44-53):**
```typescript
return {
  isError: true,
  content: [{ type: "text", text: `Error: ${errorMessage}` }],
};
```

## Decisions Made

**1. Pattern-match stderr for error classification**
- emacsclient exit codes aren't distinctive enough
- stderr patterns are reliable and distinctive
- Enables actionable suggestions based on specific failure mode

**2. Manual setTimeout with SIGTERM**
- Node.js signal option for timeouts is unreliable
- Manual tracking gives explicit control over timeout behavior
- Cleanup is straightforward with clearTimeout on close

**3. 30s-30min timeout bounds**
- 30s minimum prevents accidental instant timeouts
- 30min maximum prevents indefinite hangs
- Default 5 minutes covers most use cases

## Deviations from Plan

None - plan executed exactly as written.

## Commits

- **ac36571:** feat(02-02): add error classification module for helpful error messages
- **e4ecf51:** feat(02-02): add configurable timeout and error classification to emacs-interface
- **47bb02c:** feat(02-02): update tool schema with timeout_ms parameter and isError flag

## Requirements Addressed

- **TOOL-04:** Optional timeout_ms parameter with bounds
- **EMACS-02:** Configurable timeout in emacs-interface
- **ERR-01:** Classify emacsclient errors with helpful messages
- **ERR-02:** Return isError flag per MCP specification

## Next Phase Readiness

Phase 2 complete. All error handling infrastructure in place:
- Logging (02-01)
- Graceful shutdown (02-01)
- Error classification (02-02)
- Configurable timeouts (02-02)
- MCP-compliant error responses (02-02)

Ready for Phase 3 (Testing & Packaging).
