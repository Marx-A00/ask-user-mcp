---
phase: 02-error-handling-reliability
plan: 01
subsystem: logging-lifecycle
tags: [pino, logging, signals, cleanup, process-management]
dependency-graph:
  requires: ["01-01", "01-02"]
  provides: ["structured-logging", "graceful-shutdown", "process-tracking"]
  affects: ["02-02", "03-01"]
tech-stack:
  added: ["pino@10.x", "pino-pretty@13.x"]
  patterns: ["structured-json-logging", "signal-handlers", "child-process-tracking"]
key-files:
  created: ["src/logger.ts"]
  modified: ["src/index.ts", "src/emacs-interface.ts", "package.json"]
decisions:
  - id: "logger-stderr"
    summary: "Pino outputs to stderr (default), keeping stdout for JSON-RPC"
  - id: "level-string"
    summary: "Log level as string label (not numeric) for readability"
  - id: "no-pretty-transport"
    summary: "Use pipe to pino-pretty externally instead of in-code transport"
metrics:
  duration: "~1 minute"
  completed: "2026-01-27"
---

# Phase 02 Plan 01: Logging & Graceful Shutdown Summary

Pino JSON logger with configurable level; signal handlers kill tracked emacsclient processes on SIGINT/SIGTERM.

## What Was Built

### Logger Module (`src/logger.ts`)

- Pino logger instance with `process.env.LOG_LEVEL || 'info'`
- Level output as string label (not numeric code)
- Outputs to stderr (keeps stdout for JSON-RPC)
- Named and default exports for flexible imports

### Process Tracking (`src/emacs-interface.ts`)

- `activeProcesses` Set tracks spawned ChildProcess instances
- `getActiveProcesses()` export for signal handler access
- Processes added on spawn, removed on close/error
- Enables cleanup of pending emacsclient processes

### Signal Handlers (`src/index.ts`)

- `gracefulShutdown(signal)` function terminates active processes
- SIGINT and SIGTERM handlers installed after server connects
- Logs shutdown with signal name and process count
- Sets `process.exitCode = 0` for clean exit
- Replaced `console.error` with structured `logger.info/error`

## Technical Decisions

**Logger level as string:** Configured `formatters.level` to output `{"level":"info"}` instead of `{"level":30}` for human readability while retaining JSON structure.

**External pino-pretty:** Rather than configure pino-pretty as a transport (which adds complexity), installed as dev dependency for `npm run dev | pino-pretty` pattern.

**Process tracking Set:** Simple `Set<ChildProcess>` pattern allows O(1) add/delete and iteration for cleanup. Tracks all spawned processes even if multiple questions asked concurrently.

## Files Changed

**Created:**
- `src/logger.ts` - Pino logger module

**Modified:**
- `package.json` - Added pino, pino-pretty dependencies
- `src/emacs-interface.ts` - Process tracking, logger import
- `src/index.ts` - Signal handlers, logger usage

## Verification Results

- Build compiles cleanly
- JSON log output confirmed: `{"level":"info","time":...,"msg":"Ask User MCP Server running on stdio"}`
- Signal handlers installed (verified via code and grep)
- Process tracking Set operations confirmed

## Deviations from Plan

None - plan executed exactly as written.

## Requirements Addressed

- **ERR-03:** Structured logging (Pino JSON to stderr)
- **ERR-04:** Graceful shutdown (signal handlers, process cleanup)

## Next Plan Readiness

Ready for 02-02: Error handling improvements (input validation, defensive patterns, specific error types).
