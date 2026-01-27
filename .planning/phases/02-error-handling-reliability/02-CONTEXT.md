# Phase 2: Error Handling & Reliability - Context

**Gathered:** 2026-01-26
**Status:** Ready for planning

<domain>
## Phase Boundary

Server handles real-world failure modes gracefully without crashes or zombie processes. Includes detecting Emacs unavailability, signal handling, input validation, timeout messaging, and structured logging.

</domain>

<decisions>
## Implementation Decisions

### Error Messages
- Helpful & conversational tone — "Emacs server isn't running. Start it with M-x server-start"
- Include specific fix commands when known — exact commands like "Run: emacsclient -e (server-start)"
- Return errors as tool result content — Claude sees the full error message (matches Phase 1 approach)
- Distinguish recoverable vs fix-required — "try again" vs "fix required" errors

### Logging Format
- JSON structured format — `{"level":"error","msg":"...","ts":"..."}`
- Standard 4 levels — error, warn, info, debug
- Environment variable control — LOG_LEVEL=debug
- Rich context — Include tool name, question snippet, timing

### Timeout Behavior
- Keep 5 minute default — Good balance for thinking time
- Per-call override — Optional timeout_ms parameter in tool call
- Explain what happened — "Question timed out after 5 minutes waiting for response"
- Parameter limits — Min 30 seconds, max 30 minutes

### Graceful Degradation
- Return cancellation message — "User cancelled the question" on C-g
- Best effort shutdown — Kill child processes, exit immediately on SIGINT/SIGTERM
- Return validation errors — "Missing required field: question" for malformed input
- Specific error detection — Distinguish "Emacs not running" vs "socket permission denied" vs "emacsclient not found"
- Parse stderr — Give better error messages based on emacsclient stderr output
- No circuit breaker — Each call is independent, always try
- Log shutdown cleanup — Info level: "Terminated pending emacsclient process"

### Claude's Discretion
- Exact error message wording (following the tone decisions above)
- JSON log field names and structure
- How to detect specific emacsclient failure modes from stderr

</decisions>

<specifics>
## Specific Ideas

No specific requirements — open to standard approaches

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope

</deferred>

---

*Phase: 02-error-handling-reliability*
*Context gathered: 2026-01-26*
