# Phase 3: Emacs UX & Production Hardening - Context

**Gathered:** 2026-01-27
**Status:** Ready for planning

<domain>
## Phase Boundary

Styled prompts in Emacs minibuffer, production observability via structured logging, and self-service documentation. Users can set up the server without assistance and debug issues themselves.

</domain>

<decisions>
## Implementation Decisions

### Prompt Styling
- Bold header + normal question text (header prominent, question readable)
- Use existing Emacs faces (e.g., `font-lock-keyword-face`, `minibuffer-prompt`) so styling adapts to user's theme
- Use `header` parameter from Claude: if provided, show "Claude asks (Header):", otherwise just "Claude asks:"
- Text styling only — no mode-line indicator or echo area messages

### Audit Trail
- Verbose logging: timestamp, question, response, header, timeout, tool call ID, duration, success/error status
- Use Pino structured logging to stderr (already set up in Phase 2)
- No retention management — logs go to stderr, agent-shell or user handles retention
- Log level: debug — Q&A events only visible when debugging is enabled

### Documentation
- Primary audience: just you — minimal docs, enough to remember setup after time away
- Emacs config: use-package style formatting
- Troubleshooting: detailed walkthroughs with step-by-step debugging for each failure mode
- README includes complete agent-shell configuration example

### Fallback Behavior
- Fallback to `read-string` if custom `ask-user-question` function fails
- Log warning to stderr about missing function (silent to user, visible in logs)
- Detection: try custom function first, catch "void function" error, retry with `read-string`
- Error messaging: user-friendly message in response, technical details in logs

### Claude's Discretion
- Exact Emacs face choices for header vs question
- Pino log field names and structure
- README section ordering
- Specific elisp implementation details

</decisions>

<specifics>
## Specific Ideas

- Header format follows pattern: "Claude asks:" or "Claude asks (Context):" based on header parameter
- Troubleshooting should cover: Emacs server not running, socket path issues, function not defined

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope

</deferred>

---

*Phase: 03-emacs-ux-production-hardening*
*Context gathered: 2026-01-27*
