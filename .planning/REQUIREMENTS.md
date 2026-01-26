# Requirements: AskUserQuestion MCP Server

**Defined:** 2025-01-26
**Core Value:** Claude can pause and ask clarifying questions instead of guessing or failing silently

## v1 Requirements

Requirements for initial release. Each maps to roadmap phases.

### MCP Protocol

- [ ] **MCP-01**: Server implements JSON-RPC 2.0 via official `@modelcontextprotocol/sdk`
- [ ] **MCP-02**: Server uses stdio transport (stdin/stdout)
- [ ] **MCP-03**: Server responds to `tools/list` with AskUserQuestion tool
- [ ] **MCP-04**: Server responds to `tools/call` for AskUserQuestion
- [ ] **MCP-05**: Tool input validated with Zod schemas

### AskUserQuestion Tool

- [ ] **TOOL-01**: Tool accepts `question` string parameter
- [ ] **TOOL-02**: Tool returns user's text response
- [ ] **TOOL-03**: Tool times out after 5 minutes with graceful error message
- [ ] **TOOL-04**: Tool accepts optional `timeout` parameter (configurable per request)
- [ ] **TOOL-05**: Tool logs Q&A to response history/audit trail

### Emacsclient Integration

- [ ] **EMACS-01**: Server spawns emacsclient using `spawn()` with argument arrays (secure)
- [ ] **EMACS-02**: Server handles "Emacs server not running" with clear error message
- [ ] **EMACS-03**: Server cleans up child processes on exit (signal handlers)
- [ ] **EMACS-04**: Custom `mr-x/ask-user-question` elisp function with styled prompts
- [ ] **EMACS-05**: Fallback to plain `read-string` if custom function not defined

### Error Handling

- [ ] **ERR-01**: Errors are classified by type (timeout, server unavailable, validation, etc.)
- [ ] **ERR-02**: Server does not crash on malformed input
- [ ] **ERR-03**: Server handles SIGINT and SIGTERM with cleanup
- [ ] **ERR-04**: Structured logging via Pino to stderr

### Documentation

- [ ] **DOC-01**: README with setup instructions for agent-shell
- [ ] **DOC-02**: Emacs config snippet (elisp function + agent-shell-mcp-servers)
- [ ] **DOC-03**: Troubleshooting guide for common issues

## v2 Requirements

Deferred to future release. Tracked but not in current roadmap.

### Enhanced Prompts

- **PROMPT-01**: Multiple choice / options parameter for selection prompts
- **PROMPT-02**: `completing-read` support for predefined options
- **PROMPT-03**: Response validation with retry loop

### UX Enhancements

- **UX-01**: Visual mode-line indicator when Claude is waiting for input
- **UX-02**: Progress notifications for long operations

### Observability

- **OBS-01**: PID monitoring for zombie process detection
- **OBS-02**: Architecture diagrams in documentation

## Out of Scope

Explicitly excluded. Documented to prevent scope creep.

| Feature | Reason |
|---------|--------|
| HTTP/SSE transport | Unnecessary for local agent-shell use, adds complexity |
| Multiple tools | Single-purpose server, "tool budget" principle |
| Resources or Prompts capabilities | Not relevant to user prompting use case |
| GUI windows outside Emacs | Breaks Emacs workflow |
| Automatic retry on timeout | Annoying UX, user should control |
| Persistent state across sessions | Race condition risks, unnecessary complexity |

## Traceability

Which phases cover which requirements. Updated during roadmap creation.

| Requirement | Phase | Status |
|-------------|-------|--------|
| MCP-01 | — | Pending |
| MCP-02 | — | Pending |
| MCP-03 | — | Pending |
| MCP-04 | — | Pending |
| MCP-05 | — | Pending |
| TOOL-01 | — | Pending |
| TOOL-02 | — | Pending |
| TOOL-03 | — | Pending |
| TOOL-04 | — | Pending |
| TOOL-05 | — | Pending |
| EMACS-01 | — | Pending |
| EMACS-02 | — | Pending |
| EMACS-03 | — | Pending |
| EMACS-04 | — | Pending |
| EMACS-05 | — | Pending |
| ERR-01 | — | Pending |
| ERR-02 | — | Pending |
| ERR-03 | — | Pending |
| ERR-04 | — | Pending |
| DOC-01 | — | Pending |
| DOC-02 | — | Pending |
| DOC-03 | — | Pending |

**Coverage:**
- v1 requirements: 22 total
- Mapped to phases: 0
- Unmapped: 22 ⚠️

---
*Requirements defined: 2025-01-26*
*Last updated: 2025-01-26 after initial definition*
