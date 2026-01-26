# Roadmap: AskUserQuestion MCP Server

## Overview

Build an MCP server that bridges Claude's AskUserQuestion tool calls to Emacs minibuffer prompts via emacsclient. The journey progresses from protocol-compliant core (security and architecture foundation) to production-ready reliability (error handling and graceful degradation) to polished user experience (Emacs styling and observability).

## Phases

**Phase Numbering:**
- Integer phases (1, 2, 3): Planned milestone work
- Decimal phases (2.1, 2.2): Urgent insertions (marked with INSERTED)

Decimal phases appear between their surrounding integers in numeric order.

- [x] **Phase 1: Core MCP Server** - Protocol-compliant foundation with secure emacsclient integration
- [ ] **Phase 2: Error Handling & Reliability** - Graceful degradation and production-ready resilience
- [ ] **Phase 3: Emacs UX & Production Hardening** - Styled prompts, observability, and documentation

## Phase Details

### Phase 1: Core MCP Server
**Goal**: Claude can ask questions via MCP protocol that appear in Emacs minibuffer and receive user responses securely
**Depends on**: Nothing (first phase)
**Requirements**: MCP-01, MCP-02, MCP-03, MCP-04, MCP-05, TOOL-01, TOOL-02, TOOL-03, EMACS-01
**Success Criteria** (what must be TRUE):
  1. Server implements JSON-RPC 2.0 with stdio transport (Claude can connect)
  2. Tool declaration appears in tools/list (Claude can discover AskUserQuestion)
  3. Question sent by Claude appears in Emacs minibuffer
  4. User response flows back to Claude as structured result
  5. Server uses spawn with argument arrays (no command injection vulnerability)
  6. Tool times out after 5 minutes with error message
**Plans**: 2 plans

Plans:
- [x] 01-01-PLAN.md — Initialize TypeScript project with MCP SDK and stdio transport
- [x] 01-02-PLAN.md — Implement AskUserQuestion tool with emacsclient integration

### Phase 2: Error Handling & Reliability
**Goal**: Server handles real-world failure modes gracefully without crashes or zombie processes
**Depends on**: Phase 1
**Requirements**: TOOL-04, EMACS-02, EMACS-03, ERR-01, ERR-02, ERR-03, ERR-04
**Success Criteria** (what must be TRUE):
  1. Server detects when Emacs server is not running and returns actionable error
  2. Server handles SIGINT and SIGTERM with child process cleanup
  3. Server survives malformed tool input without crashing
  4. Timeout messages explain why timeout occurred and how to fix
  5. Configurable timeout parameter works per tool call
  6. All errors are logged with structured format to stderr
**Plans**: TBD

Plans:
- [ ] 02-01: TBD
- [ ] 02-02: TBD

### Phase 3: Emacs UX & Production Hardening
**Goal**: Prompts use Emacs styling, server has production observability, and users can self-service setup
**Depends on**: Phase 2
**Requirements**: TOOL-05, EMACS-04, EMACS-05, DOC-01, DOC-02, DOC-03
**Success Criteria** (what must be TRUE):
  1. Questions use custom styled Emacs function (visually distinct from plain prompts)
  2. Fallback to read-string works if custom function not defined
  3. Q&A history logged to audit trail for debugging
  4. README enables user to configure agent-shell without assistance
  5. Emacs config snippet is copy-paste ready
  6. Troubleshooting guide covers common failure modes (Emacs server down, socket issues)
**Plans**: TBD

Plans:
- [ ] 03-01: TBD
- [ ] 03-02: TBD

## Progress

**Execution Order:**
Phases execute in numeric order: 1 → 2 → 3

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. Core MCP Server | 2/2 | Complete | 2026-01-26 |
| 2. Error Handling & Reliability | 0/TBD | Not started | - |
| 3. Emacs UX & Production Hardening | 0/TBD | Not started | - |
