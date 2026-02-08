# Requirements: AskUserQuestion MCP Server

**Defined:** 2025-01-26 (v1), Updated 2026-02-08 (v2)
**Core Value:** Claude can pause and ask clarifying questions instead of guessing or failing silently

## v2 Requirements

Requirements for Popup UI milestone. Each maps to roadmap phases.

### Popup Infrastructure

- [ ] **POPUP-01**: Popup buffer appears at bottom of frame (~40% height)
- [ ] **POPUP-02**: Buffer uses dedicated major mode (special-mode derived)
- [ ] **POPUP-03**: Buffer blocks until user responds (recursive-edit pattern)
- [ ] **POPUP-04**: Buffer cleanup on exit (no orphaned buffers)
- [ ] **POPUP-05**: C-g cancels and returns error to Claude

### Selection Mode

- [ ] **SEL-01**: Tool accepts `options` array parameter for multiple choice
- [ ] **SEL-02**: Options display as selectable list in popup
- [ ] **SEL-03**: C-n/C-p navigation between options
- [ ] **SEL-04**: Current selection highlighted with distinct face
- [ ] **SEL-05**: RET confirms and returns selected option
- [ ] **SEL-06**: Number keys (1-9) for quick select

### Free-Text Mode

- [ ] **TEXT-01**: When no options provided, popup shows editable text area
- [ ] **TEXT-02**: User can type multi-line response
- [ ] **TEXT-03**: C-c C-c or RET submits response

### Visual Layout

- [ ] **VIS-01**: Question/header displayed prominently at top
- [ ] **VIS-02**: Description (if provided) displayed below header
- [ ] **VIS-03**: Options or text area clearly separated from header

### Integration

- [ ] **INT-01**: Node.js passes options array via emacsclient
- [ ] **INT-02**: Return value properly escaped for MCP response
- [ ] **INT-03**: Fallback to v1 minibuffer if popup function not defined
- [ ] **INT-04**: Timeout still works (5 minutes default)

## v1 Requirements (Complete)

### MCP Protocol

- [x] **MCP-01**: Server implements JSON-RPC 2.0 via official `@modelcontextprotocol/sdk`
- [x] **MCP-02**: Server uses stdio transport (stdin/stdout)
- [x] **MCP-03**: Server responds to `tools/list` with AskUserQuestion tool
- [x] **MCP-04**: Server responds to `tools/call` for AskUserQuestion
- [x] **MCP-05**: Tool input validated with Zod schemas

### AskUserQuestion Tool (v1)

- [x] **TOOL-01**: Tool accepts `question` string parameter
- [x] **TOOL-02**: Tool returns user's text response
- [x] **TOOL-03**: Tool times out after 5 minutes with graceful error message
- [x] **TOOL-04**: Tool accepts optional `timeout` parameter (configurable per request)
- [x] **TOOL-05**: Tool logs Q&A to response history/audit trail

### Emacsclient Integration (v1)

- [x] **EMACS-01**: Server spawns emacsclient using `spawn()` with argument arrays (secure)
- [x] **EMACS-02**: Server handles "Emacs server not running" with clear error message
- [x] **EMACS-03**: Server cleans up child processes on exit (signal handlers)
- [x] **EMACS-04**: Custom `mr-x/ask-user-question` elisp function with styled prompts
- [x] **EMACS-05**: Fallback to plain `read-string` if custom function not defined

### Error Handling (v1)

- [x] **ERR-01**: Errors are classified by type (timeout, server unavailable, validation, etc.)
- [x] **ERR-02**: Server does not crash on malformed input
- [x] **ERR-03**: Server handles SIGINT and SIGTERM with cleanup
- [x] **ERR-04**: Structured logging via Pino to stderr

### Documentation (v1)

- [x] **DOC-01**: README with setup instructions for agent-shell
- [x] **DOC-02**: Emacs config snippet (elisp function + agent-shell-mcp-servers)
- [x] **DOC-03**: Troubleshooting guide for common issues

## Future Requirements

Deferred to v3+. Tracked but not in current roadmap.

### Enhanced UX

- **UX-01**: Visual mode-line indicator when Claude is waiting for input
- **UX-02**: M-n/M-p as alternative navigation bindings
- **UX-03**: Home/End for jumping to first/last option

## Out of Scope

Explicitly excluded. Documented to prevent scope creep.

| Feature | Reason |
|---------|--------|
| Response validation with retry loop | Adds complexity, user can just answer again |
| Multi-turn clarification | Requires protocol extensions |
| HTTP transport | Unnecessary for local-only use case |
| Mouse-only selection | Breaks keyboard workflow |
| Auto-dismiss on focus loss | User might need to check something |
| Rich text/markdown in popup | Over-engineered for this use case |

## Traceability

| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓

| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓

| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓

| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
| Requirement | Phase | Status |
|-------------|-------|--------|
| POPUP-01 | Phase 4 | Pending |
| POPUP-02 | Phase 4 | Pending |
| POPUP-03 | Phase 4 | Pending |
| POPUP-04 | Phase 4 | Pending |
| POPUP-05 | Phase 4 | Pending |
| SEL-01 | Phase 5 | Pending |
| SEL-02 | Phase 5 | Pending |
| SEL-03 | Phase 5 | Pending |
| SEL-04 | Phase 5 | Pending |
| SEL-05 | Phase 5 | Pending |
| SEL-06 | Phase 6 | Pending |
| TEXT-01 | Phase 5 | Pending |
| TEXT-02 | Phase 5 | Pending |
| TEXT-03 | Phase 5 | Pending |
| VIS-01 | Phase 4 | Pending |
| VIS-02 | Phase 4 | Pending |
| VIS-03 | Phase 4 | Pending |
| INT-01 | Phase 5 | Pending |
| INT-02 | Phase 5 | Pending |
| INT-03 | Phase 5 | Pending |
| INT-04 | Phase 5 | Pending |

**Coverage:**
- v2 requirements: 21 total
- Mapped to phases: 21
- Unmapped: 0 ✓
