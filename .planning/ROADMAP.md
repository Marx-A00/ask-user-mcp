# Roadmap: AskUserQuestion MCP Server v2

**Milestone:** v2 Popup UI
**Goal:** Replace minibuffer prompts with a proper popup buffer UI that feels native to Emacs
**Phases:** 3 (continues from v1 which ended at Phase 3)

## Overview

This roadmap transforms the v1 minibuffer-based question prompts into a proper popup buffer UI. Phase 4 establishes the core popup infrastructure with proper positioning, blocking behavior, and cleanup. Phase 5 adds the dual interaction modes (selection with navigation and free-text input) and integrates them with the existing Node.js MCP server. Phase 6 polishes the experience with number key quick-select for efficient option selection.

Each phase delivers a complete, verifiable capability that builds toward the final popup UI experience.

## Phases

### Phase 4: Core Popup Infrastructure

**Goal:** Users see questions in a dedicated popup buffer at bottom of frame

**Dependencies:** None (builds on v1 emacsclient integration)

**Plans:** 3 plans

Plans:
- [x] 04-01-PLAN.md — Core buffer infrastructure (mode, display, blocking, cleanup)
- [x] 04-02-PLAN.md — Integration verification and human approval
- [ ] 04-03-PLAN.md — Gap closure: Wire MCP server to v2 popup function

**Requirements:**
- POPUP-01: Popup buffer appears at bottom of frame (~40% height)
- POPUP-02: Buffer uses dedicated major mode (special-mode derived)
- POPUP-03: Buffer blocks until user responds (recursive-edit pattern)
- POPUP-04: Buffer cleanup on exit (no orphaned buffers)
- POPUP-05: C-g cancels and returns error to Claude
- VIS-01: Question/header displayed prominently at top
- VIS-02: Description (if provided) displayed below header
- VIS-03: Options or text area clearly separated from header

**Success Criteria:**
1. Popup appears at bottom of Emacs frame (~40% height) when Claude asks a question
2. Question and description display clearly in read-only buffer
3. C-g cancels and returns error to Claude
4. Popup buffer cleans up automatically after response (no orphaned buffers)

### Phase 5: Selection and Free-Text Modes

**Goal:** Users can navigate and select from options OR type free-form responses

**Dependencies:** Phase 4 (requires popup infrastructure)

**Requirements:**
- SEL-01: Tool accepts `options` array parameter for multiple choice
- SEL-02: Options display as selectable list in popup
- SEL-03: C-n/C-p navigation between options
- SEL-04: Current selection highlighted with distinct face
- SEL-05: RET confirms and returns selected option
- TEXT-01: When no options provided, popup shows editable text area
- TEXT-02: User can type multi-line response
- TEXT-03: C-c C-c or RET submits response
- INT-01: Node.js passes options array via emacsclient
- INT-02: Return value properly escaped for MCP response
- INT-03: Fallback to v1 minibuffer if popup function not defined
- INT-04: Timeout still works (5 minutes default)

**Success Criteria:**
1. When Claude provides options, user can navigate with C-n/C-p and see highlighted selection
2. RET selects current option and returns it to Claude
3. When no options provided, user can type multi-line response and submit with C-c C-c or RET
4. Node.js timeout (5 minutes) still works with popup UI
5. Falls back to minibuffer if popup function not available

### Phase 6: Enhanced Navigation

**Goal:** Users can quickly select options using number keys

**Dependencies:** Phase 5 (requires selection mode)

**Requirements:**
- SEL-06: Number keys (1-9) for quick select

**Success Criteria:**
1. Number keys 1-9 instantly select corresponding option
2. Selection works for lists up to 9 items

## Progress

| Phase | Status | Requirements | Completion |
|-------|--------|--------------|------------|
| Phase 4: Core Popup Infrastructure | In Progress | 8/21 | ~90% |
| Phase 5: Selection and Free-Text Modes | Pending | 12/21 | 0% |
| Phase 6: Enhanced Navigation | Pending | 1/21 | 0% |

**Overall:** ~8/21 requirements complete (~38%)

---
*Roadmap created: 2026-02-08*
*Last updated: 2026-02-08*
