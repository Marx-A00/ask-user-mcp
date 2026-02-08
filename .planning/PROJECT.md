# AskUserQuestion MCP Server

## What This Is

An MCP server that provides an `AskUserQuestion` tool for Claude running in agent-shell (Emacs). When Claude needs clarification, it calls this tool, which prompts the user via a popup buffer UI and returns their response. The popup appears at the bottom of the Emacs frame (popper-style), supports both multiple-choice selection with C-n/C-p navigation and free-text input.

## Core Value

Claude can pause and ask clarifying questions instead of guessing or failing silently.

## Current Milestone: v2 Popup UI

**Goal:** Replace minibuffer prompts with a proper popup buffer UI that feels native to Emacs

**Target features:**
- Popup buffer at bottom (popper-style, ~40% height)
- Clear visual layout with title, description, options on separate lines
- C-n/C-p navigation between options with highlight
- Enter to select, C-g to cancel
- Free text mode with same popup UI

## Requirements

### Validated

<!-- Shipped and confirmed valuable in v1 -->

- ✓ MCP server using official `@modelcontextprotocol/sdk` (TypeScript) — v1
- ✓ `AskUserQuestion` tool with `question` string parameter — v1
- ✓ Prompts user via `emacsclient` calling custom Emacs function — v1
- ✓ 5-minute timeout on prompts — v1
- ✓ Setup documentation (agent-shell config, Emacs config) — v1
- ✓ Error handling with classified error messages — v1
- ✓ Signal handlers with child process cleanup — v1
- ✓ Structured logging via Pino — v1

### Active

<!-- Current scope for v2 -->

- [ ] Popup buffer UI instead of minibuffer
- [ ] Multiple choice options parameter support
- [ ] C-n/C-p navigation between options
- [ ] Highlight current selection
- [ ] Enter to select option
- [ ] Free text input mode in popup buffer
- [ ] C-g cancels and returns error
- [ ] Popper-style positioning at bottom (~40% height)

### Out of Scope

- Visual mode-line indicator — nice-to-have, not critical for core function
- Response validation with retry loop — adds complexity, user can just answer again
- Multi-turn clarification — requires protocol extensions
- HTTP transport — unnecessary for local-only use case

## Context

- v1 shipped with minibuffer-based prompts (basic `read-string`)
- User wants ivy/helm-style selection experience
- Popper is already used for terminal popups at 40% height
- Emacs server must be running for `emacsclient` to work

## Constraints

- **Runtime**: Node.js (already available in user's environment)
- **Protocol**: MCP (Model Context Protocol) via official SDK
- **Emacs dependency**: Requires Emacs server running (`emacsclient` available)
- **UI consistency**: Should feel like existing popper popups (terminal, etc.)

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Use official MCP SDK | Handles JSON-RPC, protocol versioning, follows MCP patterns | ✓ Good |
| TypeScript over plain JS | Type safety, better maintainability | ✓ Good |
| Custom elisp function | More visible prompt styling than plain `read-string` | ✓ Good |
| Popup buffer over minibuffer | More room for formatting, better UX for options | — Pending |
| Popper-style positioning | Consistency with existing terminal popup behavior | — Pending |

---
*Last updated: 2026-02-08 after v2 milestone start*
