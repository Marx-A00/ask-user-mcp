# AskUserQuestion MCP Server

## What This Is

An MCP server that provides an `AskUserQuestion` tool for Claude running in agent-shell (Emacs). When Claude needs clarification, it calls this tool, which prompts the user via Emacs minibuffer and returns their response. This bridges the gap between Claude Code CLI's native tool and the ACP protocol used by agent-shell.

## Core Value

Claude can pause and ask clarifying questions instead of guessing or failing silently.

## Requirements

### Validated

(None yet — ship to validate)

### Active

- [ ] MCP server using official `@modelcontextprotocol/sdk` (TypeScript)
- [ ] `AskUserQuestion` tool with `question` string parameter
- [ ] Prompts user via `emacsclient` calling custom Emacs function
- [ ] Custom `mr-x/ask-user-question` elisp function with visible styling
- [ ] 5-minute timeout on prompts
- [ ] Setup documentation (agent-shell config, Emacs config)

### Out of Scope

- Multiple choice / options parameter — defer to v2, basic text sufficient for now
- `completing-read` support — defer to v2, depends on options parameter
- Visual mode-line indicator — nice-to-have, not critical for core function
- Q&A history tracking — defer to v2

## Context

- Claude in agent-shell sometimes calls `AskUserQuestion` which exists in Claude Code CLI but not in ACP protocol
- This causes "No such tool available: AskUserQuestion" errors
- The server acts as a bridge: Claude → MCP Server → emacsclient → Emacs minibuffer → response back to Claude
- Emacs server must be running for `emacsclient` to work

## Constraints

- **Runtime**: Node.js (already available in user's environment)
- **Protocol**: MCP (Model Context Protocol) via official SDK
- **Emacs dependency**: Requires Emacs server running (`emacsclient` available)
- **Standalone project**: Built in this directory, not in dotfiles

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Use official MCP SDK | Handles JSON-RPC, protocol versioning, follows MCP patterns | — Pending |
| TypeScript over plain JS | Type safety, better maintainability | — Pending |
| Custom elisp function | More visible prompt styling than plain `read-string` | — Pending |

---
*Last updated: 2025-01-26 after initialization*
