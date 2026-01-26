# Phase 1: Core MCP Server - Context

**Gathered:** 2026-01-26
**Status:** Ready for planning

<domain>
## Phase Boundary

Protocol-compliant MCP server that bridges Claude's AskUserQuestion tool calls to Emacs minibuffer prompts via emacsclient. Users invoke the tool and see a prompt in Emacs; their response flows back to Claude securely.

</domain>

<decisions>
## Implementation Decisions

### Prompt Formatting
- Header as prefix with colon: `Header: Question`
- Match input casing (no transformation to CAPS)
- When options exist, show brief list: `Header: Question [opt1/opt2/opt3]`
- For options, add hint: "or type your own"
- For free-text (no options), use input suffix: `Header: Question > `
- No timeout hint in prompt — keep it clean
- Let Emacs handle long question wrapping naturally

### Response Structure
- Claude's discretion on response format (just answer vs structured object)
- Claude's discretion on cancel handling (whatever doesn't break Claude)
- Empty response treated same as cancel
- No metadata — just the answer, keep it simple

### Multi-select Behavior
- Use ivy/helm checkbox-style with multi-mark
- Prompt indicates multi-select: `Header: Question (select multiple) [opt1/opt2/opt3]`
- Respect tool parameters if Claude sends min/max constraints
- No fallback needed (user has ivy/helm)

### Option Presentation
- Use ivy/helm annotation feature (descriptions in margin)
- Full labels in prompt hint: `[Deploy to staging/Deploy to production/Cancel]`
- Truncate with count for 10+ options: `[opt1/opt2/opt3/...+7 more]`
- Options appear in order Claude sends them
- Truncate long descriptions at ~50 chars with ellipsis
- No visual distinction between options with/without descriptions
- Highlight default option with `*` or similar marker

### Claude's Discretion
- Response structure format
- Cancel handling approach
- Technical implementation details

</decisions>

<specifics>
## Specific Ideas

- This is for personal use — ivy/helm is available, no fallback complexity needed
- Keep it simple — minimal metadata, clean prompts

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope

</deferred>

---

*Phase: 01-core-mcp-server*
*Context gathered: 2026-01-26*
