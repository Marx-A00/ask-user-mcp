# Phase 4: Core Popup Infrastructure - Context

**Gathered:** 2026-02-08
**Status:** Ready for planning

<domain>
## Phase Boundary

A popup buffer that appears at the bottom of the Emacs frame when Claude asks a question. It displays the question prominently, blocks until the user responds, and cleans up properly afterward. This phase delivers the infrastructure; interaction modes (selection, free-text) come in Phase 5.

</domain>

<decisions>
## Implementation Decisions

### Visual Layout
- Header text larger and clearly distinguished from other elements
- Visually separated from description and options/text area
- Description text in muted/gray color
- Compact vertical spacing between sections (not excessive whitespace)
- Left-aligned throughout (no centered elements)

### Positioning Behavior
- Popup at bottom of frame (~40% height)
- Standard Emacs popup buffer behavior (like popper buffers)
- Uses `display-buffer-at-bottom` semantics
- Appears at frame bottom, not individual window bottom
- Auto-focus when popup appears

### Buffer Appearance
- No mode line
- Header line present — shows contextual instructions (e.g., "Pick one" for options)
- Distinct background color if feasible (nice to have)

### Cancel/Error UX
- C-g shows brief "Cancelled" message before closing
- No confirmation required — immediate cancel
- Keep current error message behavior for Claude
- Keep current timeout behavior (5 min)

### Claude's Discretion
- Exact font size/scaling for header
- Specific gray shade for description
- Exact spacing values (just "compact")
- Whether distinct background is achievable
- Specific error message wording
- Header line format/styling

</decisions>

<specifics>
## Specific Ideas

- "Should be like a regular popper buffer" — user expects familiar Emacs popup patterns
- Header line as instruction guidance, not static title

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope

</deferred>

---

*Phase: 04-core-popup-infrastructure*
*Context gathered: 2026-02-08*
