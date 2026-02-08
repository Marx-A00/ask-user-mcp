# Phase 5: Selection and Free-Text Modes - Context

**Gathered:** 2026-02-08
**Status:** Ready for planning

<domain>
## Phase Boundary

Users interact with the popup through two modes — navigating and selecting from options, OR typing free-form text. Both modes are always available when options exist (options list + text field for "other"). Node.js integration passes options array and handles responses.

</domain>

<decisions>
## Implementation Decisions

### Selection UI
- Numbered list display: `1. Option one`, `2. Option two` (sets up Phase 6 number keys)
- Inverse colors for selected option (classic Emacs selection style)
- Selection wraps around at ends (C-n on last → first, C-p on first → last)
- First option selected when popup opens (user can act immediately)

### Navigation Feel
- C-n/C-p for navigation (standard Emacs)
- Arrow keys also work (Up/Down mirror C-p/C-n)
- j/k also navigate (vim/evil-mode friendly)
- Silent wrap — no feedback when wrapping around
- RET confirms selection
- Single click on option confirms selection

### Free-Text Mode
- Always show both options AND text field (text field below options)
- Text field for "other" / custom response
- Submission: C-c C-c always works, RET submits for quick single-line
- Auto-expand text field — starts single-line, grows if user hits Shift+RET

### Mode Switching
- Options focused initially (first option highlighted)
- Tab cycles: options → text field → options
- C-n past last option enters text field
- C-p from text field jumps to last option
- Selecting an option discards any typed text (option wins)

### Claude's Discretion
- Exact overlay/face implementation for inverse colors
- Text field placeholder text (if any)
- Visual separator between options and text field
- Shift+RET vs other mechanism for newlines in text field

</decisions>

<specifics>
## Specific Ideas

- Navigation should feel fluid — multiple ways to move around (keys, arrows, j/k, Tab, click)
- "Both modes always available" — user never has to wonder if they can type something custom

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope

</deferred>

---

*Phase: 05-selection-and-free-text-modes*
*Context gathered: 2026-02-08*
