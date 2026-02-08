---
phase: "05"
plan: "01"
subsystem: "ui-interaction"
tags: ["emacs-lisp", "selection-ui", "navigation", "overlays"]
requires:
  - "04-03"  # Core popup infrastructure with display and blocking
provides:
  - "Selection mode with numbered options list"
  - "C-n/C-p navigation with visual highlighting"
  - "Wrap-around navigation at list boundaries"
  - "RET confirmation returning selected option"
affects:
  - "05-02"  # Free-text mode will share the same popup infrastructure
  - "06-01"  # Number key shortcuts will build on selection mode
tech-stack:
  added: []
  patterns:
    - "Emacs overlays for dynamic highlighting"
    - "Text properties for option indexing"
    - "Modular navigation functions"
key-files:
  created:
    - "test-selection-mode.sh"
    - "test-selection-auto.el"
    - "test-render.el"
    - "verify-plan.el"
  modified:
    - "emacs/ask-user-popup.el"
decisions:
  - slug: "inverse-video-highlighting"
    choice: "Use :inverse-video face for selection"
    rationale: "Classic Emacs look, works in all color themes, no face definitions needed"
  - slug: "mod-arithmetic-wraparound"
    choice: "Use mod for wrap-around navigation"
    rationale: "Simple, correct, handles both forward and backward navigation elegantly"
  - slug: "text-properties-for-indexing"
    choice: "Store option-index as text property on each line"
    rationale: "Enables mouse support and simplifies overlay positioning"
metrics:
  duration: "~6 minutes"
  completed: "2026-02-08"
---

# Phase 5 Plan 01: Selection Mode Implementation Summary

**One-liner:** C-n/C-p navigation with inverse-video highlighting for numbered option selection

## What Was Delivered

**Selection Mode for Popup Buffer**
Implemented visual option selection with keyboard navigation for the ask-user-mcp popup buffer. Users can now see numbered options, navigate with familiar Emacs keys (C-n/C-p, arrows, j/k), and confirm their selection with RET.

**Core Features:**
- Numbered option list display (1-indexed for UI, 0-indexed internally)
- Visual selection highlighting using inverse-video overlay
- Wrap-around navigation at list boundaries
- Multiple navigation keys: C-n/C-p, arrows, j/k (vim-friendly)
- RET confirms and returns selected option text
- Mouse-1 click support for instant selection
- Cancel still works (C-g/q)

**Technical Implementation:**
- Extended `mr-x/ask-user-popup` signature to accept optional OPTIONS parameter
- Added three buffer-local variables: --options, --selected-index, --selection-overlay
- Implemented four navigation functions: select-next, select-prev, move-selection-overlay, confirm-selection
- Used text properties (`option-index`) to mark each option line
- Overlay dynamically repositioned on navigation

## Tasks Completed

**Task 1: Options Rendering** (Commit: c63b6f5)
- Updated function signature to accept OPTIONS parameter
- Rendered options as numbered list with text properties
- Created selection overlay with inverse-video face
- Positioned overlay on first option by default

**Task 2: Navigation and Keybindings** (Commit: ac57df8)
- Implemented navigation functions with wrap-around
- Wired keybindings to mode map (C-n/C-p, arrows, j/k, RET, mouse-1)
- Implemented confirm-selection to return selected option
- Added mouse support for click-to-select

**Task 3: End-to-End Verification** (Commit: 5c877a4)
- Created manual test script (test-selection-mode.sh)
- Created automated unit tests for navigation
- Verified text properties and option rendering
- All verification checks passed

## Decisions Made

**Inverse-video highlighting**
Chose `:inverse-video t` face for selection highlight instead of defining custom faces. This provides classic Emacs selection appearance that works in all color themes without requiring face definitions.

**Mod arithmetic for wrap-around**
Used modulo arithmetic for navigation wrap-around: `(mod (1+ index) length)` for forward, `(mod (1- index) length)` for backward. This handles both directions elegantly without conditional logic.

**Text properties for option indexing**
Stored `option-index` as a text property on each option line. This enables both mouse support (get property at point) and simplifies overlay positioning (search for property value).

## Deviations from Plan

None - plan executed exactly as written.

## Testing

**Automated Tests:**
- Navigation function unit tests (forward, backward, wrap-around) - PASSED
- Option rendering with text properties - PASSED
- All verification criteria - PASSED (7/7)

**Manual Test Coverage:**
- test-selection-mode.sh: Interactive emacsclient testing
- Covers: display, navigation, confirmation, cancellation

**What Works:**
- Options display as numbered list with first highlighted
- C-n/C-p/j/k/arrows navigate between options
- Navigation wraps at both ends
- RET returns selected option text
- C-g/q cancel and signal error
- Works via emacsclient (MCP server integration path)

## Technical Notes

**Overlay Management:**
The selection overlay is created once when options are rendered, then repositioned on each navigation event. Overlay spans entire line including newline for clean visual appearance.

**Buffer-local State:**
All selection state (options list, selected index, overlay) is buffer-local, ensuring proper isolation if multiple popups exist (edge case).

**Keyboard Shortcuts:**
Intentionally included both Emacs-style (C-n/C-p) and vim-style (j/k) navigation for maximum accessibility. Arrow keys also supported for discoverability.

## Integration Points

**Upstream Dependencies:**
- 04-03: Core popup infrastructure (display-buffer, recursive-edit, cleanup)
- Requires functional ask-user-popup-mode and buffer lifecycle

**Downstream Impact:**
- 05-02: Free-text mode will detect absence of options and provide input field
- 06-01: Number key shortcuts (1-9) will build on this selection infrastructure
- MCP server integration: Ready for Node.js to pass options array

## Next Phase Readiness

**Blockers:** None

**Concerns:** None

**Ready for:**
- Free-text input mode (05-02)
- MCP server options array integration
- Number key shortcuts (06-01)

**Not ready for:**
- Free-text mode (need input field implementation)

## Artifacts

**New Files:**
- test-selection-mode.sh: Manual emacsclient test script
- test-selection-auto.el: Automated navigation unit tests  
- test-render.el: Option rendering verification
- verify-plan.el: Overall success criteria verification

**Modified Files:**
- emacs/ask-user-popup.el: +132 lines (options rendering, navigation, keybindings)

**Commits:**
- c63b6f5: feat(05-01): add options rendering with numbered list display
- ac57df8: feat(05-01): add navigation and selection keybindings
- 5c877a4: test(05-01): verify selection mode end-to-end functionality

## Key Learnings

**Emacs Overlays:**
Overlays are the right primitive for dynamic highlighting - they move independently of text content and support arbitrary face properties.

**Text Properties vs Overlays:**
Text properties work well for static metadata (option-index), while overlays work well for dynamic visual effects (selection highlighting).

**Navigation Patterns:**
Wrap-around navigation is expected behavior in selection UIs - users should never hit a "dead end" when navigating a circular list.

---

**Status:** ✅ Complete
**Duration:** ~6 minutes
**Quality:** All automated tests passing, ready for manual verification
