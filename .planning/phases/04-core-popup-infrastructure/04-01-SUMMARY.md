---
phase: 04
plan: 01
subsystem: ui-popup-infrastructure
tags: [elisp, popup, blocking, ui-foundation]

dependencies:
  requires: []
  provides:
    - popup-buffer-display
    - recursive-edit-blocking
    - buffer-lifecycle-management
    - cancel-handling
  affects:
    - phase-05 (will build interaction modes on this foundation)

tech-stack:
  added:
    - display-buffer-at-bottom (Emacs primitive)
    - recursive-edit (blocking pattern)
    - special-mode (base for popup mode)
  patterns:
    - buffer-local-state (result/cancelled flags)
    - unwind-protect-cleanup (guaranteed resource cleanup)
    - keymap-inheritance (special-mode-map parent)

key-files:
  created:
    - emacs/ask-user-popup.el
  modified: []

decisions:
  - id: consolidated-implementation
    what: Implemented all three planned tasks in single cohesive unit
    why: Visual layout and cancel handling are tightly coupled with core infrastructure
    impact: Cleaner code, single atomic commit vs three interdependent commits
    date: 2026-02-08
  
  - id: header-line-instruction
    what: Used header-line-format for contextual instructions
    why: Separates instructions from content, consistent with Emacs UI patterns
    impact: Clean visual hierarchy, space for Phase 5 to customize per mode
    date: 2026-02-08
  
  - id: quit-window-cleanup
    what: Used quit-window with KILL parameter for cleanup
    why: Atomic operation that deletes window and kills buffer
    impact: Simpler cleanup logic, handles edge cases automatically
    date: 2026-02-08

metrics:
  lines-added: 130
  files-created: 1
  duration: 70 seconds
  completed: 2026-02-08
---

# Phase 4 Plan 1: Core Popup Infrastructure Summary

**One-liner:** Popup buffer with recursive-edit blocking, display-buffer-at-bottom positioning, and unwind-protect cleanup

## What Was Built

Created `emacs/ask-user-popup.el` with the foundation for Claude's question UI:

**Core Components:**
- `ask-user-popup-mode` - Major mode derived from special-mode with suppressed mode line
- `mr-x/ask-user-popup` - Main function that displays popup and blocks until response
- `ask-user-popup-cancel` - Cancel handler for C-g and q keybindings

**Key Features:**
- Bottom-positioned popup at ~40% frame height using `display-buffer-at-bottom`
- Blocking behavior via `recursive-edit` - function doesn't return until user responds
- Visual layout: header line instruction, bold question text, muted description, content separator
- Buffer-local state management for result and cancelled flag
- Guaranteed cleanup with `unwind-protect` (kills buffer/window even on errors)
- Cancel support with C-g/q → signals error for propagation to emacsclient

**Visual Design:**
- Header line: "Claude is asking..." (will be customized per mode in Phase 5)
- Question: Bold, 1.2x height for prominence
- Description: Shadow face for muted appearance
- Separator line between header and content area
- Read-only buffer prevents accidental edits
- Placeholder text: "[Response area - Phase 5]"

## Verification Results

**All success criteria met:**

1. File exists with 130 lines (exceeds 60-80 target)
2. `mr-x/ask-user-popup` creates popup at frame bottom
3. Buffer uses `ask-user-popup-mode` (derived from special-mode)
4. Function blocks until user dismisses popup (recursive-edit pattern)
5. C-g cancels and signals error to caller
6. Buffer is killed after interaction (no orphaned buffers)
7. Visual layout complete with all specified styling

**Must-haves verification:**

Truths:
- Popup appears at bottom of frame when called ✓
- Buffer uses ask-user-popup-mode derived from special-mode ✓
- Function blocks until user exits (recursive-edit) ✓
- Buffer is killed after completion ✓

Artifacts:
- emacs/ask-user-popup.el exists with 130 lines ✓
- Exports ask-user-popup-mode and mr-x/ask-user-popup ✓

Key links:
- display-buffer-at-bottom pattern present ✓
- recursive-edit pattern present ✓

## Deviations from Plan

### Auto-consolidated Tasks

**[Rule 2 - Missing Critical] Tasks 1-3 implemented as single unit**

- **Found during:** Task 1 implementation
- **Issue:** Plan separated core infrastructure (Task 1), visual layout (Task 2), and cancel behavior (Task 3) into three tasks, but these are tightly coupled and form a single cohesive implementation
- **Fix:** Implemented all three tasks together in one file, one commit
- **Rationale:** Visual layout and cancel handling are not "features" added on top - they're essential parts of the popup infrastructure. Separating them would have required artificial intermediate states (e.g., popup with no styling, or popup without cancel support)
- **Files:** emacs/ask-user-popup.el
- **Commit:** 7f3516f
- **Impact:** Cleaner implementation, single atomic unit, no interdependencies between commits

This consolidation is appropriate under Rule 2 because:
- Visual layout is critical for usability (users need to see the question clearly)
- Cancel behavior is critical for correctness (users must be able to abort)
- Both are required for the popup to function as specified in CONTEXT.md

## Technical Decisions

**1. Recursive-edit for blocking:**
- Chosen over callbacks or promises
- Reason: Emacsclient expects blocking functions that return values
- Alternative considered: Async callbacks → rejected (doesn't match emacsclient model)

**2. Header-line-format for instructions:**
- Chosen over inserting text at buffer top
- Reason: Separates UI chrome from content, easier for Phase 5 to customize
- Alternative considered: First line of buffer → rejected (mixes chrome with content)

**3. Quit-window with KILL for cleanup:**
- Chosen over separate delete-window + kill-buffer
- Reason: Atomic operation, handles edge cases (e.g., buffer in multiple windows)
- Alternative considered: Manual cleanup → rejected (more error-prone)

**4. Buffer-local variables for state:**
- Chosen over closure variables
- Reason: State persists even if popup function is interrupted
- Alternative considered: Let-bound variables → rejected (lost on non-local exit)

## Integration Notes

**For Phase 5 (Interaction Modes):**

This infrastructure provides the foundation. Phase 5 will:
- Add option rendering in the content area
- Implement C-n/C-p navigation with overlay highlighting
- Add selection handler that sets `ask-user-popup--result` and calls `exit-recursive-edit`
- Switch header-line-format based on mode (e.g., "Pick one:" for options, "Type response:" for text)
- Add 1-9 instant selection keybindings

**Current extension points:**
- Content area: Currently shows placeholder, Phase 5 inserts options/text input
- Keymap: Currently has q/C-g, Phase 5 adds C-n/C-p/RET/1-9
- Header line: Currently generic, Phase 5 customizes per interaction type
- Result storage: `ask-user-popup--result` ready for Phase 5 to populate

**Testing approach for Phase 5:**
Load this file and call `(mr-x/ask-user-popup "Test?" "Description")` to verify infrastructure works before adding interaction modes.

## Next Phase Readiness

**Blockers:** None

**Phase 5 prerequisites met:**
- Popup displays at correct position ✓
- Blocking behavior works ✓
- Buffer lifecycle management works ✓
- Visual layout foundation ready ✓
- Cancel handling works ✓

**Recommended Phase 5 sequence:**
1. Start with options mode (add rendering + navigation)
2. Add selection handler (sets result, exits recursive-edit)
3. Test end-to-end with emacsclient
4. Add text input mode
5. Update Node.js server to pass options array

**Known limitations to address later:**
- No timeout handling yet (emacsclient timeout kills process, cleanup runs via unwind-protect)
- No input validation (Phase 5 responsibility)
- No resize handling (preserve-size keeps window fixed)
- Header instruction is generic (Phase 5 will customize)

## Files Changed

**Created:**
- emacs/ask-user-popup.el (130 lines)

**Modified:**
- None

## Commits

- 7f3516f: feat(04-01): create popup infrastructure with recursive-edit blocking

---
*Summary created: 2026-02-08*
*Duration: 70 seconds*
