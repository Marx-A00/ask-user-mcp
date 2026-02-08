# Project Research Summary

**Project:** AskUserQuestion MCP Server v2
**Domain:** Emacs popup buffer UI with MCP integration
**Researched:** 2026-02-08
**Confidence:** HIGH

## Executive Summary

This project replaces minibuffer prompts with a popup buffer UI for Claude's question-answer flow. Research reveals this requires building a custom popup buffer (~64 lines of elisp) using native Emacs primitives rather than heavyweight completion frameworks. The recommended approach uses `display-buffer-at-bottom` for positioning, overlays for visual highlighting, and `recursive-edit` for blocking emacsclient synchronously. This matches the existing integration pattern with minimal changes to the Node.js MCP server.

The critical insight is that vertico/helm/ivy are the wrong tools — they're minibuffer completion frameworks, not popup buffer builders. A custom implementation provides full control, zero dependencies, and simpler integration. The architecture maintains the current synchronous blocking pattern where emacsclient waits for user input, requiring careful use of `recursive-edit` and proper `server-edit` cleanup.

The main risks are emacsclient blocking failures and keymap hierarchy conflicts. Both are mitigated by following established Emacs patterns: derive from `special-mode` for keymaps, use `unwind-protect` for cleanup, and test `server-edit` call paths thoroughly. The MVP should focus on popup positioning, C-n/C-p navigation, and clean buffer lifecycle before adding enhancements.

## Key Findings

### Recommended Stack

Build directly on Emacs primitives rather than external frameworks. Total implementation: ~64 lines of elisp with zero dependencies.

**Core technologies:**
- **`display-buffer-at-bottom`** (built-in): Position popup at frame bottom with 40% height, matches popper-style behavior
- **Overlays** (`make-overlay`, `move-overlay`): Highlight selected option during C-n/C-p navigation without modifying buffer text
- **`recursive-edit`** (built-in): Block emacsclient execution until user responds, maintaining synchronous contract with MCP server
- **`special-mode`** (built-in): Base major mode providing read-mostly buffer behavior and standard keybindings like `q` for quit
- **Buffer-local variables** (`defvar-local`): Store user response scoped to popup buffer, avoiding global state races

**Why not frameworks:** Vertico/helm/ivy are designed for minibuffer completion workflows and rely on `completing-read`. They cannot easily be adapted to custom popup buffers. Custom implementation is simpler, has no dependencies, and matches the project's direct `emacsclient` integration pattern.

**Integration point:** Add `options` parameter to existing `mr-x/ask-user-question` function. If options provided, use `mr-x/popup-select`; otherwise use `mr-x/popup-free-text`. TypeScript side unchanged — same emacsclient spawning pattern.

### Expected Features

**Must have (table stakes):**
- **C-n/C-p navigation** — Universal Emacs standard for list navigation, also support arrow keys
- **Visual highlight of current selection** — Users need clear feedback on what they're selecting
- **RET selects, C-g cancels** — Standard Emacs confirmation/cancel gestures
- **Bottom-of-frame positioning** — Consistent with compilation buffers, REPLs, popper.el
- **Automatic cleanup on exit** — Popup shouldn't linger in buffer list after selection
- **Free-text input mode** — Not all questions have predefined options

**Should have (competitive):**
- **Number keys (1-9) for quick select** — Efficient for short lists, company-mode pattern
- **M-n/M-p as navigation alternatives** — Matches completion frameworks, provides binding flexibility
- **Home/End jump to first/last** — Fast navigation for longer option lists
- **Clear mode indication** — User must know if selecting vs typing

**Defer (v2+):**
- **Incremental filtering** — Substantial complexity, may conflict with free-text mode, uncommon for Q&A
- **Popper.el integration** — Can start with basic display-buffer, add later if users request
- **Page scrolling (C-v/M-v)** — Only needed for very long lists (>10 items), uncommon in Q&A context
- **Evil mode compatibility** — Detect evil-mode and add j/k bindings if users report issues

### Architecture Approach

The architecture maintains the existing synchronous blocking pattern where emacsclient spawns, evaluates elisp, and blocks until return value is available. The popup UI is entirely elisp-side; Node.js only changes which function it calls.

**Major components:**
1. **Popup Display Manager** — Creates buffer, positions at bottom with `display-buffer-at-bottom`, manages window lifecycle
2. **Navigation Handler** — Implements C-n/C-p keybindings with overlay highlighting, derives from `special-mode` for keymap precedence
3. **Blocking Controller** — Uses `recursive-edit` to block emacsclient, captures user response in buffer-local variable, calls `server-edit` on completion
4. **Cleanup Coordinator** — Uses `unwind-protect` to guarantee buffer/window cleanup even on C-g abort

**Data flow:** Node.js → emacsclient spawn → elisp evaluation → recursive-edit (blocks) → user interaction → exit-recursive-edit → return string → stdout → Node.js promise resolves

**Critical contract:** Elisp function MUST be synchronous from emacsclient's perspective. It must not return until user completes interaction. This is achieved via `recursive-edit`, not async callbacks or hooks.

### Critical Pitfalls

1. **emacsclient blocking without proper server-edit** — If popup doesn't call `server-edit` on all exit paths, emacsclient hangs indefinitely. Prevention: Always call `server-edit` on completion, `server-edit-abort` on cancellation, bind cleanup to `kill-buffer-hook` as failsafe. Test timeout behavior explicitly.

2. **Keymap override hierarchy confusion** — C-n/C-p bindings don't work because other keymaps take precedence. Prevention: Derive from `special-mode` which suppresses insert keys, test with common minor modes active (company, flycheck), use `suppress-keymap` to disable insertion.

3. **Buffer cleanup race conditions** — Popup buffer isn't cleaned up properly, either leaking (never killed) or killed too early (while emacsclient waiting). Prevention: Use `quit-window` with KILL parameter for atomic cleanup, ensure `server-edit` runs BEFORE `kill-buffer`, add cleanup to `kill-buffer-hook` as failsafe.

4. **emacsclient return value escaping** — `emacsclient --eval` automatically quotes string results, breaking shell parsing. A function returning `"hello"` becomes `"\"hello\""` on stdout. Prevention: Test emacsclient output format early, consider using `prin1-to-string` for controlled formatting.

5. **display-buffer-alist bypass** — Using `switch-to-buffer` instead of `display-buffer` bypasses user's window management rules. Popup appears in wrong location. Prevention: ALWAYS use `display-buffer` or `pop-to-buffer` with explicit action list specifying `display-buffer-at-bottom`.

## Implications for Roadmap

Based on research, suggested phase structure:

### Phase 1: Core Popup Infrastructure
**Rationale:** Foundation for all UI work. Must establish blocking pattern and cleanup before adding features. Elisp-first approach allows independent testing before MCP integration.

**Delivers:** Working popup buffer with basic rendering, proper lifecycle, and return value propagation

**Key tasks:**
- Implement major mode derived from `special-mode`
- Create popup display function with `display-buffer-at-bottom`
- Integrate `recursive-edit` for blocking behavior
- Implement buffer-local result storage
- Add cleanup with `unwind-protect`
- Test emacsclient blocking and return values

**Addresses features:**
- Bottom-of-frame positioning
- Automatic cleanup on exit
- Read-only buffer content (except in free-text mode)

**Avoids pitfalls:**
- Pitfall 2: server-edit integration from start
- Pitfall 3: special-mode derivation for keymap precedence
- Pitfall 4: unwind-protect for guaranteed cleanup

**Research needed:** None — patterns well-documented in GNU Emacs manual

### Phase 2: Selection Navigation
**Rationale:** Core UX differentiator from v1. Depends on Phase 1 infrastructure for display and lifecycle.

**Delivers:** C-n/C-p navigation with visual highlighting, RET to select, C-g to cancel

**Key tasks:**
- Create overlay for selection highlighting
- Implement C-n/C-p keybindings that move overlay
- Add RET handler to capture selected line
- Add C-g handler to cancel with empty result
- Test keymap precedence with common minor modes
- Handle edge cases (first/last option wrapping)

**Addresses features:**
- C-n/C-p navigation
- Visual highlight of current selection
- RET selects, C-g cancels
- First item selected by default

**Avoids pitfalls:**
- Pitfall 3: Tested with company-mode, flycheck active
- Pitfall 7: Use `inhibit-read-only` for overlay updates

**Research needed:** None — overlay patterns standard, hl-line-mode provides reference

### Phase 3: Free-Text Input Mode
**Rationale:** Required for open-ended questions. Reuses Phase 1 infrastructure but makes buffer editable. Independent of Phase 2 (different interaction mode).

**Delivers:** Editable prompt area for user to type multi-line responses

**Key tasks:**
- Add mode-switching logic (options vs free-text)
- Render editable input area below question
- Use standard editing keybindings (C-a/C-e, C-k, etc.)
- Capture full buffer content on submit
- Add clear mode indication in mode-line or prompt

**Addresses features:**
- Free-text input mode
- Clear mode indication
- Status information (mode-line)

**Avoids pitfalls:**
- Pitfall 7: Make buffer editable in correct region only
- Pitfall 8: Same quit-window cleanup as selection mode

**Research needed:** None — editable buffer regions standard pattern

### Phase 4: MCP Integration
**Rationale:** Connects working popup to MCP server. Kept separate to allow elisp testing in isolation first.

**Delivers:** MCP server calls popup instead of minibuffer, full error handling

**Key tasks:**
- Add `options` parameter to existing elisp function
- Update emacsclient call in TypeScript to use new function
- Add condition-case fallback to minibuffer if popup fails
- Test timeout behavior (Node.js kills process)
- Test error propagation (elisp errors → Node.js)
- Verify return value escaping

**Addresses features:**
- Mode switching (selection vs free-text)

**Avoids pitfalls:**
- Pitfall 1: Test return value escaping early
- Pitfall 2: Verify server-edit on all code paths
- Pitfall 4: Test cleanup under timeout conditions

**Research needed:** None — existing integration pattern well-understood

### Phase 5: Enhanced Navigation (Optional)
**Rationale:** Polish features that improve UX but aren't blockers. Can be deferred post-MVP if time constrained.

**Delivers:** Number key quick-select, M-n/M-p alternatives, Home/End jumps

**Key tasks:**
- Add 1-9 keybindings for instant selection
- Add M-n/M-p as navigation alternatives
- Add Home/End to jump to first/last option
- Consider C-v/M-v page scrolling if lists commonly >10 items
- Add contextual help (C-h m shows keybindings)

**Addresses features:**
- Number keys for quick select
- M-n/M-p alternatives
- Home/End jump navigation
- Contextual help

**Research needed:** None — standard keybinding patterns

### Phase Ordering Rationale

- **Phase 1 first:** All other work depends on popup infrastructure and blocking pattern. Elisp-first approach enables independent testing.
- **Phase 2 before Phase 4:** Navigation must work before integrating with MCP, otherwise hard to test integration issues vs navigation bugs.
- **Phase 3 parallel to Phase 2:** Free-text mode is independent of selection navigation, can be developed in parallel if resources allow.
- **Phase 4 after Phases 1-3:** Integration comes last because elisp must be working before connecting to MCP server.
- **Phase 5 deferred:** Enhanced navigation is polish, not required for MVP functionality.

**Critical path:** Phase 1 → Phase 2 → Phase 4 (Phases 3 and 5 can be deferred if needed)

### Research Flags

Phases with standard patterns (skip research-phase):
- **Phase 1:** Popup buffer creation, recursive-edit, cleanup — all well-documented in GNU Emacs manual
- **Phase 2:** Overlay highlighting, keymap bindings — hl-line-mode provides reference implementation
- **Phase 3:** Editable buffer regions — standard pattern for org-capture, magit commit buffers
- **Phase 4:** emacsclient integration — existing v1 implementation provides template
- **Phase 5:** Keybinding additions — straightforward extension of Phase 2

**No phases need deeper research.** All patterns are established and well-documented. Research was front-loaded into this document.

## Confidence Assessment

**Area: Stack**
- Confidence: HIGH
- Notes: All technologies are built-in Emacs primitives with official GNU documentation. No external dependencies or version compatibility issues.

**Area: Features**
- Confidence: HIGH
- Notes: Feature expectations verified against established completion UIs (vertico, company-mode) and popup buffer patterns (popper.el, org-capture). Table stakes clear from community consensus.

**Area: Architecture**
- Confidence: HIGH
- Notes: Architecture verified against official recursive-edit documentation and existing patterns in org-capture, magit. Blocking pattern is well-understood.

**Area: Pitfalls**
- Confidence: MEDIUM
- Notes: Critical pitfalls verified through official documentation and GitHub issues. Process filter races may not apply (stdio-based, not async filters). emacsclient escaping needs practical testing.

**Overall confidence:** HIGH

### Gaps to Address

**Practical testing gaps:**
- emacsclient return value escaping needs verification in actual integration (Pitfall 1) — Test early in Phase 4
- Behavior under rapid concurrent requests not fully researched (Pitfall 12) — Document as known limitation, test if users report issues
- Interaction with user's custom display-buffer-alist rules (Pitfall 11) — Can't predict all user configs, document expected behavior

**Handling strategy:**
- Test emacsclient output format in first Phase 4 task
- Document buffer name collision limitation (unlikely for single-user tool)
- Provide user customization hooks for display-buffer action if needed

**No research blockers.** All gaps are testing validation, not architectural unknowns.

## Sources

### Primary (HIGH confidence)
- [GNU Emacs Lisp Reference Manual - Recursive Editing](https://www.gnu.org/software/emacs/manual/html_node/elisp/Recursive-Editing.html)
- [GNU Emacs Lisp Reference Manual - Overlays](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html)
- [GNU Emacs Lisp Reference Manual - Buffer Display Action Alists](https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Display-Action-Alists.html)
- [GNU Emacs Lisp Reference Manual - Quitting Windows](https://www.gnu.org/software/emacs/manual/html_node/elisp/Quitting-Windows.html)
- [GNU Emacs Lisp Reference Manual - Derived Modes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Derived-Modes.html)

### Secondary (MEDIUM confidence)
- [Vertico GitHub](https://github.com/minad/vertico) — Vertical completion UI patterns
- [Popper GitHub](https://github.com/karthink/popper) — Popup positioning patterns
- [Company Mode](http://company-mode.github.io/) — C-n/C-p navigation, number key quick-select
- [Mastering Emacs: Understanding Minibuffer Completion](https://www.masteringemacs.org/article/understanding-minibuffer-completion)
- [Karthinks: Emacs Window Management Almanac](https://karthinks.com/software/emacs-window-management-almanac/)

### Tertiary (LOW confidence)
- [GitHub - grettke/ebse](https://github.com/grettke/ebse) — emacsclient return value escaping workarounds
- [Jorgen's Weblog: Race conditions in Emacs process filters](http://blog.jorgenschaefer.de/2014/05/race-conditions-in-emacs-process-filter.html)

---
*Research completed: 2026-02-08*
*Ready for roadmap: yes*
