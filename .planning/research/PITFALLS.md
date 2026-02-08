# Domain Pitfalls: Emacs Popup Buffer UI

**Domain:** Popup buffer UI with C-n/C-p navigation and emacsclient blocking
**Researched:** 2026-02-08
**Confidence:** MEDIUM (WebSearch-verified with official Emacs documentation)

## Critical Pitfalls

Mistakes that cause rewrites, blocking failures, or major integration issues.

### Pitfall 1: emacsclient Return Value Escaping
**What goes wrong:** `emacsclient --eval` automatically quotes string results, breaking shell script parsing. A function returning `"hello"` becomes `"\"hello\""` on stdout, requiring complex unescaping.

**Why it happens:** Emacs server delegates to `server-eval-and-print`, which uses `pp` to format output for `read` compatibility, automatically quoting strings.

**Consequences:**
- MCP server receives incorrectly escaped JSON
- Shell scripts parsing output break
- Return values need post-processing

**Prevention:**
- Use `prin1-to-string` instead of relying on default `server-eval-and-print`
- Consider alternative return mechanisms (temp files, dedicated return protocol)
- Test emacsclient output format early in development
- Consider `ebse` tool pattern (grettke/ebse) for unquoted results

**Detection:**
- Shell script tests fail with quote parsing errors
- JSON parsing fails in MCP server
- Return values have unexpected escape characters

**Phase impact:** Core implementation phase (MVP) - must be resolved before MCP integration works.

**Sources:**
- [GitHub - grettke/ebse: Get unquoted result from Emacs server eval](https://github.com/grettke/ebse)
- [emacsclient --eval return code contains weird invisible characters · Issue #15362](https://github.com/syl20bnr/spacemacs/issues/15362)

---

### Pitfall 2: emacsclient Blocking Without Proper server-edit
**What goes wrong:** emacsclient blocks indefinitely if popup doesn't call `server-edit` or exits abnormally. Caller hangs waiting for Emacs response.

**Why it happens:** emacsclient waits for explicit `C-x #` (server-edit) signal before returning. If popup is dismissed without calling server-edit, the client never receives completion signal.

**Consequences:**
- MCP server request times out
- emacsclient process hangs
- No error propagation to caller
- Resources leak on Emacs side

**Prevention:**
- ALWAYS call `server-edit` on normal completion
- Call `server-edit-abort` on cancellation/error paths
- Bind cleanup to `kill-buffer-hook` as failsafe
- Test timeout behavior explicitly

**Detection:**
- emacsclient stuck in "Waiting for Emacs..." state
- MCP requests never complete
- Manual buffer kill required to unblock client

**Phase impact:** Core implementation phase (MVP) - blocking breaks entire MCP integration.

**Sources:**
- [emacsclient Options (GNU Emacs Manual)](https://www.gnu.org/software/emacs/manual/html_node/emacs/emacsclient-Options.html)
- [save-buffers-kill-terminal and emacsclient --no-wait](https://emacs-devel.gnu.narkive.com/DTapMGPc/save-buffers-kill-terminal-and-emacsclient-no-wait)

---

### Pitfall 3: Keymap Override Hierarchy Confusion
**What goes wrong:** C-n/C-p bindings don't work because other keymaps take precedence. User presses C-n expecting navigation, but gets `next-line` or some minor mode binding instead.

**Why it happens:** Emacs keymap hierarchy is complex. Keymaps are checked in order: `overriding-terminal-local-map` → `overriding-local-map` → `emulation-mode-map-alists` → minor modes → major mode → global. Popup keymap must be placed high enough in hierarchy.

**Consequences:**
- Navigation keys don't work as expected
- User confusion (keys do wrong thing)
- Popup feels broken/non-functional
- Accessibility issues for keyboard users

**Prevention:**
- Use minor mode keymap for popup bindings (not just buffer-local keymap)
- Derive from `special-mode` which suppresses insert keys
- Test with common minor modes active (company, flycheck, etc.)
- Use `suppress-keymap` to disable insertion
- Consider `overriding-local-map` for temporary exclusive control

**Detection:**
- C-n moves to next line instead of next option
- C-p scrolls up instead of previous option
- Keybindings work inconsistently depending on what modes are active

**Phase impact:** Core implementation phase (MVP) - unusable without working navigation.

**Sources:**
- [Mastering Key Bindings in Emacs - Mastering Emacs](https://www.masteringemacs.org/article/mastering-key-bindings-emacs)
- [Changing Key Bindings (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Changing-Key-Bindings.html)

---

### Pitfall 4: Buffer Cleanup Race Conditions
**What goes wrong:** Popup buffer isn't cleaned up properly. Either buffer leaks (never killed) or gets killed too early (while emacsclient still waiting).

**Why it happens:** Multiple cleanup paths exist: `quit-window`, `kill-buffer`, `server-edit`, and `kill-buffer-hook`. If not coordinated, they race or miss each other.

**Consequences:**
- Buffer accumulation (memory leak)
- emacsclient receives wrong completion status
- Popup reappears when cycling buffers
- Cleanup hooks don't run

**Prevention:**
- Use `quit-window` with KILL parameter for atomic window+buffer cleanup
- Ensure `server-edit` runs BEFORE `kill-buffer`
- Add cleanup to `kill-buffer-hook` as failsafe (but check buffer-live-p)
- Test both normal completion and cancellation paths
- Beware: `kill-buffer-hook` doesn't run for buffers created with `inhibit-buffer-hooks`

**Detection:**
- `M-x ibuffer` shows accumulating popup buffers
- Buffer list grows with hidden ask-user buffers
- `C-x b` autocomplete shows old popup buffers
- Memory usage increases over time

**Phase impact:** Production hardening - leaks become visible with repeated use.

**Sources:**
- [Killing Buffers (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Killing-Buffers.html)
- [bug#77323: [PATCH] Allow temp buffer cleanup in ediff-current-file](https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-03/msg02615.html)

---

## Moderate Pitfalls

Mistakes that cause delays, poor UX, or technical debt.

### Pitfall 5: display-buffer-alist Bypass
**What goes wrong:** Popup doesn't appear in expected location (bottom of frame). It opens in random window or splits existing window unexpectedly.

**Why it happens:** Not all buffer display methods respect `display-buffer-alist`. Using `switch-to-buffer` or `set-buffer` bypasses display rules entirely. Popup must use `display-buffer` or `pop-to-buffer` with proper action list.

**Consequences:**
- Popup appears in wrong location
- Splits user's carefully arranged windows
- Breaks popper.el integration
- Inconsistent with user's window management setup

**Prevention:**
- ALWAYS use `display-buffer` with explicit action list
- Specify `display-buffer-at-bottom` or similar action function
- Test with various window configurations (splits, tabs, multiple frames)
- Don't use `switch-to-buffer` for popup display
- Consider popper.el integration for consistency

**Detection:**
- Popup appears at top instead of bottom
- Existing window gets reused unexpectedly
- Popup behavior changes based on window layout
- User reports "window management is broken"

**Phase impact:** UX refinement phase - works but feels janky.

**Sources:**
- [Demystifying Emacs's Window Manager - Mastering Emacs](https://www.masteringemacs.org/article/demystifying-emacs-window-manager)
- [The Emacs Window Management Almanac | Karthinks](https://karthinks.com/software/emacs-window-management-almanac/)

---

### Pitfall 6: Focus Stealing from User's Working Buffer
**What goes wrong:** When popup appears, focus moves to popup buffer. User loses position in code they were editing.

**Why it happens:** `display-buffer` actions can specify whether to select the new window. Without `(inhibit-same-window . t)` and `(inhibit-switch-frame . t)`, Emacs may switch focus.

**Consequences:**
- Cursor moves to popup, breaking user's flow
- User must manually switch back to work buffer after responding
- `save-excursion` in user code gets disrupted
- Point position lost

**Prevention:**
- Use `(inhibit-same-window . t)` in display-buffer action
- Keep `(current-buffer)` unchanged after popup creation
- Test that point position unchanged in original buffer
- Consider whether popup actually needs focus (may not for read-only prompts)

**Detection:**
- After popup appears, cursor is in popup instead of work buffer
- User must `C-x o` to continue working
- `(current-buffer)` changes after popup creation

**Phase impact:** UX refinement phase - functional but annoying.

**Sources:**
- [The Zen of Buffer Display (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Zen-of-Buffer-Display.html)
- [Emacs window management tweaking | The Art Of Not Asking Why](https://taonaw.com/2025/05/03/emacs-window-management-tweaking.html)

---

### Pitfall 7: Read-Only Buffer Violations
**What goes wrong:** If popup buffer is read-only (for safety), internal code may still try to modify it, causing `buffer-read-only` errors.

**Why it happens:** Setting `buffer-read-only` prevents ALL modifications. Programmatic insertions (for rendering options) will fail unless wrapped with `inhibit-read-only`.

**Consequences:**
- Errors when trying to update popup content
- Option rendering fails
- Dynamic content updates break
- User sees incomplete/empty popup

**Prevention:**
- Use `let ((inhibit-read-only t))` for programmatic modifications
- Set read-only AFTER initial content insertion
- Use `with-silent-modifications` for updates
- Test that dynamic updates work with read-only buffer

**Detection:**
- `buffer-read-only` errors in *Messages*
- Popup content doesn't update when options change
- Errors when trying to render option list

**Phase impact:** Core implementation phase - breaks option rendering if read-only set too early.

**Sources:**
- [Read Only Buffers (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Read-Only-Buffers.html)
- [27.7 Read-Only Buffers | Emacs Docs](https://emacsdocs.org/docs/elisp/Read-Only-Buffers)

---

### Pitfall 8: quit-window vs kill-buffer Semantics
**What goes wrong:** Using wrong cleanup function. `quit-window` tries to restore previous window config, `kill-buffer` just kills. Choosing wrong one causes UX issues.

**Why it happens:** `quit-window` uses `quit-restore` window parameter to decide whether to delete window, switch buffer, or delete frame. `kill-buffer` doesn't touch window at all.

**Consequences:**
- Using `kill-buffer`: Window remains showing "next" buffer (often *scratch*), wasting screen space
- Using `quit-window` without KILL: Buffer buried but not killed, accumulates over time
- Window layout disrupted when popup closes

**Prevention:**
- Use `quit-window` with KILL=t for atomic cleanup: `(quit-window t)`
- This deletes window AND kills buffer in one operation
- Ensure `display-buffer` sets `quit-restore` parameter properly
- Test that window disappears after popup closes

**Detection:**
- Window remains after popup dismissed (showing *scratch* or other buffer)
- Popup buffers accumulate in buffer list
- Window layout changes unexpectedly after popup

**Phase impact:** UX refinement phase - cleanup feels incomplete.

**Sources:**
- [Quitting Windows (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Quitting-Windows.html)
- [Quit and Close Emacs Special Windows Like Help and Compilation Results](https://christiantietze.de/posts/2019/10/emacs-quit-special-windows/)

---

### Pitfall 9: Process Filter Race Conditions
**What goes wrong:** If popup implementation involves process filters (for external communication), filters can run while other filters are running, causing state corruption.

**Why it happens:** Emacs runs process filters whenever data arrives, even during existing filter execution. Functions like `process-send-string` both send AND read, triggering filters unexpectedly.

**Consequences:**
- Corrupted popup state
- Out-of-order message processing
- Duplicate responses sent to MCP server
- Hard-to-reproduce bugs

**Prevention:**
- Use locking/guard flags during filter execution
- Queue incoming data instead of processing immediately
- Avoid `process-send-string` during filter execution
- Test with rapid-fire requests (race condition reproduction)

**Detection:**
- Intermittent bugs that disappear on retry
- State corruption under load
- Out-of-order responses
- Debugger shows nested filter invocations

**Phase impact:** Production hardening - only visible under concurrent use.

**Sources:**
- [Jorgen's Weblog: Race conditions in Emacs' process filter functions](http://blog.jorgenschaefer.de/2014/05/race-conditions-in-emacs-process-filter.html)
- [A vision of a multi-threaded Emacs | Hacker News](https://news.ycombinator.com/item?id=31559818)

---

## Minor Pitfalls

Mistakes that cause annoyance but are easily fixable.

### Pitfall 10: Forgetting special-mode Derivation
**What goes wrong:** Manual keymap setup instead of deriving from `special-mode`. Reinvents wheel and misses standard bindings like `q` for quit-window.

**Why it happens:** Not knowing `special-mode` exists. Documentation shows manual keymap creation examples.

**Consequences:**
- Missing standard bindings (`q`, `g`, etc.)
- More boilerplate code
- Inconsistent with other Emacs popups

**Prevention:**
- Derive popup major mode from `special-mode`
- Inherit `special-mode-map` automatically
- Add only custom bindings on top

**Detection:**
- `q` doesn't quit popup
- `g` doesn't refresh (if applicable)
- Keymap definition is long and manual

**Phase impact:** Code quality - works but messy.

**Sources:**
- [23.2.5 Basic Major Modes | Emacs Docs](https://emacsdocs.org/docs/elisp/Basic-Major-Modes)
- [Major Mode Conventions (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Major-Mode-Conventions.html)

---

### Pitfall 11: Not Testing with User's display-buffer-alist
**What goes wrong:** Popup works in clean config but breaks with user's custom `display-buffer-alist` rules.

**Why it happens:** User may have rules that override popup's display preferences. Package assumes default Emacs behavior.

**Consequences:**
- Popup appears in wrong location for some users
- Conflicts with popper.el or similar packages
- Bug reports that can't reproduce in clean config

**Prevention:**
- Use specific buffer name pattern unlikely to match user rules
- Document expected display-buffer behavior
- Test with popper.el and popwin installed
- Allow user customization of display-buffer action

**Detection:**
- Works in `emacs -Q` but not user's config
- User reports popup behavior inconsistency
- Conflicts with window management packages

**Phase impact:** Production polish - edge case issues.

**Sources:**
- [GitHub - karthink/popper: Emacs minor-mode to summon and dismiss buffers easily](https://github.com/karthink/popper)
- [:ui popup - Doom Emacs v21.12 documentation](https://docs.doomemacs.org/v21.12/modules/ui/popup/)

---

### Pitfall 12: Buffer Name Collisions
**What goes wrong:** Multiple concurrent popups share buffer name, causing display/state confusion.

**Why it happens:** Using static buffer name like `*ask-user*` without unique identifier. Second popup reuses existing buffer.

**Consequences:**
- First popup's content replaced by second
- Both emacsclient callers get same buffer
- State corruption when multiple requests in flight

**Prevention:**
- Use `generate-new-buffer` for unique names
- Or include request ID in buffer name: `*ask-user-<uuid>*`
- Store buffer reference in request context
- Clean up by buffer object, not by name lookup

**Detection:**
- Popup content changes unexpectedly
- Multiple MCP requests interfere with each other
- Buffer list shows only one popup despite multiple active requests

**Phase impact:** Concurrent request support - likely post-MVP.

**Sources:**
- [Current Buffer (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html)

---

## Phase-Specific Warnings

**Phase: Core Implementation (MVP)**
- Pitfall 1 (emacsclient return escaping) - MUST resolve for MCP integration
- Pitfall 2 (blocking/server-edit) - MUST resolve for basic functionality
- Pitfall 3 (keymap hierarchy) - MUST resolve for navigation
- Pitfall 7 (read-only violations) - MUST resolve for option rendering

**Phase: UX Refinement**
- Pitfall 5 (display-buffer bypass) - Should fix for bottom-placement
- Pitfall 6 (focus stealing) - Should fix for smooth UX
- Pitfall 8 (quit-window semantics) - Should fix for proper cleanup

**Phase: Production Hardening**
- Pitfall 4 (buffer cleanup races) - Must fix for long-running sessions
- Pitfall 9 (process filter races) - Must fix for concurrent requests
- Pitfall 11 (user config compatibility) - Should test thoroughly

**Phase: Concurrent Support (if needed)**
- Pitfall 12 (buffer name collisions) - MUST resolve for multiple simultaneous popups

---

## Quick Reference Checklist

**Before submitting MVP:**
- [ ] emacsclient output tested (Pitfall 1)
- [ ] server-edit called on all code paths (Pitfall 2)
- [ ] C-n/C-p navigation working (Pitfall 3)
- [ ] Buffer cleanup verified (no leaks) (Pitfall 4)
- [ ] Read-only buffer allows programmatic updates (Pitfall 7)

**Before production release:**
- [ ] Popup appears at bottom consistently (Pitfall 5)
- [ ] Focus remains in user's buffer (Pitfall 6)
- [ ] Window cleanup tested (Pitfall 8)
- [ ] Tested with popper.el and custom display-buffer-alist (Pitfall 11)
- [ ] Process filter locking if applicable (Pitfall 9)

**Code quality improvements:**
- [ ] Derived from special-mode (Pitfall 10)
- [ ] Unique buffer names for concurrent support (Pitfall 12)

---

## Confidence Assessment

**HIGH confidence:**
- Keymap hierarchy issues (well-documented in Emacs manual)
- display-buffer behavior (official documentation)
- server-edit blocking requirements (official documentation)

**MEDIUM confidence:**
- emacsclient output escaping (GitHub issues and third-party tools confirm)
- Process filter race conditions (blog post + multiple GitHub issues)
- Buffer cleanup patterns (official manual + 2025 bug report)

**LOW confidence:**
- Specific interaction with popper.el (limited 2025 documentation)
- MCP-specific edge cases (novel integration, limited prior art)

**Verification needed:**
- Test emacsclient blocking with actual MCP server integration
- Verify read-only buffer modification patterns in practice
- Confirm process filter races don't affect stdio-based MCP communication
