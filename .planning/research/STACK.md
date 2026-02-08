# Technology Stack: Popup Buffer UI

**Project:** AskUserQuestion MCP Server v2
**Researched:** 2026-02-08
**Confidence:** HIGH

## Executive Summary

Build a custom popup buffer with overlays for selection highlighting. Use `display-buffer-at-bottom` with 0.4 height ratio for popper-style positioning. Do NOT use ivy/helm/vertico frameworks — they're built for minibuffer completion, not custom selection UIs. Build on top of Emacs's primitive display and overlay systems directly.

## Recommended Approach: Custom Popup Buffer

### Why Custom Over Framework

**Vertico/Helm/Ivy are wrong tools:**
- Designed for minibuffer completion workflows
- Rely on `completing-read` which requires minibuffer
- Cannot easily be adapted to custom popup buffers
- Add unnecessary dependency weight

**Custom buffer is simpler:**
- Direct control over display and keybindings
- ~50 lines of elisp for full feature set
- Reuses Emacs primitives (overlays, display-buffer)
- Matches existing `read-string` integration pattern

## Core Technologies

### 1. Buffer Display: `display-buffer-at-bottom`

**What:** Native Emacs function for displaying buffers at frame bottom
**Version:** Built-in since Emacs 24.1+
**Purpose:** Position popup buffer at bottom with 40% height

**Usage:**
```elisp
(display-buffer buffer-name
  '(display-buffer-at-bottom
    (window-height . 0.4)))
```

**Why this over alternatives:**
- `display-buffer-at-bottom` is the standard way to show bottom buffers
- `window-height . 0.4` sets height as 40% of frame (matches popper behavior)
- No external packages needed
- Works with all Emacs versions user likely has

**Alternatives considered:**
- `popper.el` package — Overkill, adds dependency for classification logic we don't need
- `popwin.el` package — Deprecated/orphaned (emacsorphanage)
- Side windows — More complex, designed for persistent windows

### 2. Selection Highlighting: Overlays

**What:** Emacs primitives for text highlighting
**Version:** Built-in core feature
**Purpose:** Highlight currently selected option

**Key functions:**
```elisp
;; Create overlay
(setq overlay (make-overlay start end))

;; Style it
(overlay-put overlay 'face '(:background "#3a3f5a" :foreground "#ffffff"))

;; Move it
(move-overlay overlay new-start new-end)

;; Clean up
(delete-overlay overlay)
```

**Why overlays over text properties:**
- Overlays don't affect buffer text (read-only safe)
- Easy to move for C-n/C-p navigation
- Can be deleted on exit without modifying buffer
- Higher priority than font-lock (won't be overridden)

**Implementation pattern (from hl-line-mode):**
- Create overlay once
- Move it via `post-command-hook` after each C-n/C-p
- Delete overlay when buffer closes

### 3. Navigation: Custom Keymap with `set-transient-map`

**What:** Emacs function for temporary keybindings
**Version:** Built-in since Emacs 24.4
**Purpose:** C-n/C-p navigation active only in popup

**Usage:**
```elisp
(let ((map (make-sparse-keymap)))
  (define-key map (kbd "C-n") 'my-next-option)
  (define-key map (kbd "C-p") 'my-previous-option)
  (define-key map (kbd "RET") 'my-select-option)
  (define-key map (kbd "C-g") 'my-cancel)
  (set-transient-map map t))
```

**Why this over buffer-local keymap:**
- Temporary — automatically deactivates after selection
- Takes precedence over other keymaps
- Can optionally stay active with keep-pred parameter
- Simpler than full major-mode

**Alternative:** Use `read-event` loop for single-key selection (like `read-multiple-choice`)
- Good for 1-9 key selection
- Not good for C-n/C-p multi-line navigation

### 4. Buffer Structure: Special Buffer with `with-current-buffer`

**What:** Temporary buffer for UI
**Version:** Built-in core
**Purpose:** Contain formatted options and handle state

**Pattern:**
```elisp
(defun my-popup-select (prompt options)
  (let ((buffer (generate-new-buffer "*Claude Question*")))
    (with-current-buffer buffer
      (insert (propertize prompt 'face 'bold) "\n\n")
      (dolist (option options)
        (insert option "\n"))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (forward-line 2))  ; First option
    (display-buffer buffer '(display-buffer-at-bottom (window-height . 0.4)))
    (select-window (get-buffer-window buffer))
    ;; ... navigation loop ...
    (kill-buffer buffer)))
```

**Why special buffer:**
- Clean state (no interference with user buffers)
- Easy cleanup (kill-buffer removes it)
- Can be read-only to prevent accidental editing
- Name like "*Claude Question*" clearly indicates purpose

## Integration with Existing v1 Code

### Current State (v1)

**TypeScript side:** `emacs-interface.ts`
```typescript
const result = execSync(
  `emacsclient --eval '(mr-x/ask-user-question "${escaped}" ${headerArg})'`,
  { encoding: 'utf-8', timeout }
);
```

**Emacs side:** `ask-user.el`
```elisp
(defun mr-x/ask-user-question (question &optional header timeout-ms)
  (read-string (concat prefix question-styled " ")))
```

### v2 Changes

**Add new parameter for options:**
```elisp
(defun mr-x/ask-user-question (question &optional header timeout-ms options)
  (if options
      (mr-x/popup-select question options)
    (mr-x/popup-free-text question)))
```

**No changes needed to TypeScript side:**
- `emacsclient` spawning stays the same
- Just pass additional `options` parameter when provided
- Elisp function handles UI mode switching

## Complete Elisp Stack

### Required Functions

| Function | Purpose | Lines |
|----------|---------|-------|
| `mr-x/popup-select` | Display options, handle C-n/C-p, return selection | ~30 |
| `mr-x/popup-free-text` | Display prompt, collect multi-line input | ~15 |
| `mr-x/popup--highlight-line` | Move overlay to current line | ~5 |
| `mr-x/popup--next-option` | C-n handler | ~3 |
| `mr-x/popup--prev-option` | C-p handler | ~3 |
| `mr-x/popup--select` | RET handler, return value | ~5 |
| `mr-x/popup--cancel` | C-g handler, signal quit | ~3 |

**Total: ~64 lines of elisp**

### Required Emacs Primitives

All built-in, no packages needed:

- `make-overlay` — Highlighting
- `overlay-put` — Styling
- `move-overlay` — Navigation
- `delete-overlay` — Cleanup
- `display-buffer-at-bottom` — Positioning
- `set-transient-map` — Keybindings
- `with-current-buffer` — Buffer management
- `generate-new-buffer` — Buffer creation
- `kill-buffer` — Cleanup

### Face for Highlighting

Use existing face or define custom:

```elisp
;; Option 1: Reuse hl-line face (exists in all Emacs)
(overlay-put overlay 'face 'hl-line)

;; Option 2: Custom face for more control
(defface mr-x/popup-selection
  '((t (:background "#3a3f5a" :foreground "#ffffff")))
  "Face for selected option in popup buffer.")
```

**Recommendation:** Start with `hl-line` face (simpler), add custom face if user requests different styling.

## Display Buffer Configuration

### For 40% Height (Popper-Style)

```elisp
(display-buffer buffer
  '(display-buffer-at-bottom
    (window-height . 0.4)
    (inhibit-same-window . t)))
```

**Key parameters:**
- `window-height . 0.4` — 40% of frame height (float = percentage)
- `inhibit-same-window . t` — Don't reuse selected window
- `display-buffer-at-bottom` — Position at bottom

**This matches user's existing popper configuration for terminals.**

### Alternative: Let User Control via display-buffer-alist

If user wants custom positioning:

```elisp
;; In user's config (not our code)
(add-to-list 'display-buffer-alist
  '("\\*Claude Question\\*"
    (display-buffer-at-bottom)
    (window-height . 0.5)))  ; User override to 50%
```

Our code uses default 0.4, but respects user's `display-buffer-alist` rules.

## Comparison: Custom vs Frameworks

### Option A: Custom Buffer (RECOMMENDED)

**Pros:**
- 64 lines of code vs 1000+ lines dependency
- Full control over UX
- No version compatibility issues
- Matches existing `emacsclient` pattern
- No learning curve for maintainers

**Cons:**
- Must implement navigation ourselves (trivial)
- No fuzzy matching (not needed for small option lists)

### Option B: Vertico + Extensions

**Pros:**
- Fuzzy matching built-in
- Well-tested navigation

**Cons:**
- Designed for minibuffer, not popup buffers
- `vertico-buffer-mode` shows in separate buffer but still tied to `completing-read`
- Adds dependency that user must install
- Would need to force-load in `emacsclient` call
- Overkill for simple option selection

### Option C: Read-Multiple-Choice

**What:** Built-in Emacs function for multiple choice
**Pros:** Already in Emacs core
**Cons:**
- Uses help buffer, not bottom popup
- Single-key selection only (no C-n/C-p)
- Can't customize positioning
- Text-based, not visual navigation

**Verdict:** Not suitable for requirements (needs C-n/C-p navigation).

## Implementation Pattern

### Phase 1: Basic Popup with Static Selection

```elisp
(defun mr-x/popup-select (prompt options)
  "Display PROMPT and OPTIONS in bottom popup. Return selected option."
  (let* ((buffer (generate-new-buffer "*Claude Question*"))
         (selection 0))
    (with-current-buffer buffer
      ;; Render content
      (insert (propertize prompt 'face 'bold) "\n\n")
      (dotimes (i (length options))
        (insert (format "%d. %s\n" (1+ i) (nth i options))))
      (setq buffer-read-only t))
    
    ;; Display at bottom
    (display-buffer buffer '(display-buffer-at-bottom (window-height . 0.4)))
    (select-window (get-buffer-window buffer))
    
    ;; Read single key (1-9)
    (let ((key (read-char "Select option: ")))
      (kill-buffer buffer)
      (when (and (>= key ?1) (<= key ?9))
        (nth (- key ?1) options)))))
```

### Phase 2: Add C-n/C-p Navigation

```elisp
(defun mr-x/popup-select (prompt options)
  "Display PROMPT and OPTIONS in bottom popup with C-n/C-p navigation."
  (let* ((buffer (generate-new-buffer "*Claude Question*"))
         (overlay nil)
         (current-line 0)
         (done nil)
         (result nil))
    
    ;; Render buffer
    (with-current-buffer buffer
      (insert (propertize prompt 'face 'bold) "\n\n")
      (dolist (option options)
        (insert option "\n"))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (forward-line 2)
      
      ;; Create highlight overlay
      (setq overlay (make-overlay (line-beginning-position) (line-end-position)))
      (overlay-put overlay 'face 'hl-line))
    
    ;; Display and select
    (display-buffer buffer '(display-buffer-at-bottom (window-height . 0.4)))
    (select-window (get-buffer-window buffer))
    
    ;; Navigation loop
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-n")
        (lambda () (interactive)
          (forward-line 1)
          (move-overlay overlay (line-beginning-position) (line-end-position))))
      (define-key map (kbd "C-p")
        (lambda () (interactive)
          (forward-line -1)
          (move-overlay overlay (line-beginning-position) (line-end-position))))
      (define-key map (kbd "RET")
        (lambda () (interactive)
          (setq result (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
          (setq done t)))
      (define-key map (kbd "C-g")
        (lambda () (interactive)
          (setq done t)))
      
      ;; Keep map active until RET or C-g
      (set-transient-map map t (lambda () (not done))))
    
    ;; Cleanup
    (delete-overlay overlay)
    (kill-buffer buffer)
    result))
```

### Phase 3: Add Free Text Mode

```elisp
(defun mr-x/popup-free-text (prompt)
  "Display PROMPT in bottom popup and collect free text input."
  (let ((buffer (generate-new-buffer "*Claude Question*")))
    (with-current-buffer buffer
      (insert (propertize prompt 'face 'bold) "\n\n")
      (insert (propertize "Type your answer below:\n" 'face 'italic)))
    
    (display-buffer buffer '(display-buffer-at-bottom (window-height . 0.4)))
    (select-window (get-buffer-window buffer))
    
    ;; Make buffer editable
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (goto-char (point-max)))
    
    ;; Read until C-c C-c
    (local-set-key (kbd "C-c C-c")
      (lambda () (interactive)
        (setq result (buffer-substring-no-properties
                      (+ (point-min) 3)  ; Skip prompt lines
                      (point-max)))))
    
    ;; ... wait for C-c C-c ...
    (recursive-edit)
    
    (kill-buffer buffer)
    result))
```

## Sources

### High Confidence (Official Documentation)

- [GNU Emacs Lisp Reference Manual - Overlays](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html)
- [GNU Emacs Lisp Reference Manual - Buffer Display Action Alists](https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Display-Action-Alists.html)
- [GNU Emacs Lisp Reference Manual - Text from Minibuffer](https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-from-Minibuffer.html)
- [GNU Emacs Lisp Reference Manual - Multiple Queries](https://www.gnu.org/software/emacs/manual/html_node/elisp/Multiple-Queries.html)

### High Confidence (Verified Implementations)

- [Vertico GitHub](https://github.com/minad/vertico) — Architecture reference (not for reuse)
- [Popper GitHub](https://github.com/karthink/popper) — Popup positioning patterns
- [hl-line-mode implementation](https://posts.tonyaldon.com/2022-03-05-i-bet-you-use-hl-line-mode/) — Overlay movement pattern

### Medium Confidence (Community Resources)

- [Mastering Emacs - Window Management](https://www.masteringemacs.org/article/demystifying-emacs-window-manager)
- [Karthinks - Window Management Almanac](https://karthinks.com/software/emacs-window-management-almanac/)
- [System Crafters - Vertico Guide](https://systemcrafters.net/emacs-tips/streamline-completions-with-vertico/)

## Recommendation Summary

**Stack dimension:** Emacs popup buffer UI

**Build custom popup buffer using:**

1. `display-buffer-at-bottom` with `window-height . 0.4` for positioning
2. `make-overlay` + `move-overlay` for selection highlighting
3. `set-transient-map` for C-n/C-p navigation keybindings
4. `generate-new-buffer` for temporary display buffer
5. `hl-line` face for highlight styling (or custom face)

**Total complexity:** ~64 lines of elisp, zero external dependencies

**Why not frameworks:** Vertico/Helm/Ivy are minibuffer completion tools, not popup buffer builders. Custom implementation is simpler and matches project's direct `emacsclient` integration pattern.

**Integration:** Add `options` parameter to existing `mr-x/ask-user-question` function. If options provided, use `mr-x/popup-select`. Otherwise use `mr-x/popup-free-text`. TypeScript side unchanged.
