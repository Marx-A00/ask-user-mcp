# Architecture: Popup Buffer UI Integration

**Project:** ask-user-mcp
**Domain:** Emacs integration for MCP server
**Researched:** 2026-02-08
**Confidence:** HIGH (based on official GNU Emacs documentation and established patterns)

## Current Architecture

The existing implementation follows a simple synchronous pattern:

```
Node.js MCP Server
    │
    ├─ spawn("emacsclient", ["--eval", elisp-expr])
    │
    └─ Wait for stdout (blocks until elisp returns)
    
Emacs (via emacsclient)
    │
    ├─ Evaluate: (mr-x/ask-user-question "...")
    │
    └─ Return string value
```

**Key characteristics:**
- emacsclient blocks until elisp evaluation completes
- Return value is printed to stdout
- Node.js captures stdout and resolves promise
- Simple minibuffer UI with read-string

## Target Architecture: Popup Buffer UI

The popup buffer architecture maintains the same blocking pattern but adds visual complexity:

```
Node.js MCP Server
    │
    ├─ spawn("emacsclient", ["--eval", "(mr-x/ask-user-popup ...)"])
    │
    └─ Wait for stdout (blocks until user submits/cancels)
    
Emacs (via emacsclient)
    │
    ├─ Create popup buffer
    ├─ Display question content
    ├─ Enter recursive-edit (BLOCKS)
    │   │
    │   ├─ User edits/interacts
    │   │
    │   └─ User presses submit/cancel keybinding
    │       │
    │       └─ Exit recursive-edit with result
    │
    ├─ Clean up buffer/window
    │
    └─ Return result string
```

**Critical requirement:** The elisp function MUST be synchronous from emacsclient's perspective. It must not return until the user completes the interaction.

## Blocking Pattern: recursive-edit

The key to making popup buffers work with emacsclient is `recursive-edit`.

### How recursive-edit Works

From the GNU Emacs Lisp Reference Manual:

> `recursive-edit` invokes the editor command loop. This function contains the command loop; it also contains a call to `catch` with tag `exit`, which makes it possible to exit the recursive editing level by throwing to `exit`.

**Return values:**
- Throwing any value makes `recursive-edit` return normally
- Throwing `t` causes `recursive-edit` to quit (abort)
- Throwing a string signals an error with that message
- Command `C-M-c` (`exit-recursive-edit`) throws `nil` to exit normally
- Command `C-]` (`abort-recursive-edit`) throws `t` to abort

### Example Pattern

```elisp
(defun mr-x/ask-user-popup (question &optional header timeout-ms)
  "Show QUESTION in popup buffer, wait for user response."
  (let ((buffer (generate-new-buffer "*Claude Question*"))
        (result nil))
    (unwind-protect
        (progn
          ;; Set up buffer
          (with-current-buffer buffer
            (insert question)
            (goto-char (point-min))
            (mr-x/ask-user-mode))  ; Custom major mode
          
          ;; Display buffer as popup
          (pop-to-buffer buffer)
          
          ;; Enter recursive edit - BLOCKS HERE
          (recursive-edit)
          
          ;; After exit, capture result
          (with-current-buffer buffer
            (setq result (buffer-string))))
      
      ;; Cleanup: always kill buffer
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    
    ;; Return result to emacsclient
    result))
```

**Key points:**
1. `recursive-edit` blocks execution
2. User keybindings call `exit-recursive-edit` when done
3. `unwind-protect` ensures buffer cleanup
4. Function returns result string to emacsclient

## Data Flow: User Input to Return Value

### Submit Flow

```
User presses submit key (e.g., C-c C-c)
    │
    ├─ Keybinding calls: (mr-x/ask-user-submit)
    │
    └─ Function logic:
        │
        ├─ Validate/process buffer content
        ├─ Store result in buffer-local variable
        ├─ Call (exit-recursive-edit)
        │   │
        │   └─ Returns from recursive-edit call
        │
        └─ Main function resumes
            │
            ├─ Read result from buffer-local variable
            ├─ Clean up buffer (unwind-protect)
            │
            └─ Return string value
```

### Cancel Flow

```
User presses cancel key (e.g., C-c C-k)
    │
    ├─ Keybinding calls: (mr-x/ask-user-cancel)
    │
    └─ Function logic:
        │
        ├─ Set buffer-local result to nil or empty
        ├─ Call (exit-recursive-edit)
        │   │
        │   └─ Returns from recursive-edit call
        │
        └─ Main function resumes
            │
            ├─ Read result (empty/nil)
            ├─ Clean up buffer
            │
            └─ Return empty string
```

### Implementation Example

```elisp
(defvar-local mr-x/ask-user--result nil
  "Buffer-local storage for user's response.")

(defun mr-x/ask-user-submit ()
  "Submit response and exit popup."
  (interactive)
  (setq mr-x/ask-user--result (buffer-string))
  (exit-recursive-edit))

(defun mr-x/ask-user-cancel ()
  "Cancel and exit popup."
  (interactive)
  (setq mr-x/ask-user--result "")
  (exit-recursive-edit))

(defvar mr-x/ask-user-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mr-x/ask-user-submit)
    (define-key map (kbd "C-c C-k") #'mr-x/ask-user-cancel)
    map)
  "Keymap for ask-user popup buffers.")

(define-derived-mode mr-x/ask-user-mode special-mode "AskUser"
  "Major mode for Claude question popup buffers."
  (setq-local mr-x/ask-user--result nil))
```

## Major Mode vs Minor Mode

**Recommendation:** Use a custom major mode derived from `special-mode`.

### Why Major Mode?

**Derived from special-mode:**
- Provides standard behavior for read-mostly buffers
- Automatically suppresses self-insert commands
- Inherits `q` binding for `quit-window` (useful for cancel)
- Clean keymap without text editing commands

**Advantages:**
- Complete control over buffer behavior
- Clear separation from normal editing
- User can't accidentally modify question text (unless we enable it)
- Conventional pattern for transient buffers (magit, org-capture, etc.)

**Example:**
```elisp
(define-derived-mode mr-x/ask-user-mode special-mode "AskUser"
  "Major mode for Claude question popup buffers.
\\{mr-x/ask-user-mode-map}"
  (setq-local buffer-read-only nil)  ; Allow editing if needed
  (setq-local mr-x/ask-user--result nil))
```

### When to Use Minor Mode?

**Not recommended for this use case.**

Minor modes are for:
- Optional behavior applied to existing buffers
- Features that work alongside any major mode
- Global or buffer-local toggles

For a dedicated popup buffer, major mode is the conventional choice.

## Window Management

### Display Strategy

Use `pop-to-buffer` or `display-buffer` with appropriate actions:

```elisp
(display-buffer 
  buffer
  '((display-buffer-at-bottom)
    (window-height . 0.3)
    (window-parameters . ((no-other-window . t)
                         (mode-line-format . none)))))
```

**Or simpler:**
```elisp
(pop-to-buffer buffer)  ; Respects user's display-buffer-alist
```

### Cleanup Strategy

Use `quit-restore` parameter for proper cleanup:

```elisp
(defun mr-x/ask-user-cleanup ()
  "Clean up popup window and buffer."
  (let ((win (get-buffer-window (current-buffer))))
    (when win
      (quit-window t win))))  ; t = kill buffer
```

**Called in unwind-protect:**
```elisp
(unwind-protect
    (progn
      (pop-to-buffer buffer)
      (recursive-edit)
      (with-current-buffer buffer
        (setq result mr-x/ask-user--result)))
  ;; Cleanup
  (when (buffer-live-p buffer)
    (let ((win (get-buffer-window buffer)))
      (when win (quit-window t win))
      (unless (get-buffer-window buffer)
        (kill-buffer buffer)))))
```

## Error Handling

### Timeout Handling

The timeout is handled in Node.js (already implemented):

```typescript
// Current implementation in emacs-interface.ts
const timeoutTimer = setTimeout(() => {
  timedOut = true;
  proc.kill("SIGTERM");
}, timeout);
```

**Elisp doesn't need timeout logic** - when Node.js kills emacsclient, the process terminates and returns error code.

### User Abort (C-g)

When user presses `C-g` during recursive-edit:

```elisp
(condition-case err
    (progn
      (pop-to-buffer buffer)
      (recursive-edit)
      (with-current-buffer buffer
        mr-x/ask-user--result))
  (quit 
   ;; User pressed C-g
   ""))  ; Return empty string
```

### Emacsclient Disconnection

Already handled by Node.js error classification (`src/errors.ts`). No elisp changes needed.

## Build Order and Dependencies

### Phase 1: Elisp Infrastructure (First)

**Build first because:**
- Node.js code depends on elisp function existing
- Can test elisp interactively before MCP integration
- Easier to debug in isolation

**Deliverables:**
1. Major mode definition (`mr-x/ask-user-mode`)
2. Keymap with submit/cancel bindings
3. Popup display function (`mr-x/ask-user-popup`)
4. Buffer-local result storage
5. recursive-edit integration
6. Cleanup logic

**Testing:**
```elisp
;; Interactive test
(mr-x/ask-user-popup "What is your name?" "Test Header")
```

### Phase 2: Node.js Integration (Second)

**Build after elisp is working because:**
- Need elisp function to exist for emacsclient call
- Can test elisp independently first
- Node.js changes are minimal (just change function name)

**Changes required:**
```typescript
// In emacs-interface.ts
const elispExpr = `(condition-case err
    (mr-x/ask-user-popup "${escapedQuestion}" ${headerArg})
  (void-function
    (progn
      (message "ask-user-mcp: mr-x/ask-user-popup not defined, falling back")
      (mr-x/ask-user-question "${escapedQuestion}" ${headerArg}))))`;
```

**No other Node.js changes needed** - the blocking/timeout/error handling already works.

### Phase 3: Enhanced Features (Optional)

After basic popup works:
- Syntax highlighting for question text
- Multiple choice support (buttons/completing-read)
- Rich text rendering (markdown, etc.)
- Custom popup sizes/positions
- History/undo support

## Integration with Existing Code

### Backward Compatibility

Keep `mr-x/ask-user-question` as fallback:

```elisp
(condition-case err
    (mr-x/ask-user-popup question header)
  (void-function
    ;; Popup function not defined, use simple version
    (mr-x/ask-user-question question header)))
```

### No Changes to MCP Server Core

The MCP server in `src/index.ts` doesn't need changes:
- Same tool schema
- Same parameter validation
- Same error handling

**Only change:** Which elisp function emacsclient calls.

### No Changes to Error Classification

The error handling in `src/errors.ts` already covers:
- emacsclient not running
- Timeout
- User interruption
- Elisp errors

Popup mode doesn't introduce new error cases.

## Component Boundaries

### Node.js Layer Responsibilities

**What it does:**
- Spawn emacsclient process
- Pass question/header/timeout to elisp
- Wait for stdout (synchronous)
- Handle process errors/timeout
- Parse return value

**What it doesn't do:**
- UI rendering (that's Emacs)
- User interaction (that's Emacs)
- Window management (that's Emacs)
- Buffer lifecycle (that's elisp)

### Elisp Layer Responsibilities

**What it does:**
- Create and configure buffer
- Display as popup window
- Render question content
- Provide keybindings for submit/cancel
- Block execution with recursive-edit
- Capture user input
- Clean up buffer/window
- Return result string

**What it doesn't do:**
- Process spawning (that's Node.js)
- Timeout enforcement (that's Node.js)
- MCP protocol (that's Node.js)
- Logging (that's Node.js)

### Clear Interface Contract

**Node.js → Elisp:**
```typescript
emacsclient --eval "(mr-x/ask-user-popup \"question\" \"header\")"
```

**Elisp → Node.js:**
```
"user response string"  // Written to stdout
```

**Contract guarantees:**
1. Elisp function blocks until user completes interaction
2. Elisp function returns string value (empty if cancelled)
3. Elisp function handles its own cleanup
4. Node.js handles timeout by killing process

## Architecture Patterns

### Pattern 1: Synchronous Popup with recursive-edit

**When:** Need to block until user responds
**How:** recursive-edit + buffer-local result storage
**Example:** Current ask-user-mcp requirement

```elisp
(defun blocking-popup (question)
  (let ((buffer (generate-new-buffer "*Popup*"))
        (result nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert question)
            (popup-mode))
          (pop-to-buffer buffer)
          (recursive-edit)
          (with-current-buffer buffer
            (setq result popup--result)))
      (kill-buffer buffer))
    result))
```

### Pattern 2: Display with quit-restore

**When:** Want standard Emacs window cleanup behavior
**How:** Let `quit-window` handle restoration automatically

```elisp
;; Display buffer, quit-restore is set automatically
(display-buffer buffer '((display-buffer-at-bottom)))

;; User presses q or custom key
(quit-window t)  ; Restores previous window config, kills buffer
```

### Pattern 3: unwind-protect for Cleanup

**When:** Need guaranteed cleanup even if user aborts (C-g)
**How:** Wrap all buffer operations in unwind-protect

```elisp
(unwind-protect
    (progn
      ;; Main logic
      (create-buffer)
      (display-buffer)
      (recursive-edit)
      (capture-result))
  ;; Always runs, even on C-g or error
  (cleanup-buffer)
  (cleanup-window))
```

## Anti-Patterns to Avoid

### Anti-Pattern 1: Async Callbacks

**What:** Using hooks/timers/callbacks instead of blocking
**Why bad:** emacsclient returns immediately, Node.js gets no result
**Instead:** Use recursive-edit to block synchronously

```elisp
;; DON'T DO THIS
(defun bad-popup (question)
  (pop-to-buffer "*Question*")
  (add-hook 'some-hook #'handle-response)  ; Returns immediately!
  nil)  ; emacsclient gets nil

;; DO THIS
(defun good-popup (question)
  (pop-to-buffer "*Question*")
  (recursive-edit)  ; Blocks until user responds
  (get-response))  ; emacsclient gets actual response
```

### Anti-Pattern 2: Global State for Results

**What:** Using global variables to store user responses
**Why bad:** Race conditions if multiple popups open, hard to debug
**Instead:** Use buffer-local variables

```elisp
;; DON'T DO THIS
(defvar global-result nil)  ; Bad: shared across all buffers

;; DO THIS
(defvar-local buffer-result nil)  ; Good: scoped to buffer
```

### Anti-Pattern 3: Manual Window Deletion

**What:** Using `delete-window` directly instead of `quit-window`
**Why bad:** Loses window history, can't restore previous config
**Instead:** Use quit-window which respects quit-restore parameter

```elisp
;; DON'T DO THIS
(delete-window (get-buffer-window buf))  ; Loses restoration info

;; DO THIS
(quit-window t (get-buffer-window buf))  ; Proper cleanup
```

### Anti-Pattern 4: Forgetting Cleanup on Error

**What:** Not using unwind-protect for buffer cleanup
**Why bad:** Buffer/window leaks when user presses C-g or error occurs
**Instead:** Always wrap in unwind-protect

```elisp
;; DON'T DO THIS
(defun bad-popup (q)
  (let ((buf (generate-new-buffer "*Q*")))
    (pop-to-buffer buf)
    (recursive-edit)  ; If C-g here, buffer never cleaned up!
    (kill-buffer buf)))

;; DO THIS
(defun good-popup (q)
  (let ((buf (generate-new-buffer "*Q*")))
    (unwind-protect
        (progn
          (pop-to-buffer buf)
          (recursive-edit))
      (kill-buffer buf))))  ; Always runs
```

## Testing Strategy

### Unit Test: Elisp Functions

Test elisp in isolation:

```elisp
;; Test 1: Basic display
(mr-x/ask-user-popup "Test question" "Test header")
;; Verify: buffer appears, contains question

;; Test 2: Submit
;; In buffer, type response, press C-c C-c
;; Verify: returns typed response

;; Test 3: Cancel
;; Press C-c C-k
;; Verify: returns empty string

;; Test 4: Abort
;; Press C-g
;; Verify: buffer cleaned up, returns empty
```

### Integration Test: Node.js ↔ Emacs

Test via emacsclient:

```bash
# Test from command line
emacsclient --eval '(mr-x/ask-user-popup "What is your name?" "Test")'
# Type response in Emacs, submit
# Verify: response printed to stdout

# Test timeout (in Node.js test)
# Set 5 second timeout, wait without responding
# Verify: process killed, error returned
```

### End-to-End Test: MCP Tool

Test through MCP protocol:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "AskUserQuestion",
    "arguments": {
      "question": "What is your favorite color?",
      "header": "Testing popup"
    }
  }
}
```

Verify response flows back through MCP protocol.

## Scalability Considerations

Not applicable - single-user, single-session tool. No concurrency or scaling issues.

**Possible edge case:** Multiple rapid questions
- Each gets own buffer
- Each blocks on its own recursive-edit
- Emacs handles multiple recursive-edit levels (shows in mode-line: `[[...]]`)
- No code changes needed

## Sources

**Official Documentation:**
- [Recursive Editing (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Recursive-Editing.html)
- [Quitting Windows (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Quitting-Windows.html)
- [Derived Modes (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Derived-Modes.html)
- [Cleanups (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Cleanups.html)
- [Major Mode Conventions (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Major-Mode-Conventions.html)

**Community Resources:**
- [EmacsWiki: Recursive Edit](https://www.emacswiki.org/emacs/RecursiveEdit)
- [Mastering Emacs: Evaluating Elisp](https://www.masteringemacs.org/article/evaluating-elisp-emacs)
- [Emacs: commands in popup frames with 'emacsclient' - Protesilaos Stavrou](https://protesilaos.com/codelog/2024-09-19-emacs-command-popup-frame-emacsclient/)

**Package Examples:**
- [karthink/popper - Popup buffer management](https://github.com/karthink/popper)
- [auto-complete/popup-el - Visual popup library](https://github.com/auto-complete/popup-el)
- [Using org-capture (The Org Manual)](https://www.gnu.org/software/emacs/manual/html_node/org/Using-capture.html) - Example of popup buffer that blocks and returns result
