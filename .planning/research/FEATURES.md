# Feature Landscape: Emacs Popup Selection UI

**Domain:** Interactive selection buffers for Emacs
**Researched:** 2026-02-08
**Confidence:** HIGH

## Executive Summary

Emacs popup selection UIs have evolved into two distinct patterns: **minibuffer completion** (vertico, ivy, helm) and **dedicated popup buffers** (popper, popwin, transient). Both share common UX expectations around keyboard navigation, visual feedback, and predictable positioning. For AskUserQuestion MCP server v2, a popup buffer approach is most appropriate because it provides more visual space for formatted questions and options while maintaining familiar navigation patterns.

The research identifies clear table stakes (C-n/C-p navigation, highlight current selection, RET to confirm), differentiators (contextual help, smooth visual transitions), and anti-features (mouse-only interaction, modal blocking).

## Table Stakes

Features users expect. Missing = product feels incomplete.

**Navigation & Selection**

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| C-n/C-p navigation | Universal Emacs standard for list navigation | Low | Also support arrow keys (down/up) |
| RET selects current item | Standard confirmation across all selection UIs | Low | Enter key completes the selection |
| C-g cancels | Universal Emacs cancel gesture | Low | Must clean up buffer and return error state |
| Visual highlight of current selection | Users need to see what they're selecting | Medium | Use face like `vertico-current` or `company-tooltip-selection` |
| First item selected by default | Reduces keystrokes for common case | Low | Cursor starts on first option |

**Display & Positioning**

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| Bottom-of-frame positioning | Matches popper, compilation buffers, REPLs | Low | Consistent with existing ephemeral buffers |
| Fixed height (~40% or configurable) | Predictable, doesn't fight for space | Low | Match user's popper configuration if available |
| Automatic cleanup on exit | Popup shouldn't linger after selection | Low | Delete window and kill buffer on RET/C-g |
| Read-only buffer content | Prevents accidental edits to options | Low | Only prompt area should be editable in free-text mode |

**Visual Feedback**

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| Clear question text | User must understand what's being asked | Low | Bold or distinct face for question header |
| Option numbering or bullets | Makes list structure obvious | Low | Especially important for multiple choice |
| Distinct selection indicator | More than just highlight - cursor/arrow/> | Medium | Visual marker like `> Option text` on current line |
| Status information | Current position (e.g., "3/5") | Medium | Shows context in large option lists |

**Mode Switching**

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| Free-text input mode | Not all questions have predefined options | Medium | Different mode with editable prompt area |
| Clear mode indication | User must know if they're selecting vs typing | Medium | Mode-line indicator or prompt text difference |
| TAB for completion (in free-text) | Standard Emacs completion gesture | Medium | Optional but expected by power users |

## Differentiators

Features that set product apart. Not expected, but valued.

**Enhanced Navigation**

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| M-n/M-p as navigation alternatives | Matches completion frameworks like vertico | Low | Provides choice without conflict with other bindings |
| C-v/M-v for page scrolling | Handles long option lists gracefully | Low | Only needed if >10 options |
| Home/End jump to first/last | Fast navigation in longer lists | Low | Common in vertico, appreciated by users |
| Number keys (1-9) for quick select | Instant selection without navigation | Medium | Company-mode pattern, very efficient for short lists |

**Visual Polish**

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| Smooth window creation | Doesn't feel jarring | Low | Use `display-buffer` properly |
| Matching input highlighting | Show what text matches (if filtering) | Medium | Only relevant if adding incremental filtering |
| Icons or prefixes for option types | Visual categorization | Medium | Low priority unless options have natural categories |
| Contextual help (C-h m) | Users can learn keybindings | Low | Standard Emacs discoverability pattern |

**Smart Behavior**

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| Remember last position | If same question asked twice, start where left off | Medium | Requires state tracking in Emacs |
| Incremental filtering | Type to narrow options | High | Substantial complexity, may conflict with free-text mode |
| Grouped options | Organize by category with headings | Medium | Only useful for complex option sets |

**Integration**

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| Popper integration | Respect user's popup config | Low | Use popper-display-control if available |
| Theme compatibility | Honor user's color scheme | Low | Use semantic faces, not hard-coded colors |
| Evil mode compatibility | Works with evil navigation (j/k) | Medium | Detect evil-mode and add j/k bindings |

## Anti-Features

Features to explicitly NOT build. Common mistakes in this domain.

| Anti-Feature | Why Avoid | What to Do Instead |
|--------------|-----------|-------------------|
| Mouse-only selection | Breaks keyboard-driven workflow | Always provide keyboard navigation first |
| Modal blocking | Freezes Emacs until answered | Let C-g cancel gracefully |
| Tiny minibuffer display | Not enough space for formatted questions | Use proper popup buffer at bottom |
| Auto-dismiss on focus loss | User might switch windows to check something | Only dismiss on explicit C-g or RET |
| Complex keybinding scheme | Learning curve too steep | Stick to standard Emacs conventions |
| Animation/transitions | Distracting, feels un-Emacs-like | Instant display is fine |
| Rich text formatting (HTML/Markdown) | Over-engineered for simple Q&A | Plain text with font faces is sufficient |
| Persistent history buffer | Clutters buffer list | Temporary buffer with clear naming |
| Multiple simultaneous popups | Confusing state | Queue or block subsequent questions |
| Custom completion framework | Reinventing wheel | Use built-in completing-read for free-text if possible |

## Mode-Specific Patterns

### Selection Mode (Multiple Choice)

**Expected Behavior:**
- Display options as numbered or bulleted list
- Highlight current selection with distinct face
- Navigate with C-n/C-p (or arrows)
- Confirm with RET
- Cancel with C-g returns empty/error

**Buffer Structure:**
```
┌─────────────────────────────────────────┐
│ Claude asks: What color do you prefer?  │
│                                         │
│  > Red     (current selection)          │
│    Blue                                 │
│    Green                                │
│    Yellow                               │
│                                         │
│ [1/4] C-n/C-p: navigate  RET: select  C-g: cancel │
└─────────────────────────────────────────┘
```

**Reference:** Company-mode tooltip, vertico list display

### Free-Text Mode

**Expected Behavior:**
- Display question at top
- Provide editable input area
- Show example/placeholder if helpful
- Standard editing keybindings (C-a/C-e, C-k, etc.)
- Submit with RET, cancel with C-g

**Buffer Structure:**
```
┌─────────────────────────────────────────┐
│ Claude asks: What is your name?         │
│                                         │
│ Your answer: _                          │
│                                         │
│ RET: submit  C-g: cancel                │
└─────────────────────────────────────────┘
```

**Reference:** Minibuffer prompt, read-string behavior

## Feature Dependencies

```
Basic Navigation (C-n/C-p, RET, C-g)
  ↓
Visual Highlighting
  ↓
Popup Positioning (bottom of frame)
  ↓
Mode Switching (selection vs free-text)
  ↓
Enhanced Features (quick select, scrolling, etc.)
```

**Critical Path for MVP:**
1. Popup buffer creation at bottom of frame
2. C-n/C-p navigation with highlight
3. RET selects, C-g cancels
4. Clean buffer lifecycle (create/destroy)
5. Free-text input mode with same popup UI

**Can Defer:**
- Number key quick-select
- Incremental filtering
- Popper integration (use basic display-buffer first)
- Evil mode compatibility

## MVP Recommendation

For v2 MVP, prioritize:

1. **Popup buffer at bottom** - Core differentiator from v1 minibuffer
2. **Selection mode with C-n/C-p** - Primary use case for multiple choice
3. **Visual highlight** - Critical feedback
4. **RET/C-g handling** - Table stakes for any selection UI
5. **Free-text mode** - Fallback for open-ended questions
6. **Read-only options, editable prompt** - Prevents confusion

Defer to post-MVP:
- Number key quick-select: Nice but not critical, adds keybinding complexity
- Incremental filtering: Substantial complexity, may not be needed for typical Q&A
- Popper integration: Can start with basic display-buffer, add later
- Page scrolling (C-v/M-v): Only needed for very long lists (uncommon)

## Implementation Notes

### Keyboard Navigation Bindings

**Standard (all modes):**
- `C-n` / `<down>`: Next option
- `C-p` / `<up>`: Previous option
- `RET`: Confirm selection
- `C-g`: Cancel

**Enhanced (optional):**
- `M-n` / `M-p`: Alternative navigation (avoid conflict with custom bindings)
- `C-v` / `M-v`: Page down/up (if >10 items)
- `<home>` / `<end>`: Jump to first/last
- `1-9`: Quick select (company-mode pattern)

### Visual Faces to Use

Reuse existing Emacs faces for theme compatibility:
- `bold` or `info-title-1` for question header
- `highlight` or `vertico-current` for current selection
- `shadow` for help text
- `completions-common-part` for matching text (if filtering)
- `default` for options

### Buffer Management

- Buffer name: `*Ask User: <truncated question>*`
- Window: Create with `display-buffer-at-bottom`
- Cleanup: Kill buffer and delete window on exit
- No buffer-list pollution: Use space prefix for truly ephemeral buffers?

## Sources

### High Confidence (Official Documentation & Code)

- [Vertico GitHub](https://github.com/minad/vertico) - Vertical completion UI patterns, navigation with arrow keys, highlighting
- [Popper GitHub](https://github.com/karthink/popper) - Popup buffer positioning at bottom, context awareness, one-key toggling
- [Company Mode](http://company-mode.github.io/) - C-n/C-p navigation, number key quick-select, visual feedback
- [GNU Emacs Manual: Completion Commands](https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Commands.html) - TAB, RET, navigation standards
- [GNU Emacs Manual: Completion Options](https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Options.html) - completions-highlight-face, completion-auto-select

### Medium Confidence (Community Documentation)

- [From Ivy to Vertico](https://midirus.com/blog/from-ivy-to-vertico) - Migration patterns, UX expectations from ivy users
- [Streamline Completions with Vertico](https://systemcrafters.net/emacs-tips/streamline-completions-with-vertico/) - Modern completion UI patterns
- [Transient Menus in Emacs](https://jd.codes/posts/transient-emacs/) - Key-driven menu design, state persistence
- [Mastering Emacs: Understanding Minibuffer Completion](https://www.masteringemacs.org/article/understanding-minibuffer-completion) - Standard completion behaviors
- [Mastering Emacs: Window Manager](https://www.masteringemacs.org/article/demystifying-emacs-window-manager) - display-buffer-alist patterns

### Low Confidence (General Web Search)

- [Emacs Carnival February 2026: Completion](https://sachachua.com/blog/2026/01/emacs-carnival-february-2026-completion/) - Recent community activity (2026), confirms completion as active topic
- [Corfu GitHub](https://github.com/minad/corfu) - In-buffer completion popup patterns
- Various Reddit/forum discussions about ivy/helm/vertico comparisons

---

**Research completeness:** All major domains covered (navigation, visual feedback, positioning, mode switching)

**Gaps to address:**
- Performance considerations for very large option lists (>100 items) - unlikely in Q&A context
- Accessibility features (screen reader support) - low priority for personal tool
- Multi-column layouts - unnecessary complexity for Q&A
