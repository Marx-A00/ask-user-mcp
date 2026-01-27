# Phase 3: Emacs UX & Production Hardening - Research

**Researched:** 2026-01-27
**Domain:** Emacs Lisp minibuffer styling, Pino structured logging, technical documentation
**Confidence:** HIGH

## Summary

This phase implements styled Emacs prompts, production-grade audit logging, and self-service documentation. The research reveals established patterns in each area: Emacs provides `propertize` with face properties for minibuffer styling, Pino offers child loggers and field formatters for structured Q&A logging, and MCP servers follow standardized README patterns for agent integration.

The standard approach combines:
1. **Emacs styling**: `propertize` with built-in faces (`bold`, `minibuffer-prompt`) ensures theme compatibility
2. **Error handling**: `condition-case` with `void-function` detection enables graceful fallback to `read-string`
3. **Audit logging**: Pino child loggers with debug-level Q&A events prevent production noise
4. **Documentation**: MCP server READMEs use agent-shell JSON config examples with absolute paths

**Primary recommendation:** Use `propertize` with standard Emacs faces for styling, implement `condition-case` for fallback, use Pino child loggers for contextual Q&A logging at debug level, and provide copy-paste ready JSON configuration examples in documentation.

## Standard Stack

The established libraries/tools for this domain:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| Emacs Lisp built-ins | bundled | Text properties, error handling | Native Emacs functionality, no dependencies |
| Pino | 9.x | Structured JSON logging | Already integrated in Phase 2, high performance |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| pino formatters | bundled | Custom field formatting | Standardizing log field names (level, bindings) |
| pino child loggers | bundled | Contextual logging | Adding request-scoped fields (tool call ID, question context) |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| `propertize` | Custom face definitions | Standard faces adapt to user themes automatically |
| Pino child loggers | Manual field passing | Child loggers ensure fields appear in all scoped logs |
| Debug log level | Info level | Debug prevents Q&A noise in production, easily enabled when needed |

**Installation:**
No additional packages required. Pino already installed in Phase 2.

## Architecture Patterns

### Recommended Project Structure
```
/
├── src/
│   ├── logger.ts           # Existing Pino logger (Phase 2)
│   ├── emacs-interface.ts  # Add Q&A logging here
│   └── index.ts
├── emacs/
│   └── ask-user.el         # Custom Emacs function
├── README.md               # Setup instructions
├── TROUBLESHOOTING.md      # Debugging guide
└── docs/                   # Optional detailed docs
    └── emacs-config.md     # Emacs configuration reference
```

### Pattern 1: Styled Minibuffer Prompts with Fallback-Safe Text Properties
**What:** Use `propertize` to style prompt text with standard Emacs faces, applying properties only to prompt (not user input).
**When to use:** Custom Emacs functions that display styled prompts in minibuffer.
**Example:**
```elisp
;; Source: GNU Emacs Lisp Reference Manual - Text from Minibuffer
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-from-Minibuffer.html

(defun mr-x/ask-user-question (question &optional header timeout-ms)
  "Ask user a QUESTION with styled prompt.
Optional HEADER provides context. TIMEOUT-MS controls timeout (default 300000ms)."
  (let* ((header-text (if header
                          (propertize (format "Claude asks (%s): " header)
                                      'face '(bold minibuffer-prompt))
                        (propertize "Claude asks: "
                                    'face '(bold minibuffer-prompt))))
         (question-text (propertize question 'face 'minibuffer-prompt))
         (full-prompt (concat header-text question-text)))
    (read-string full-prompt)))
```

**Key insight:** Text properties in prompt are preserved but stripped from user input by default (see `minibuffer-allow-text-properties` documentation). This ensures styling only affects display, not returned data.

### Pattern 2: Error Handling with Graceful Fallback
**What:** Detect undefined function errors with `condition-case`, retry with fallback implementation.
**When to use:** TypeScript calling Emacs functions that may not be defined.
**Example:**
```typescript
// Source: GNU Emacs Lisp Reference Manual - Handling Errors
// https://www.gnu.org/software/emacs/manual/html_node/elisp/Handling-Errors.html

async function askUserWithFallback(question: string, header?: string): Promise<string> {
  const emacsCode = `
    (condition-case err
        (mr-x/ask-user-question "${question}" ${header ? `"${header}"` : "nil"})
      (void-function
       (progn
         (message "Warning: mr-x/ask-user-question not defined, using read-string")
         (read-string "Claude asks: ${question}"))))
  `;
  
  const result = await emacsClient.eval(emacsCode);
  return result;
}
```

**Key insight:** `condition-case` with `void-function` handler catches undefined function errors specifically. `progn` allows multiple forms in handler (message + fallback).

### Pattern 3: Contextual Q&A Logging with Pino Child Loggers
**What:** Create child logger per tool call with contextual fields, log Q&A events at debug level.
**When to use:** Logging Q&A interactions with request-scoped context (tool call ID, timing, metadata).
**Example:**
```typescript
// Source: Better Stack - Complete Guide to Pino Logging
// https://betterstack.com/community/guides/logging/how-to-install-setup-and-use-pino-to-log-node-js-applications/

import logger from './logger';

function askUserTool(args: { question: string; header?: string; timeout_ms?: number }) {
  const startTime = Date.now();
  const toolCallId = generateId(); // or from MCP context
  
  // Create child logger with contextual fields
  const qaLogger = logger.child({
    toolCallId,
    component: 'ask-user',
    question: args.question,
    header: args.header,
    timeout_ms: args.timeout_ms || 300000,
  });
  
  qaLogger.debug('Q&A initiated');
  
  try {
    const response = await askUser(args);
    qaLogger.debug({
      response,
      duration_ms: Date.now() - startTime,
      success: true,
    }, 'Q&A completed');
    
    return response;
  } catch (err) {
    qaLogger.error({
      error: err,
      duration_ms: Date.now() - startTime,
      success: false,
    }, 'Q&A failed');
    throw err;
  }
}
```

**Key insight:** Child logger fields (toolCallId, question, etc.) automatically appear in all subsequent logs within scope. Debug level prevents production noise—only visible when `LOG_LEVEL=debug`.

### Pattern 4: MCP Server Documentation Structure
**What:** README with quick start, agent-shell configuration example, and troubleshooting links.
**When to use:** MCP servers that integrate with agent-shell or other MCP clients.
**Example structure:**
```markdown
# ask-user-mcp

Ask the user questions from Claude via Emacs.

## Quick Start

1. Build the server:
   ```bash
   npm install
   npm run build
   ```

2. Add to agent-shell configuration (`~/.config/agent-shell/mcp-servers.json`):
   ```json
   {
     "mcpServers": {
       "ask-user": {
         "command": "node",
         "args": ["/absolute/path/to/ask-user-mcp/build/index.js"]
       }
     }
   }
   ```

3. Configure Emacs (see Emacs Setup section below)

## Emacs Setup

Add to your Emacs config:

```elisp
;; Load ask-user function
(load-file "/absolute/path/to/ask-user-mcp/emacs/ask-user.el")
```

## Troubleshooting

See [TROUBLESHOOTING.md](./TROUBLESHOOTING.md) for common issues:
- Emacs server not responding
- Function not defined errors
- Timeout issues
```

**Source:** Multiple MCP server examples from GitHub and MCP documentation
- https://github.com/modelcontextprotocol/typescript-sdk
- https://modelcontextprotocol.io/docs/develop/build-server

### Anti-Patterns to Avoid
- **Logging Q&A at info level**: Creates excessive production noise for every question. Use debug level instead.
- **Inline field passing**: Manually passing contextual fields to every log call instead of using child loggers.
- **Relative paths in documentation**: Agent-shell requires absolute paths in JSON config; relative paths will fail.
- **Custom face definitions**: Overrides user themes. Use standard faces (`bold`, `minibuffer-prompt`) that adapt automatically.
- **Using console.log in MCP servers**: Writes to stdout, breaking JSON-RPC protocol. Always use stderr (Pino default).

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Log field formatting | Custom JSON stringification | Pino formatters | Handles edge cases (circular refs, errors), standardizes field names |
| Error message styling | ANSI color codes in elisp | Emacs faces | Theme-aware, accessible, standard |
| Request context tracking | Global variables | Pino child loggers | Thread-safe, automatic field propagation, scoped cleanup |
| Minibuffer input validation | Custom error checking | Emacs built-in validation | Handles edge cases (empty input, special chars) |

**Key insight:** Pino child loggers solve request-scoped logging correctly—fields propagate automatically without manual passing, and cleanup happens when scope exits. Custom solutions leak context or require manual cleanup.

## Common Pitfalls

### Pitfall 1: Text Properties Leak to User Input
**What goes wrong:** Applying text properties to entire minibuffer content including user's typed response, causing properties to leak into returned string.
**Why it happens:** Confusion about where text properties apply—prompt vs input.
**How to avoid:** Only apply `propertize` to prompt string passed to `read-string`. User input is separate and won't inherit properties (unless `minibuffer-allow-text-properties` is set, which strips face properties anyway).
**Warning signs:** Tests show face properties in returned strings, or styled text appears in logs.

### Pitfall 2: Catching Too-Broad Error Conditions
**What goes wrong:** Using `(error ...)` handler in `condition-case` catches ALL errors, masking real bugs.
**Why it happens:** Following simple examples without understanding error taxonomy.
**How to avoid:** Be specific—use `void-function` for undefined functions, `file-error` for file operations, etc. Only catch errors you can handle gracefully.
**Warning signs:** Unexpected errors silently become fallback cases; real bugs hidden in production.
**Example:**
```elisp
;; BAD: Catches everything, masks bugs
(condition-case err
    (mr-x/ask-user-question question)
  (error (read-string question)))  ; Might hide typos, syntax errors, etc.

;; GOOD: Only catches undefined function
(condition-case err
    (mr-x/ask-user-question question)
  (void-function (read-string question)))  ; Other errors still surface
```

### Pitfall 3: Logging at Wrong Level for Audit Trail
**What goes wrong:** Using `info` level for Q&A events floods production logs with noise, or using `trace` level makes audit trail invisible even during debugging.
**Why it happens:** Misunderstanding log level semantics—info is for business events, debug is for diagnostic events.
**How to avoid:** Use `debug` level for Q&A audit trail. This keeps production clean (default level is `info`) while enabling detailed Q&A logging with `LOG_LEVEL=debug` environment variable.
**Warning signs:** Production logs filled with Q&A events, or needing to set `LOG_LEVEL=trace` to see audit trail.

### Pitfall 4: Relative Paths in agent-shell Configuration
**What goes wrong:** agent-shell JSON config with relative paths fails to launch server.
**Why it happens:** Assumption that paths are resolved relative to config file location.
**How to avoid:** Always use absolute paths in `command` and `args` fields. Document this requirement prominently.
**Warning signs:** "command not found" errors in agent-shell, server fails to start.
**Example:**
```json
// BAD: Relative path
{
  "mcpServers": {
    "ask-user": {
      "command": "node",
      "args": ["./build/index.js"]  // Will fail
    }
  }
}

// GOOD: Absolute path
{
  "mcpServers": {
    "ask-user": {
      "command": "node",
      "args": ["/Users/user/projects/ask-user-mcp/build/index.js"]
    }
  }
}
```

### Pitfall 5: Forgetting to Handle Errors in Fallback Chain
**What goes wrong:** Fallback to `read-string` succeeds but `read-string` itself errors (e.g., non-interactive Emacs), leaving user with cryptic error.
**Why it happens:** Only handling first-level fallback, not considering fallback failure modes.
**How to avoid:** Wrap entire chain in error handling, provide meaningful error messages about what went wrong and how to fix it.
**Warning signs:** Error messages like "error in process filter" with no context.

## Code Examples

Verified patterns from official sources:

### Complete Styled Prompt Function with Error Handling
```elisp
;; Source: Composite of GNU Emacs Lisp Reference Manual examples
;; - Text from Minibuffer: https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-from-Minibuffer.html
;; - Handling Errors: https://www.gnu.org/software/emacs/manual/html_node/elisp/Handling-Errors.html

(defun mr-x/ask-user-question (question &optional header timeout-ms)
  "Ask user a QUESTION with styled prompt.

QUESTION is the question text to display.
HEADER is optional context shown as \"Claude asks (HEADER):\" prefix.
TIMEOUT-MS is currently not implemented (Emacs read-string doesn't support timeouts natively).

Returns the user's response string."
  (interactive "sQuestion: ")
  (let* ((header-part (if header
                          (format "(%s) " header)
                        ""))
         (prefix (propertize (concat "Claude asks " header-part)
                             'face 'bold))
         (question-text (propertize question
                                    'face 'minibuffer-prompt))
         (full-prompt (concat prefix question-text " ")))
    (read-string full-prompt)))
```

### Pino Logger with Q&A Debug Level Configuration
```typescript
// Source: Better Stack - Complete Guide to Pino Logging
// https://betterstack.com/community/guides/logging/how-to-install-setup-and-use-pino-to-log-node-js-applications/

import pino from "pino";

export const logger = pino({
  level: process.env.LOG_LEVEL || "info",
  formatters: {
    level: (label) => ({ level: label }),
    bindings: (bindings) => ({
      pid: bindings.pid,
      // Omit hostname if not needed to reduce log size
    }),
  },
});

// Usage in ask-user tool
export function createQALogger(toolCallId: string, question: string, header?: string) {
  return logger.child({
    toolCallId,
    component: 'ask-user',
    question,
    ...(header && { header }),
  });
}
```

### MCP Server README Template (Minimal for Solo Developer)
```markdown
# ask-user-mcp

MCP server for asking the user questions via Emacs.

## Setup

### 1. Build
```bash
npm install
npm run build
```

### 2. Configure agent-shell

Edit `~/.config/agent-shell/mcp-servers.json`:
```json
{
  "mcpServers": {
    "ask-user": {
      "command": "node",
      "args": ["/REPLACE/WITH/ABSOLUTE/PATH/ask-user-mcp/build/index.js"]
    }
  }
}
```

### 3. Configure Emacs

Add to your init file:
```elisp
(load-file "/REPLACE/WITH/ABSOLUTE/PATH/ask-user-mcp/emacs/ask-user.el")
```

## Troubleshooting

**Q: "void function" error**
A: Custom function not loaded. Check Emacs config path is correct and eval the elisp file.

**Q: "Emacs server timeout"**
A: Emacs server not responding. Check server is running with `M-x server-start`.

See [TROUBLESHOOTING.md](./TROUBLESHOOTING.md) for detailed debugging steps.
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| `read-from-minibuffer` | `read-string` for simple prompts | Emacs 20+ | `read-string` is specialized, simpler API for string input |
| Numeric log levels | String labels with formatters | Pino 7.0+ (2021) | Better human readability in logs |
| Manual bindings management | Child loggers | Pino 5.0+ (2019) | Automatic context propagation |
| Global logger instance | Child loggers per request | Industry shift 2020+ | Better correlation, safer concurrency |

**Deprecated/outdated:**
- **`minibuffer-complete`**: Replaced by `completion-at-point` in modern Emacs (v24+). Not relevant for basic string prompts.
- **Pino pretty-print in production**: Pino v7+ docs strongly recommend JSON in production, pino-pretty only for development.

## Open Questions

Things that couldn't be fully resolved:

1. **Emacs timeout implementation**
   - What we know: Emacs `read-string` doesn't support native timeouts. Would need asynchronous approach (timers + `abort-recursive-edit`) or C-level changes.
   - What's unclear: Whether timeout is actually needed in Phase 3, or if server-side timeout (already implemented in Phase 2) is sufficient.
   - Recommendation: Defer Emacs-side timeout to future phase. Server timeout_ms parameter already handles this at MCP protocol level.

2. **Log retention strategy**
   - What we know: Pino logs to stderr, agent-shell or systemd captures logs. Decision explicitly deferred to user/agent-shell.
   - What's unclear: Whether to recommend specific retention tools (logrotate, journalctl).
   - Recommendation: Document that logs go to stderr, mention common tools (journalctl for systemd, shell redirection for standalone), but don't prescribe specific retention.

3. **Testing styled prompts automatically**
   - What we know: Text properties are visual, hard to test in automated tests without Emacs GUI.
   - What's unclear: Best practice for testing propertize'd text in elisp unit tests.
   - Recommendation: Test that properties are applied (check with `get-text-property`), manual verification of visual appearance. Document expected appearance in comments.

## Sources

### Primary (HIGH confidence)
- [GNU Emacs Lisp Reference - Text from Minibuffer](https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-from-Minibuffer.html) - `read-string`, `propertize`, text properties in prompts
- [GNU Emacs Lisp Reference - Handling Errors](https://www.gnu.org/software/emacs/manual/html_node/elisp/Handling-Errors.html) - `condition-case`, `void-function` error handling
- [Better Stack - Complete Guide to Pino Logging](https://betterstack.com/community/guides/logging/how-to-install-setup-and-use-pino-to-log-node-js-applications/) - Child loggers, formatters, production config
- [GNU Emacs Manual - Standard Faces](https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html) - `bold`, `minibuffer-prompt`, `warning` faces

### Secondary (MEDIUM confidence)
- [Mastering Emacs - use-package](https://www.masteringemacs.org/article/spotlight-use-package-a-declarative-configuration-tool) - use-package patterns, verified against official docs
- [Model Context Protocol - TypeScript SDK](https://github.com/modelcontextprotocol/typescript-sdk) - MCP server setup patterns
- [SigNoz - Pino Logger Guide 2026](https://signoz.io/guides/pino-logger/) - Production best practices
- [VS Code - MCP Servers Configuration](https://code.visualstudio.com/docs/copilot/customization/mcp-servers) - MCP JSON config format

### Tertiary (LOW confidence)
- WebSearch results for troubleshooting guide best practices - General software debugging patterns (5-step framework: Reproduce, Isolate, Analyze, Correct, Prevent)
- WebSearch results for MCP agent-shell configuration - Examples from multiple MCP servers, patterns cross-verified

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - Official Emacs docs + existing Pino integration
- Architecture: HIGH - Verified patterns from GNU Emacs manual and Pino official guides
- Pitfalls: MEDIUM - Based on common issues in elisp/Node.js, some from experience vs documentation

**Research date:** 2026-01-27
**Valid until:** ~60 days (Emacs and Pino are stable; documentation patterns evolve slowly)
