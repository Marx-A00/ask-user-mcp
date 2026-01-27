# Phase 2: Error Handling & Reliability - Research

**Researched:** 2026-01-26
**Domain:** Node.js process management, error handling, structured logging
**Confidence:** HIGH

## Summary

Error handling and reliability in Node.js MCP servers involves five interconnected domains: signal handling for graceful shutdown, child process lifecycle management, input validation with helpful messages, structured logging for debugging, and timeout implementation with clear feedback.

The standard approach uses native Node.js APIs (process signals, child_process events) combined with Pino for structured logging and Zod for validation. The key insight: errors should be descriptive for LLMs, not just developers. "Emacs server isn't running" is better than "ECONNREFUSED".

**Primary recommendation:** Return errors as tool result content with isError flag (not thrown exceptions), use Pino JSON logging to stderr, handle SIGINT/SIGTERM with child process cleanup, and parse emacsclient stderr for specific failure modes.

## Standard Stack

The established libraries/tools for this domain:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| pino | ^9.x | Structured JSON logging | 5x faster than alternatives, minimal overhead, default logger for Fastify |
| zod | ^4.x | Input validation | Already in Phase 1, excellent error customization |
| Node.js built-ins | - | process signals, child_process | Native APIs, no dependencies needed |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| pino-pretty | ^13.x | Development log formatting | Dev environment only, NOT production |
| zod-validation-error | ^3.x | User-friendly Zod errors | Optional: wraps ZodError in readable format |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| Pino | Winston | Winston is slower, more complex, but has transport plugins |
| Pino | Bunyan | Bunyan is unmaintained as of 2024 |
| Native validation | joi | Joi lacks TypeScript inference, more verbose |

**Installation:**
```bash
npm install pino
npm install --save-dev pino-pretty
```

## Architecture Patterns

### Recommended Project Structure
```
src/
├── index.ts              # Server setup, signal handlers
├── emacs-interface.ts    # Child process management, error detection
├── logger.ts             # Pino instance configuration
└── errors.ts             # Error classification helpers
```

### Pattern 1: Signal Handling with Child Cleanup
**What:** Install SIGINT/SIGTERM handlers that kill child processes before exit
**When to use:** Any server that spawns child processes
**Example:**
```typescript
// Source: https://nodejs.org/api/process.html
import process from 'node:process';

const activeProcesses = new Set<ChildProcess>();

function gracefulShutdown(signal: string) {
  logger.info({ signal }, 'Received termination signal');
  
  // Kill all active child processes
  for (const proc of activeProcesses) {
    logger.info({ pid: proc.pid }, 'Terminating child process');
    proc.kill('SIGTERM');
  }
  
  // Exit immediately (best effort shutdown)
  process.exitCode = 0;
}

process.on('SIGINT', () => gracefulShutdown('SIGINT'));
process.on('SIGTERM', () => gracefulShutdown('SIGTERM'));
```

### Pattern 2: Child Process Error Detection
**What:** Listen to error, exit, close events and parse stderr for specific failures
**When to use:** When spawning external commands that can fail in multiple ways
**Example:**
```typescript
// Source: https://nodejs.org/api/child_process.html
const proc = spawn('emacsclient', ['--eval', expr]);

let stderr = '';
proc.stderr?.on('data', (data) => {
  stderr += data.toString();
});

proc.on('error', (err) => {
  // Process spawn failed (e.g., command not found)
  throw new Error(`Failed to spawn emacsclient: ${err.message}`);
});

proc.on('exit', (code, signal) => {
  if (code !== 0) {
    // Parse stderr to classify error
    if (stderr.includes("can't find socket")) {
      throw new Error("Emacs server isn't running. Start it with M-x server-start");
    }
    throw new Error(`emacsclient failed (code ${code}): ${stderr}`);
  }
});
```

### Pattern 3: Timeout with AbortController
**What:** Use timeout option with spawn() and detect timeout vs normal exit
**When to use:** Long-running child processes that need configurable timeouts
**Example:**
```typescript
// Source: https://nodejs.org/api/child_process.html
const proc = spawn('emacsclient', args, {
  timeout: timeoutMs,
  killSignal: 'SIGTERM'
});

proc.on('exit', (code, signal) => {
  if (signal === 'SIGTERM' && proc.killed) {
    // Could be timeout or manual kill - no way to distinguish!
    throw new Error(`Question timed out after ${timeoutMs}ms waiting for response`);
  }
});
```

### Pattern 4: MCP Tool Error Responses
**What:** Return errors as content with isError flag, not thrown exceptions
**When to use:** All MCP tool implementations
**Example:**
```typescript
// Source: https://apxml.com/courses/getting-started-model-context-protocol/chapter-3-implementing-tools-and-logic/error-handling-reporting
server.registerTool("AskUserQuestion", schema, async (args) => {
  try {
    const result = await askViaEmacs(args.question);
    return { content: [{ type: "text", text: result }] };
  } catch (error) {
    // Return error as content so LLM can see and handle it
    return {
      isError: true,
      content: [{
        type: "text",
        text: error instanceof Error ? error.message : String(error)
      }]
    };
  }
});
```

### Pattern 5: Pino Structured Logging
**What:** Log JSON to stderr with context fields, use child loggers for scoped context
**When to use:** All production logging
**Example:**
```typescript
// Source: https://github.com/pinojs/pino
import pino from 'pino';

const logger = pino({
  level: process.env.LOG_LEVEL || 'info',
  formatters: {
    level: (label) => ({ level: label })
  }
});

// Log with structured context
logger.info({ tool: 'AskUserQuestion', questionLength: q.length }, 'Asking user');

// Use child logger for request context
const requestLogger = logger.child({ requestId: uuid() });
requestLogger.error({ err, code: proc.exitCode }, 'emacsclient failed');
```

### Pattern 6: Zod Error Customization
**What:** Provide custom error messages at schema level for user-friendly validation errors
**When to use:** When Zod's default messages aren't helpful for LLMs
**Example:**
```typescript
// Source: https://zod.dev/error-customization
const schema = z.object({
  question: z.string().min(1, "Question cannot be empty"),
  timeout_ms: z.number()
    .min(30000, "Timeout must be at least 30 seconds")
    .max(30 * 60000, "Timeout cannot exceed 30 minutes")
    .optional()
});

// Handle validation errors
const result = schema.safeParse(input);
if (!result.success) {
  return {
    isError: true,
    content: [{
      type: "text",
      text: `Invalid input: ${result.error.issues.map(i => i.message).join(', ')}`
    }]
  };
}
```

### Anti-Patterns to Avoid
- **Calling process.exit() in signal handlers:** Prevents async cleanup, may lose logs. Use process.exitCode instead.
- **Using pino-pretty in production:** Adds overhead and negates Pino's performance benefits. Only use in development.
- **Throwing errors from tool handlers:** MCP spec requires returning isError flag. Thrown errors become protocol errors.
- **Vague error messages:** "Error: failed" doesn't help LLMs. Include what, why, and how to fix.
- **Ignoring stderr:** Critical error details are often in stderr, not exit code.

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Structured logging | Custom JSON formatter | pino | Battle-tested, 5x faster, handles edge cases (circular refs, errors) |
| Input validation | Manual type checks | zod | Type inference, composable schemas, custom error messages |
| Signal handling | Simple process.on('SIGINT') | Tracked child processes + exitCode | Prevents zombie processes, handles force-kill scenarios |
| Timeout implementation | setTimeout + manual cleanup | spawn timeout option | Automatic cleanup, though lacks timeout detection (known limitation) |
| Error message formatting | String concatenation | Template literals with context | Maintainable, testable, consistent format |

**Key insight:** Error handling is full of edge cases. Emacsclient can fail because: command not found, server not running, socket permission denied, wrong socket file, timeout, user cancelled (C-g), or Emacs crashed. Each needs a different message.

## Common Pitfalls

### Pitfall 1: Timeout Detection Ambiguity
**What goes wrong:** spawn() timeout option sends SIGTERM, but exit event doesn't indicate if termination was due to timeout or another reason
**Why it happens:** Node.js issue #51561 - no way to know if timeout caused termination
**How to avoid:** Track timeout yourself with setTimeout and set flag before killing process
**Warning signs:** Users report "timed out" when process actually failed for other reasons

**Workaround:**
```typescript
let timedOut = false;
const timer = setTimeout(() => {
  timedOut = true;
  proc.kill('SIGTERM');
}, timeoutMs);

proc.on('exit', (code, signal) => {
  clearTimeout(timer);
  if (timedOut) {
    throw new Error('Timed out');
  }
  // Handle other exit reasons
});
```

### Pitfall 2: Emacs Server Detection False Positives
**What goes wrong:** Checking for "can't find socket" in stderr misses other ways Emacs server can be unavailable
**Why it happens:** emacsclient has multiple error messages for server unavailability depending on configuration
**How to avoid:** Parse for multiple patterns: "can't find socket", "No socket or alternate editor", "server-start"
**Warning signs:** User reports server is running but tool says it isn't

### Pitfall 3: Signal Handling Without Listener Cleanup
**What goes wrong:** Installing SIGTERM/SIGINT listeners removes Node.js default exit behavior but doesn't exit explicitly
**Why it happens:** Node.js docs: "Installing a listener removes default behavior"
**How to avoid:** Always set process.exitCode in signal handlers
**Warning signs:** Server hangs on Ctrl+C instead of exiting

### Pitfall 4: Logging to stdout in MCP Server
**What goes wrong:** MCP protocol uses stdout for JSON-RPC messages. Logs to stdout corrupt the protocol.
**Why it happens:** console.log() defaults to stdout
**How to avoid:** All logs must go to stderr. Use console.error() or pino (defaults to stderr).
**Warning signs:** Claude reports "invalid JSON-RPC message" or server connection fails

### Pitfall 5: Child Process Zombie Prevention
**What goes wrong:** Spawned emacsclient processes continue running after server terminates
**Why it happens:** No cleanup on SIGTERM/SIGINT
**How to avoid:** Track active child processes in a Set, kill them in signal handler
**Warning signs:** Multiple emacsclient processes accumulate over time (check with `ps aux | grep emacsclient`)

### Pitfall 6: Zod Error Messages for Machines
**What goes wrong:** Default Zod errors like "Expected string, received number" don't help LLMs fix their tool calls
**Why it happens:** Zod optimizes for developer DX, not LLM consumption
**How to avoid:** Always provide custom error messages describing valid input format
**Warning signs:** LLM repeatedly makes same validation error

### Pitfall 7: Async Cleanup in 'exit' Event
**What goes wrong:** Async operations in process.on('exit') don't run
**Why it happens:** Event loop is stopped by the time 'exit' fires
**How to avoid:** Use 'beforeExit' for async cleanup, or make cleanup synchronous
**Warning signs:** Logs or cleanup code in exit handler never execute

## Code Examples

Verified patterns from official sources:

### Comprehensive Error Classification
```typescript
// Source: Phase 2 CONTEXT.md decisions + emacsclient error patterns
function classifyEmacsError(stderr: string, code: number): string {
  // Server not running
  if (stderr.includes("can't find socket") || stderr.includes("server-start")) {
    return "Emacs server isn't running. Start it with M-x server-start or run: emacsclient -e '(server-start)'";
  }
  
  // Permission denied
  if (stderr.includes("Permission denied") || stderr.includes("socket")) {
    return "Cannot connect to Emacs server socket. Check permissions on ~/.emacs.d/server/";
  }
  
  // Command not found
  if (stderr.includes("command not found") || code === 127) {
    return "emacsclient command not found. Install Emacs or ensure it's in PATH.";
  }
  
  // Authentication failed
  if (stderr.includes("Authentication failed")) {
    return "Emacs server authentication failed. Check server-auth-dir configuration.";
  }
  
  // Generic failure
  return `emacsclient failed (exit code ${code}): ${stderr}`;
}
```

### Pino Logger Setup for MCP Server
```typescript
// Source: https://github.com/pinojs/pino README + MCP best practices
import pino from 'pino';

const logger = pino({
  level: process.env.LOG_LEVEL || 'info',
  
  // Format for readability in production logs
  formatters: {
    level: (label) => ({ level: label }),
    bindings: (bindings) => ({
      pid: bindings.pid,
      hostname: bindings.hostname
    })
  },
  
  // Redact sensitive data
  redact: {
    paths: ['question'],  // Don't log user questions
    censor: '[REDACTED]'
  },
  
  // Use pino-pretty only in development
  transport: process.env.NODE_ENV === 'development' ? {
    target: 'pino-pretty',
    options: { colorize: true }
  } : undefined
});

export default logger;
```

### Full Signal Handler with Child Cleanup
```typescript
// Source: https://nodejs.org/api/process.html + CONTEXT.md decisions
import process from 'node:process';
import type { ChildProcess } from 'node:child_process';
import logger from './logger.js';

const activeProcesses = new Set<ChildProcess>();

export function trackProcess(proc: ChildProcess): void {
  activeProcesses.add(proc);
  proc.on('exit', () => activeProcesses.delete(proc));
}

function gracefulShutdown(signal: string): void {
  logger.info({ signal, activeProcessCount: activeProcesses.size }, 'Shutting down');
  
  // Kill all active child processes (best effort)
  for (const proc of activeProcesses) {
    if (!proc.killed) {
      logger.info({ pid: proc.pid }, 'Terminated pending emacsclient process');
      proc.kill('SIGTERM');
    }
  }
  
  // Don't call process.exit() - let it exit naturally
  process.exitCode = 0;
}

// Install signal handlers
process.on('SIGINT', () => gracefulShutdown('SIGINT'));
process.on('SIGTERM', () => gracefulShutdown('SIGTERM'));
```

### Configurable Timeout with Clear Messages
```typescript
// Source: https://nodejs.org/api/child_process.html + Phase 2 decisions
import { spawn } from 'node:child_process';

interface AskOptions {
  timeout_ms?: number;  // Min 30s, max 30min
}

export async function askViaEmacs(
  question: string,
  options: AskOptions = {}
): Promise<string> {
  const timeout = options.timeout_ms ?? 5 * 60 * 1000;  // Default 5min
  
  // Validate timeout bounds
  if (timeout < 30_000) {
    throw new Error('Timeout must be at least 30 seconds');
  }
  if (timeout > 30 * 60_000) {
    throw new Error('Timeout cannot exceed 30 minutes');
  }
  
  const proc = spawn('emacsclient', ['--eval', expr], { timeout });
  
  // Track if timeout fired (workaround for Node.js limitation)
  let timedOut = false;
  const timer = setTimeout(() => {
    timedOut = true;
  }, timeout);
  
  return new Promise((resolve, reject) => {
    proc.on('exit', (code, signal) => {
      clearTimeout(timer);
      
      if (timedOut) {
        reject(new Error(
          `Question timed out after ${timeout / 1000} seconds waiting for response. ` +
          `Increase timeout_ms parameter if needed.`
        ));
      } else if (code !== 0) {
        reject(new Error(classifyEmacsError(stderr, code)));
      } else {
        resolve(stdout);
      }
    });
  });
}
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| Winston logging | Pino logging | 2023-2024 | 5x performance improvement, simpler API |
| Bunyan logging | Pino logging | 2024 | Bunyan unmaintained, Pino actively developed |
| Manual JSON parsing | Zod validation | 2023+ | Type safety, better error messages |
| Thrown errors in tools | isError flag | MCP 1.0 (2024) | LLMs can see and handle errors |
| process.exit() | process.exitCode | Best practice since Node 0.12 | Prevents data loss on shutdown |
| spawn() without timeout | timeout option | Node.js 15.13.0 (2021) | Built-in timeout support |

**Deprecated/outdated:**
- **Bunyan logger:** Unmaintained as of 2024. Use Pino instead.
- **Winston for performance-critical apps:** Pino is 5x faster with lower overhead.
- **process.exit() in signal handlers:** Risks data loss. Use process.exitCode.
- **Throwing errors from MCP tool handlers:** Returns protocol error instead of tool error. Use isError flag.

## Open Questions

Things that couldn't be fully resolved:

1. **Detecting timeout vs other SIGTERM causes**
   - What we know: spawn() timeout sends SIGTERM but exit event doesn't indicate cause
   - What's unclear: If Node.js will add timeout detection (issue #51561 open since 2024)
   - Recommendation: Implement workaround with manual timeout tracking (see Pattern 3)

2. **Emacsclient cancellation (C-g) detection**
   - What we know: User can cancel prompt with C-g, emacsclient exits with code 0
   - What's unclear: How to distinguish cancellation from empty response
   - Recommendation: Document as limitation, or modify Elisp to return special value on C-g

3. **Optimal timeout values for different question types**
   - What we know: 5 minutes is reasonable default, but some questions need longer
   - What's unclear: Should we have preset timeouts (quick=30s, normal=5m, long=30m)?
   - Recommendation: Start with configurable timeout_ms, gather usage data

4. **Circuit breaker necessity**
   - What we know: CONTEXT.md decided against circuit breaker ("each call is independent")
   - What's unclear: If repeated Emacs crashes indicate user should be notified differently
   - Recommendation: Follow decision, monitor for patterns in production

## Sources

### Primary (HIGH confidence)
- [Node.js Process API](https://nodejs.org/api/process.html) - Signal handling, exit behavior
- [Node.js Child Process API](https://nodejs.org/api/child_process.html) - spawn() options, error handling
- [Pino GitHub](https://github.com/pinojs/pino) - Installation, usage patterns, best practices
- [Zod Error Customization](https://zod.dev/error-customization) - Error map API, safeParse behavior

### Secondary (MEDIUM confidence)
- [Pino Logger Complete Guide (SigNoz)](https://signoz.io/guides/pino-logger/) - 2026 best practices, production setup
- [Pino Logging in Node.js (Better Stack)](https://betterstack.com/community/guides/logging/how-to-install-setup-and-use-pino-to-log-node-js-applications/) - Child loggers, redaction
- [MCP Error Handling Guide (MCPcat)](https://mcpcat.io/guides/error-handling-custom-mcp-servers/) - isError flag usage
- [Error Handling in MCP Tools (APXML)](https://apxml.com/courses/getting-started-model-context-protocol/chapter-3-implementing-tools-and-logic/error-handling-reporting) - Tool error patterns
- [Node.js Graceful Shutdown (DEV)](https://dev.to/superiqbal7/graceful-shutdown-in-nodejs-handling-stranger-danger-29jo) - Signal handler patterns
- [Handling signals/terminating child processes (Colin's Blog)](https://colinchjs.github.io/2023-10-10/08-49-28-631116-handling-signalsterminating-child-processes-in-nodejs/) - Child process cleanup

### Secondary (MEDIUM confidence - WebSearch verified)
- [Zod Validation Error npm package](https://www.npmjs.com/package/zod-validation-error) - User-friendly error wrapper
- [GNU Emacs Manual - emacsclient](https://www.gnu.org/software/emacs/manual/html_node/emacs/Invoking-emacsclient.html) - Error messages, --alternate-editor
- [Node.js Process Exit Strategies (Leapcell)](https://leapcell.io/blog/nodejs-process-exit-strategies) - Exit codes, signal behavior

### Tertiary (LOW confidence - patterns only)
- [Node.js timeout option issue #51561](https://github.com/nodejs/node/issues/51561) - Known limitation with timeout detection
- Various Stack Overflow discussions on emacsclient error patterns - Used for error message examples only

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - Pino and Zod are industry standard, Node.js built-ins are authoritative
- Architecture patterns: HIGH - All patterns verified with official Node.js docs or Phase 1 code
- Pitfalls: MEDIUM-HIGH - Based on official docs and known issues, some from community experience
- Error messages: MEDIUM - Emacsclient error patterns verified with manual but need testing
- Timeout detection: MEDIUM - Workaround needed due to known Node.js limitation

**Research date:** 2026-01-26
**Valid until:** 2026-02-26 (30 days - stable domain, slow-moving APIs)
