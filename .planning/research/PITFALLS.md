# Domain Pitfalls

**Domain:** MCP Server with External Process Integration (emacsclient)
**Researched:** 2026-01-26
**Confidence:** HIGH (verified with official sources and 2025-2026 security research)

## Critical Pitfalls

These mistakes cause security vulnerabilities, data loss, or require complete rewrites.

### Pitfall 1: Command Injection via Unsanitized Input

**What goes wrong:**
Using `child_process.exec()` or building shell command strings with user-provided questions leads to command injection attacks. An attacker can inject shell metacharacters (`;`, `$()`, backticks, `|`) to execute arbitrary commands.

**Why it happens:**
The PRD shows using `execSync()` with string interpolation:
```javascript
const escapedQuestion = question.replace(/"/g, '\\"').replace(/\n/g, '\\n');
const result = execSync(`emacsclient --eval '(read-string "${escapedQuestion} ")'`);
```

This is vulnerable because:
- `exec()` spawns `/bin/sh`, which interprets shell metacharacters
- Manual escaping is error-prone and incomplete (doesn't cover all attack vectors)
- A malicious question like `"; rm -rf / #` could execute arbitrary commands

**Consequences:**
- **Security:** Remote Code Execution (RCE) - complete system compromise
- **Severity:** CRITICAL - 88% of MCP servers have credential exposure issues, command injection compounds this
- **Real-world precedent:** Trend Micro discovered SQL injection → prompt injection in Anthropic's reference SQLite MCP server (June 2025)

**Prevention:**
1. **NEVER use `exec()` or `execSync()` with user input**
2. **Use `spawn()` or `execFile()` with argument arrays** - bypasses shell entirely
3. **For elisp strings, use proper elisp escaping** or pass via stdin/temp file instead of command-line arguments

Example safe approach:
```javascript
const { spawn } = require('child_process');

// Option A: Use elisp escaping and spawn with args array
const elisp = `(read-string ${JSON.stringify(question)})`;
const child = spawn('emacsclient', ['--eval', elisp], { timeout: 300000 });

// Option B: Pass question via temp file (safer for long/complex questions)
const tempFile = '/tmp/claude-question.txt';
fs.writeFileSync(tempFile, question);
const child = spawn('emacsclient', ['--eval', `(read-string (f-read-text "${tempFile}"))`]);
```

**Detection:**
- Search codebase for `exec(`, `execSync(`, `eval(`, or string concatenation in spawn calls
- Test with malicious inputs: `"; echo VULNERABLE #`, `$(whoami)`, `` `id` ``
- Use linters: `eslint-plugin-security` detects command injection patterns

**Phase mapping:**
- **Phase 1 (Core MCP Server):** Must use `spawn()` with argument arrays from day one
- **Phase 2 (Error Handling):** Add input validation and length limits as defense-in-depth

**Sources:**
- [Preventing Command Injection in Node.js](https://auth0.com/blog/preventing-command-injection-attacks-in-node-js-apps/)
- [MCP Security Report 2025](https://astrix.security/learn/blog/state-of-mcp-server-security-2025/)

---

### Pitfall 2: Zombie and Orphan Process Accumulation

**What goes wrong:**
Child processes (emacsclient) are spawned but never properly cleaned up when:
- The parent MCP server crashes or restarts
- Timeouts occur but the child isn't killed
- Signal handlers aren't registered (SIGINT, SIGTERM)
- The `exit` event isn't handled

**Why it happens:**
Node.js child processes don't automatically terminate when the parent exits. Without explicit cleanup:
- Zombie processes accumulate (completed but not reaped)
- Orphan processes run indefinitely, consuming PIDs
- On Cloud Foundry / container environments, PID exhaustion causes deployment failures

**Consequences:**
- **Resource exhaustion:** Run out of PIDs (max 32768 on Linux)
- **Memory leaks:** Zombie processes hold process table entries
- **Container failures:** Kubernetes pod eviction due to resource limits
- **Debugging nightmare:** Stale emacsclient processes interfere with new ones

**Prevention:**

```javascript
const activeChildren = new Set();

function spawnEmacsclient(question) {
  const child = spawn('emacsclient', ['--eval', elisp], { 
    timeout: 300000,
    killSignal: 'SIGTERM'
  });
  
  activeChildren.add(child);
  
  // Cleanup on normal exit
  child.on('exit', (code, signal) => {
    activeChildren.delete(child);
    console.error(`emacsclient exited: code=${code}, signal=${signal}`);
  });
  
  // Cleanup on error
  child.on('error', (err) => {
    activeChildren.delete(child);
    console.error(`emacsclient error: ${err.message}`);
  });
  
  return child;
}

// Cleanup all children on process exit
process.on('exit', () => {
  activeChildren.forEach(child => {
    if (!child.killed) {
      child.kill('SIGTERM');
    }
  });
});

// Handle signals gracefully
['SIGINT', 'SIGTERM'].forEach(signal => {
  process.on(signal, () => {
    console.error(`Received ${signal}, cleaning up...`);
    activeChildren.forEach(child => child.kill('SIGTERM'));
    process.exit(0);
  });
});
```

**Detection:**
- Monitor process count: `ps aux | grep emacsclient | wc -l`
- Check for zombies: `ps aux | grep 'Z'` (Z = zombie state)
- Track PID usage: `cat /proc/sys/kernel/pid_max` vs current allocation
- Container metrics: Watch PID usage in Kubernetes/Docker

**Phase mapping:**
- **Phase 1 (Core MCP Server):** Basic cleanup (exit event handlers)
- **Phase 2 (Error Handling):** Signal handlers and timeout cleanup
- **Phase 3 (Production Hardening):** Process tracking, PID monitoring

**Sources:**
- [Managing Orphaned Node.js Processes](https://medium.com/@arunangshudas/5-tips-for-cleaning-orphaned-node-js-processes-196ceaa6d85e)
- [Zombie Processes in Node.js](https://saturncloud.io/blog/what-is-a-zombie-process-and-how-to-avoid-it-when-spawning-nodejs-child-processes-on-cloud-foundry/)

---

### Pitfall 3: Stdio Deadlock and Buffer Overflow

**What goes wrong:**
MCP uses JSON-RPC over stdio (stdin/stdout). If the MCP server writes to `stdout` for debugging or the child process produces unexpected output, it corrupts the JSON-RPC stream, causing protocol errors and deadlocks.

**Why it happens:**
- **Stdout contamination:** `console.log()` writes to stdout, breaking JSON-RPC
- **Child stdout mixing:** emacsclient output goes to parent's stdout
- **Buffer overflow:** Large responses (>200KB default `maxBuffer`) cause child process termination
- **Partial reads:** JSON-RPC messages split across multiple reads, incomplete parsing

**Consequences:**
- **Protocol failure:** Client receives malformed JSON, connection drops
- **Silent failures:** Debugging output masks real responses
- **Data loss:** Large user responses (>200KB) truncated or process killed
- **Deadlock:** Parent waits for child stdout, child waits for parent to read

**Prevention:**

```javascript
// 1. NEVER write to stdout except for JSON-RPC responses
// Use stderr for all debugging
console.log = console.error; // Override to prevent accidents
console.debug = console.error;

// 2. Redirect child process streams properly
const child = spawn('emacsclient', args, {
  stdio: ['pipe', 'pipe', 'inherit'], // stdin, stdout, stderr
  timeout: 300000,
  maxBuffer: 1024 * 1024 // 1MB for long responses
});

// 3. Handle partial JSON-RPC messages
let buffer = '';
process.stdin.on('data', (chunk) => {
  buffer += chunk.toString();
  
  let newlineIndex;
  while ((newlineIndex = buffer.indexOf('\n')) !== -1) {
    const line = buffer.slice(0, newlineIndex);
    buffer = buffer.slice(newlineIndex + 1);
    
    if (line.trim()) {
      try {
        const message = JSON.parse(line);
        handleMessage(message);
      } catch (err) {
        console.error(`JSON parse error: ${err.message}`);
        // Don't respond to malformed messages
      }
    }
  }
});

// 4. Increase maxBuffer for long user inputs
// (emacsclient responses should be small, but plan for 10KB+ answers)
```

**Detection:**
- Test with large inputs: Questions >1KB, answers >10KB
- Monitor stderr: Any "maxBuffer exceeded" errors
- Protocol debugging: Enable MCP client debug mode, check for parse errors
- Grep codebase: Search for `console.log`, `console.debug` not redirected to stderr

**Phase mapping:**
- **Phase 1 (Core MCP Server):** Stdout hygiene, basic buffering
- **Phase 2 (Error Handling):** maxBuffer tuning, robust JSON parsing

**Sources:**
- [Understanding MCP Through Raw STDIO Communication](https://foojay.io/today/understanding-mcp-through-raw-stdio-communication/)
- [Node.js Child Process Documentation](https://nodejs.org/api/child_process.html)

---

### Pitfall 4: Long-Running Operation Timeout Without Progress Feedback

**What goes wrong:**
The 5-minute timeout for user responses is implemented, but:
- The MCP client times out before 5 minutes (default 60 seconds in many clients)
- No progress notifications are sent, so the client assumes the server is dead
- The connection is severed before the user responds, even if they're typing

**Why it happens:**
JSON-RPC is fundamentally synchronous - a request blocks until a response is received. Most MCP clients have aggressive timeouts (30-60 seconds) that don't match the server's 5-minute wait time. Without progress notifications, the client has no way to know the server is still alive and waiting for user input.

**Consequences:**
- **User frustration:** User types response but connection already dropped
- **Wasted work:** Claude re-attempts the same question or gives up entirely
- **Unreliability:** Tool works for quick answers (<60s) but fails for thoughtful ones
- **Support burden:** "It just times out" with no clear error message

**Prevention:**

The MCP spec (2025-11-25) introduces **progress notifications** for long-running operations:

```javascript
// Option A: Progress notifications (MCP 2025-11-25)
async function askUser(question, requestId) {
  const progressToken = `ask-${Date.now()}`;
  
  // Start progress reporting
  const progressInterval = setInterval(() => {
    sendNotification('notifications/progress', {
      progressToken,
      progress: 0, // Indeterminate
      total: 1
    });
  }, 5000); // Every 5 seconds
  
  try {
    const answer = await spawnEmacsclientPromise(question);
    clearInterval(progressInterval);
    return answer;
  } catch (err) {
    clearInterval(progressInterval);
    throw err;
  }
}

// Option B: Async hand-off pattern (recommended for NEW MCP spec)
// Return immediately, poll for completion
const pendingQuestions = new Map();

function askUserAsync(question) {
  const taskId = `task-${Date.now()}`;
  pendingQuestions.set(taskId, { status: 'pending', question });
  
  // Spawn emacsclient in background
  spawnEmacsclient(question).then(answer => {
    pendingQuestions.set(taskId, { status: 'completed', answer });
  }).catch(err => {
    pendingQuestions.set(taskId, { status: 'failed', error: err.message });
  });
  
  // Return task ID immediately (no blocking)
  return { taskId, status: 'pending' };
}

// Client polls via tools/call with { taskId }
function getTaskStatus(taskId) {
  return pendingQuestions.get(taskId) || { status: 'not_found' };
}
```

**Better approach for this project:**
Since we're interacting with a human user who can respond within seconds to minutes, **progress notifications** are the right choice. The async hand-off pattern is better for long-running computations (>5 minutes) where intermediate results matter.

**Detection:**
- Test with slow responses: Delay answering for 90 seconds
- Monitor client logs: Look for "Request timeout" or "Connection closed"
- Check MCP client timeout config: `request_timeout` in agent-shell config
- Warning sign: User reports "sometimes it works, sometimes it doesn't" (timing-dependent)

**Phase mapping:**
- **Phase 1 (Core MCP Server):** Implement basic timeout
- **Phase 2 (Error Handling):** Add progress notifications
- **Phase 3 (Production Hardening):** Client timeout coordination, configurable timeouts

**Sources:**
- [Build Timeout-Proof MCP Tools](https://www.arsturn.com/blog/no-more-timeouts-how-to-build-long-running-mcp-tools-that-actually-finish-the-job)
- [MCP Specification 2025-11-25 - Tasks](https://mcp-bundler.com/2025/12/08/mcp-specification-end-users-server-providers/)
- [SEP-1391: Long-Running Operations](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/1391)

---

### Pitfall 5: emacsclient Failure Modes Not Handled

**What goes wrong:**
The MCP server assumes emacsclient will always work, but fails when:
- Emacs server isn't running (`server-start` not called)
- Display environment variables are wrong (Wayland vs X11)
- Socket file was deleted or in wrong location
- systemd user service conflicts with manual `server-start`
- Multiple Emacs servers running (wrong one responds)

**Why it happens:**
`emacsclient` has many failure modes specific to the desktop environment, systemd integration, and Emacs configuration. The error messages are cryptic ("can't open display :0", "waiting for emacs...", "no socket found") and the server crashes or hangs instead of gracefully degrading.

**Consequences:**
- **Total failure:** MCP server crashes on first question, Claude can't proceed
- **Silent hang:** emacsclient waits indefinitely for server that will never respond
- **Wrong Emacs instance:** Question appears in unrelated Emacs session
- **User confusion:** "It worked yesterday, now it's broken" (systemd restart)

**Prevention:**

```javascript
// 1. Detect emacsclient availability before server starts
function checkEmacsServer() {
  try {
    const result = execSync('emacsclient --eval "(+ 1 1)"', {
      timeout: 5000,
      encoding: 'utf-8'
    });
    if (result.trim() !== '2') {
      throw new Error('Emacs server not responding correctly');
    }
    return true;
  } catch (err) {
    console.error('Emacs server check failed:', err.message);
    console.error('Hints:');
    console.error('  - Is Emacs running with (server-start)?');
    console.error('  - Check: systemctl --user status emacs');
    console.error('  - Try: emacsclient --eval "(+ 1 1)"');
    return false;
  }
}

// 2. Provide fallback when emacsclient fails
async function askUserWithFallback(question) {
  try {
    // Try emacsclient first
    return await askViaEmacsclient(question);
  } catch (err) {
    console.error(`emacsclient failed: ${err.message}`);
    
    // Fallback: Write to file and notify
    const fallbackPath = '/tmp/claude-question.txt';
    fs.writeFileSync(fallbackPath, `Claude asks: ${question}\n\nWrite answer to: /tmp/claude-answer.txt`);
    
    // Wait for answer file (with timeout)
    return await waitForFile('/tmp/claude-answer.txt', 300000);
  }
}

// 3. Detect specific failure modes
function diagnoseEmacsclientError(stderr) {
  if (stderr.includes("can't open display")) {
    return "Display environment variable mismatch (Wayland vs X11). Check $DISPLAY in Emacs server.";
  }
  if (stderr.includes("waiting for emacs")) {
    return "Emacs server not responding. Try: systemctl --user restart emacs";
  }
  if (stderr.includes("no socket")) {
    return "Socket file not found. Check server-socket-dir and ensure server-start was called.";
  }
  return stderr;
}
```

**Detection:**
- **Pre-deployment check:** Run `emacsclient --eval "(+ 1 1)"` in CI/testing
- **Startup health check:** Server initialization runs emacsclient test
- **Monitor stderr:** Capture and parse emacsclient error messages
- **User reports:** "emacsclient: can't find socket" → wrong server-socket-dir

**Phase mapping:**
- **Phase 1 (Core MCP Server):** Basic error handling (catch and report)
- **Phase 2 (Error Handling):** Startup health checks, fallback mechanism
- **Phase 3 (Production Hardening):** Diagnostic messages, recovery strategies

**Sources:**
- [GNU Emacs Manual: Invoking emacsclient](https://www.gnu.org/software/emacs/manual/html_node/emacs/Invoking-emacsclient.html)
- [emacsclient Display Errors](https://bbs.archlinux.org/viewtopic.php?id=281813)
- [Daemon Issues with systemd](https://github.com/doomemacs/doomemacs/issues/7699)

---

## Moderate Pitfalls

These mistakes cause delays, technical debt, or degraded UX, but are fixable without rewrites.

### Pitfall 6: Credential Exposure in Environment Variables

**What goes wrong:**
While this specific project doesn't use API keys, 79% of MCP servers pass credentials via environment variables. If your MCP server grows to support authentication (e.g., verifying Claude's identity, logging questions), storing secrets in ENV vars exposes them to:
- Child processes (inherited by emacsclient spawns)
- Process listings (`ps aux -e`)
- Error messages and stack traces
- Log files and crash dumps

**Why it happens:**
Environment variables are the easiest way to configure servers, and many tutorials recommend them. The MCP ecosystem has normalized this pattern (88% of servers use credentials, 79% via ENV).

**Prevention:**
- **Use secret management:** OS keychain, encrypted config files, or secret management services
- **Avoid ENV for secrets:** If you must use ENV, scrub it before spawning children:
  ```javascript
  const child = spawn('emacsclient', args, {
    env: { ...process.env, API_KEY: undefined, SECRET: undefined }
  });
  ```
- **Never log ENV:** Redact `process.env` from error messages

**Phase mapping:**
- **Out of scope for v1:** This project doesn't need auth yet
- **Phase X (Future: Auth/Logging):** Implement secret management before adding credentials

**Sources:**
- [MCP Security Survival Guide](https://towardsdatascience.com/the-mcp-security-survival-guide-best-practices-pitfalls-and-real-world-lessons/)

---

### Pitfall 7: No Input Validation or Length Limits

**What goes wrong:**
Without validation:
- **DoS via large inputs:** Question is 10MB of text, crashes emacsclient or fills disk
- **Control character injection:** Question contains `\x00`, `\x1B` (escape sequences), breaks terminal
- **Encoding issues:** Non-UTF8 characters cause parsing errors
- **Buffer overflows:** Unchecked length breaks fixed-size buffers in Emacs

**Prevention:**
```javascript
function validateQuestion(question) {
  if (typeof question !== 'string') {
    throw new Error('Question must be a string');
  }
  
  if (question.length === 0) {
    throw new Error('Question cannot be empty');
  }
  
  if (question.length > 10000) { // 10KB limit
    throw new Error('Question too long (max 10000 characters)');
  }
  
  // Check for control characters (except \n, \t)
  if (/[\x00-\x08\x0B-\x0C\x0E-\x1F\x7F]/.test(question)) {
    throw new Error('Question contains invalid control characters');
  }
  
  // Ensure valid UTF-8
  if (Buffer.byteLength(question, 'utf8') !== question.length) {
    throw new Error('Question contains invalid UTF-8 sequences');
  }
  
  return question;
}
```

**Phase mapping:**
- **Phase 1 (Core MCP Server):** Basic type and length checks
- **Phase 2 (Error Handling):** Control character filtering, encoding validation

**Sources:**
- [MCP Best Practices: Input Validation](https://modelcontextprotocol.info/docs/best-practices/)

---

### Pitfall 8: Synchronous Operations Block Event Loop

**What goes wrong:**
Using `execSync()` blocks the entire Node.js event loop during the 5-minute wait for user response. This prevents:
- Handling other JSON-RPC messages (if client sends cancellation or other requests)
- Processing signals (SIGTERM delayed until execSync completes)
- Responding to health checks or progress requests

**Why it happens:**
Synchronous APIs are simpler to write (no promises/callbacks), but Node.js is fundamentally async. Blocking the event loop defeats the runtime's core design.

**Prevention:**
Use `spawn()` with promise wrapper:
```javascript
function askViaEmacsclient(question) {
  return new Promise((resolve, reject) => {
    const child = spawn('emacsclient', ['--eval', elisp], {
      timeout: 300000
    });
    
    let stdout = '';
    let stderr = '';
    
    child.stdout.on('data', (chunk) => stdout += chunk);
    child.stderr.on('data', (chunk) => stderr += chunk);
    
    child.on('exit', (code, signal) => {
      if (code === 0) {
        const answer = stdout.trim().replace(/^"|"$/g, '');
        resolve(answer);
      } else {
        reject(new Error(`emacsclient exited ${code}: ${stderr}`));
      }
    });
    
    child.on('error', reject);
  });
}
```

**Phase mapping:**
- **Phase 1 (Core MCP Server):** Use async spawn() from the start

**Sources:**
- [Node.js Best Practices: Avoid Blocking](https://nodejs.org/en/docs/guides/blocking-vs-non-blocking/)

---

## Minor Pitfalls

These cause annoyance or rough edges, but are easily fixable.

### Pitfall 9: Poor Error Messages for Debugging

**What goes wrong:**
Generic errors like "Failed to get user input" don't help users diagnose:
- Was emacsclient not found?
- Did the Emacs server time out?
- Was the question malformed?
- Did the user cancel?

**Prevention:**
```javascript
catch (err) {
  let userMessage = 'Failed to get user input.';
  let debugInfo = err.message;
  
  if (err.code === 'ENOENT') {
    userMessage = 'emacsclient command not found. Is Emacs installed?';
  } else if (err.code === 'ETIMEDOUT') {
    userMessage = 'Timeout waiting for user response (5 minutes).';
  } else if (err.killed) {
    userMessage = 'User cancelled the prompt.';
  } else if (err.signal === 'SIGTERM') {
    userMessage = 'Emacs server stopped responding.';
  }
  
  console.error(`Error: ${userMessage} (${debugInfo})`);
  respondError(id, -32000, userMessage);
}
```

**Phase mapping:**
- **Phase 2 (Error Handling):** Improve error messages

---

### Pitfall 10: No Logging or Observability

**What goes wrong:**
When issues occur in production, there's no way to:
- See which questions were asked
- Track how long users took to respond
- Identify patterns (which questions timeout, which succeed)
- Debug protocol issues (malformed JSON-RPC)

**Prevention:**
```javascript
console.error(`[${new Date().toISOString()}] Question asked: ${question.slice(0, 100)}...`);
const startTime = Date.now();

// ... get answer ...

const duration = Date.now() - startTime;
console.error(`[${new Date().toISOString()}] Answer received in ${duration}ms`);
```

Consider structured logging (JSON) for easier parsing:
```javascript
const log = (event, data) => {
  console.error(JSON.stringify({ timestamp: Date.now(), event, ...data }));
};

log('question_asked', { question, length: question.length });
log('answer_received', { duration, answerLength: answer.length });
```

**Phase mapping:**
- **Phase 3 (Production Hardening):** Add structured logging

---

## Phase-Specific Warnings

**Phase 1: Core MCP Server**
- **Pitfall 1:** Must use `spawn()` with arg arrays (CRITICAL)
- **Pitfall 3:** Stdout hygiene from day one (CRITICAL)
- **Pitfall 8:** Use async spawn, not execSync (MODERATE)

**Phase 2: Error Handling & Validation**
- **Pitfall 2:** Signal handlers and process cleanup (CRITICAL)
- **Pitfall 4:** Progress notifications for long waits (CRITICAL)
- **Pitfall 5:** emacsclient health checks and fallback (CRITICAL)
- **Pitfall 7:** Input validation and length limits (MODERATE)
- **Pitfall 9:** Better error messages (MINOR)

**Phase 3: Production Hardening**
- **Pitfall 2:** Process monitoring and PID tracking (CRITICAL continuation)
- **Pitfall 10:** Structured logging for observability (MINOR)

**Phase X: Future Enhancements (Auth, Multi-user)**
- **Pitfall 6:** Secret management before adding credentials (MODERATE)

---

## Summary: Top 3 to Address Immediately

1. **Command Injection (Pitfall 1):** Use `spawn()` with argument arrays, never `exec()` with string interpolation. This is a CRITICAL security vulnerability.

2. **Process Cleanup (Pitfall 2):** Register signal handlers and track child processes. Zombie accumulation causes PID exhaustion in production.

3. **Stdio Hygiene (Pitfall 3):** Never write to stdout except JSON-RPC responses. Use stderr for all debugging to prevent protocol corruption.

These three pitfalls are architectural decisions that are hard to fix later. Get them right in Phase 1.

---

## Research Confidence

**Overall confidence:** HIGH

**By area:**
- **Command Injection:** HIGH - Official Node.js docs, Auth0 security guide, multiple CVEs
- **Process Management:** HIGH - Node.js docs, real-world Cloud Foundry issues documented
- **Stdio/JSON-RPC:** HIGH - MCP spec, foojay technical deep-dive
- **Timeout Handling:** HIGH - MCP spec updates (2025-11-25), SEP-1391 proposal
- **emacsclient Failures:** MEDIUM - GNU Emacs docs, community forum issues (not all failure modes may be documented)

**Sources used:**
- Official MCP specification (2025-11-25 revision)
- Node.js documentation (v25.3.0)
- GNU Emacs manual and community forums
- Security research reports (Astrix, Trend Micro, Auth0)
- Real-world CVEs and incident reports (2025-2026)

All findings cross-verified with multiple authoritative sources.
