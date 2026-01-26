# Phase 1: Core MCP Server - Research

**Researched:** 2026-01-26
**Domain:** MCP protocol implementation with TypeScript, stdio transport, Emacs integration
**Confidence:** HIGH

## Summary

Phase 1 requires building an MCP server that bridges Claude's tool calls to Emacs minibuffer prompts. The implementation uses the official `@modelcontextprotocol/sdk` for TypeScript, which provides JSON-RPC 2.0 handling, stdio transport, and tool registration capabilities. The server spawns `emacsclient` processes to display prompts and receive user input, with timeout handling after 5 minutes.

The standard approach is to create a TypeScript server using the MCP SDK with stdio transport (since Claude Desktop connects via stdio), register a single `AskUserQuestion` tool with Zod schema validation, spawn emacsclient with argument arrays (for security), and implement custom elisp functions for rich minibuffer prompts with ivy/helm integration.

**Primary recommendation:** Use `@modelcontextprotocol/sdk` v1.x (stable, production-ready) with TypeScript, stdio transport, Zod for input validation, and Node.js `spawn()` with argument arrays for secure emacsclient invocation.

## Standard Stack

The established libraries/tools for MCP server development:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| `@modelcontextprotocol/sdk` | 1.x (stable) | MCP protocol implementation | Official SDK from Anthropic, handles JSON-RPC 2.0, type-safe, production-ready |
| `zod` | 3.25+ | Input schema validation | Required peer dependency of MCP SDK, runtime type safety |
| TypeScript | 5.5+ | Type-safe development | SDK is TypeScript-first, static and runtime type safety |
| Node.js | 18+ | Runtime environment | Required for MCP SDK, provides `child_process` for spawning |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| `@types/node` | Latest | Node.js type definitions | Development dependency for TypeScript |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| Official SDK | Hand-rolled JSON-RPC | SDK provides protocol compliance, capability negotiation, type safety - hand-rolling is error-prone |
| TypeScript | Plain JavaScript | Lose compile-time type safety, but SDK requires TypeScript for full type inference |
| stdio transport | HTTP/SSE transport | HTTP requires different setup, stdio is standard for Claude Desktop local servers |

**Installation:**
```bash
npm install @modelcontextprotocol/sdk zod@3
npm install -D @types/node typescript
```

## Architecture Patterns

### Recommended Project Structure
```
ask-user-mcp/
├── src/
│   ├── index.ts              # Main server entry point
│   ├── emacs-interface.ts    # Emacsclient spawning logic
│   └── types.ts              # Shared type definitions
├── emacs/
│   └── ask-user.el           # Elisp prompt functions
├── build/                    # Compiled TypeScript output
├── tsconfig.json
└── package.json
```

### Pattern 1: MCP Server Initialization with Stdio Transport
**What:** Create MCP server instance, register tools, connect to stdio transport
**When to use:** Every MCP server with stdio transport (Claude Desktop standard)
**Example:**
```typescript
// Source: https://modelcontextprotocol.io/docs/develop/build-server
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";

const server = new McpServer({
  name: "ask-user-mcp",
  version: "1.0.0",
});

// Register tools here...

async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error("Ask User MCP Server running on stdio");
}

main().catch((error) => {
  console.error("Fatal error in main():", error);
  process.exit(1);
});
```

### Pattern 2: Tool Registration with Zod Validation
**What:** Register MCP tool with input schema validation using Zod
**When to use:** Every MCP tool to ensure type safety and validation
**Example:**
```typescript
// Source: https://modelcontextprotocol.io/docs/develop/build-server
import { z } from "zod";

server.registerTool(
  "AskUserQuestion",
  {
    description: "Ask the user a question and wait for their response",
    inputSchema: {
      question: z.string().describe("The question to ask the user"),
      header: z.string().optional().describe("Optional header/context for the question"),
      options: z.array(z.string()).optional().describe("Optional list of suggested responses"),
    },
  },
  async ({ question, header, options }) => {
    // Tool implementation
    return {
      content: [
        {
          type: "text",
          text: "User response here",
        },
      ],
    };
  },
);
```

### Pattern 3: Secure Process Spawning with Argument Arrays
**What:** Use `spawn()` with argument arrays to prevent command injection
**When to use:** Always when spawning external processes (never use shell: true with user input)
**Example:**
```typescript
// Source: https://nodejs.org/api/child_process.html
import { spawn } from "child_process";

// ✅ SECURE: Arguments passed as array
const emacsclient = spawn("emacsclient", [
  "--eval",
  `(ask-user-question "${sanitized_question}")`,
]);

// ❌ INSECURE: Shell expansion vulnerability
// const emacsclient = spawn("emacsclient", ["--eval", `(ask-user-question "${question}")`], { shell: true });
```

### Pattern 4: Timeout Handling with Promise.race
**What:** Implement 5-minute timeout using Promise.race and AbortController
**When to use:** MCP tools that wait for external input/long-running operations
**Example:**
```typescript
// Source: General Node.js pattern
function withTimeout<T>(promise: Promise<T>, timeoutMs: number): Promise<T> {
  return Promise.race([
    promise,
    new Promise<T>((_, reject) =>
      setTimeout(() => reject(new Error("Operation timed out")), timeoutMs)
    ),
  ]);
}

// Usage in tool handler
try {
  const result = await withTimeout(askEmacsUser(question), 5 * 60 * 1000);
  return { content: [{ type: "text", text: result }] };
} catch (error) {
  if (error.message === "Operation timed out") {
    return { content: [{ type: "text", text: "Question timed out after 5 minutes" }] };
  }
  throw error;
}
```

### Pattern 5: Emacs Elisp Minibuffer Prompt with Ivy/Helm
**What:** Custom elisp function using `completing-read` for option selection or `read-from-minibuffer` for free text
**When to use:** When displaying prompts in Emacs with completion/selection support
**Example:**
```elisp
;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffer-Completion.html
(defun ask-user-question (question &optional header options)
  "Prompt user with QUESTION in minibuffer, optionally with HEADER and OPTIONS."
  (let ((prompt (if header
                   (format "%s: %s" header question)
                 question)))
    (if options
        ;; Use completing-read for options (ivy/helm take over)
        (completing-read (format "%s [%s]: " prompt (string-join options "/")) options)
      ;; Use read-from-minibuffer for free text
      (read-from-minibuffer (format "%s > " prompt)))))
```

### Anti-Patterns to Avoid
- **Logging to stdout in stdio servers:** Use `console.error()` instead of `console.log()` - stdout is reserved for JSON-RPC messages
- **Using shell: true with spawn():** Creates command injection vulnerability, especially with user-controlled input
- **Throwing protocol-level errors for tool failures:** Return errors in result object with `isError` flag instead
- **Ignoring timeout handling:** MCP tools that wait indefinitely can hang Claude sessions

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| JSON-RPC 2.0 protocol | Custom message parser/handler | `@modelcontextprotocol/sdk` | Protocol has capability negotiation, error codes, notifications - SDK handles all edge cases |
| Input validation | Manual type checking | Zod schemas in `inputSchema` | Runtime validation + TypeScript type inference, required by SDK |
| Stdio transport | Custom stdin/stdout handlers | `StdioServerTransport` from SDK | Handles buffering, parsing, connection lifecycle |
| Command injection prevention | String escaping functions | `spawn()` with argument arrays | Shell parsing has many edge cases, argument arrays bypass shell entirely |
| Timeout implementation | Manual setTimeout + cleanup | Promise.race + AbortController | Prevents resource leaks, proper cleanup on timeout/success |

**Key insight:** MCP SDK handles the entire protocol layer (initialization, capability negotiation, tool discovery, JSON-RPC message handling). Hand-rolling any of this creates protocol compliance issues that break Claude integration.

## Common Pitfalls

### Pitfall 1: Writing to stdout in stdio-based servers
**What goes wrong:** JSON-RPC messages become corrupted, Claude Desktop cannot parse responses
**Why it happens:** Developers use `console.log()` for debugging, forgetting stdout is the transport channel
**How to avoid:** Always use `console.error()` for logging in stdio servers, configure logging libraries to write to stderr
**Warning signs:** Claude Desktop shows "MCP error" or server disconnects immediately after startup

### Pitfall 2: Command injection via spawn with shell: true
**What goes wrong:** User input in question text or options can execute arbitrary commands
**Why it happens:** Using `shell: true` or template strings that get shell-evaluated
**How to avoid:** Always use `spawn(command, [arg1, arg2, ...])` with explicit argument arrays, never `shell: true`
**Warning signs:** Security vulnerability, could execute `rm -rf` or similar if malicious input provided

### Pitfall 3: No timeout on emacsclient calls
**What goes wrong:** Server hangs indefinitely if user doesn't respond, blocking Claude
**Why it happens:** `spawn()` doesn't have built-in timeout, promise never resolves
**How to avoid:** Wrap emacsclient calls in Promise.race with 5-minute timeout
**Warning signs:** Claude appears stuck "waiting for tool response", server process never exits

### Pitfall 4: Forgetting to register tools before server.connect()
**What goes wrong:** Claude doesn't see tools in tools/list, can't use AskUserQuestion
**Why it happens:** Calling server.connect() before server.registerTool()
**How to avoid:** Always register all tools before calling server.connect(transport)
**Warning signs:** Claude Desktop shows server connected but no tools appear in connector list

### Pitfall 5: Returning raw exceptions as protocol errors
**What goes wrong:** Claude sees MCP protocol error instead of tool error, can't recover gracefully
**Why it happens:** Throwing errors in tool handler instead of returning error in result
**How to avoid:** Catch exceptions in tool handler, return `{ content: [{ type: "text", text: error.message }] }`
**Warning signs:** Claude shows "MCP protocol error" instead of displaying error to user

### Pitfall 6: Not sanitizing emacsclient eval strings
**What goes wrong:** Elisp code injection if question contains quotes or special characters
**Why it happens:** Direct string interpolation into `--eval` argument
**How to avoid:** Properly escape quotes in elisp strings, or pass data via temp file instead of --eval
**Warning signs:** Emacsclient fails with "unexpected token" or executes unintended elisp

## Code Examples

Verified patterns from official sources:

### Minimal MCP Server with Single Tool
```typescript
// Source: https://modelcontextprotocol.io/docs/develop/build-server
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";

const server = new McpServer({
  name: "ask-user-mcp",
  version: "1.0.0",
});

server.registerTool(
  "AskUserQuestion",
  {
    description: "Ask the user a question via Emacs minibuffer",
    inputSchema: {
      question: z.string().describe("The question to ask"),
      header: z.string().optional().describe("Optional header/context"),
      options: z.array(z.string()).optional().describe("Optional response options"),
    },
  },
  async ({ question, header, options }) => {
    // Implementation here
    return {
      content: [
        {
          type: "text",
          text: "User's response",
        },
      ],
    };
  },
);

async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error("Server running");
}

main().catch(console.error);
```

### Secure Emacsclient Spawning with Timeout
```typescript
// Source: https://nodejs.org/api/child_process.html + general pattern
import { spawn } from "child_process";

async function askViaEmacs(question: string, header?: string, options?: string[]): Promise<string> {
  return new Promise((resolve, reject) => {
    const args = ["--eval", `(ask-user-question "${escapeElispString(question)}" ...)`];
    const proc = spawn("emacsclient", args);
    
    let stdout = "";
    let stderr = "";
    
    proc.stdout.on("data", (data) => { stdout += data; });
    proc.stderr.on("data", (data) => { stderr += data; });
    
    proc.on("close", (code) => {
      if (code === 0) {
        resolve(stdout.trim());
      } else {
        reject(new Error(`emacsclient exited with code ${code}: ${stderr}`));
      }
    });
    
    proc.on("error", (err) => {
      reject(new Error(`Failed to spawn emacsclient: ${err.message}`));
    });
  });
}

function escapeElispString(str: string): string {
  return str.replace(/\\/g, "\\\\").replace(/"/g, '\\"');
}
```

### TypeScript Configuration for MCP Server
```json
// Source: https://modelcontextprotocol.io/docs/develop/build-server
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "Node16",
    "moduleResolution": "Node16",
    "outDir": "./build",
    "rootDir": "./src",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules"]
}
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| MCP SDK v0.x (pre-release) | MCP SDK v1.x (stable) | Q4 2024 | v1.x is production-ready, v2 coming Q1 2026 with breaking changes |
| HTTP + SSE transport | Streamable HTTP (remote) or stdio (local) | Nov 2024 | HTTP + SSE deprecated, stdio standard for Claude Desktop |
| Hand-rolled JSON-RPC | Official SDK with `registerTool()` | Nov 2024 | SDK handles protocol compliance, capability negotiation |
| Shell commands with exec() | spawn() with argument arrays | CVE-2024-27980 fix | Security fix prevents command injection on Windows |

**Deprecated/outdated:**
- **HTTP + SSE transport:** Replaced by Streamable HTTP for remote servers, stdio for local
- **Pre-1.0 SDK versions:** Use 1.x for production, not pre-release versions
- **spawn() without argument arrays:** CVE-2024-27980 requires argument arrays or shell: true (avoid latter)

## Open Questions

Things that couldn't be fully resolved:

1. **Emacsclient timeout handling at Emacs level**
   - What we know: Node.js can timeout the spawn(), killing the process
   - What's unclear: Whether Emacs provides a built-in way to timeout `read-from-minibuffer`
   - Recommendation: Implement timeout on Node.js side with Promise.race, kill emacsclient process on timeout

2. **Multi-select in ivy/helm for checkbox-style selection**
   - What we know: User context specifies ivy/helm checkbox-style multi-select
   - What's unclear: Exact elisp API for ivy-read with multi-select enabled
   - Recommendation: Research ivy-read :multi-action or similar during planning phase

3. **Best way to pass complex data to elisp via emacsclient**
   - What we know: `--eval` works for simple strings, needs escaping
   - What's unclear: Whether temp files or other mechanisms are better for complex option lists
   - Recommendation: Start with --eval and escaping, switch to temp file if escaping becomes brittle

## Sources

### Primary (HIGH confidence)
- [Official MCP Build Server Guide](https://modelcontextprotocol.io/docs/develop/build-server) - Complete TypeScript server implementation patterns
- [MCP TypeScript SDK GitHub](https://github.com/modelcontextprotocol/typescript-sdk) - Official SDK repository and documentation
- [Node.js child_process Documentation](https://nodejs.org/api/child_process.html) - spawn() security and usage
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification) - Error codes and message format
- [Zod Documentation](https://zod.dev/) - Schema validation patterns
- [GNU Emacs Lisp Reference Manual - Minibuffers](https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffers.html) - Elisp minibuffer API
- [GNU Emacs Manual - emacsclient](https://www.gnu.org/software/emacs/manual/html_node/emacs/Invoking-emacsclient.html) - emacsclient command line options

### Secondary (MEDIUM confidence)
- [Node.js Security Releases April 2024](https://nodejs.org/en/blog/vulnerability/april-2024-security-releases-2) - CVE-2024-27980 spawn vulnerability
- [MCP Timeout and Retry Strategies](https://octopus.com/blog/mcp-timeout-retry) - Timeout handling patterns
- [Building MCP Servers Production Guide](https://maurocanuto.medium.com/building-mcp-servers-the-right-way-a-production-ready-guide-in-typescript-8ceb9eae9c7f) - Best practices compilation
- [Ivy User Manual](https://oremacs.com/swiper/) - Ivy completion framework documentation

### Tertiary (LOW confidence)
- [MCP Complete Developer Guide 2026](https://publicapis.io/blog/mcp-model-context-protocol-guide) - Overview article, not official docs
- [Understanding MCP Through STDIO](https://foojay.io/today/understanding-mcp-through-raw-stdio-communication/) - Community tutorial

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - Official SDK is well-documented, TypeScript/Zod are required dependencies
- Architecture: HIGH - Official examples demonstrate stdio transport, tool registration patterns
- Pitfalls: HIGH - Documented in official guides (stdout logging, tool registration order) and security advisories (CVE-2024-27980)
- Emacsclient integration: MEDIUM - Standard Emacs documentation, but MCP-specific patterns need validation
- Timeout handling: MEDIUM - General Node.js patterns, MCP-specific timeout guidance is emerging

**Research date:** 2026-01-26
**Valid until:** 2026-02-26 (30 days - MCP SDK is stable, but v2 coming Q1 2026 may change patterns)
