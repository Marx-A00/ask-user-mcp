# Architecture Patterns: MCP Server

**Domain:** Model Context Protocol Server (TypeScript)
**Researched:** 2026-01-26
**Confidence:** HIGH

## Recommended Architecture

MCP servers using the official TypeScript SDK follow a layered architecture with clear separation between protocol handling, business logic, and transport mechanisms.

```
┌─────────────────────────────────────────────────┐
│              Claude (Host)                      │
│  - Manages client lifecycle                     │
│  - Enforces security policies                   │
│  - Aggregates context from multiple servers     │
└────────────────┬────────────────────────────────┘
                 │ JSON-RPC 2.0
                 ▼
┌─────────────────────────────────────────────────┐
│           MCP Client (in Host)                  │
│  - 1:1 connection with server                   │
│  - Protocol negotiation                         │
│  - Capability exchange                          │
└────────────────┬────────────────────────────────┘
                 │ Transport Layer
                 │ (stdio / Streamable HTTP)
                 ▼
┌─────────────────────────────────────────────────┐
│            MCP Server Process                   │
│                                                 │
│  ┌──────────────────────────────────────┐      │
│  │  McpServer (SDK Core)                │      │
│  │  - Lifecycle management              │      │
│  │  - Capability registration           │      │
│  │  - Message routing                   │      │
│  └──────────────┬───────────────────────┘      │
│                 │                               │
│  ┌──────────────▼───────────────────────┐      │
│  │  Transport Adapter                   │      │
│  │  - StdioServerTransport              │      │
│  │  - StreamableHttpServerTransport     │      │
│  │  - JSON-RPC serialization            │      │
│  └──────────────┬───────────────────────┘      │
│                 │                               │
│  ┌──────────────▼───────────────────────┐      │
│  │  Business Logic Layer                │      │
│  │  - Tool handlers                     │      │
│  │  - Resource providers                │      │
│  │  - Prompt templates                  │      │
│  │  - Input validation (Zod schemas)    │      │
│  └──────────────┬───────────────────────┘      │
│                 │                               │
│  ┌──────────────▼───────────────────────┐      │
│  │  External Integrations               │      │
│  │  - File system                       │      │
│  │  - External processes                │      │
│  │  - APIs / databases                  │      │
│  └──────────────────────────────────────┘      │
│                                                 │
└─────────────────────────────────────────────────┘
```

### Component Boundaries

| Component | Responsibility | Communicates With | Confidence |
|-----------|---------------|-------------------|------------|
| **McpServer** | Server lifecycle, capability registration, message routing | Transport adapter, tool handlers | HIGH |
| **Transport Adapter** | JSON-RPC serialization, stdin/stdout management or HTTP handling | McpServer, process I/O | HIGH |
| **Tool Handlers** | Business logic for tool invocation, input validation, result formatting | McpServer, external integrations | HIGH |
| **Resource Providers** | Expose read-only data, URI-based resource access | McpServer, data sources | MEDIUM |
| **Prompt Templates** | Reusable message sequences, argument handling | McpServer | MEDIUM |
| **External Integrations** | Interface with OS, APIs, processes (e.g., emacsclient) | Tool handlers | HIGH |

### Data Flow

**Initialization (Server Startup)**
1. Process starts → McpServer instance created with name/version
2. Transport adapter initialized (stdio or HTTP)
3. Tools/resources/prompts registered with schemas
4. Server connects to transport → waits for client connection
5. Client sends `initialize` request with capabilities
6. Server responds with its capabilities → session established
7. Client sends `notifications/initialized` → server ready

**Tool Invocation (Runtime)**
1. Host/LLM requests tool via client → `tools/list` (optional, for discovery)
2. Client calls tool → `tools/call` with name + arguments (JSON-RPC)
3. Transport deserializes message → routes to McpServer
4. McpServer validates input against Zod schema
5. McpServer invokes registered tool handler
6. Handler executes business logic (e.g., calls emacsclient)
7. Handler returns result → McpServer serializes response
8. Response sent via transport → client receives result
9. Client forwards to host/LLM → conversation continues

**Resource Access**
1. Client requests `resources/list` → server returns available resources
2. Client reads resource → `resources/read` with URI
3. Server returns content → client uses for context

**Error Flow**
1. Error occurs in handler → throw or return error
2. McpServer catches error → formats JSON-RPC error response
3. Client receives error → host decides next action

### AskUserQuestion Server-Specific Flow

For the AskUserQuestion MCP server project:

```
Claude needs input
    ↓
Calls AskUserQuestion tool
    ↓ (JSON-RPC via stdio)
MCP Server receives tools/call
    ↓
Validates { question: string } with Zod
    ↓
Tool handler executes:
    execSync('emacsclient --eval ...')
    ↓
    Emacs server receives eval request
    ↓
    mr-x/ask-user-question elisp function runs
    ↓
    Minibuffer prompts user with styled question
    ↓
    User types answer + presses Enter
    ↓
    emacsclient returns answer string
    ↓
Tool handler receives answer
    ↓
Returns { content: [{ type: 'text', text: answer }] }
    ↓ (JSON-RPC response via stdio)
Claude receives answer → continues conversation
```

## Patterns to Follow

### Pattern 1: Single-Responsibility Tools
**What:** Each tool should do one thing well, with clear input/output contracts.

**When:** Defining tools for your MCP server.

**Why:** Simplifies testing, reduces cognitive load, makes tools composable.

**Example:**
```typescript
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";

const server = new McpServer({
  name: "ask-user-server",
  version: "1.0.0"
});

// Single tool with focused responsibility
server.tool(
  "AskUserQuestion",
  {
    question: z.string().describe("The question to ask the user")
  },
  async ({ question }) => {
    const answer = await promptUser(question);
    return {
      content: [{ type: "text", text: answer }]
    };
  }
);
```

### Pattern 2: Zod Schema Validation
**What:** Use Zod schemas for all tool inputs to ensure type safety and runtime validation.

**When:** Defining tool parameters.

**Why:** SDK automatically validates inputs, provides clear error messages, enables autocomplete.

**Example:**
```typescript
server.tool(
  "AskUserQuestion",
  {
    question: z.string()
      .min(1, "Question cannot be empty")
      .describe("The question to ask the user"),
    timeout: z.number()
      .optional()
      .default(300000)
      .describe("Timeout in milliseconds")
  },
  async ({ question, timeout }) => {
    // question: string (guaranteed non-empty)
    // timeout: number (defaults to 300000)
  }
);
```

### Pattern 3: Stdio Transport for Local Tools
**What:** Use StdioServerTransport for servers invoked as child processes by the host.

**When:** Server needs to run locally, launched per-session by the host.

**Why:** Simple process model, no network complexity, automatic cleanup, secure by default.

**Example:**
```typescript
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";

const transport = new StdioServerTransport();
await server.connect(transport);

// Host launches: node server.js
// Host communicates via stdin/stdout
// Host kills process when done
```

### Pattern 4: Error Boundaries in Handlers
**What:** Catch errors in tool handlers and return meaningful error messages to the client.

**When:** Tool handlers interact with external systems (file I/O, processes, APIs).

**Why:** Prevents server crashes, provides debugging context to LLM/user.

**Example:**
```typescript
server.tool(
  "AskUserQuestion",
  { question: z.string() },
  async ({ question }) => {
    try {
      const result = execSync(
        `emacsclient --eval '(mr-x/ask-user-question "${escapeString(question)}")'`,
        { encoding: 'utf-8', timeout: 300000 }
      );
      const answer = parseEmacsResult(result);
      return { content: [{ type: "text", text: answer }] };
    } catch (err) {
      // Return structured error instead of crashing
      return {
        content: [{
          type: "text",
          text: `Failed to prompt user: ${err.message}`
        }],
        isError: true
      };
    }
  }
);
```

### Pattern 5: Separation of Concerns via Modules
**What:** Organize code into focused modules (server setup, tool handlers, utilities, integrations).

**When:** Server has multiple tools or complex logic.

**Why:** Improves testability, readability, maintainability.

**Example:**
```
src/
├── index.ts              # Entry point, server setup, transport connection
├── tools/
│   ├── ask-user.ts       # AskUserQuestion tool handler
│   └── index.ts          # Tool registration
├── utils/
│   ├── emacs.ts          # Emacsclient integration utilities
│   └── validation.ts     # Input sanitization, escaping
└── types.ts              # Shared TypeScript types
```

## Anti-Patterns to Avoid

### Anti-Pattern 1: Writing to stdout Outside JSON-RPC
**What:** Writing logs, debug output, or non-JSON-RPC data to stdout when using stdio transport.

**Why bad:** stdout is the JSON-RPC message channel; any non-JSON output breaks the protocol.

**Instead:** Write logs to stderr or use a logging file.

**Example:**
```typescript
// BAD - breaks stdio transport
console.log("Server starting..."); // goes to stdout!

// GOOD - logs to stderr
console.error("Server starting..."); // goes to stderr
process.stderr.write("Debug info\n");

// BETTER - use a logger configured for stderr
import winston from "winston";
const logger = winston.createLogger({
  transports: [new winston.transports.Stream({ stream: process.stderr })]
});
logger.info("Server starting...");
```

### Anti-Pattern 2: Hardcoding Paths or Configuration
**What:** Embedding absolute paths, usernames, or environment-specific config in code.

**Why bad:** Not portable, breaks for other users, difficult to test.

**Instead:** Use environment variables, configuration files, or CLI arguments.

**Example:**
```typescript
// BAD
const emacsClient = "/Users/john/.local/bin/emacsclient";

// GOOD
const emacsClient = process.env.EMACSCLIENT_PATH || "emacsclient";
```

### Anti-Pattern 3: Blocking Operations Without Timeouts
**What:** Calling external processes or APIs without timeout limits.

**Why bad:** Server can hang indefinitely if external system is unresponsive.

**Instead:** Always set timeouts on external calls.

**Example:**
```typescript
// BAD
execSync('emacsclient --eval ...');

// GOOD
execSync('emacsclient --eval ...', { timeout: 300000 }); // 5 min timeout
```

### Anti-Pattern 4: Exposing Sensitive Data in Tool Descriptions
**What:** Including API keys, passwords, or internal paths in tool metadata.

**Why bad:** Tool descriptions are sent to the LLM and may be logged or exposed.

**Instead:** Keep descriptions generic, handle secrets in environment variables.

### Anti-Pattern 5: Ignoring Capability Negotiation
**What:** Assuming the client supports advanced features without checking capabilities.

**Why bad:** May cause errors or unexpected behavior with different clients.

**Instead:** Check client capabilities during initialization and gracefully degrade.

## Build Order and Dependencies

For the AskUserQuestion MCP server, components should be built in this order to minimize rework and enable incremental testing:

### Phase 1: Foundation
**Build:** Project scaffolding, dependency setup, TypeScript configuration
**Why first:** Establishes development environment; all subsequent work depends on this.
**Components:**
- `package.json` with `@modelcontextprotocol/sdk`, `zod`, TypeScript
- `tsconfig.json` for TypeScript compilation
- `.gitignore` for Node.js projects
- Build scripts (`npm run build`, `npm start`)

**Dependencies:** None
**Validates:** `npm install` succeeds, `tsc` compiles successfully

### Phase 2: Transport & Server Initialization
**Build:** Basic MCP server with stdio transport, no tools yet
**Why second:** Validates protocol handshake before adding business logic.
**Components:**
- `src/index.ts` with McpServer instance
- StdioServerTransport connection
- Minimal `initialize` response

**Dependencies:** Phase 1
**Validates:** Server starts, responds to `initialize` request, lists zero tools

### Phase 3: Emacsclient Integration Utility
**Build:** Utility function for calling emacsclient with error handling
**Why third:** Isolates external integration; can be tested independently.
**Components:**
- `src/utils/emacs.ts` with `callEmacsFunction()`
- String escaping for elisp
- Timeout handling
- Error parsing

**Dependencies:** Phase 1 (no MCP dependencies)
**Validates:** Can call emacsclient, parse responses, handle timeouts

### Phase 4: Tool Handler Implementation
**Build:** AskUserQuestion tool handler using emacsclient utility
**Why fourth:** Business logic layer; depends on both MCP server and emacsclient utility.
**Components:**
- `src/tools/ask-user.ts` with tool logic
- Zod schema for `{ question: string }`
- Integration with `callEmacsFunction()`
- Result formatting

**Dependencies:** Phase 2 (McpServer), Phase 3 (emacsclient utility)
**Validates:** Tool can be called via JSON-RPC, returns structured response

### Phase 5: Tool Registration
**Build:** Register AskUserQuestion tool with McpServer
**Why fifth:** Connects tool handler to MCP protocol; completes core functionality.
**Components:**
- Tool registration in `src/index.ts`
- Schema definition with Zod
- Handler binding

**Dependencies:** Phase 2 (McpServer), Phase 4 (tool handler)
**Validates:** `tools/list` returns AskUserQuestion, `tools/call` invokes handler

### Phase 6: Emacs Integration
**Build:** Elisp function for user prompts, agent-shell configuration
**Why sixth:** Completes end-to-end flow; requires working MCP server.
**Components:**
- `mr-x/ask-user-question` elisp function
- `agent-shell-mcp-servers` configuration
- Styling and UX enhancements

**Dependencies:** Phase 5 (working MCP server)
**Validates:** User prompts appear in Emacs, answers flow back to Claude

### Phase 7: Error Handling & Polish
**Build:** Timeouts, fallbacks, helpful error messages, documentation
**Why last:** Non-blocking improvements after core functionality works.
**Components:**
- Timeout handling in tool handler
- Graceful degradation if emacsclient unavailable
- User documentation (README)
- Setup instructions

**Dependencies:** Phase 6 (end-to-end flow working)
**Validates:** Server handles edge cases gracefully

## Component Dependencies Graph

```
Phase 1: Foundation
    ↓
    ├─→ Phase 2: Server Init (depends on Phase 1)
    │       ↓
    │       └─→ Phase 5: Tool Registration (depends on Phase 2, 4)
    │
    └─→ Phase 3: Emacsclient Util (depends on Phase 1)
            ↓
            └─→ Phase 4: Tool Handler (depends on Phase 3)
                    ↓
                    └─→ Phase 5: Tool Registration
                            ↓
                            └─→ Phase 6: Emacs Integration
                                    ↓
                                    └─→ Phase 7: Polish
```

**Critical path:** 1 → 2 → 4 → 5 → 6 (must be sequential)
**Parallel opportunities:** Phase 3 can be built/tested independently of Phase 2

## Security Considerations

**User Consent:** The host (Claude/agent-shell) must display the question to the user before invoking the tool. MCP servers cannot silently prompt users.

**Input Sanitization:** Tool handlers must escape shell metacharacters before passing questions to emacsclient to prevent command injection.

**Timeout Enforcement:** Prevent indefinite hangs by setting timeouts on external calls (5 minutes recommended for user input).

**Error Disclosure:** Avoid leaking sensitive paths or environment details in error messages sent to the LLM.

**Process Isolation:** Stdio transport ensures server runs as a child process with limited privileges; avoid requiring elevated permissions.

## Sources

This architecture research is based on:

**Official Documentation:**
- [MCP Specification (2025-11-25)](https://modelcontextprotocol.io/specification/2025-11-25) - Protocol architecture and component relationships
- [MCP Architecture Overview](https://modelcontextprotocol.io/specification/2025-11-25/architecture) - Host-client-server model
- [TypeScript SDK Documentation](https://github.com/modelcontextprotocol/typescript-sdk/blob/main/docs/server.md) - Server implementation patterns
- [TypeScript SDK Repository](https://github.com/modelcontextprotocol/typescript-sdk) - Official SDK source and examples

**Reference Implementations:**
- [MCP Reference Servers](https://github.com/modelcontextprotocol/servers) - Official server implementations
- [Filesystem Server](https://github.com/modelcontextprotocol/servers/tree/main/src/filesystem) - Example of tool-focused server structure

**Community Resources:**
- [How to Build an MCP Server](https://www.leanware.co/insights/how-to-build-mcp-server) - Step-by-step build patterns
- [MCP Architecture Tutorial](https://obot.ai/resources/learning-center/mcp-architecture/) - Component lifecycle and initialization
- [MCP Stdio Transport Guide](https://medium.com/@laurentkubaski/understanding-mcp-stdio-transport-protocol-ae3d5daf64db) - Transport layer details
- [Build an MCP Server Guide](https://modelcontextprotocol.io/docs/develop/build-server) - Official build instructions

**Emacs Integration:**
- [mcp.el - MCP Client for Emacs](https://github.com/lizqwerscott/mcp.el) - Reference for Emacs MCP patterns
- [emacs-mcp-server](https://github.com/vivekhaldar/emacs-mcp-server) - Example Emacs integration architecture

**Confidence:** HIGH - All patterns verified against official SDK documentation and reference implementations.
