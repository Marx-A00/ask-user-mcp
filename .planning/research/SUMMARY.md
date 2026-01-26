# Project Research Summary

**Project:** ask-user-mcp
**Domain:** Interactive user-prompting MCP tool server with Emacs integration
**Researched:** 2026-01-26
**Confidence:** HIGH

## Executive Summary

MCP servers are lightweight protocol adapters that expose tools to AI agents via JSON-RPC 2.0 over stdio/HTTP transports. For interactive user prompting servers like ask-user-mcp, the technical landscape is well-defined but the feature expectations are still emerging. Multiple similar implementations exist (Human-In-the-Loop MCP, user-prompt-mcp, Interactive MCP) but none dominate as the standard approach.

The recommended stack centers on the official TypeScript SDK v1.25.2 with Node.js 24 LTS, using stdio transport for local process communication. The architecture follows a simple layered pattern: SDK core handles protocol mechanics, transport adapter manages stdio serialization, tool handlers contain business logic, and external integrations (emacsclient) execute actions. This is a greenfield opportunity to differentiate through Emacs integration with styled prompts, where competitors use generic UIs.

**Critical risks:** Command injection (user questions passed unsanitized to shell), zombie process accumulation (emacsclient not cleaned up), and stdio deadlock (mixing debug output with JSON-RPC). All three are architectural decisions that must be correct from Phase 1, as retrofitting is expensive. Prevention requires spawn() with argument arrays (never exec with strings), comprehensive signal handlers, and redirecting all logs to stderr.

## Key Findings

### Recommended Stack

The modern MCP server stack is remarkably standardized around the official TypeScript SDK and proven Node.js tooling. No framework complexity—just protocol handling, schema validation, and subprocess management.

**Core technologies:**

- **@modelcontextprotocol/sdk v1.25.2**: Official MCP implementation with full spec compliance (2025-11-25). Maintained by Anthropic, used by 21,227+ projects. Provides server lifecycle, capability negotiation, and message routing.

- **Node.js 24 LTS (Krypton)**: Current LTS with native Web Crypto API (SDK requirement). Long-term support through April 2028. No experimental flags needed.

- **TypeScript 5.7+ with NodeNext modules**: Modern ESM support with strict type checking. Use tsx for zero-config development (10x faster than ts-node, used by official MCP examples).

- **pnpm 9.x**: Package manager with 70% disk savings vs npm/yarn, strict dependency management prevents bugs. Official MCP SDK repository uses pnpm.

- **Vitest 3.x**: Testing framework 10-20x faster than Jest with zero-config TypeScript support. Drop-in Jest replacement with 95% API compatibility.

- **Zod 3.25+**: Schema validation (required peer dependency for SDK). Validates tool inputs at runtime and generates TypeScript types.

- **stdio transport (built-in SDK)**: Local process communication with microsecond latency, zero network overhead, no authentication needed. Standard for Claude Desktop, Cursor, agent-shell integrations.

**Critical version notes:**
- SDK v1.x is production-ready (v2 expected Q1 2026 with breaking changes)
- Node.js 24 LTS avoids Web Crypto polyfill needs (required by older versions)
- Never write to stdout when using stdio transport—corrupts JSON-RPC stream

### Expected Features

Interactive user-prompting servers sit in a gray area between tool-providing servers (well-defined expectations) and novel interaction patterns (no established standards). The protocol itself is table stakes; most "features" are implementation quality concerns.

**Must have (table stakes):**

- JSON-RPC 2.0 protocol with capability negotiation (MCP spec requirement)
- Single AskUserQuestion tool declaration with input schema
- Synchronous blocking until user responds
- Input validation against declared schema
- Error responses (not crashes) for malformed input
- 5-minute timeout to prevent indefinite hangs
- No stdout pollution (STDIO transport requirement—all logs to stderr)

**Should have (competitive differentiation):**

- Rich prompt styling in Emacs (project's core differentiator vs generic UI implementations)
- Configurable timeout per tool call
- Graceful timeout UX with helpful error messages
- Response validation with retry loop
- Progress notifications for long waits (prevents client timeout)

**Defer (v2+ features):**

- Multiple response types (choice, confirmation, multi-line)
- Response history audit trail
- Multi-turn clarification (requires protocol extensions)
- Authentication for multi-user environments
- HTTP transport (unnecessary for local-only use case)
- Multiple tools (single-tool simplicity is advantage)
- Persistent state (introduces race conditions)

**Anti-features to avoid:**
Default answers (defeats purpose), caching responses (context-dependent questions), async/background prompts (violates synchronous tool model), GUI windows outside Emacs (breaks workflow), complex configuration (YAGNI for single-tool server).

### Architecture Approach

MCP servers using the official SDK follow a straightforward layered architecture with clear separation between protocol handling, business logic, and external integrations.

**Major components:**

1. **McpServer (SDK core)**: Manages server lifecycle, registers capabilities (tools/resources/prompts), routes JSON-RPC messages to handlers. No customization needed—SDK provides complete implementation.

2. **StdioServerTransport**: Handles stdin/stdout I/O, JSON-RPC serialization/deserialization, message framing. Spawned as child process by host, automatically cleaned up on connection close.

3. **Tool handler (askUser.ts)**: Validates input with Zod schema, spawns emacsclient subprocess with timeout, parses response, returns structured CallToolResult. Contains all business logic.

4. **Emacsclient integration utility**: Wrapper around Node.js child_process.spawn() with proper argument arrays (prevents injection), timeout enforcement, signal handlers, and error classification.

**Data flow:**
Claude requests tool → JSON-RPC via stdio → SDK deserializes → validates against Zod schema → invokes tool handler → spawns emacsclient → Emacs prompts user → answer returned → handler formats response → SDK serializes → stdout back to Claude.

**Build order rationale:**
Foundation (package.json, tsconfig) → Transport/server init (validates protocol handshake) → Emacsclient utility (testable in isolation) → Tool handler (combines utility + MCP) → Tool registration (completes protocol) → Emacs integration (end-to-end) → Polish (timeouts, errors). This order enables incremental validation at each layer.

### Critical Pitfalls

**1. Command injection via unsanitized input**
Using exec() or execSync() with string interpolation allows shell metacharacters in questions to execute arbitrary commands. Example: question `"; rm -rf / #"` could wipe filesystem. Prevention: Use spawn() with argument arrays (bypasses shell entirely), never build command strings. **CRITICAL security vulnerability—must be correct in Phase 1.**

**2. Zombie and orphan process accumulation**
Child processes (emacsclient) accumulate if not cleaned up on timeouts, crashes, or signals. Causes PID exhaustion (max 32768 on Linux), memory leaks, container eviction. Prevention: Track all children in Set, register exit handlers, add signal handlers (SIGINT, SIGTERM), kill children with SIGTERM on shutdown. **CRITICAL resource leak—requires Phase 1 architecture.**

**3. Stdio deadlock and buffer overflow**
Writing to stdout corrupts JSON-RPC stream (protocol breaks silently). Large responses exceed maxBuffer (default 200KB) and kill child process. Prevention: Override console.log to stderr, set maxBuffer to 1MB+, handle partial JSON-RPC messages with line buffering. **CRITICAL protocol issue—breaks all communication.**

**4. Client timeout before server timeout**
Server waits 5 minutes for user, but client times out in 60 seconds and drops connection. User's answer arrives after connection is dead. Prevention: Send progress notifications every 5 seconds during wait (MCP 2025-11-25 spec feature), coordinate timeout values with client config. **Degraded UX—Phase 2 priority.**

**5. emacsclient failure modes not handled**
Server crashes when Emacs server isn't running (server-start not called), socket file missing, systemd conflicts, or display variables wrong (Wayland vs X11). Prevention: Startup health check (test emacsclient before accepting connections), classify error modes (ENOENT, ETIMEDOUT, socket errors), provide fallback mechanism (file-based I/O). **Reliability issue—Phase 2 hardening.**

**Additional concerns:**
Input validation (prevent 10MB questions, control characters), synchronous operations blocking event loop (use async spawn), poor error messages (classify and explain failures), no observability (structured logging for debugging).

## Implications for Roadmap

Based on research, recommend 3-phase structure prioritizing security and protocol correctness, then reliability and UX, finally production hardening.

### Phase 1: Core MCP Server + Basic Integration

**Rationale:** Establish protocol compliance and security foundation. These decisions (spawn vs exec, stdout hygiene, async patterns) are architectural and expensive to retrofit. Validate end-to-end flow before adding sophistication.

**Delivers:**
- Working MCP server with stdio transport
- Single AskUserQuestion tool declaration
- Secure emacsclient spawning (spawn with arg arrays, no injection)
- Basic error handling (catch and return errors, don't crash)
- 5-minute hardcoded timeout
- Input validation (type, length, UTF-8)

**Implements (from ARCHITECTURE.md):**
- McpServer setup with StdioServerTransport
- Tool registration with Zod schema
- Emacsclient utility with spawn() and argument arrays
- Basic process cleanup (exit event handlers)

**Avoids (from PITFALLS.md):**
- Pitfall 1: Command injection (use spawn, never exec)
- Pitfall 3: Stdout pollution (logs to stderr only)
- Pitfall 8: Event loop blocking (async spawn, not execSync)

**Uses (from STACK.md):**
- @modelcontextprotocol/sdk 1.25.2
- Node.js 24 LTS with native Web Crypto
- TypeScript 5.7+ with NodeNext modules
- tsx for development execution
- Zod 3.25+ for input validation

**Success criteria:** Can ask question via Claude → appears in Emacs → answer flows back → structured response returned. Server doesn't crash on errors.

### Phase 2: Error Handling & Reliability

**Rationale:** Users encounter real-world failure modes (Emacs not running, timeouts, wrong sockets). Without graceful degradation, server appears broken. Progress notifications prevent client timeouts.

**Delivers:**
- Signal handlers (SIGINT, SIGTERM) with child cleanup
- Progress notifications (5-second heartbeat during user wait)
- Emacsclient health check at startup
- Classified error messages (ENOENT, ETIMEDOUT, socket errors)
- Response validation (retry loop for invalid formats)
- Graceful timeout UX (explain why timeout occurred)

**Addresses (from FEATURES.md):**
- Progress notifications (competitive differentiator)
- Graceful timeout UX
- Response validation

**Avoids (from PITFALLS.md):**
- Pitfall 2: Zombie processes (comprehensive signal handlers)
- Pitfall 4: Client timeout (progress notifications)
- Pitfall 5: emacsclient failures (health checks, fallback)
- Pitfall 7: Input validation gaps (length limits, control chars)
- Pitfall 9: Poor error messages (classify and explain)

**Success criteria:** Server handles Emacs server down, user timeout, signal interrupts gracefully. Client doesn't timeout waiting for user. Errors are actionable.

### Phase 3: Emacs UX & Production Hardening

**Rationale:** Core functionality proven, now optimize UX and observability. Styled prompts are the project's key differentiator. Logging enables production debugging.

**Delivers:**
- Custom elisp for styled Emacs prompts (project differentiator)
- Structured logging (JSON to stderr)
- Process tracking and PID monitoring
- Configurable timeout (tool parameter)
- Documentation (README, setup guide, troubleshooting)

**Addresses (from FEATURES.md):**
- Rich prompt styling (core differentiator vs competitors)

**Avoids (from PITFALLS.md):**
- Pitfall 2: PID exhaustion monitoring (production hardening)
- Pitfall 10: No observability (structured logging)

**Success criteria:** Prompts use Emacs styling, logs are parseable JSON, monitoring alerts on PID issues, docs enable self-service setup.

### Phase Ordering Rationale

**Why Phase 1 first:**
Security and protocol correctness are architectural foundations. Command injection, stdout pollution, and event loop blocking require specific implementation patterns (spawn with args, stderr logging, async operations) that are expensive to change later. Validating basic end-to-end flow before adding complexity reduces rework risk.

**Why Phase 2 before Phase 3:**
Reliability issues (zombie processes, timeouts, emacsclient failures) block real-world usage and cause "works on my machine" debugging sessions. Progress notifications prevent client-side timeout issues that would otherwise require protocol changes. Error handling must be solid before optimizing UX.

**Why Phase 3 last:**
Styled prompts and logging are polish—valuable but not blocking. Once core reliability is proven, these enhancements have clear requirements (what to log, what errors occur) and lower rework risk. Production hardening needs data from real usage.

**Dependency chain:**
- Phase 2 depends on Phase 1 (can't handle emacsclient errors before emacsclient integration exists)
- Phase 3 depends on Phase 2 (styling enhances working prompts, logging requires error classification)
- Emacs integration in Phase 3 validates end-to-end flow that was protocol-tested in Phase 1

### Research Flags

**Phases needing deeper research during planning:**

- **Phase 3 (Emacs Styling):** Elisp patterns for styled minibuffer prompts are niche. May need Emacs-specific research for read-string customization, face properties, agent-shell integration. FEATURES.md notes custom elisp is in v1 scope but implementation patterns not fully documented.

**Phases with standard patterns (skip research-phase):**

- **Phase 1 (Core MCP Server):** Well-documented SDK patterns. Reference implementations (official filesystem server) demonstrate tool declaration, stdio transport, Zod validation. High confidence.

- **Phase 2 (Error Handling):** Standard Node.js child process management. Signal handlers, timeout strategies, and error classification are well-established patterns. Sources verified from Node.js docs and production experience.

**Confidence rationale:**
STACK.md and ARCHITECTURE.md show HIGH confidence (official docs, reference implementations). FEATURES.md shows HIGH confidence on protocol requirements but MEDIUM on interactive prompting patterns (emerging niche). PITFALLS.md shows HIGH confidence on technical pitfalls (verified CVEs, official security research) but MEDIUM on emacsclient-specific failure modes (community forums, not exhaustive).

## Confidence Assessment

**Stack:** HIGH
- Official SDK documentation verified (v1.25.2 current stable)
- Node.js LTS versions confirmed (24.x current, support through 2028)
- Tooling comparisons based on benchmarks (tsx vs ts-node, Vitest vs Jest, pnpm vs npm)
- All version numbers checked against npm registry and GitHub releases

**Features:** HIGH
- MCP protocol requirements verified from official spec (2025-11-25 revision)
- Tool-providing server expectations confirmed via reference implementations
- Interactive prompting servers compared (4 existing implementations analyzed)
- Table stakes vs differentiators validated against competitor features

**Architecture:** HIGH
- Component boundaries documented in official SDK (McpServer, Transport, handlers)
- Data flow verified from MCP specification and TypeScript SDK source
- Build order derived from dependency analysis and reference server structure
- Patterns validated against official examples (filesystem server, SQLite server)

**Pitfalls:** HIGH (technical) / MEDIUM (emacsclient-specific)
- Command injection, zombie processes, stdio deadlock verified from Node.js docs and CVEs
- Timeout handling confirmed from MCP spec updates (progress notifications in 2025-11-25)
- Security issues validated from recent research (Astrix 2025 report, Trend Micro findings)
- emacsclient failure modes based on GNU Emacs docs and community reports (not exhaustive)

**Overall confidence:** HIGH

Research is comprehensive enough to proceed with roadmap creation. Known gaps (emacsclient edge cases, Emacs styling patterns) can be addressed during phase planning via targeted research.

### Gaps to Address

**Emacsclient timeout edge cases:**
Research documents timeout strategy (300000ms, spawn timeout option) but not handling of edge cases: What if emacsclient hangs after accepting command? What if socket exists but server is zombie? These require validation during Phase 2 implementation—plan time for experimentation and fallback mechanism design.

**Emacs styling patterns:**
FEATURES.md identifies rich prompt styling as v1 scope and key differentiator, but ARCHITECTURE.md doesn't detail elisp implementation patterns. Gap: How to customize read-string appearance, what face properties work in minibuffer, how to integrate with agent-shell existing styles. Requires Phase 3 research-phase or prototype experimentation.

**Agent-shell integration specifics:**
Research references agent-shell as deployment target but doesn't document its MCP configuration format, timeout settings, or Emacs integration requirements. Gap: May need agent-shell documentation review before Phase 3 to ensure styling compatibility and config correctness. Low risk—likely standard MCP client config.

**Progress notification client support:**
Phase 2 recommends progress notifications (MCP 2025-11-25 feature) but doesn't confirm agent-shell client support. Gap: Verify client implements notifications/progress before investing development time. If unsupported, fallback is just accepting client timeout behavior (degrades UX but doesn't block functionality).

**Production monitoring strategy:**
PITFALLS.md identifies PID exhaustion risk and recommends monitoring, but doesn't specify thresholds, alerting tools, or metrics collection patterns. Gap: Production deployment needs observability design—consider this in Phase 3 or defer to post-MVP operations work.

**How to handle during planning:**
- Emacsclient edge cases: Budget extra time in Phase 2 for experimentation, design fallback mechanism (file-based I/O)
- Emacs styling: Use `/gsd:research-phase` at start of Phase 3 to research elisp patterns and agent-shell integration
- Agent-shell config: Review agent-shell docs during Phase 3 planning (1-2 hour task, low complexity)
- Progress notifications: Quick validation at start of Phase 2 (test client support with minimal prototype)
- Production monitoring: Defer to post-MVP unless deploying to multi-user environment

## Sources

### Primary Sources (HIGH confidence)

**MCP Specification & SDK:**
- [MCP Specification 2025-11-25](https://modelcontextprotocol.io/specification/2025-11-25) — Protocol architecture, transport layers, capability negotiation
- [TypeScript SDK v1.25.2](https://github.com/modelcontextprotocol/typescript-sdk) — Official implementation, server patterns, reference examples
- [MCP SDK npm package](https://www.npmjs.com/package/@modelcontextprotocol/sdk) — Version verification, peer dependencies
- [TypeScript SDK Documentation](https://github.com/modelcontextprotocol/typescript-sdk/blob/main/docs/server.md) — Server implementation guide

**Node.js & Runtime:**
- [Node.js LTS releases](https://nodejs.org/en/about/previous-releases) — Version numbers, LTS schedules
- [endoflife.date Node.js](https://endoflife.date/nodejs) — Support timelines verified
- [Node.js child_process documentation](https://nodejs.org/api/child_process.html) — spawn vs exec behavior
- [Node.js best practices](https://nodejs.org/en/docs/guides/blocking-vs-non-blocking/) — Async patterns

**Security Research:**
- [Astrix MCP Security Report 2025](https://astrix.security/learn/blog/state-of-mcp-server-security-2025/) — 88% credential exposure, 79% ENV vars
- [Trend Micro MCP Vulnerability Disclosure](https://modelcontextprotocol.io/specification/2025-11-25) — SQL injection → prompt injection in reference server
- [Auth0 Command Injection Prevention](https://auth0.com/blog/preventing-command-injection-attacks-in-node-js-apps/) — spawn vs exec security
- [OWASP GenAI MCP Security Guide](https://genai.owasp.org/resource/cheatsheet-a-practical-guide-for-securely-using-third-party-mcp-servers-1-0/) — Best practices

### Secondary Sources (MEDIUM confidence)

**Feature Landscape:**
- [Human-In-the-Loop MCP Server](https://github.com/GongRzhe/Human-In-the-Loop-MCP-Server) — Multiple response types implementation
- [User Prompt MCP Server](https://github.com/nazar256/user-prompt-mcp) — Simple text input for Cursor
- [Interactive MCP Server](https://playbooks.com/mcp/ttommyth/interactive-mcp) — Persistent chats, notifications
- [Interactive Feedback MCP](https://github.com/noopstudios/interactive-feedback-mcp) — Command execution with feedback loop

**Tooling Comparisons:**
- [tsx vs ts-node comparison](https://betterstack.com/community/guides/scaling-nodejs/tsx-vs-ts-node/) — Benchmarks, use cases
- [Vitest vs Jest 2025](https://medium.com/@ruverd/jest-vs-vitest-which-test-runner-should-you-use-in-2025-5c85e4f2bda9) — Performance metrics
- [pnpm vs npm vs yarn](https://dev.to/hamzakhan/npm-vs-yarn-vs-pnpm-which-package-manager-should-you-use-in-2025-2f1g) — Disk usage, speed
- [Pino vs Winston](https://betterstack.com/community/comparisons/pino-vs-winston/) — Logging performance

**Architecture Patterns:**
- [MCP Reference Servers](https://github.com/modelcontextprotocol/servers) — Official implementation examples
- [MCP Best Practices Guide](https://modelcontextprotocol.info/docs/best-practices/) — Architecture recommendations
- [Understanding MCP Through Raw STDIO](https://foojay.io/today/understanding-mcp-through-raw-stdio-communication/) — Transport layer deep dive

### Tertiary Sources (LOW confidence, needs validation)

**Emacs Integration:**
- [GNU Emacs Manual: emacsclient](https://www.gnu.org/software/emacs/manual/html_node/emacs/Invoking-emacsclient.html) — Official docs
- [emacsclient display errors](https://bbs.archlinux.org/viewtopic.php?id=281813) — Community troubleshooting
- [Emacs daemon systemd issues](https://github.com/doomemacs/doomemacs/issues/7699) — Failure mode reports
- [mcp.el](https://github.com/lizqwerscott/mcp.el) — Reference Emacs MCP client

**Process Management:**
- [Managing Orphaned Node.js Processes](https://medium.com/@arunangshudas/5-tips-for-cleaning-orphaned-node-js-processes-196ceaa6d85e) — Cleanup strategies
- [Zombie Processes on Cloud Foundry](https://saturncloud.io/blog/what-is-a-zombie-process-and-how-to-avoid-it-when-spawning-nodejs-child-processes-on-cloud-foundry/) — Real-world incident

**Long-Running Operations:**
- [Build Timeout-Proof MCP Tools](https://www.arsturn.com/blog/no-more-timeouts-how-to-build-long-running-mcp-tools-that-actually-finish-the-job) — Progress notifications
- [MCP SEP-1391: Long-Running Operations](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/1391) — Proposal discussion

---

**Research completed:** 2026-01-26
**Ready for roadmap:** Yes

**Next steps:**
1. Proceed to roadmap creation with 3-phase structure outlined above
2. Use `/gsd:research-phase` for Phase 3 (Emacs styling patterns)
3. Plan experimentation time in Phase 2 for emacsclient edge case handling
4. Verify agent-shell progress notification support before Phase 2 implementation
