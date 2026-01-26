# Feature Landscape: MCP Tool-Providing Servers

**Domain:** Tool-providing MCP server (interactive user prompting)
**Researched:** 2026-01-26
**Overall Confidence:** HIGH

## Executive Summary

MCP servers that provide tools exist on a spectrum from minimal reference implementations to production-grade systems. For a single-tool server focused on user prompting (like ask-user-mcp), the feature landscape is surprisingly shallow — the protocol itself is table stakes, and most "features" are actually implementation quality concerns.

**Key insight:** Interactive user-prompting servers are an emerging niche within MCP. Multiple implementations exist (Human-In-the-Loop MCP, user-prompt-mcp, Interactive MCP, Interactive Feedback MCP) but none are considered "standard." This is a greenfield opportunity with unclear table stakes.

## Table Stakes

Features users expect. Missing = server feels broken or unusable.

| Feature | Why Expected | Complexity | Dependencies | Notes |
|---------|--------------|------------|--------------|-------|
| **JSON-RPC 2.0 protocol** | MCP specification requirement | Low | None | Non-negotiable for MCP compliance |
| **Capability negotiation** | Required during connection initialization | Low | None | Server must declare it provides tools |
| **Tool declaration (list_tools)** | Clients need to discover available tools | Low | None | Must include name, description, inputSchema |
| **Tool execution (call_tool)** | The actual work of the tool | Medium | Transport layer | Must return structured response |
| **STDIO transport** | Default for local MCP servers | Low | None | Required for Claude Desktop, Cursor, etc. |
| **Input validation** | Prevent crashes from malformed input | Low | None | Validate against declared inputSchema |
| **Error responses (not crashes)** | Server must stay alive on errors | Low | None | Return error in result, don't exit(1) |
| **Synchronous blocking** | For user input, must wait for response | Low | None | Tool doesn't return until user responds |
| **No stdout pollution** | STDIO transport requirement | Low | None | Never write to stdout (breaks JSON-RPC) |
| **Timeout handling** | User might not respond | Medium | None | Must not hang forever |

**Implementation notes:**
- Input validation: Check that `question` parameter exists and is a string
- Error responses: Use `isError: true` in CallToolResult, not exceptions
- Timeout: 5 minutes is reasonable default (matches v1 scope)
- STDIO: All logging MUST go to stderr, never stdout

## Differentiators

Features that set products apart. Not expected, but valued when present.

| Feature | Value Proposition | Complexity | Dependencies | Notes |
|---------|-------------------|------------|--------------|-------|
| **Rich prompt styling** | Better UX than plain text | Medium | Emacs/elisp | Custom elisp for styled prompts (v1 scope) |
| **Multiple response types** | Choice, confirmation, multi-line | High | Client UI | Beyond simple text input |
| **Response validation** | Ensure user provides valid format | Medium | None | Retry loop for invalid input |
| **Configurable timeout** | User control per-tool-call | Low | Input schema | Timeout as optional parameter |
| **Graceful timeout UX** | Inform user why request timed out | Low | Client display | Return helpful error message |
| **Conversation context** | Show why Claude is asking | High | Protocol extension | Requires client support |
| **Response history** | Audit trail of user answers | Medium | Logging/storage | Security/compliance feature |
| **Multi-turn clarification** | Follow-up questions | Very High | Protocol extension | Requires client support |
| **Authentication** | Verify user identity | Medium | Auth system | For multi-user environments |
| **Rate limiting** | Prevent prompt spam | Low | State management | Limit questions per conversation |

**Recommendations:**
- **v1 should include:** Rich prompt styling (already scoped), configurable timeout
- **Consider for v2:** Response validation, graceful timeout UX, response history
- **Defer indefinitely:** Multi-turn clarification (requires protocol changes), authentication (single-user assumption)

**Competitive analysis:**
- Human-In-the-Loop MCP: Offers text input, multiple choice, multi-line, confirmations, info messages
- user-prompt-mcp: Simple text input for Cursor
- Interactive MCP: Persistent chats, notifications
- Interactive Feedback MCP: Command execution with feedback loop

Ask-user-mcp's differentiation: **Emacs integration with styled prompts**. Other servers use generic UI.

## Anti-Features

Features to explicitly NOT build. Common mistakes in this domain.

| Anti-Feature | Why Avoid | What to Do Instead |
|--------------|-----------|-------------------|
| **HTTP/SSE transport in v1** | Adds complexity for no benefit | Use STDIO (local-only use case) |
| **Multiple tools** | Tool budget bloat | Single `AskUserQuestion` tool only |
| **Resources or Prompts** | Not relevant to use case | Tools only, skip resources/prompts |
| **Automatic retry on timeout** | Annoys user with duplicate prompts | Fail cleanly, let Claude decide to retry |
| **Persistent state** | Race conditions, complexity | Stateless tool execution |
| **Caching responses** | User might change mind | Always prompt fresh |
| **Interrupt/cancel** | Protocol doesn't support well | Simple timeout is sufficient |
| **Rich media (images, files)** | Out of scope for text questions | Text-only input/output |
| **Async/background prompts** | Violates synchronous tool model | Block until response |
| **Default answers** | Defeats purpose of asking | Require explicit user response |
| **GUI window outside Emacs** | Breaks workflow integration | Use emacsclient (v1 scope) |
| **Complex configuration** | YAGNI for single-tool server | Hardcode reasonable defaults |

**Rationale:**
- **HTTP transport:** STDIO is simpler and sufficient for agent-shell use case
- **Tool budget:** Each tool costs context window. Single tool = minimal overhead
- **Stateless:** MCP servers should be process-per-request for reliability
- **No caching:** User prompts are context-dependent, caching would be wrong
- **Text-only:** Scope creep. Other MCP servers handle files/media

## Feature Dependencies

```
JSON-RPC 2.0 protocol (base)
    ├─→ Capability negotiation
    │   └─→ Tool declaration
    │       └─→ Tool execution
    │           ├─→ Input validation
    │           ├─→ Synchronous blocking
    │           ├─→ Timeout handling
    │           └─→ Error responses
    └─→ STDIO transport
        └─→ No stdout pollution
            └─→ Logging to stderr

Tool execution (implementation)
    ├─→ Emacsclient integration (v1)
    │   └─→ Rich prompt styling (v1)
    ├─→ Timeout handling (v1)
    │   └─→ Graceful timeout UX (v2)
    └─→ Input validation (v1)
        └─→ Response validation (v2)
```

**Critical path:** JSON-RPC → STDIO → Tool execution → Emacsclient → Timeout

**Optional enhancements:** Everything else builds on core tool execution

## MVP Recommendation

For ask-user-mcp v1, prioritize:

1. **STDIO transport with JSON-RPC** (table stakes)
2. **Single AskUserQuestion tool** (table stakes)
3. **Emacsclient prompting** (core value prop)
4. **Custom elisp styling** (v1 scope, differentiator)
5. **5-minute timeout** (v1 scope, table stakes)
6. **Input validation** (table stakes)
7. **Graceful error handling** (table stakes)

Defer to post-MVP:
- **Configurable timeout:** Easy to add, not critical (can hardcode 5min)
- **Response validation:** Nice-to-have, user can always re-prompt
- **Response history:** Audit/compliance feature, not core UX
- **Multiple response types:** Scope creep, text input covers 90% of use cases

**Reasoning:**
- v1 should prove core value: "Claude can ask me clarifying questions"
- Styled Emacs prompts differentiate from generic implementations
- 5-minute timeout prevents hangs but allows thoughtful responses
- Everything else can be added based on real usage patterns

## Complexity Analysis

| Feature Category | Estimated Effort | Risk Level | Notes |
|------------------|------------------|------------|-------|
| MCP protocol basics | 1-2 days | Low | Well-documented, SDKs available |
| STDIO transport | 1 day | Low | Standard pattern |
| Tool declaration | 1 day | Low | JSON schema definition |
| Emacsclient integration | 2-3 days | Medium | Subprocess management, error handling |
| Custom elisp prompt | 2-3 days | Medium | Emacs-specific, testing in agent-shell |
| Timeout handling | 1-2 days | Medium | Async/subprocess timeout, cleanup |
| Input validation | 1 day | Low | Schema validation |
| Error handling | 1-2 days | Low | Structured error responses |

**Total v1 estimate:** 10-16 days (2-3 weeks)

**High-risk areas:**
- Emacsclient integration: Subprocess management, agent-shell compatibility
- Timeout handling: Must not leave zombie processes
- Testing: End-to-end testing requires live Emacs + Claude

**Low-risk areas:**
- MCP protocol: Well-trodden path with SDKs
- STDIO: Standard, simple pattern
- Input validation: Straightforward schema checks

## Production Readiness Considerations

While not in v1 scope, consider these for production use:

| Concern | Mitigation Strategy | Complexity |
|---------|---------------------|------------|
| Process cleanup | Ensure timeout kills emacsclient subprocess | Medium |
| Error logging | Log to stderr with timestamps, levels | Low |
| Metrics | Track timeout rate, prompt response time | Medium |
| Testing | Unit tests for tool logic, integration tests for e2e | High |
| Documentation | Clear README, example usage | Low |
| Versioning | Semantic versioning, changelog | Low |
| Security | Input sanitization (prevent injection) | Medium |
| Performance | Single tool = minimal overhead | Low |

**Not needed for v1:**
- Monitoring/alerting (single-user tool)
- HA/clustering (local process)
- Database (stateless)
- Caching (defeats purpose)

## Sources

**MCP Specification & Best Practices:**
- [MCP Specification](https://modelcontextprotocol.io/specification/2025-11-25)
- [Build an MCP Server](https://modelcontextprotocol.io/docs/develop/build-server)
- [MCP Best Practices: Architecture & Implementation Guide](https://modelcontextprotocol.info/docs/best-practices/)
- [15 Best Practices for Building MCP Servers in Production](https://thenewstack.io/15-best-practices-for-building-mcp-servers-in-production/)
- [Top 5 MCP Server Best Practices | Docker](https://www.docker.com/blog/mcp-server-best-practices/)

**Implementation Patterns:**
- [Less is More: 4 design patterns for building better MCP servers](https://www.klavis.ai/blog/less-is-more-mcp-design-patterns-for-ai-agents)
- [Design Patterns in MCP: Toolhost Pattern](https://glassbead-tc.medium.com/design-patterns-in-mcp-toolhost-pattern-59e887885df3)
- [MCP tool descriptions: overview, examples, and best practices](https://www.merge.dev/blog/mcp-tool-description)

**Error Handling & Resilience:**
- [Error Handling in MCP Servers - Best Practices Guide | MCPcat](https://mcpcat.io/guides/error-handling-custom-mcp-servers/)
- [Resilient AI Agents With MCP: Timeout And Retry Strategies | Octopus blog](https://octopus.com/blog/mcp-timeout-retry)
- [Fix MCP Error -32001: Request Timeout - Complete Guide | MCPcat](https://mcpcat.io/guides/fixing-mcp-error-32001-request-timeout/)

**Interactive User Input Servers:**
- [Human-In-the-Loop MCP Server](https://github.com/GongRzhe/Human-In-the-Loop-MCP-Server)
- [User Prompt MCP Server](https://github.com/nazar256/user-prompt-mcp)
- [Interactive MCP Server](https://playbooks.com/mcp/ttommyth/interactive-mcp)
- [Interactive Feedback MCP](https://github.com/noopstudios/interactive-feedback-mcp)
- [User Elicitation - FastMCP](https://gofastmcp.com/servers/elicitation)

**MCP Ecosystem:**
- [GitHub - modelcontextprotocol/servers](https://github.com/modelcontextprotocol/servers)
- [Example Servers - Model Context Protocol](https://modelcontextprotocol.io/examples)
- [Awesome MCP Servers](https://github.com/wong2/awesome-mcp-servers)
- [Top 10 Best Model Context Protocol (MCP) Servers in 2026](https://cybersecuritynews.com/best-model-context-protocol-mcp-servers/)

**Transport & SDK:**
- [STDIO Transport | MCP Framework](https://mcp-framework.com/docs/Transports/stdio-transport/)
- [MCP Practical Guide with STDIO Transport - F22 Labs](https://www.f22labs.com/blogs/mcp-practical-guide-with-stdio-transport/)
- [GitHub - modelcontextprotocol/typescript-sdk](https://github.com/modelcontextprotocol/typescript-sdk)
