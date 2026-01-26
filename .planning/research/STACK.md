# Technology Stack - Ask User MCP Server

**Project:** ask-user-mcp  
**Domain:** MCP server for interactive user prompts via Emacs  
**Researched:** 2026-01-26  
**Overall Confidence:** HIGH

## Executive Summary

The standard 2025 stack for building MCP servers centers on the official TypeScript SDK with modern tooling for zero-config development. This document provides prescriptive recommendations with specific versions verified against current documentation.

## Core Framework & SDK

### MCP SDK

| Technology | Version | Purpose | Rationale |
|------------|---------|---------|-----------|
| `@modelcontextprotocol/sdk` | `^1.25.2` | Official MCP implementation | The official TypeScript SDK maintained by Anthropic. Version 1.x is stable and recommended for production. v2 is in pre-alpha (anticipated Q1 2026). Implements full MCP spec 2025-11-25. |
| `zod` | `^3.25.0` | Schema validation (peer dependency) | Required peer dependency for SDK. SDK imports from zod/v4 internally but maintains backward compatibility with v3.25+. Essential for tool parameter validation. |

**Confidence:** HIGH  
**Source:** [Official npm package](https://www.npmjs.com/package/@modelcontextprotocol/sdk), [GitHub TypeScript SDK](https://github.com/modelcontextprotocol/typescript-sdk)

**Why this SDK:**
- Official implementation by MCP protocol creators
- Active maintenance with regular updates
- Full spec compliance
- Comprehensive documentation in docs/server.md
- 21,227+ projects using it in production

**Version notes:**
- Use v1.x for production (currently 1.25.2)
- v1.x will receive bug fixes and security updates for at least 6 months after v2 ships
- Do NOT use main branch (v2 pre-alpha) for production

### Runtime Environment

| Technology | Version | Purpose | Rationale |
|------------|---------|---------|-----------|
| Node.js | `24.x LTS (Krypton)` | JavaScript runtime | Current LTS with native `globalThis.crypto` support (required for SDK OAuth). LTS support until April 2028. Alternative: Node.js 22.x LTS (support until April 2027). Minimum: Node.js v19+ for Web Crypto API. |
| TypeScript | `^5.7.0` | Type safety | Latest stable version with modern type inference and performance improvements. Excellent LSP support for development. |

**Confidence:** HIGH  
**Source:** [Node.js LTS releases](https://nodejs.org/en/about/previous-releases), [endoflife.date](https://endoflife.date/nodejs), [MCP SDK FAQ](https://github.com/modelcontextprotocol/typescript-sdk/blob/main/docs/faq.md)

**Why Node.js 24 LTS:**
- Native Web Crypto API (required by SDK for OAuth)
- Long-term support through April 2028
- No need for `--experimental-global-webcrypto` flag
- Battle-tested in production

**Node.js v18 caveat:**
- If targeting v18, requires `--experimental-global-webcrypto` flag or polyfill
- SDK FAQ documents polyfill approach for older runtimes
- Not recommended for new projects

### Transport Layer

| Technology | Purpose | When to Use |
|------------|---------|-------------|
| stdio transport (built-in SDK) | Local process communication | **USE THIS** - Local MCP servers (recommended for Claude Desktop, Emacs integrations). Zero network overhead, microsecond latency, no CORS issues. |
| Streamable HTTP transport (built-in SDK) | Remote server communication | Remote/cloud deployments, enterprise-wide servers, horizontal scaling requirements. Standard as of March 2025. |

**Confidence:** HIGH  
**Source:** [MCP Transports Specification](https://modelcontextprotocol.io/specification/2025-03-26/basic/transports), [Transport comparison guide](https://mcpcat.io/guides/comparing-stdio-sse-streamablehttp/)

**Why stdio for this project:**
- Emacs integration is inherently local
- No network stack overhead
- Simpler security model (no authentication needed)
- Standard for local MCP servers
- Client spawns server as child process

**What NOT to use:**
- SSE (Server-Sent Events) - Legacy transport, deprecated. Use Streamable HTTP instead if remote needed.
- HTTP+SSE - Replaced by Streamable HTTP in March 2025

## TypeScript Configuration

### Compiler Settings (tsconfig.json)

**Recommended configuration for Node.js 24 LTS + ESM:**

```json
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "NodeNext",
    "moduleResolution": "NodeNext",
    "esModuleInterop": true,
    "sourceMap": true,
    "outDir": "./dist",
    "rootDir": "./src",
    "strict": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "resolveJsonModule": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist"]
}
```

**Confidence:** HIGH  
**Source:** [Modern Node.js TypeScript setup 2025](https://dev.to/woovi/a-modern-nodejs-typescript-setup-for-2025-nlk), [tsconfig best practices](https://notes.shiv.info/javascript/2025/04/21/tsconfig-best-practices/)

**Rationale:**
- `NodeNext` module system: Native ESM support, aligns with modern Node.js
- `strict: true`: Catches subtle bugs, enforces type safety
- `skipLibCheck: true`: Avoid checking node_modules types (performance)
- `forceConsistentCasingInFileNames: true`: Prevent cross-platform issues

**Alternative for CommonJS projects:**
- `"module": "CommonJS"` if you must use CJS
- Not recommended for new projects in 2025

### TypeScript Execution

| Tool | Version | Purpose | Rationale |
|------|---------|---------|-----------|
| `tsx` | `^4.23.0` | TypeScript execution for development | **RECOMMENDED** - Zero-config, 10x faster than ts-node, built on esbuild, native ESM support, includes watch mode. Official MCP examples use tsx. |

**Confidence:** HIGH  
**Source:** [tsx vs ts-node comparison](https://betterstack.com/community/guides/scaling-nodejs/tsx-vs-ts-node/), [TypeScript runtime comparison](https://github.com/privatenumber/ts-runtime-comparison)

**Why tsx over ts-node:**
- 2-10x faster execution (esbuild vs TypeScript compiler)
- Zero configuration required
- Native ESM and modern JS features
- Watch mode built-in
- Official MCP SDK examples use tsx
- Modern IDEs provide type checking anyway (runtime type checking less critical)

**When to use ts-node instead:**
- Legacy projects already using ts-node
- Require runtime type checking
- Very large monorepos (some report ts-node faster at scale)

**What NOT to use:**
- `esno` - Just a wrapper around tsx, use tsx directly
- `bun` - Alternative runtime, not standard Node.js (adds deployment complexity)

## Package Management

| Tool | Version | Purpose | Rationale |
|------|---------|---------|-----------|
| `pnpm` | `^9.0.0` | Package manager | **RECOMMENDED** - 70% less disk space than npm/Yarn, fastest installs, strict dependency management prevents bugs. Official MCP SDK repo uses pnpm for monorepo. |

**Confidence:** MEDIUM  
**Source:** [pnpm vs npm vs yarn 2025](https://dev.to/hamzakhan/npm-vs-yarn-vs-pnpm-which-package-manager-should-you-use-in-2025-2f1g), [pnpm benchmarks](https://pnpm.io/benchmarks)

**Why pnpm:**
- Content-addressable storage: Hard links instead of duplicates
- 70% less disk space than npm/Yarn
- Faster than npm and Yarn in benchmarks
- Strict mode fails on incorrect package.json (catches bugs early)
- Excellent monorepo support
- Official MCP SDK uses pnpm

**Alternatives:**
- **npm** (bundled with Node.js) - Use if you want zero extra installs, acceptable performance for small projects
- **Yarn** - Use if existing project already uses it, not recommended for greenfield 2025

**Installation:**
```bash
npm install -g pnpm
```

## Code Quality Tools

### Linting & Formatting

| Tool | Version | Purpose | Rationale |
|------|---------|---------|-----------|
| `eslint` | `^9.18.0` | Linting | Modern flat config (eslint.config.js), industry standard for JavaScript/TypeScript. |
| `@typescript-eslint/eslint-plugin` | `^8.20.0` | TypeScript rules | TypeScript-specific linting rules. |
| `@typescript-eslint/parser` | `^8.20.0` | TypeScript parser | Parses TypeScript for ESLint. |
| `prettier` | `^3.4.2` | Code formatting | Opinionated formatter, prevents bikeshedding on style. |
| `eslint-config-prettier` | `^10.0.1` | ESLint/Prettier integration | Disables ESLint formatting rules that conflict with Prettier. |
| `eslint-plugin-prettier` | `^5.2.2` | Run Prettier via ESLint | Exposes Prettier errors through ESLint. |

**Confidence:** HIGH  
**Source:** [ESLint flat config guide 2025](https://advancedfrontends.com/eslint-flat-config-typescript-javascript/), [TypeScript ESLint setup](https://medium.com/@gabrieldrouin/node-js-2025-guide-how-to-setup-express-js-with-typescript-eslint-and-prettier-b342cd21c30d)

**Modern ESLint flat config (eslint.config.js):**
```javascript
import eslint from '@eslint/js';
import tseslint from 'typescript-eslint';
import prettierRecommended from 'eslint-plugin-prettier/recommended';

export default tseslint.config(
  {
    ignores: ['dist/', 'node_modules/', '**/*.d.ts'],
  },
  prettierRecommended,
  eslint.configs.recommended,
  {
    files: ['**/*.ts'],
    extends: [tseslint.configs.recommended],
    plugins: { '@typescript-eslint': tseslint.plugin },
    languageOptions: {
      parser: tseslint.parser,
    },
  }
);
```

**Prettier config (prettier.config.js):**
```javascript
export default {
  semi: true,
  trailingComma: 'all',
  singleQuote: true,
  printWidth: 100,
  tabWidth: 2,
  endOfLine: 'auto',
};
```

**Why these versions:**
- ESLint 9.x introduced flat config (modern, simpler than legacy extends chains)
- typescript-eslint 8.x supports latest TypeScript features
- Prettier 3.x is current stable
- Flat config eliminates legacy complexity

## Testing Framework

| Tool | Version | Purpose | Rationale |
|------|---------|---------|-----------|
| `vitest` | `^3.0.0` | Unit/integration testing | **RECOMMENDED** - 10-20x faster than Jest, zero-config TypeScript support, native ESM, compatible with 95% of Jest API. Fastest-growing test framework (400% adoption increase 2023-2024). |

**Confidence:** HIGH  
**Source:** [Vitest vs Jest 2025](https://medium.com/@ruverd/jest-vs-vitest-which-test-runner-should-you-use-in-2025-5c85e4f2bda9), [Vitest comparison guide](https://vitest.dev/guide/comparisons)

**Why Vitest over Jest:**
- 10-20x faster test execution (especially in watch mode)
- Zero-config TypeScript, ESM, JSX support
- Drop-in replacement for Jest (95% API compatible)
- Native ESM - no Babel/transform configuration needed
- Modern developer experience
- Released Vitest 3 in January 2025 with new features

**When to use Jest instead:**
- React Native projects (Jest mandatory)
- Large legacy codebase already using Jest
- Stability over speed priority

**Testing MCP servers:**
- Official MCP Inspector for protocol validation
- Vitest for unit/integration tests of tool logic
- In-memory client-server binding for fast tests (no subprocess overhead)

**MCP-specific testing:**
- Use `@modelcontextprotocol/inspector` for visual testing/debugging
- Test tool schemas, parameter validation, and execution
- Measure tool "hit rate" (correct tool calls) and success rate

**Confidence on MCP testing:** MEDIUM  
**Source:** [MCP testing best practices](https://www.merge.dev/blog/mcp-server-testing), [MCP Inspector](https://github.com/modelcontextprotocol/inspector)

## Logging

| Tool | Version | Purpose | Rationale |
|------|---------|---------|-----------|
| `pino` | `^9.0.0` | Structured logging | **RECOMMENDED** - Fastest Node.js logger (5-10x faster than Winston), JSON-structured logs, minimal CPU overhead, good defaults. |

**Confidence:** MEDIUM  
**Source:** [Pino vs Winston 2025](https://betterstack.com/community/comparisons/pino-vs-winston/), [Node.js logging guide](https://last9.io/blog/node-js-logging-libraries/)

**Why Pino:**
- 5-10x faster than Winston (async logging, optimized serialization)
- JSON-structured by default (machine-readable)
- Minimal performance impact (critical for stdio transport low latency)
- Zero-config, good defaults
- Strong ecosystem (transports available via separate packages)

**Alternative - Winston:**
- Use if you need multiple transports built-in
- More customization flexibility
- Human-readable logs by default
- 12M+ weekly downloads (most popular)
- Trade-off: Slower, more memory, requires configuration

**For this project (ask-user-mcp):**
- Pino recommended due to stdio transport's low-latency requirement
- JSON logs integrate well with MCP Inspector
- Simple use case doesn't need Winston's complexity

## Emacs Integration

### Process Spawning

| Technology | Module | Purpose | Rationale |
|------------|--------|---------|-----------|
| Node.js `child_process` | Built-in | Spawn emacsclient | Standard library module for executing external commands. Use `spawn()` for streaming I/O with emacsclient. |

**Confidence:** MEDIUM  
**Source:** [Node.js child_process docs](https://www.geeksforgeeks.org/node-js/node-js-child-process/)

**Implementation approach:**
```typescript
import { spawn } from 'child_process';

// Example: Invoke emacsclient to get user input
const emacs = spawn('emacsclient', ['-e', '(read-from-minibuffer "Your question: ")']);
```

**Why child_process.spawn:**
- Built-in Node.js module (no dependencies)
- Streaming I/O (suitable for interactive prompts)
- Non-blocking async execution

**Alternative methods:**
- `exec()` - Buffers output, suitable for short-running commands
- `execFile()` - Similar to exec but doesn't spawn shell
- `fork()` - For spawning Node.js processes only

**Emacsclient integration notes:**
- Requires Emacs server running (`M-x server-start` or `(server-start)` in init.el)
- Use `-e` flag to evaluate Elisp expressions
- Capture stdout for return values
- Handle stderr for errors
- Consider timeout handling for unresponsive Emacs

**Confidence caveat:** Search results didn't show specific emacsclient+Node.js examples. This recommendation is based on general Node.js process spawning best practices.

## Build & Deployment

### Build Process

**For production:**
```bash
# Compile TypeScript to JavaScript
tsc

# Or use esbuild for faster builds (optional)
esbuild src/index.ts --bundle --platform=node --outfile=dist/index.js
```

**For development:**
```bash
# Direct execution with tsx
tsx src/index.ts

# Watch mode for development
tsx watch src/index.ts
```

**Recommended approach:**
- Development: Use `tsx` for zero-config hot reload
- Production: Compile with `tsc` to JavaScript, ship dist/ folder
- Alternative: Use esbuild for faster production builds (optional)

**What NOT to use:**
- Node.js `--experimental-strip-types` (Node.js 23+) - Still experimental, not recommended for production
- Babel/SWC - Unnecessary complexity with modern TypeScript + esbuild/tsx

### Package Scripts (package.json)

```json
{
  "scripts": {
    "dev": "tsx watch src/index.ts",
    "build": "tsc",
    "start": "node dist/index.js",
    "test": "vitest",
    "test:watch": "vitest watch",
    "lint": "eslint .",
    "format": "prettier --write ."
  }
}
```

## Security Considerations

### Authentication

| Aspect | Recommendation | Rationale |
|--------|---------------|-----------|
| stdio transport | No authentication needed | Local process communication, inherently trusted. |
| If using HTTP | OAuth 2.0 with PKCE | MCP spec requirement. SDK provides OAuth helpers. Avoid static tokens (hard to rotate/audit). |

**Confidence:** HIGH  
**Source:** [MCP Security Best Practices](https://modelcontextprotocol.io/specification/draft/basic/security_best_practices), [MCP security guide 2025](https://workos.com/blog/mcp-security-risks-best-practices)

**Security best practices for MCP servers:**
1. **stdio servers (like this project):**
   - No authentication required (local trust boundary)
   - Validate all tool inputs with Zod schemas
   - Sanitize user input before executing external commands (emacsclient)

2. **If deploying remotely (HTTP):**
   - Use OAuth 2.0 / OIDC (NOT static tokens)
   - Implement Resource Indicators (RFC 8707) to prevent token mis-redemption
   - Enforce TLS/mTLS on all connections
   - Use short-lived tokens with audience scoping

3. **Common vulnerabilities:**
   - Prompt injection via context manipulation
   - Token exposure in logs/URLs
   - Command injection (sanitize before spawning processes)

**For this project:**
- stdio = no authentication needed
- Focus on input validation (Zod) and command sanitization (emacsclient args)
- No sensitive data in logs

## Alternative Stacks Considered

### Why NOT Python SDK

| Aspect | TypeScript SDK | Python SDK |
|--------|---------------|------------|
| Performance | V8 engine, fast I/O | Slower for stdio transport |
| Type safety | Native TypeScript support | Requires type stubs |
| Ecosystem fit | Node.js for local tools | Python for AI/ML integrations |
| Official status | Both official | Both official |

**Verdict:** TypeScript is standard for stdio MCP servers. Python better for MCP servers integrating with AI/ML workflows.

### Why NOT Bun runtime

| Aspect | Node.js | Bun |
|--------|---------|-----|
| Stability | Battle-tested since 2009 | Still evolving (released 2022) |
| Compatibility | 100% ecosystem support | Some compatibility gaps |
| Deployment | Universal support (AWS, GCP, etc.) | Limited managed runtime support |
| Speed | Fast enough for stdio | 4-7x faster than Node.js |

**Verdict:** Node.js is the safe choice for production. Bun is bleeding-edge - not recommended until wider ecosystem adoption.

### Why NOT Deno runtime

| Aspect | Node.js | Deno |
|--------|---------|-----|
| npm ecosystem | Full compatibility | Partial npm compat via npm: specifier |
| SDK support | Official SDK targets Node.js | Would require adaptation |
| Tooling maturity | Mature LSP, debugging | Good but less mature |

**Verdict:** Node.js remains standard. Deno's security model and TypeScript-first are attractive but ecosystem immature for MCP.

## Installation Checklist

### Greenfield Setup

```bash
# 1. Initialize project
mkdir ask-user-mcp
cd ask-user-mcp
pnpm init

# 2. Install core dependencies
pnpm add @modelcontextprotocol/sdk zod

# 3. Install dev dependencies
pnpm add -D typescript tsx vitest \
  eslint @typescript-eslint/parser @typescript-eslint/eslint-plugin \
  prettier eslint-config-prettier eslint-plugin-prettier \
  @types/node

# 4. Initialize TypeScript
pnpx tsc --init

# 5. Initialize git (if needed)
git init
```

### Configuration Files Needed

1. `tsconfig.json` - TypeScript compiler config (see section above)
2. `eslint.config.js` - ESLint flat config (see section above)
3. `prettier.config.js` - Prettier formatting rules
4. `package.json` - Scripts and dependencies
5. `.gitignore` - Ignore dist/, node_modules/, etc.

### Recommended Project Structure

```
ask-user-mcp/
├── src/
│   ├── index.ts          # Main entry point, MCP server setup
│   ├── tools/            # Tool implementations
│   │   └── askUser.ts    # AskUserQuestion tool
│   ├── emacs/            # Emacs integration logic
│   │   └── client.ts     # emacsclient wrapper
│   └── types/            # TypeScript type definitions
├── dist/                 # Compiled JavaScript (gitignored)
├── tests/                # Vitest tests
│   └── askUser.test.ts
├── tsconfig.json
├── eslint.config.js
├── prettier.config.js
├── package.json
└── README.md
```

## Confidence Summary

| Area | Confidence | Rationale |
|------|------------|-----------|
| MCP SDK | **HIGH** | Official SDK, verified current version (1.25.2), documentation reviewed |
| Node.js Runtime | **HIGH** | LTS versions verified, Web Crypto requirement confirmed via SDK FAQ |
| TypeScript Config | **HIGH** | 2025 best practices from multiple authoritative sources |
| tsx vs ts-node | **HIGH** | Benchmarks and MCP SDK examples confirm tsx preference |
| Package Manager | **MEDIUM** | pnpm widely recommended but npm acceptable alternative |
| Testing (Vitest) | **HIGH** | Clear performance/DX advantages, strong 2025 adoption trend |
| Linting/Formatting | **HIGH** | Flat config is modern standard, versions verified |
| Logging (Pino) | **MEDIUM** | Performance clear winner, but Winston viable if features needed |
| Emacsclient Integration | **MEDIUM** | General Node.js practices, no specific MCP+Emacs examples found |
| Security Practices | **HIGH** | Official MCP spec and OWASP GenAI guidelines |

## Gaps & Future Research

1. **Emacsclient timeout handling** - No documented patterns found for long-running emacsclient interactions. Need to research:
   - Timeout strategies for unresponsive Emacs
   - Graceful fallback if Emacs server not running
   - Error handling for emacsclient failures

2. **MCP Inspector integration** - Official testing tool mentioned but integration patterns not deeply researched. Phase-specific research needed for:
   - How to configure Inspector for stdio servers
   - Best practices for protocol validation during development

3. **Structured logging for MCP** - Pino recommended but specific MCP logging patterns not documented. Research:
   - What events to log (tool calls, errors, context)
   - Log levels for different MCP operations
   - Integration with MCP Inspector logs

## Version Update Protocol

This stack was researched on 2026-01-26. To verify currency:

1. **MCP SDK:** Check [@modelcontextprotocol/sdk npm](https://www.npmjs.com/package/@modelcontextprotocol/sdk) for latest 1.x version
2. **Node.js:** Check [Node.js releases](https://nodejs.org/en/about/previous-releases) for current LTS
3. **TypeScript:** Check [TypeScript releases](https://www.typescriptlang.org/) for latest stable
4. **Other deps:** Run `pnpm outdated` to check for updates

**Breaking change watch:**
- MCP SDK v2 expected Q1 2026 (breaking changes)
- ESLint major versions (flat config may evolve)
- Node.js LTS transitions (Node.js 24 → 26 in 2026)

## Sources

**MCP SDK:**
- [Official npm package](https://www.npmjs.com/package/@modelcontextprotocol/sdk)
- [GitHub TypeScript SDK](https://github.com/modelcontextprotocol/typescript-sdk)
- [SDK FAQ (Node.js requirements)](https://github.com/modelcontextprotocol/typescript-sdk/blob/main/docs/faq.md)

**Node.js & Runtime:**
- [Node.js LTS releases](https://nodejs.org/en/about/previous-releases)
- [endoflife.date Node.js](https://endoflife.date/nodejs)

**TypeScript & Configuration:**
- [Modern Node.js TypeScript setup 2025](https://dev.to/woovi/a-modern-nodejs-typescript-setup-for-2025-nlk)
- [tsconfig best practices](https://notes.shiv.info/javascript/2025/04/21/tsconfig-best-practices/)

**MCP Transports:**
- [MCP Transports Specification](https://modelcontextprotocol.io/specification/2025-03-26/basic/transports)
- [Transport comparison (stdio vs HTTP vs SSE)](https://mcpcat.io/guides/comparing-stdio-sse-streamablehttp/)

**Tooling Comparisons:**
- [tsx vs ts-node](https://betterstack.com/community/guides/scaling-nodejs/tsx-vs-ts-node/)
- [pnpm vs npm vs yarn 2025](https://dev.to/hamzakhan/npm-vs-yarn-vs-pnpm-which-package-manager-should-you-use-in-2025-2f1g)
- [Vitest vs Jest 2025](https://medium.com/@ruverd/jest-vs-vitest-which-test-runner-should-you-use-in-2025-5c85e4f2bda9)
- [Pino vs Winston](https://betterstack.com/community/comparisons/pino-vs-winston/)

**ESLint & Prettier:**
- [ESLint flat config guide 2025](https://advancedfrontends.com/eslint-flat-config-typescript-javascript/)
- [TypeScript ESLint Prettier setup](https://medium.com/@gabrieldrouin/node-js-2025-guide-how-to-setup-express-js-with-typescript-eslint-and-prettier-b342cd21c30d)

**MCP Testing & Security:**
- [MCP testing best practices](https://www.merge.dev/blog/mcp-server-testing)
- [MCP Inspector](https://github.com/modelcontextprotocol/inspector)
- [MCP Security Best Practices (official spec)](https://modelcontextprotocol.io/specification/draft/basic/security_best_practices)
- [OWASP GenAI MCP Security Guide](https://genai.owasp.org/resource/cheatsheet-a-practical-guide-for-securely-using-third-party-mcp-servers-1-0/)
- [MCP security guide 2025](https://workos.com/blog/mcp-security-risks-best-practices)

**Node.js child_process:**
- [Node.js child_process guide](https://www.geeksforgeeks.org/node-js/node-js-child-process/)
