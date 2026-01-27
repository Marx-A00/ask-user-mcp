# ask-user-mcp

MCP server that enables Claude to pause and ask clarifying questions via Emacs.

## Quick Start

```bash
npm install
npm run build
```

Built files are in `build/`.

## agent-shell Configuration

Add to `~/.config/agent-shell/mcp-servers.json`:

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

Replace `/REPLACE/WITH/ABSOLUTE/PATH/` with the actual path to this project.

## Emacs Setup

Add to your Emacs config:

```elisp
;; ask-user-mcp support
(load-file "/REPLACE/WITH/ABSOLUTE/PATH/ask-user-mcp/emacs/ask-user.el")

;; Ensure Emacs server is running (required for emacsclient)
(unless (server-running-p)
  (server-start))
```

Replace `/REPLACE/WITH/ABSOLUTE/PATH/` with the actual path to this project.

## Tool Parameters

The `ask_user` tool accepts:

- **question** (required): The question to ask the user
- **header** (optional): Context displayed above the question in the prompt
- **timeout_ms** (optional): How long to wait for response
  - Minimum: 30 seconds (30000)
  - Maximum: 30 minutes (1800000)
  - Default: 5 minutes (300000)

## Debugging

Enable debug logs to see Q&A audit trail:

```bash
LOG_LEVEL=debug node build/index.js
```

Debug logs include: question, response, duration, and success/error status.

For common issues, see [TROUBLESHOOTING.md](./TROUBLESHOOTING.md).
