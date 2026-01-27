#!/usr/bin/env node
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import { askViaEmacs, getActiveProcesses } from "./emacs-interface.js";
import logger from "./logger.js";

const server = new McpServer({
  name: "ask-user-mcp",
  version: "1.0.0",
});

// Register AskUserQuestion tool
server.registerTool(
  "AskUserQuestion",
  {
    description: "Ask the user a question via Emacs minibuffer and wait for their response. Use this when you need clarification or user input.",
    inputSchema: {
      question: z.string().describe("The question to ask the user"),
      header: z.string().optional().describe("Optional header/context shown before the question"),
    },
  },
  async (args) => {
    try {
      const response = await askViaEmacs(args.question, args.header);
      return {
        content: [
          {
            type: "text",
            text: response,
          },
        ],
      };
    } catch (error) {
      // Return errors as content so Claude sees them (don't throw)
      const errorMessage = error instanceof Error ? error.message : String(error);
      return {
        content: [
          {
            type: "text",
            text: `Error: ${errorMessage}`,
          },
        ],
      };
    }
  }
);

/**
 * Graceful shutdown handler - terminates active child processes.
 */
function gracefulShutdown(signal: string): void {
  const processes = getActiveProcesses();
  logger.info({ signal, activeCount: processes.size }, "Shutting down");

  for (const proc of processes) {
    if (!proc.killed) {
      logger.info({ pid: proc.pid }, "Terminated pending emacsclient process");
      proc.kill("SIGTERM");
    }
  }

  process.exitCode = 0;
}

async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  logger.info("Ask User MCP Server running on stdio");

  // Install signal handlers for graceful shutdown
  process.on("SIGINT", () => gracefulShutdown("SIGINT"));
  process.on("SIGTERM", () => gracefulShutdown("SIGTERM"));
}

main().catch((error) => {
  logger.error({ err: error }, "Fatal error in main()");
  process.exit(1);
});
