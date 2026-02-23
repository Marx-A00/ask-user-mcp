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

server.registerTool(
  "AskUserQuestion",
  {
    description:
      "Ask the user a question via Emacs minibuffer and wait for their response. Use this when you need clarification or user input.",
    inputSchema: {
      question: z
        .string()
        .min(1, "Question cannot be empty")
        .describe(
          "A short, concise question or title (1-2 sentences max). Do NOT put lengthy context, analysis, or explanations here — use the header parameter for that.",
        ),
      header: z
        .string()
        .optional()
        .describe(
          "Optional context, analysis, or explanation shown before the question. Put any detailed reasoning, options analysis, or background information here — NOT in the question field. This is rendered as normal body text below the question title.",
        ),
      timeout_ms: z
        .number()
        .min(30000, "Timeout must be at least 30 seconds")
        .max(30 * 60000, "Timeout cannot exceed 30 minutes")
        .optional()
        .describe("Timeout in milliseconds (default: 300000 = 5 minutes)"),
      options: z
        .array(z.string())
        .optional()
        .describe("Optional list of choices for the user to select from"),
    },
  },
  async (args) => {
    try {
      const response = await askViaEmacs(args.question, {
        header: args.header,
        timeout_ms: args.timeout_ms,
        options: args.options,
      });
      return {
        content: [
          {
            type: "text",
            text: response,
          },
        ],
      };
    } catch (error) {
      const errorMessage =
        error instanceof Error ? error.message : String(error);
      logger.warn({ error: errorMessage }, "Tool call failed");
      return {
        isError: true,
        content: [
          {
            type: "text",
            text: `Error: ${errorMessage}`,
          },
        ],
      };
    }
  },
);

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

  process.on("SIGINT", () => gracefulShutdown("SIGINT"));
  process.on("SIGTERM", () => gracefulShutdown("SIGTERM"));
}

main().catch((error) => {
  logger.error({ err: error }, "Fatal error in main()");
  process.exit(1);
});
