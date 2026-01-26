#!/usr/bin/env node
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import { askViaEmacs } from "./emacs-interface.js";

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

async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error("Ask User MCP Server running on stdio");
}

main().catch((error) => {
  console.error("Fatal error in main():", error);
  process.exit(1);
});
