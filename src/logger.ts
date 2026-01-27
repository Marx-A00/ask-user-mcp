import pino from "pino";

/**
 * Structured JSON logger using Pino.
 * 
 * Outputs to stderr (Pino default) to keep stdout clear for JSON-RPC.
 * Log level configurable via LOG_LEVEL environment variable.
 */
export const logger = pino({
  level: process.env.LOG_LEVEL || "info",
  formatters: {
    level: (label) => ({ level: label }),
  },
});

export default logger;
