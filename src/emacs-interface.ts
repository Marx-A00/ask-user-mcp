import { spawn, ChildProcess } from "child_process";
import logger from "./logger.js";
import { classifyEmacsError } from "./errors.js";

const activeProcesses = new Set<ChildProcess>();

export function getActiveProcesses(): Set<ChildProcess> {
  return activeProcesses;
}

export interface AskOptions {
  header?: string;
  timeout_ms?: number;
}

function escapeElispString(str: string): string {
  return str
    .replace(/\\/g, "\\\\")
    .replace(/"/g, '\\"');
}

export async function askViaEmacs(
  question: string,
  options: AskOptions = {}
): Promise<string> {
  // Extract and validate timeout
  const timeout = options.timeout_ms ?? 5 * 60 * 1000; // default 5 minutes
  
  if (timeout < 30_000) {
    throw new Error("Timeout must be at least 30 seconds (30000 ms)");
  }
  if (timeout > 30 * 60_000) {
    throw new Error("Timeout cannot exceed 30 minutes (1800000 ms)");
  }

  const timeoutSeconds = Math.round(timeout / 1000);
  
  logger.debug(
    { question: question.slice(0, 50), timeout_ms: timeout },
    "Asking question via Emacs"
  );

  const prompt = options.header 
    ? `${options.header}: ${question} > `
    : `${question} > `;

  const escapedPrompt = escapeElispString(prompt);
  const elispExpr = `(ask-user-question "${escapedPrompt}")`;

  const proc = spawn("emacsclient", ["--eval", elispExpr]);
  
  activeProcesses.add(proc);

  let stdout = "";
  let stderr = "";

  proc.stdout.on("data", (data) => {
    stdout += data.toString();
  });

  proc.stderr.on("data", (data) => {
    stderr += data.toString();
  });

  // Manual timeout tracking (workaround for Node.js signal limitations)
  let timedOut = false;
  const timeoutTimer = setTimeout(() => {
    timedOut = true;
    proc.kill("SIGTERM");
  }, timeout);

  return new Promise<string>((resolve, reject) => {
    proc.on("close", (code) => {
      clearTimeout(timeoutTimer);
      activeProcesses.delete(proc);
      
      if (timedOut) {
        const errorMsg = `Question timed out after ${timeoutSeconds} seconds waiting for response. If you need more time, increase the timeout_ms parameter (max 30 minutes).`;
        logger.error({ timeout_ms: timeout }, errorMsg);
        reject(new Error(errorMsg));
        return;
      }

      if (code === 0) {
        let result = stdout.trim();
        
        if (result.startsWith('"') && result.endsWith('"')) {
          result = result.slice(1, -1);
        }
        
        logger.debug({ responseLength: result.length }, "Received response from Emacs");
        resolve(result);
      } else {
        const errorMsg = classifyEmacsError(stderr, code ?? 1);
        logger.error({ code, stderr: stderr.trim() }, errorMsg);
        reject(new Error(errorMsg));
      }
    });

    proc.on("error", (err) => {
      clearTimeout(timeoutTimer);
      activeProcesses.delete(proc);
      const errorMsg = `Failed to spawn emacsclient: ${err.message}`;
      logger.error({ err }, errorMsg);
      reject(new Error(errorMsg));
    });
  });
}
