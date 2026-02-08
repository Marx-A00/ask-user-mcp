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
  
  // Create child logger for Q&A audit trail
  const startTime = Date.now();
  const qaLogger = logger.child({
    component: 'ask-user',
    question: question.slice(0, 100), // truncate for logs
    header: options.header,
    timeout_ms: timeout,
  });

  qaLogger.debug('Q&A initiated');

  const escapedQuestion = escapeElispString(question);
  const descriptionArg = options.header 
    ? `"${escapeElispString(options.header)}"` 
    : "nil";
  
  // Build elisp with graceful fallback chain:
  // 1. Try v2 popup (mr-x/ask-user-popup)
  // 2. Fall back to v1 minibuffer (mr-x/ask-user-question)
  // 3. Ultimate fallback to read-string
  const elispExpr = `(condition-case err
    (mr-x/ask-user-popup "${escapedQuestion}" ${descriptionArg})
  (void-function
    (condition-case err2
        (progn
          (message "ask-user-mcp: popup not available, using v1 minibuffer")
          (mr-x/ask-user-question "${escapedQuestion}" ${descriptionArg}))
      (void-function
        (progn
          (message "ask-user-mcp: no ask-user functions defined, using read-string")
          (read-string "Claude asks: ${escapedQuestion} "))))))`;

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
        qaLogger.debug({
          error: errorMsg,
          duration_ms: Date.now() - startTime,
          success: false,
        }, 'Q&A failed');
        reject(new Error(errorMsg));
        return;
      }

      if (code === 0) {
        let result = stdout.trim();
        
        if (result.startsWith('"') && result.endsWith('"')) {
          result = result.slice(1, -1);
        }
        
        qaLogger.debug({
          response: result.slice(0, 100), // truncate for logs
          duration_ms: Date.now() - startTime,
          success: true,
        }, 'Q&A completed');
        resolve(result);
      } else {
        const errorMsg = classifyEmacsError(stderr, code ?? 1);
        qaLogger.debug({
          error: errorMsg,
          duration_ms: Date.now() - startTime,
          success: false,
        }, 'Q&A failed');
        reject(new Error(errorMsg));
      }
    });

    proc.on("error", (err) => {
      clearTimeout(timeoutTimer);
      activeProcesses.delete(proc);
      const errorMsg = `Failed to spawn emacsclient: ${err.message}`;
      qaLogger.debug({
        error: errorMsg,
        duration_ms: Date.now() - startTime,
        success: false,
      }, 'Q&A failed');
      reject(new Error(errorMsg));
    });
  });
}
