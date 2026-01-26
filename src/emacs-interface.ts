import { spawn } from "child_process";

/**
 * Escape a string for safe inclusion in an elisp expression.
 * Prevents elisp code injection by escaping backslashes and quotes.
 */
function escapeElispString(str: string): string {
  return str
    .replace(/\\/g, "\\\\")  // Escape backslashes first
    .replace(/"/g, '\\"');    // Then escape quotes
}

/**
 * Wrap a promise with a timeout that rejects if the promise doesn't resolve in time.
 */
function withTimeout<T>(
  promise: Promise<T>,
  timeoutMs: number,
  timeoutError: string
): Promise<T> {
  return Promise.race([
    promise,
    new Promise<T>((_, reject) =>
      setTimeout(() => reject(new Error(timeoutError)), timeoutMs)
    ),
  ]);
}

/**
 * Ask the user a question via Emacs minibuffer using emacsclient.
 * 
 * @param question The question to ask
 * @param header Optional header/context to show before the question
 * @returns Promise resolving to the user's response
 * @throws Error if emacsclient fails, times out, or is not available
 */
export async function askViaEmacs(
  question: string,
  header?: string
): Promise<string> {
  // Format prompt according to CONTEXT.md decisions
  const prompt = header 
    ? `${header}: ${question} > `
    : `${question} > `;

  // Build elisp expression with escaped prompt
  const escapedPrompt = escapeElispString(prompt);
  const elispExpr = `(ask-user-question "${escapedPrompt}")`;

  // Create the emacsclient process with SECURE argument array (no shell injection)
  const proc = spawn("emacsclient", ["--eval", elispExpr]);

  // Collect output
  let stdout = "";
  let stderr = "";

  proc.stdout.on("data", (data) => {
    stdout += data.toString();
  });

  proc.stderr.on("data", (data) => {
    stderr += data.toString();
  });

  // Wait for process to complete
  const processPromise = new Promise<string>((resolve, reject) => {
    proc.on("close", (code) => {
      if (code === 0) {
        // Success - parse elisp return value
        // emacsclient --eval returns strings with surrounding quotes: "user input"
        let result = stdout.trim();
        
        // Strip outer quotes if present
        if (result.startsWith('"') && result.endsWith('"')) {
          result = result.slice(1, -1);
        }
        
        resolve(result);
      } else {
        // Error - include stderr in error message
        reject(new Error(`emacsclient exited with code ${code}: ${stderr}`));
      }
    });

    proc.on("error", (err) => {
      // Process spawn failed (e.g., emacsclient not found)
      reject(new Error(`Failed to spawn emacsclient: ${err.message}`));
    });
  });

  // Wrap with 5-minute timeout
  return withTimeout(
    processPromise,
    5 * 60 * 1000,
    "Question timed out after 5 minutes"
  );
}
