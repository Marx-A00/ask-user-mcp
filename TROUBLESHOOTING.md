# Troubleshooting Guide

Common issues and how to fix them.

## Emacs Server Not Running

**Symptom:** "can't find socket" error or timeouts with no prompt appearing

**Diagnostic:**
```bash
emacsclient -e "(+ 1 1)"
```
Should return `2`. If it errors, the server isn't running.

**Fix:**
1. In Emacs: `M-x server-start`
2. Or add to your Emacs config:
   ```elisp
   (unless (server-running-p)
     (server-start))
   ```

## Socket Path Issues

**Symptom:** "socket directory" errors or "connection refused"

**Diagnostic:**
```bash
# Check if socket exists
ls ~/.emacs.d/server/
# Or on some systems:
ls $TMPDIR/emacs$(id -u)/
```

**Fix:**
1. Ensure `server-socket-dir` is set correctly in Emacs
2. Check directory permissions: `chmod 700 ~/.emacs.d/server/`
3. Remove stale socket files and restart Emacs server

## Function Not Defined (void-function)

**Symptom:** Message in *Messages* buffer: "mr-x/ask-user-question not defined, using read-string"

**Diagnostic:**
```elisp
M-: (fboundp 'mr-x/ask-user-question)
```
Should return `t`. If it returns `nil`, the function isn't loaded.

**Fix:**
1. Verify the load-file path in your Emacs config is correct
2. Manually load: `M-x load-file RET /path/to/ask-user-mcp/emacs/ask-user.el`
3. Check for load errors in *Messages* buffer

Note: The server will still work using `read-string` fallback, but you won't get the styled prompt.

## Permission Denied

**Symptom:** Error mentions "permission denied" for socket or directory

**Diagnostic:**
```bash
ls -la ~/.emacs.d/server/
```

**Fix:**
```bash
chmod 700 ~/.emacs.d/server/
```

Ensure the socket directory is owned by your user and has mode 700.

## Timeout Issues

**Symptom:** "timed out after X seconds waiting for response"

**Diagnostic:**
1. Is the question prompt appearing in Emacs?
2. Is Emacs responsive? (try `M-x` to check)
3. Was the timeout sufficient for the question complexity?

**Fix:**
1. If prompt not appearing: Check Emacs server is running (see above)
2. If Emacs hung: Kill and restart Emacs
3. If need more time: Increase `timeout_ms` parameter (max 30 minutes / 1800000 ms)

## Command Not Found (emacsclient)

**Symptom:** "command not found: emacsclient" or "Failed to spawn emacsclient"

**Diagnostic:**
```bash
which emacsclient
```

**Fix:**
1. Install Emacs if not installed
2. Add Emacs bin directory to PATH:
   - macOS (Homebrew): `/opt/homebrew/bin` or `/usr/local/bin`
   - macOS (Emacs.app): `/Applications/Emacs.app/Contents/MacOS/bin`
   - Linux: Usually `/usr/bin`
