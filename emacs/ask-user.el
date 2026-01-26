;;; ask-user.el --- MCP AskUserQuestion support for agent-shell -*- lexical-binding: t -*-

;;; Commentary:
;; Provides minibuffer prompting for the ask-user-mcp server.
;; Load this file in your Emacs config to enable AskUserQuestion tool support.

;;; Code:

(defun ask-user-question (prompt)
  "Prompt user with PROMPT in minibuffer and return their response.
PROMPT is the formatted question string from the MCP server."
  (read-from-minibuffer prompt))

(provide 'ask-user)

;;; ask-user.el ends here
