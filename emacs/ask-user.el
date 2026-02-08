;;; ask-user.el --- MCP AskUserQuestion support for agent-shell -*- lexical-binding: t -*-

;;; Commentary:
;; Provides minibuffer prompting for the ask-user-mcp server.
;; Load this file in your Emacs config to enable AskUserQuestion tool support.
;;
;; Usage:
;;   (mr-x/ask-user-question "What is your name?")
;;   (mr-x/ask-user-question "Pick color" "Color")  ; with header

;;; Code:

(defun mr-x/ask-user-question (question &optional header timeout-ms)
  "Prompt user with QUESTION in minibuffer and return their response.
QUESTION is the question text from the MCP server.
HEADER is an optional context string displayed in parentheses.
TIMEOUT-MS is reserved for future use (currently ignored).

The prompt is styled with bold 'Claude asks:' prefix for visibility."
  (let* ((prefix (if header
                     (propertize (format "Claude asks (%s): " header) 'face 'bold)
                   (propertize "Claude asks: " 'face 'bold)))
         (question-styled (propertize question 'face 'minibuffer-prompt))
         (full-prompt (concat prefix question-styled " ")))
    (read-string full-prompt)))

;; Backwards compatibility alias
(defalias 'ask-user-question 'mr-x/ask-user-question
  "Legacy alias for `mr-x/ask-user-question'.
Kept for backwards compatibility with older server versions.")

;; Load popup UI module (v2)
;; ask-user-popup.el provides the new popup-based interface that appears
;; at the bottom of the frame (popper-style) instead of using the minibuffer.
(require 'ask-user-popup nil t)  ; noerror if not found

(provide 'ask-user)

;;; ask-user.el ends here
