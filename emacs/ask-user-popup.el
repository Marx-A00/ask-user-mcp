;;; ask-user-popup.el --- Popup buffer UI for MCP AskUserQuestion -*- lexical-binding: t -*-

;;; Commentary:
;; Provides popup buffer UI for the ask-user-mcp server (v2).
;; This replaces the minibuffer-based prompts from v1 with a proper popup
;; buffer that appears at the bottom of the frame (popper-style).
;;
;; Core features:
;; - Bottom-positioned popup (~40% frame height)
;; - Blocking behavior using recursive-edit
;; - Buffer lifecycle management with proper cleanup
;; - Visual layout with header line and styled content
;; - Cancel support (C-g, q)
;;
;; Usage:
;;   (mr-x/ask-user-popup "What is your name?" "Optional description")

;;; Code:

(require 'cl-lib)

;;; Buffer-local variables

(defvar-local ask-user-popup--result nil
  "Stores the result value to return from the popup.")

(defvar-local ask-user-popup--cancelled nil
  "Non-nil if the popup was cancelled by the user.")

;;; Major mode

(defvar ask-user-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") 'ask-user-popup-cancel)
    (define-key map (kbd "C-g") 'ask-user-popup-cancel)
    map)
  "Keymap for `ask-user-popup-mode'.")

(define-derived-mode ask-user-popup-mode special-mode "AskUser"
  "Major mode for Claude's AskUserQuestion popup buffer.

\\{ask-user-popup-mode-map}"
  (setq-local mode-line-format nil)
  (setq-local cursor-type nil))

;;; Cancel function

(defun ask-user-popup-cancel ()
  "Cancel the popup and return error to caller."
  (interactive)
  (setq ask-user-popup--cancelled t)
  (setq ask-user-popup--result nil)
  (message "Cancelled")
  (exit-recursive-edit))

;;; Main popup function

(defun mr-x/ask-user-popup (question &optional description)
  "Display QUESTION in a popup buffer at bottom of frame.
DESCRIPTION is optional context displayed in muted text.

This function blocks until the user responds or cancels.
Returns the user's response string.
Signals an error if cancelled."
  (let* ((buf (get-buffer-create "*ask-user*"))
         (win nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (ask-user-popup-mode)
              
              ;; Set header line
              (setq-local header-line-format " Claude is asking...")
              
              ;; Insert question (bold, larger)
              (insert (propertize question 'face '(:weight bold :height 1.2)))
              (insert "\n\n")
              
              ;; Insert description if provided (muted)
              (when description
                (insert (propertize description 'face 'shadow))
                (insert "\n\n"))
              
              ;; Content area separator
              (insert (propertize "────────────────────────────────────────" 'face 'shadow))
              (insert "\n\n")
              
              ;; Placeholder for Phase 5 content
              (insert (propertize "[Response area - Phase 5]" 'face 'italic))
              (insert "\n")
              
              ;; Make buffer read-only
              (setq buffer-read-only t)
              
              ;; Reset state variables
              (setq ask-user-popup--result nil)
              (setq ask-user-popup--cancelled nil)))
          
          ;; Display buffer at bottom
          (setq win (display-buffer buf
                                    '((display-buffer-at-bottom)
                                      (window-height . 0.4)
                                      (preserve-size . (nil . t)))))
          
          ;; Select the popup window
          (select-window win)
          
          ;; Block until user responds or cancels
          (recursive-edit)
          
          ;; Check if cancelled
          (when (buffer-local-value 'ask-user-popup--cancelled buf)
            (error "User cancelled the question"))
          
          ;; Return result
          (buffer-local-value 'ask-user-popup--result buf))
      
      ;; Cleanup: kill buffer and window
      (when (buffer-live-p buf)
        (when (get-buffer-window buf)
          (quit-window t (get-buffer-window buf)))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(provide 'ask-user-popup)

;;; ask-user-popup.el ends here
