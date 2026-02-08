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
;; - Selection mode with C-n/C-p navigation (Phase 5)
;; - Free-text input mode with Tab switching (Phase 5)
;;
;; Usage:
;;   (mr-x/ask-user-popup "What is your name?" "Optional description")
;;   (mr-x/ask-user-popup "Choose color" "Pick one" '("Red" "Green" "Blue"))
;;
;; Emacsclient Integration (MCP Server):
;; This function is designed to be called via emacsclient from the Node.js server:
;;   emacsclient --eval '(mr-x/ask-user-popup "question" "description")'
;; It blocks until the user responds, then returns the response string.
;; Cancel (C-g/q) signals an error, causing non-zero exit code from emacsclient.

;;; Code:

(require 'cl-lib)

;;; Buffer-local variables

(defvar-local ask-user-popup--result nil
  "Stores the result value to return from the popup.")

(defvar-local ask-user-popup--cancelled nil
  "Non-nil if the popup was cancelled by the user.")

(defvar-local ask-user-popup--options nil
  "List of option strings for selection mode.")

(defvar-local ask-user-popup--selected-index 0
  "Currently selected option index (0-based).")

(defvar-local ask-user-popup--selection-overlay nil
  "Overlay used to highlight the currently selected option.")

(defvar-local ask-user-popup--text-start nil
  "Marker for start of editable text region.")

(defvar-local ask-user-popup--text-end nil
  "Marker for end of editable text region.")

(defvar-local ask-user-popup--focus nil
  "Current focus: 'options or 'text.")

;;; Text editing functions

(defun ask-user-popup--submit-text ()
  "Submit text content from editable region and exit."
  (interactive)
  (if ask-user-popup--text-start
      (let* ((text-content (buffer-substring-no-properties 
                           ask-user-popup--text-start 
                           ask-user-popup--text-end))
             (trimmed (string-trim text-content)))
        (if (and (string-empty-p trimmed) ask-user-popup--options)
            ;; Empty text and options exist - confirm current option instead
            (ask-user-popup--confirm-selection)
          ;; Non-empty text or no options - return text
          (setq ask-user-popup--result trimmed)
          (exit-recursive-edit)))
    ;; No text region - shouldn't happen, but fallback to cancel
    (ask-user-popup-cancel)))

(defun ask-user-popup--insert-newline ()
  "Insert newline in text field."
  (interactive)
  (when (and ask-user-popup--text-start
             (>= (point) ask-user-popup--text-start)
             (<= (point) ask-user-popup--text-end))
    (let ((inhibit-read-only t))
      (insert "\n"))))

(defun ask-user-popup--handle-return ()
  "Handle RET key based on context.
In text field: submit text.
In options: confirm selection."
  (interactive)
  (if (eq ask-user-popup--focus 'text)
      (ask-user-popup--submit-text)
    (ask-user-popup--confirm-selection)))

;;; Focus management functions

(defun ask-user-popup--focus-options ()
  "Move focus to options list."
  (interactive)
  (when ask-user-popup--options
    (setq ask-user-popup--focus 'options)
    ;; Move point to first option
    (let ((first-option-pos (text-property-any (point-min) (point-max) 'option-index 0)))
      (when first-option-pos
        (goto-char first-option-pos)))
    ;; Restore selection highlight
    (when ask-user-popup--selection-overlay
      (overlay-put ask-user-popup--selection-overlay 'face '(:inverse-video t)))
    (ask-user-popup--move-selection-overlay)))

(defun ask-user-popup--focus-text ()
  "Move focus to text input field."
  (interactive)
  (when ask-user-popup--text-start
    (setq ask-user-popup--focus 'text)
    ;; Move point into text field
    (goto-char ask-user-popup--text-start)
    ;; Dim selection highlight if in options mode
    (when (and ask-user-popup--selection-overlay ask-user-popup--options)
      (overlay-put ask-user-popup--selection-overlay 'face '(:inverse-video t :foreground "gray50")))))

(defun ask-user-popup--toggle-focus ()
  "Toggle focus between options and text field."
  (interactive)
  (if (eq ask-user-popup--focus 'text)
      (ask-user-popup--focus-options)
    (ask-user-popup--focus-text)))

;;; Navigation functions

(defun ask-user-popup--move-selection-overlay ()
  "Move selection overlay to the currently selected option line."
  (when (and ask-user-popup--selection-overlay ask-user-popup--options)
    (let ((target-pos (text-property-any (point-min) (point-max) 
                                         'option-index 
                                         ask-user-popup--selected-index)))
      (when target-pos
        (save-excursion
          (goto-char target-pos)
          (let ((line-start (line-beginning-position))
                (line-end (1+ (line-end-position))))
            (move-overlay ask-user-popup--selection-overlay line-start line-end)))))))

(defun ask-user-popup--select-next ()
  "Move selection to next option (with wrap-around), or enter text field from last option."
  (interactive)
  (if (eq ask-user-popup--focus 'text)
      ;; In text field, do nothing (or could wrap to first option)
      nil
    ;; In options
    (when ask-user-popup--options
      (let ((last-index (1- (length ask-user-popup--options))))
        (if (= ask-user-popup--selected-index last-index)
            ;; On last option, move to text field
            (ask-user-popup--focus-text)
          ;; Not on last option, move to next
          (setq ask-user-popup--selected-index 
                (mod (1+ ask-user-popup--selected-index) 
                     (length ask-user-popup--options)))
          (ask-user-popup--move-selection-overlay))))))

(defun ask-user-popup--select-prev ()
  "Move selection to previous option (with wrap-around), or jump to last option from text field."
  (interactive)
  (if (eq ask-user-popup--focus 'text)
      ;; In text field, jump to last option
      (when ask-user-popup--options
        (setq ask-user-popup--selected-index (1- (length ask-user-popup--options)))
        (ask-user-popup--focus-options))
    ;; In options
    (when ask-user-popup--options
      (setq ask-user-popup--selected-index 
            (mod (1- ask-user-popup--selected-index) 
                 (length ask-user-popup--options)))
      (ask-user-popup--move-selection-overlay))))

(defun ask-user-popup--confirm-selection ()
  "Confirm the current selection and exit."
  (interactive)
  (when ask-user-popup--options
    (let ((selected-option (nth ask-user-popup--selected-index ask-user-popup--options)))
      (setq ask-user-popup--result selected-option)
      (exit-recursive-edit))))

(defun ask-user-popup--select-option-at-point ()
  "Select and confirm the option at point (for mouse support)."
  (interactive)
  (when ask-user-popup--options
    (let ((idx (get-text-property (point) 'option-index)))
      (when (and idx (>= idx 0) (< idx (length ask-user-popup--options)))
        (setq ask-user-popup--selected-index idx)
        (ask-user-popup--confirm-selection)))))

;;; Major mode

(defvar ask-user-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") 'ask-user-popup-cancel)
    (define-key map (kbd "C-g") 'ask-user-popup-cancel)
    ;; Navigation keys (work when options exist)
    (define-key map (kbd "C-n") 'ask-user-popup--select-next)
    (define-key map (kbd "C-p") 'ask-user-popup--select-prev)
    (define-key map (kbd "<down>") 'ask-user-popup--select-next)
    (define-key map (kbd "<up>") 'ask-user-popup--select-prev)
    (define-key map (kbd "j") 'ask-user-popup--select-next)
    (define-key map (kbd "k") 'ask-user-popup--select-prev)
    (define-key map (kbd "RET") 'ask-user-popup--handle-return)
    (define-key map (kbd "<mouse-1>") 'ask-user-popup--select-option-at-point)
    ;; Tab for focus switching
    (define-key map (kbd "TAB") 'ask-user-popup--toggle-focus)
    ;; Text editing keys (work when focus is text)
    (define-key map (kbd "C-c C-c") 'ask-user-popup--submit-text)
    (define-key map (kbd "C-j") 'ask-user-popup--insert-newline)
    (define-key map (kbd "S-RET") 'ask-user-popup--insert-newline)
    (define-key map (kbd "S-<return>") 'ask-user-popup--insert-newline)
    map)
  "Keymap for `ask-user-popup-mode'.")

(define-derived-mode ask-user-popup-mode special-mode "AskUser"
  "Major mode for Claude's AskUserQuestion popup buffer.

\\{ask-user-popup-mode-map}"
  (setq-local mode-line-format nil)
  (setq-local cursor-type 'bar))

;;; Cancel function

(defun ask-user-popup-cancel ()
  "Cancel the popup and return error to caller."
  (interactive)
  (setq ask-user-popup--cancelled t)
  (setq ask-user-popup--result nil)
  (message "Cancelled")
  (exit-recursive-edit))

;;; Main popup function

(defun mr-x/ask-user-popup (question &optional description options)
  "Display QUESTION in a popup buffer at bottom of frame.
DESCRIPTION is optional context displayed in muted text.
OPTIONS is an optional list of strings for selection mode.

When OPTIONS is provided, displays a numbered list and enables
selection mode with C-n/C-p navigation. Text field is also available.

When OPTIONS is nil, only displays text field for free-form input.

This function blocks until the user responds or cancels.
Returns the user's response string.
Signals an error if cancelled.

Emacsclient integration:
When called via emacsclient, this function blocks the calling process
until the user responds. Cancel operations (C-g/q) signal an error,
which causes emacsclient to exit with non-zero status."
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
              (insert "\n")
              
              ;; Insert description if provided (muted)
              (when description
                (insert (propertize description 'face 'shadow))
                (insert "\n\n"))
              
              ;; Content area separator
              (insert (propertize "────────────────────────────────────────" 'face 'shadow))
              (insert "\n\n")
              
              ;; Render options if provided
              (when options
                ;; Store options in buffer-local var
                (setq ask-user-popup--options options)
                (setq ask-user-popup--selected-index 0)
                (setq ask-user-popup--focus 'options)
                
                ;; Render numbered option list
                (let ((idx 0)
                      (option-start nil))
                  (dolist (option options)
                    (setq option-start (point))
                    (insert (format "%d. %s\n" (1+ idx) option))
                    ;; Add text property to mark this as an option line
                    (put-text-property option-start (point) 'option-index idx)
                    (setq idx (1+ idx))))
                
                ;; Add blank line after options
                (insert "\n")
                
                ;; Create selection overlay for first option
                (setq ask-user-popup--selection-overlay (make-overlay 1 1))
                (overlay-put ask-user-popup--selection-overlay 'face '(:inverse-video t))
                
                ;; Move overlay to first option
                (goto-char (point-min))
                (let ((first-option-pos (text-property-any (point-min) (point-max) 'option-index 0)))
                  (when first-option-pos
                    (goto-char first-option-pos)
                    (let ((line-start (line-beginning-position))
                          (line-end (1+ (line-end-position))))
                      (move-overlay ask-user-popup--selection-overlay line-start line-end)))))
              
              ;; Add text input field (always present)
              (let ((text-label-pos (point)))
                ;; Visual separator if options exist
                (when options
                  (insert (propertize "────────────────────────────────────────" 'face 'shadow))
                  (insert "\n"))
                
                ;; Label for text field
                (insert (propertize (if options 
                                       "Or type a response:" 
                                     "Type your response:")
                                   'face 'shadow))
                (insert "\n")
                
                ;; Create editable text region
                (setq ask-user-popup--text-start (point-marker))
                (set-marker-insertion-type ask-user-popup--text-start nil)
                
                ;; Insert initial empty line for text input
                (insert "\n")
                
                (setq ask-user-popup--text-end (point-marker))
                (set-marker-insertion-type ask-user-popup--text-end t)
                
                ;; Make text region editable by removing read-only property
                ;; We'll set buffer-read-only globally, but this region stays editable
                (put-text-property ask-user-popup--text-start 
                                 ask-user-popup--text-end 
                                 'read-only nil)
                (put-text-property ask-user-popup--text-start 
                                 ask-user-popup--text-end 
                                 'rear-nonsticky t))
              
              ;; Set initial focus
              (if options
                  (setq ask-user-popup--focus 'options)
                (setq ask-user-popup--focus 'text)
                (goto-char ask-user-popup--text-start))
              
              ;; Make buffer read-only (except text region which has read-only nil)
              (setq buffer-read-only t)
              
              ;; Enable self-insert in text region
              (add-hook 'post-command-hook 'ask-user-popup--maintain-text-region nil t)
              
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

(defun ask-user-popup--maintain-text-region ()
  "Maintain editable text region after commands."
  (when (and ask-user-popup--text-start ask-user-popup--text-end)
    ;; Ensure text region properties remain
    (let ((inhibit-read-only t))
      (put-text-property ask-user-popup--text-start 
                        ask-user-popup--text-end 
                        'read-only nil))))

(provide 'ask-user-popup)

;;; ask-user-popup.el ends here
