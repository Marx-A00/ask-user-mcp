;;; ask-user-popup.el --- Popup buffer UI for MCP AskUserQuestion -*- lexical-binding: t -*-

;;; Commentary:
;; Provides popup buffer UI for the ask-user-mcp server (v2).
;; This replaces the minibuffer-based prompts from v1 with a proper popup
;; buffer that appears at the bottom of the frame (popper-style).
;;
;; Keymap architecture (overlay local-map pattern):
;; - Buffer-wide mode map uses `suppress-keymap' to block all self-insert.
;;   j/k/q/RET are bound for option navigation in the read-only region.
;; - The text input field has an overlay with `local-map' set to a sparse
;;   keymap. Unbound keys (j, k, q, etc.) fall through to `global-map'
;;   where they self-insert normally. No conflicts.
;; - Evil normal state works in the options region (j/k navigate).
;;   Evil insert state works in the text field (all keys type).
;;
;; Core features:
;; - Bottom-positioned popup (~40% frame height)
;; - Blocking behavior using recursive-edit
;; - Buffer lifecycle management with proper cleanup
;; - Visual layout with header line and styled content
;; - Cancel support (C-g in any region, q in options region)
;; - Selection mode with j/k/C-n/C-p navigation
;; - Free-text input with overlay local-map (no keybinding conflicts)
;;
;; Usage:
;;   (mr-x/ask-user-popup "What is your name?" "Optional description")
;;   (mr-x/ask-user-popup "Choose color" "Pick one" '("Red" "Green" "Blue"))

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

(defvar-local ask-user-popup--field-overlay nil
  "Overlay for the text field with its own local-map.")

;;; Interactive commands

(defun ask-user-popup-cancel ()
  "Cancel the popup and return error to caller."
  (interactive)
  (setq ask-user-popup--cancelled t)
  (setq ask-user-popup--result nil)
  (message "Cancelled")
  (exit-recursive-edit))

(defun ask-user-popup--submit-text ()
  "Submit text content from editable region and exit."
  (interactive)
  (if ask-user-popup--text-start
      (let* ((text-content (buffer-substring-no-properties
                            ask-user-popup--text-start
                            ask-user-popup--text-end))
             (trimmed (string-trim text-content)))
        (if (and (string-empty-p trimmed) ask-user-popup--options)
            ;; Empty text and options exist — confirm current option instead
            (ask-user-popup--confirm-selection)
          (setq ask-user-popup--result trimmed)
          (exit-recursive-edit)))
    (ask-user-popup-cancel)))

(defun ask-user-popup--insert-newline ()
  "Insert newline in text field."
  (interactive)
  (when (and ask-user-popup--text-start
             (>= (point) (marker-position ask-user-popup--text-start))
             (<= (point) (marker-position ask-user-popup--text-end)))
    (insert "\n")))

(defun ask-user-popup--handle-return ()
  "Handle RET: in text field submit text, in options confirm selection."
  (interactive)
  (if (and ask-user-popup--text-start
           (>= (point) (marker-position ask-user-popup--text-start))
           (<= (point) (marker-position ask-user-popup--text-end)))
      (ask-user-popup--submit-text)
    (ask-user-popup--confirm-selection)))

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
          (move-overlay ask-user-popup--selection-overlay
                        (line-beginning-position)
                        (1+ (line-end-position))))))))

(defun ask-user-popup--select-next ()
  "Move selection to next option with wrap-around."
  (interactive)
  (when ask-user-popup--options
    (setq ask-user-popup--selected-index
          (mod (1+ ask-user-popup--selected-index)
               (length ask-user-popup--options)))
    (ask-user-popup--move-selection-overlay)
    (let ((pos (text-property-any (point-min) (point-max)
                                  'option-index ask-user-popup--selected-index)))
      (when pos (goto-char pos)))))

(defun ask-user-popup--select-prev ()
  "Move selection to previous option with wrap-around."
  (interactive)
  (when ask-user-popup--options
    (setq ask-user-popup--selected-index
          (mod (1- ask-user-popup--selected-index)
               (length ask-user-popup--options)))
    (ask-user-popup--move-selection-overlay)
    (let ((pos (text-property-any (point-min) (point-max)
                                  'option-index ask-user-popup--selected-index)))
      (when pos (goto-char pos)))))

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

(defun ask-user-popup--jump-to-field ()
  "Jump to the text input field and enter insert state."
  (interactive)
  (when ask-user-popup--text-start
    (goto-char ask-user-popup--text-start)
    ;; Dim selection highlight
    (when ask-user-popup--selection-overlay
      (overlay-put ask-user-popup--selection-overlay
                   'face '(:inverse-video t :foreground "gray50")))
    (when (fboundp 'evil-insert-state)
      (evil-insert-state))))

(defun ask-user-popup--jump-to-options ()
  "Jump back to the options list from the text field."
  (interactive)
  (when ask-user-popup--options
    ;; Restore selection highlight
    (when ask-user-popup--selection-overlay
      (overlay-put ask-user-popup--selection-overlay 'face '(:inverse-video t)))
    (ask-user-popup--move-selection-overlay)
    (let ((pos (text-property-any (point-min) (point-max)
                                  'option-index ask-user-popup--selected-index)))
      (when pos (goto-char pos)))
    (when (fboundp 'evil-normal-state)
      (evil-normal-state))))

;;; Keymaps

;; Buffer-wide keymap: suppress all self-insert, bind navigation keys.
;; This is active in the read-only options region.
(defvar ask-user-popup-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    ;; Cancel
    (define-key map (kbd "C-g") #'ask-user-popup-cancel)
    (define-key map (kbd "q") #'ask-user-popup-cancel)
    ;; Option navigation
    (define-key map (kbd "j") #'ask-user-popup--select-next)
    (define-key map (kbd "k") #'ask-user-popup--select-prev)
    (define-key map (kbd "C-n") #'ask-user-popup--select-next)
    (define-key map (kbd "C-p") #'ask-user-popup--select-prev)
    (define-key map (kbd "<down>") #'ask-user-popup--select-next)
    (define-key map (kbd "<up>") #'ask-user-popup--select-prev)
    ;; Confirm
    (define-key map (kbd "RET") #'ask-user-popup--handle-return)
    (define-key map (kbd "<return>") #'ask-user-popup--handle-return)
    ;; Mouse
    (define-key map (kbd "<mouse-1>") #'ask-user-popup--select-option-at-point)
    ;; Jump to text field
    (define-key map (kbd "TAB") #'ask-user-popup--jump-to-field)
    (define-key map (kbd "<tab>") #'ask-user-popup--jump-to-field)
    (define-key map (kbd "i") #'ask-user-popup--jump-to-field)
    map)
  "Keymap for `ask-user-popup-mode'.
Uses `suppress-keymap' so j/k/q work as navigation in the options region.
The text field overlay replaces this with a sparse keymap via `local-map'.")

;; Text field keymap: sparse, so unbound keys (j/k/q/letters) fall through
;; to global-map where they self-insert normally.
(defvar ask-user-popup-field-map
  (let ((map (make-sparse-keymap)))
    ;; Cancel still works in the field
    (define-key map (kbd "C-g") #'ask-user-popup-cancel)
    ;; Submit
    (define-key map (kbd "RET") #'ask-user-popup--submit-text)
    (define-key map (kbd "<return>") #'ask-user-popup--submit-text)
    (define-key map (kbd "C-c C-c") #'ask-user-popup--submit-text)
    ;; Newline in text
    (define-key map (kbd "C-j") #'ask-user-popup--insert-newline)
    (define-key map (kbd "S-RET") #'ask-user-popup--insert-newline)
    (define-key map (kbd "S-<return>") #'ask-user-popup--insert-newline)
    ;; Jump back to options
    (define-key map (kbd "TAB") #'ask-user-popup--jump-to-options)
    (define-key map (kbd "<tab>") #'ask-user-popup--jump-to-options)
    map)
  "Keymap for the text input field overlay.
Sparse keymap — unbound keys fall through to `global-map' and self-insert.")

;;; Evil integration

(with-eval-after-load 'evil
  ;; Start in normal state (options navigation)
  (evil-set-initial-state 'ask-user-popup-mode 'normal)

  ;; Normal state: option navigation (only matters in the suppressed region)
  (evil-define-key 'normal ask-user-popup-mode-map
    (kbd "j") #'ask-user-popup--select-next
    (kbd "k") #'ask-user-popup--select-prev
    (kbd "G") #'end-of-buffer
    (kbd "gg") #'beginning-of-buffer
    (kbd "RET") #'ask-user-popup--handle-return
    (kbd "<return>") #'ask-user-popup--handle-return
    (kbd "q") #'ask-user-popup-cancel
    (kbd "TAB") #'ask-user-popup--jump-to-field
    (kbd "<tab>") #'ask-user-popup--jump-to-field
    (kbd "i") #'ask-user-popup--jump-to-field
    (kbd "<escape>") #'ignore)

  ;; Insert state in the text field: C-c C-c to submit, Esc to go back
  (evil-define-key 'insert ask-user-popup-mode-map
    (kbd "C-c C-c") #'ask-user-popup--submit-text
    (kbd "C-j") #'ask-user-popup--insert-newline
    (kbd "<escape>") (lambda () (interactive)
                       (when (fboundp 'evil-normal-state)
                         (evil-normal-state))
                       (ask-user-popup--jump-to-options))))

;;; Major mode

(define-derived-mode ask-user-popup-mode fundamental-mode "AskUser"
  "Major mode for Claude's AskUserQuestion popup buffer.
Uses overlay `local-map' pattern: the buffer-wide keymap suppresses
self-insert for option navigation, while the text field overlay
replaces it with a sparse keymap that allows normal typing.

\\{ask-user-popup-mode-map}"
  (setq-local mode-line-format nil)
  (setq-local cursor-type 'bar)
  ;; Protect read-only regions
  (add-hook 'before-change-functions #'ask-user-popup--before-change nil t))

(defun ask-user-popup--before-change (beg end)
  "Prevent edits outside the text field region.
BEG and END are the change boundaries."
  (when (and ask-user-popup--text-start
             (not inhibit-read-only)
             (or (< beg (marker-position ask-user-popup--text-start))
                 (> end (marker-position ask-user-popup--text-end))))
    (signal 'text-read-only '("Cannot edit outside the text field"))))

;;; Markdown rendering

(defun ask-user-popup--render-markdown (start end)
  "Apply basic markdown styling to text between START and END.
Handles **bold**, *italic*, and `code` spans."
  (save-excursion
    ;; Bold: **text**
    (goto-char start)
    (while (re-search-forward "\\*\\*\\(.+?\\)\\*\\*" end t)
      (let ((content (match-string 1))
            (m-start (match-beginning 0)))
        (replace-match content t t)
        ;; Adjust end marker for removed syntax chars
        (setq end (- end 4))
        (put-text-property m-start (+ m-start (length content))
                           'face '(:weight bold))))
    ;; Italic: *text* (but not inside words like file*name)
    (goto-char start)
    (while (re-search-forward "\\(?:^\\|[[:space:](]\\)\\(\\*\\(.+?\\)\\*\\)\\(?:[[:space:].,;:!?)]\\|$\\)" end t)
      (let ((content (match-string 2))
            (m-start (match-beginning 1))
            (m-end (match-end 1)))
        (save-excursion
          (goto-char m-start)
          (delete-region m-start m-end)
          (insert content)
          (setq end (- end 2))
          (put-text-property m-start (+ m-start (length content))
                             'face '(:slant italic)))))
    ;; Inline code: `text`
    (goto-char start)
    (while (re-search-forward "`\\([^`\n]+?\\)`" end t)
      (let ((content (match-string 1))
            (m-start (match-beginning 0)))
        (replace-match content t t)
        (setq end (- end 2))
        (put-text-property m-start (+ m-start (length content))
                           'face '(:family "monospace" :background "#2a2a2a"))))))

;;; Main popup function

(defun mr-x/ask-user-popup (question &optional description options)
  "Display QUESTION in a popup buffer at bottom of frame.
DESCRIPTION is optional context displayed in muted text.
OPTIONS is an optional list of strings for selection mode.

When OPTIONS is provided, displays a numbered list and enables
selection mode with j/k navigation. A text field is also available
below (press TAB or i to jump to it).

When OPTIONS is nil, only displays a text field for free-form input.
The cursor starts in the field in evil insert state.

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
              (remove-overlays)
              (ask-user-popup-mode)

              ;; Header line
              (setq-local header-line-format
                          (propertize " Claude is asking..." 'face '(:weight bold)))

              ;; Question (bold, larger)
              (insert (propertize question 'face '(:weight bold :height 1.2)))
              (insert "\n")

              ;; Description (with markdown rendering)
              (when description
                (let ((desc-start (point)))
                  (insert description)
                  (ask-user-popup--render-markdown desc-start (point)))
                (insert "\n\n"))

              ;; Separator
              (insert (propertize "────────────────────────────────────────"
                                  'face 'shadow))
              (insert "\n\n")

              ;; Options
              (when options
                (setq ask-user-popup--options options)
                (setq ask-user-popup--selected-index 0)

                ;; Render numbered option list
                (let ((idx 0))
                  (dolist (option options)
                    (let ((start (point)))
                      (insert (format "%d. %s\n" (1+ idx) option))
                      (put-text-property start (point) 'option-index idx))
                    (setq idx (1+ idx))))

                (insert "\n")

                ;; Selection overlay
                (setq ask-user-popup--selection-overlay (make-overlay 1 1))
                (overlay-put ask-user-popup--selection-overlay 'face '(:inverse-video t))
                (save-excursion
                  (let ((pos (text-property-any (point-min) (point-max)
                                                'option-index 0)))
                    (when pos
                      (goto-char pos)
                      (move-overlay ask-user-popup--selection-overlay
                                    (line-beginning-position)
                                    (1+ (line-end-position)))))))

              ;; Text input field
              (goto-char (point-max))

              ;; Separator before text field (only when options exist)
              (when options
                (insert (propertize "────────────────────────────────────────"
                                    'face 'shadow))
                (insert "\n"))

              ;; Label
              (insert (propertize (if options "Or type a response:" "Type your response:")
                                  'face 'shadow))
              (insert "\n")

              ;; Editable text region
              (setq ask-user-popup--text-start (point-marker))
              (set-marker-insertion-type ask-user-popup--text-start nil)
              (insert "\n")
              (setq ask-user-popup--text-end (point-marker))
              (set-marker-insertion-type ask-user-popup--text-end t)

              ;; Create field overlay with local-map — this is the key mechanism.
              ;; The sparse field keymap REPLACES the suppressed mode map in this
              ;; region, so all unbound keys (letters) self-insert via global-map.
              (setq ask-user-popup--field-overlay
                    (make-overlay (marker-position ask-user-popup--text-start)
                                  (marker-position ask-user-popup--text-end)
                                  nil nil t))
              (overlay-put ask-user-popup--field-overlay 'local-map ask-user-popup-field-map)
              (overlay-put ask-user-popup--field-overlay 'face '(:extend t))

              ;; Reset state
              (setq ask-user-popup--result nil)
              (setq ask-user-popup--cancelled nil)))

          ;; Display at bottom
          (setq win (display-buffer buf
                                    '((display-buffer-at-bottom)
                                      (window-height . 0.4)
                                      (preserve-size . (nil . t)))))
          (select-window win)

          ;; Position cursor
          (if (buffer-local-value 'ask-user-popup--options buf)
              ;; Options mode: start on first option in normal state
              (progn
                (goto-char (point-min))
                (let ((pos (text-property-any (point-min) (point-max) 'option-index 0)))
                  (when pos (goto-char pos))))
            ;; Text-only mode: start in the field in insert state
            (goto-char (buffer-local-value 'ask-user-popup--text-start buf))
            (when (fboundp 'evil-insert-state)
              (evil-insert-state)))

          (set-window-start win (point-min))

          ;; Block until response
          (recursive-edit)

          ;; Check cancelled
          (when (buffer-local-value 'ask-user-popup--cancelled buf)
            (error "User cancelled the question"))

          ;; Return result
          (buffer-local-value 'ask-user-popup--result buf))

      ;; Cleanup
      (when (buffer-live-p buf)
        (when (get-buffer-window buf)
          (quit-window t (get-buffer-window buf)))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(provide 'ask-user-popup)

;;; ask-user-popup.el ends here
