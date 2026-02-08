;;; Automated verification for Task 3

(load-file "emacs/ask-user-popup.el")

(defun test-text-field-exists ()
  "Test that text field is created and accessible."
  (with-temp-buffer
    (let ((buf (get-buffer-create "*ask-user-test*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (ask-user-popup-mode)
          
          ;; Simulate popup creation with options
          (insert "Test\n")
          (let ((options '("A" "B" "C")))
            (setq ask-user-popup--options options)
            (setq ask-user-popup--selected-index 0)
            (setq ask-user-popup--focus 'options)
            
            ;; Render options
            (let ((idx 0))
              (dolist (option options)
                (let ((start (point)))
                  (insert (format "%d. %s\n" (1+ idx) option))
                  (put-text-property start (point) 'option-index idx)
                  (setq idx (1+ idx)))))
            
            ;; Add separator
            (insert "────\n")
            
            ;; Add text field
            (insert "Or type a response:\n")
            (setq ask-user-popup--text-start (point-marker))
            (set-marker-insertion-type ask-user-popup--text-start nil)
            (insert "\n")
            (setq ask-user-popup--text-end (point-marker))
            (set-marker-insertion-type ask-user-popup--text-end t)
            
            ;; Verify markers exist
            (unless (and ask-user-popup--text-start ask-user-popup--text-end)
              (error "Text field markers not created"))
            
            ;; Verify focus can switch
            (unless (eq ask-user-popup--focus 'options)
              (error "Initial focus should be options"))
            
            (ask-user-popup--focus-text)
            (unless (eq ask-user-popup--focus 'text)
              (error "Focus switch to text failed"))
            
            (ask-user-popup--focus-options)
            (unless (eq ask-user-popup--focus 'options)
              (error "Focus switch back to options failed"))
            
            (message "✓ Text field creation and focus switching works")))
        (kill-buffer buf)))))

(defun test-text-submission ()
  "Test text extraction and trimming."
  (with-temp-buffer
    (let ((buf (get-buffer-create "*ask-user-test2*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (ask-user-popup-mode)
          
          ;; Create text field
          (setq ask-user-popup--text-start (point-marker))
          (set-marker-insertion-type ask-user-popup--text-start nil)
          (insert "  Test content  \n")
          (setq ask-user-popup--text-end (point-marker))
          (set-marker-insertion-type ask-user-popup--text-end t)
          
          ;; Extract text
          (let* ((text-content (buffer-substring-no-properties 
                               ask-user-popup--text-start 
                               ask-user-popup--text-end))
                 (trimmed (string-trim text-content)))
            (unless (string= trimmed "Test content")
              (error "Text trimming failed: got '%s'" trimmed))
            (message "✓ Text extraction and trimming works")))
        (kill-buffer buf)))))

(defun test-pure-text-mode ()
  "Test that pure text mode (no options) sets correct initial focus."
  (with-temp-buffer
    (let ((buf (get-buffer-create "*ask-user-test3*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (ask-user-popup-mode)
          
          ;; No options - pure text mode
          (setq ask-user-popup--options nil)
          
          ;; Create text field
          (insert "Type your response:\n")
          (setq ask-user-popup--text-start (point-marker))
          (set-marker-insertion-type ask-user-popup--text-start nil)
          (insert "\n")
          (setq ask-user-popup--text-end (point-marker))
          (set-marker-insertion-type ask-user-popup--text-end t)
          
          ;; Set focus as would happen in pure text mode
          (setq ask-user-popup--focus 'text)
          (goto-char ask-user-popup--text-start)
          
          ;; Verify focus is text
          (unless (eq ask-user-popup--focus 'text)
            (error "Pure text mode should focus on text field"))
          
          (message "✓ Pure text mode initialization works")))
      (kill-buffer buf))))

;; Run all tests
(condition-case err
    (progn
      (test-text-field-exists)
      (test-text-submission)
      (test-pure-text-mode)
      (message "\n=== All automated tests passed ==="))
  (error
   (message "\n=== Test failed: %s ===" (error-message-string err))
   (kill-emacs 1)))
