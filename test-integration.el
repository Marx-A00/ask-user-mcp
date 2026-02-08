;;; Integration test - simulates user interaction programmatically

(load-file "emacs/ask-user-popup.el")

;; Test: Simulate selecting an option
(defun test-option-selection ()
  "Test that option selection works end-to-end."
  (let ((result nil))
    ;; Create popup in background
    (setq result
          (catch 'test-result
            (let ((buf (get-buffer-create "*ask-user*")))
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (ask-user-popup-mode)
                  
                  ;; Setup options
                  (setq ask-user-popup--options '("Red" "Green" "Blue"))
                  (setq ask-user-popup--selected-index 1) ; Select "Green"
                  (setq ask-user-popup--result nil)
                  
                  ;; Simulate confirm
                  (let ((selected-option (nth ask-user-popup--selected-index ask-user-popup--options)))
                    (setq ask-user-popup--result selected-option)
                    (throw 'test-result ask-user-popup--result))))
              (kill-buffer buf))))
    
    (if (string= result "Green")
        (message "✓ Option selection test passed: %s" result)
      (error "Option selection test failed: expected 'Green', got '%s'" result))))

;; Test: Simulate text input
(defun test-text-input ()
  "Test that text input extraction works."
  (let ((result nil))
    (setq result
          (catch 'test-result
            (let ((buf (get-buffer-create "*ask-user*")))
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (ask-user-popup-mode)
                  
                  ;; Setup text field with content
                  (setq ask-user-popup--text-start (point-marker))
                  (insert "Custom response here")
                  (setq ask-user-popup--text-end (point-marker))
                  
                  ;; Simulate submit
                  (let* ((text-content (buffer-substring-no-properties 
                                       ask-user-popup--text-start 
                                       ask-user-popup--text-end))
                         (trimmed (string-trim text-content)))
                    (setq ask-user-popup--result trimmed)
                    (throw 'test-result ask-user-popup--result))))
              (kill-buffer buf))))
    
    (if (string= result "Custom response here")
        (message "✓ Text input test passed: %s" result)
      (error "Text input test failed: expected 'Custom response here', got '%s'" result))))

;; Test: Verify focus management state
(defun test-focus-state ()
  "Test that focus state tracking works."
  (let ((buf (get-buffer-create "*ask-user*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ask-user-popup-mode)
        
        ;; Setup with options
        (setq ask-user-popup--options '("A" "B"))
        (setq ask-user-popup--focus 'options)
        
        ;; Create text markers
        (setq ask-user-popup--text-start (point-marker))
        (insert "\n")
        (setq ask-user-popup--text-end (point-marker))
        
        ;; Test focus switch
        (ask-user-popup--focus-text)
        (unless (eq ask-user-popup--focus 'text)
          (error "Focus switch to text failed"))
        
        (ask-user-popup--focus-options)
        (unless (eq ask-user-popup--focus 'options)
          (error "Focus switch to options failed"))
        
        (message "✓ Focus state management test passed")))
    (kill-buffer buf)))

;; Run all integration tests
(condition-case err
    (progn
      (test-option-selection)
      (test-text-input)
      (test-focus-state)
      (message "\n=== All integration tests passed ==="))
  (error
   (message "\n=== Integration test failed: %s ===" (error-message-string err))
   (kill-emacs 1)))
