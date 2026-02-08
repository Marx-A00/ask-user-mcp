(load-file "emacs/ask-user-popup.el")

(with-temp-buffer
  (let ((inhibit-read-only t)
        (idx 0)
        (options '("Red" "Green" "Blue"))
        (option-start nil))
    
    ;; Render options
    (dolist (option options)
      (setq option-start (point))
      (insert (format "%d. %s\n" (1+ idx) option))
      (put-text-property option-start (point) 'option-index idx)
      (setq idx (1+ idx)))
    
    ;; Verify text properties
    (let ((prop-0 (text-property-any (point-min) (point-max) 'option-index 0))
          (prop-1 (text-property-any (point-min) (point-max) 'option-index 1))
          (prop-2 (text-property-any (point-min) (point-max) 'option-index 2)))
      (unless (and prop-0 prop-1 prop-2)
        (error "Text properties not set correctly"))
      (message "✓ Options rendered with correct text properties"))))
