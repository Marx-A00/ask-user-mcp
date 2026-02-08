;; Automated test for selection mode
;; This tests the mechanics without interactive input

(load-file "emacs/ask-user-popup.el")

;; Test 1: Verify options are stored correctly
(let* ((buf (get-buffer-create "*ask-user-test*"))
       (result nil))
  (with-current-buffer buf
    (erase-buffer)
    (ask-user-popup-mode)
    (setq ask-user-popup--options '("Red" "Green" "Blue"))
    (setq ask-user-popup--selected-index 0)
    
    ;; Test navigation forward
    (ask-user-popup--select-next)
    (unless (= ask-user-popup--selected-index 1)
      (error "select-next failed: expected 1, got %d" ask-user-popup--selected-index))
    
    ;; Test navigation forward again
    (ask-user-popup--select-next)
    (unless (= ask-user-popup--selected-index 2)
      (error "select-next failed: expected 2, got %d" ask-user-popup--selected-index))
    
    ;; Test wrap-around forward
    (ask-user-popup--select-next)
    (unless (= ask-user-popup--selected-index 0)
      (error "wrap-around forward failed: expected 0, got %d" ask-user-popup--selected-index))
    
    ;; Test navigation backward
    (ask-user-popup--select-prev)
    (unless (= ask-user-popup--selected-index 2)
      (error "wrap-around backward failed: expected 2, got %d" ask-user-popup--selected-index))
    
    (setq result "All navigation tests passed"))
  
  (kill-buffer buf)
  (message "%s" result))

"Tests complete"
