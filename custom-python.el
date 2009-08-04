
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "python"
  '(progn
     (add-hook 'python-mode-hook
               (lambda () (imenu-add-to-menubar "Declarations")))
     (add-hook 'python-mode-hook
               (lambda () (semantic-decoration-mode -1)))
     ;; restore python's backspace binding after it is clobbered by my autopairs stuff:
     (add-hook 'python-mode-hook
               (lambda () (local-set-key (kbd "<backspace>") 'python-backspace)))
     (when (load "flymake" t)
       (defun flymake-pylint-init ()
         (let* ((temp-file (flymake-init-create-temp-buffer-copy
                            'flymake-create-temp-inplace))
                (local-file (file-relative-name
                             temp-file
                             (file-name-directory buffer-file-name))))
           (list "epylint" (list local-file))))
    
       (add-to-list 'flymake-allowed-file-name-masks
                    '("\\.py\\'" flymake-pylint-init))
       ;; (add-hook 'python-mode-hook 'flymake-mode)
       )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
