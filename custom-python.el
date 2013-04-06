
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set up for ipython:
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(eval-after-load "python"
  '(progn
     (add-hook 'python-mode-hook
               (lambda () (imenu-add-to-menubar "Declarations")))
     (add-hook 'python-mode-hook
               (lambda () (semantic-decoration-mode -1)))
     ;; restore python's backspace binding after it is clobbered by my autopairs stuff:
     (add-hook 'python-mode-hook
               (lambda () (local-set-key (kbd "<backspace>") 'python-indent-dedent-line-backspace)))

     ;; Advice ':' so typing ':)' results in in '):', eg for function
     ;; definitions.  Useful in conjunction with autopair
     ;; functionality:
     (defadvice python-indent-electric-colon (around python-electric-colon-autoplace (arg)
                                                     activate)
       (if (and (looking-at ")")
                (not arg))
         (progn
           (move-end-of-line nil)
           ad-do-it
           (newline-and-indent))
         ad-do-it))

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

;;; ipython-notebook integration:
(eval-after-load "ein"
  '(progn
     (setq ein:use-auto-complete t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
