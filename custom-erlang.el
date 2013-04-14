
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.[eh]rl\\'" . erlang-mode))
(autoload 'erlang-mode "erlang-start" "Major mode for editing erlang." t)
(eval-after-load "erlang-start"
  '(progn
     (require 'erlang-start)
     (defvar inferior-erlang-prompt-timeout t)

     ;; use distel as well:
     (require 'distel)
     (distel-setup)

     (add-hook 'erlang-mode-hook
               (lambda ()
                 ;; when starting an erlang shell in emacs, default is the node name
                 (setq inferior-erlang-machine-options '("-sname" "emacs"))
                 ;; add Erlang functions to imenu
                 (imenu-add-to-menubar "imenu")))
     ;; use flymake as well of course:
     (when (load "flymake" t)
       (defun flymake-erlang-init ()
         (let* ((temp-file (flymake-init-create-temp-buffer-copy
                            'flymake-create-temp-inplace))
                (local-file (file-relative-name
                             temp-file
                             (file-name-directory buffer-file-name))))
           (list "eflymake" (list local-file))))

       (add-to-list 'flymake-allowed-file-name-masks
                    '("\\.erl\\'" flymake-erlang-init)))

     (add-hook 'find-file-hook 'flymake-find-file-hook)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
