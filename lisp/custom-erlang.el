;;; custom-erlang.el --- Customations for Erlang

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;; 

;;; Code:

(use-package edts-start
  :disabled t
  :ensure edts
  :mode "\\.[eh]rl\\'"
  :config
  (defvar inferior-erlang-prompt-timeout t)
  (add-hook 'erlang-mode-hook
            (lambda ()
              ;; when starting an erlang shell in emacs, default is the node name
              (setq inferior-erlang-machine-options '("-sname" "emacs"))
              ;; add Erlang functions to imenu
              (imenu-add-to-menubar "imenu")))
  ;; use flymake as well of course:
  (use-package flymake
    :config
    (defun flymake-erlang-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "eflymake" (list local-file))))

    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.erl\\'" flymake-erlang-init))
    (add-hook 'find-file-hook 'flymake-find-file-hook)))

;;; Elixir section

(use-package elixir-mode
  :init (add-hook 'elixir-mode-hook #'eglot-ensure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-erlang)

;;; custom-erlang.el ends here
