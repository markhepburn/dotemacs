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
  :bind (:map elixir-mode-map
         ("C-c C-f" . elixir-format)))
(use-package elixir-ts-mode
  :ensure nil
  :bind (:map elixir-ts-mode-map
              ("C-c C-f" . elixir-format)))
(use-package mix
  :hook ((elixir-mode elixir-ts-mode-hook) . mix-minor-mode))

(use-package apprentice
  :vc (:url "https://github.com/Sasanidas/Apprentice")
  :hook ((elixir-mode elixir-ts-mode) . apprentice-mode))

;;; https://blog.evalcode.com/phoenix-liveview-inline-syntax-highlighting-for-emacs/
(use-package polymode
  :mode ("\.ex$" . poly-elixir-web-mode)
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-liveview-expr-elixir-innermode
    :mode 'web-mode
    :head-matcher (rx line-start (* space) "~L" (= 3 (char "\"'")) line-end)
    :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-web-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-liveview-expr-elixir-innermode)))
(setq web-mode-engines-alist '(("elixir" . "\\.ex\\'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-erlang)

;;; custom-erlang.el ends here
