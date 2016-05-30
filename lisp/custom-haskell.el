;;; custom-haskell.el --- Haskell development customisation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set up some path stuff first:

;;; Commentary:
;;

;;; Code:

(let ((cabal-path  (expand-file-name "~/.cabal/bin")))
 (add-to-list 'exec-path cabal-path)
 (setenv "PATH" (concat cabal-path path-separator (getenv "PATH"))))

;;; Most of this is taken/tweaked from https://github.com/serras/emacs-haskell-tutorial/:
(use-package haskell-mode
  :bind (:map haskell-mode-map
         ("<f8>"        . haskell-navigate-imports)
         ("C-c C-l"     . haskell-process-load-or-reload)
         ("C-c C-z"     . haskell-interactive-switch)
         ("C-c C-n C-t" . haskell-process-do-type)
         ("C-c C-n C-i" . haskell-process-do-info)
         ("C-c C-n C-c" . haskell-process-cabal-build)
         ("C-c C-n c"   . haskell-process-cabal)
         ("SPC"         . haskell-mode-contextual-space)
         ("C-c C-o"     . haskell-compile))
  :init
  (custom-set-variables
   ;; Use cabal-dev for the GHCi session. Ensures our dependencies are in scope.
   '(haskell-process-type 'cabal-repl)
   ;; or 'ghci

   ;; To enable tags generation on save.
   '(haskell-tags-on-save t)

   '(company-ghc-show-info t)

   ;; To enable stylish on save.
   ;; '(haskell-stylish-on-save t)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t))
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'haskell-indent-mode))

(use-package haskell-cabal
  :ensure nil
  :after (haskell)
  :bind (:map haskell-cabal-mode-map
         ("C-c C-z" . haskell-interactive-switch)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c C-c" . haskell-process-cabal-build)
         ("C-c c"   . haskell-process-cabal)
         ("C-c C-o" . haskell-compile)))

(use-package company-ghc
  :after (company)
  :config (add-to-list 'company-backends 'company-ghc))

;;; interact with a running ghc process:
(use-package ghc
  :after (haskell-mode)
  :config (add-hook 'haskell-mode-hook 'ghc-init))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-haskell)

;;; custom-haskell.el ends here
