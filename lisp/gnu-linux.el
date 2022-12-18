;; -*- lexical-binding: t -*-
;;; gnu-linux.el --- Linux-specific Customisations

;;; See: https://bugs.launchpad.net/ubuntu/+source/emacs-snapshot/+bug/291399 for the need for this:

;;; Commentary:
;; 

;;; Code:

;; (set-frame-parameter nil 'font-backend '(xft))
;;; this is now actually set in ~/.Xresources (otherwise, it doesn't
;;; seem to get set correctly when the daemon starts up); relevant
;;; line:
;;; emacs.font:-microsoft-Consolas-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1
;;; or
;;; emacs.font:Ubuntu Mono:pixelsize=16:antialias=true
;;; May not need this now.
;(set-frame-font "Ubuntu Mono-12")
;(set-frame-font "Inconsolata-11")
;(set-frame-font "Bitstream Vera Sans Mono-10")
;(set-frame-font "Monospace-10")
;; (set-frame-font "JetBrains Mono-6")
(let ((frame-font "Cascadia Code PL-7"))
  (set-frame-font frame-font)
  (add-hook 'server-after-make-frame-hook
            (lambda () (set-frame-font frame-font))))

;;; font-lock for apt sources:
(add-to-list 'auto-mode-alist '("sources\\.list\\'" . conf-mode))

;;; Tree-sitter mode where supported:
(use-package tree-sitter-langs :defer 1)
(use-package tree-sitter
  :after tree-sitter-langs
  :commands (global-tree-sitter-mode)
  :config (global-tree-sitter-mode 1)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package exec-path-from-shell
  :init
  (setenv "PATH" "") ; otherwise zsh is run with the existing PATH, leading to lots of dupes, etc
  (exec-path-from-shell-initialize))

(defun uuidgen ()
  (interactive)
  (insert
   (replace-regexp-in-string
    "\n$" "" (shell-command-to-string "uuidgen"))))

;;; Make sure that the default browser is used by browse-url*:
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "x-www-browser")

(use-package ligature
  :defer 2
  :init (pkg-help/vc-install :name "ligature" :repo "mickeynp/ligature.el")
  :config
  ;; Enable the "www" ligature in every possible major mode
  ;(ligature-set-ligatures 't '("www"))
  ;; Enable all Jetbrains Mono ligatures in programming modes
  ;; (ligature-set-ligatures 'prog-mode '("-->" "//" "/**" "/*" "*/" "//" "<!--" ":="
  ;;                                      "->>" "<<-" "->" "<-" "<=>" "==" "!=" "<=" ">=" "=:=" "!=="
  ;;                                      "&&" "&&&" "||" "..." ".." "///" "===" "++" "--"
  ;;                                      "=>" "|>" "<|" "||>" "<||" "|||>" "<|||::="
  ;;                                      "|]" "[|" "|}" "{|" "[<" ">]" ":?>" ":?" "/=" "[||]"
  ;;                                      "!!" "?:" "?." "::" "+++" "??" "##" "###" "####" ":::"
  ;;                                      ".?" "?=" "=!=" "<|>" "<:" ":<" ":>" ">:" "<>" "***"
  ;;                                      ";;" "/==" ".=" ".-" "__" "=/="
  ;;                                      "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>"
  ;;                                      "=>>" ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "<<" "---"
  ;;                                      "<-|" "<=|" "\\" "\\/" "|=>" "|->" "<~~" "<~" "~~" "~~>" "~>"
  ;;                                      "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<->"
  ;;                                      "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-" "|=" "||="
  ;;                                      "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "||=" "||>" ;"***"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; nxml doesn't derive from prog-mode:
  (ligature-set-ligatures 'nxml-mode '("<!--" "-->" "</"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;; Docker support (see also lsp-mode: https://emacs-lsp.github.io/lsp-mode/page/lsp-dockerfile/ ):
(use-package docker :defer t)
(use-package dockerfile-mode
  :mode ("Dockerfile" . dockerfile-mode))

;;; (These cause issues on windows, so make linux-only for now):
(use-package docker-tramp  :defer t)
(use-package vagrant-tramp :defer t)
(setq tramp-default-method "ssh")

;;; only use org-agenda mode in Linux:
;; (eval-after-load "org"
;;   '(progn
;;      (add-to-list 'org-agenda-files (expand-file-name "~/todo-apa.org"))
;;      (define-key org-mode-map  "\C-ca" 'org-agenda)))

;;; Only available on linux:
(use-package direnv
  :defer 3
  :config (direnv-mode))

(add-to-list 'exec-path (expand-file-name "~/.local/bin"))

(add-to-list 'exec-path (expand-file-name "~/Projects/elixir-ls-git/release-1.12"))

;;; Flutter support for dart-lsp
(setq lsp-dart-flutter-sdk-dir (expand-file-name "~/Vendor/flutter/"))
(setq flutter-sdk-path lsp-dart-flutter-sdk-dir)

(provide 'gnu-linux)

;;; gnu-linux.el ends here
