;;; custom-lisp.el --- Lisp Development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;

;;; Code:

(use-package elisp-slime-nav
  :hook (emacs-lisp-mode . (lambda () (elisp-slime-nav-mode t)))
  :diminish elisp-slime-nav-mode)

;;; From http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
;;; (changed prefix to my own mh/, but not claiming ownership!)
(after "cider"
  (autoload 'cider--make-result-overlay "cider-overlays")

  (defun mh/eval-overlay (value point)
    (cider--make-result-overlay (format "%S" value)
      :where point
      :duration 'command)
    ;; Preserve the return value.
    value)

  (advice-add 'eval-region :around
              (lambda (f beg end &rest r)
                (mh/eval-overlay
                 (apply f beg end r)
                 end)))

  (advice-add 'eval-last-sexp :filter-return
              (lambda (r)
                (mh/eval-overlay r (point))))

  (advice-add 'eval-defun :filter-return
              (lambda (r)
                (mh/eval-overlay
                 r
                 (save-excursion
                   (end-of-defun)
                   (point))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sly (a fork of SLIME, seems more popular and active now)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sly
  :config
  (setq inferior-lisp-program "sbcl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode
  :config
  :hook ((clojure-mode . turn-on-eldoc-mode)
         (clojure-mode . lsp)))

;;; refactoring:
(use-package clj-refactor
  :pin melpa-stable
  :after (clojure-mode)
  :diminish clj-refactor-mode
  :init
  (setq cljr-warn-on-eval nil)
  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (cljr-add-keybindings-with-prefix "C-c C-r"))))
;;; helm interface to refactoring:
(use-package cljr-helm
  :after (clojure-mode)
  :hook (clojure-mode . (lambda ()
                          (define-key clojure-mode-map (kbd "C-c r") 'cljr-helm))))

;;; clojurescript (build from emacs, and pop up stacktrack when
;;; there's a error):
(use-package cljsbuild-mode
  :diminish cljsbuild-mode)

(use-package cider-eval-sexp-fu)

;;; ...and cider (formerly nrepl) integration:
(use-package cider
  :pin melpa-stable
  :init (setq cider-repl-use-pretty-printing t
              cider-prompt-for-symbol nil)
  :bind (:map cider-repl-mode-map
              ;; Swap these around a bit; next/previous use search history (also on M-p/n)
              ("<up>"     . cider-repl-backward-input)
              ("<down>"   . cider-repl-forward-input)
              ("C-<up>"   . cider-repl-previous-input)
              ("C-<down>" . cider-repl-next-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See http://mumble.net/~campbell/emacs/paredit.html for a reference
;; table with examples.
(use-package paredit
  :demand t
  :diminish paredit-mode
  :hook ((cider-mode
          cider-repl-mode
          clojure-mode
          emacs-lisp-mode
          inferior-emacs-lisp-mode
          inferior-lisp-mode
          lisp-mode
          slime-repl-mode
          sly-mrepl-mode) . (lambda () (paredit-mode 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-lisp)

;;; custom-lisp.el ends here
