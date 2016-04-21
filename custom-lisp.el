;;; custom-lisp.el --- Lisp Development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;

;;; Code:

(use-package elisp-slime-nav
  :init (add-hook 'emacs-lisp-mode-hook
                  (lambda () (elisp-slime-nav-mode t)))
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
;;; Slime (not installed for now):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note: slime is now included with and loaded by clojure-mode,
;;; typically invoked from clojure-jack-in (and this conflicts with
;;; other versions of slime, and since I don't anticipate much CL dev
;;; for a while, this should be OK).  So leaving this commented out
;;; for now, but may remove entirely later on.

;; (add-to-list 'load-path (concat *mh/lisp-base* "slime"))
;; (setq slime-lisp-implementations
;;       `((sbcl ("sbcl"))))
;; (require 'slime-autoloads)
;; (add-hook 'lisp-mode-hook (lambda ()
;;                             (cond ((not (featurep 'slime))
;;                                    (require 'slime)
;;                                    (normal-mode)))))
;; (add-hook 'slime-repl-mode-hook (lambda ()
;;                                   (define-key slime-repl-mode-map
;;                                     (kbd "<up>") 'slime-repl-previous-input)
;;                                   (define-key slime-repl-mode-map
;;                                     (kbd "<down>") 'slime-repl-next-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode
  :config (add-hook 'clojure-mode-hook #'turn-on-eldoc-mode))

;;; refactoring:
(use-package clj-refactor
  :pin melpa-stable
  :after (clojure-mode)
  :diminish clj-refactor-mode
  :init
  (setq cljr-warn-on-eval nil)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-r"))))
;;; helm interface to refactoring:
(use-package cljr-helm
  :after (clojure-mode)
  :init
  (add-hook 'clojure-mode-hook (lambda ()
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
  :diminish paredit-mode)
(enable-minor-mode-for paredit-mode
                       '(cider
                         cider-repl
                         clojure
                         emacs-lisp
                         inferior-emacs-lisp
                         inferior-lisp
                         lisp
                         slime-repl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-lisp)

;;; custom-lisp.el ends here
