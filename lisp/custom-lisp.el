;;; custom-lisp.el --- Lisp Development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;

;;; Code:

(use-package elisp-slime-nav
  :hook emacs-lisp-mode
  :diminish elisp-slime-nav-mode)

(use-package macrostep
  :after elisp-mode
  :bind
  (:map emacs-lisp-mode-map
        ("C-c e" . macrostep-expand)
        ("C-c C-e" . macrostep-expand)))

;;; Cider-style overlays for elisp; see also
;;; http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
(use-package eros
  :hook emacs-lisp-mode)

;;; edebug overlays:
;;; https://xenodium.com/inline-previous-result-and-why-you-should-edebug/
(defun adviced:edebug-compute-previous-result (_ &rest r)
  "Adviced `edebug-compute-previous-result'."
  (let ((previous-value (nth 0 r)))
    (if edebug-unwrap-results
        (setq previous-value
              (edebug-unwrap* previous-value)))
    (setq edebug-previous-result
          (edebug-safe-prin1-to-string previous-value))))
(advice-add #'edebug-compute-previous-result
            :around
            #'adviced:edebug-compute-previous-result)

(defun adviced:edebug-previous-result (_ &rest r)
  "Adviced `edebug-previous-result'."
  (eros--make-result-overlay edebug-previous-result
    :where (point)
    :duration eros-eval-result-duration))
(advice-add #'edebug-previous-result
            :around
            #'adviced:edebug-previous-result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sly (a fork of SLIME, seems more popular and active now)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sly
  :init (setq inferior-lisp-program "sbcl")
  :commands sly
  :hook (lisp-mode . sly-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode
  :hook ((clojure-mode . turn-on-eldoc-mode)
         (clojure-mode . lsp)))

;;; refactoring:
(use-package clj-refactor
  :pin melpa-stable
  :after (clojure-mode)
  :diminish clj-refactor-mode
  :commands (clj-refactor-mode cljr-add-keybindings-with-prefix)
  :init (setq cljr-warn-on-eval nil)
  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (cljr-add-keybindings-with-prefix "C-c C-r"))))

;;; clojurescript (build from emacs, and pop up stacktrack when
;;; there's a error):
(use-package cljsbuild-mode
  :commands cljsbuild-start
  :diminish cljsbuild-mode)

(use-package cider-eval-sexp-fu :after (cider))

;;; ...and cider (formerly nrepl) integration:
(use-package cider
  :pin melpa-stable
  :init (setq cider-repl-use-pretty-printing t
              cider-prompt-for-symbol nil
              cider-repl-history-file "~/.cider-repl-history")
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
  :diminish paredit-mode
  :bind (:map paredit-mode-map ("RET" . nil)) ; https://docs.cider.mx/cider/additional_packages.html#paredit
  :hook ((cider-mode
          cider-repl-mode
          clojure-mode
          clojurescript-mode
          emacs-lisp-mode
          inferior-emacs-lisp-mode
          inferior-lisp-mode
          lisp-mode
          slime-repl-mode
          sly-mrepl-mode) . paredit-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-lisp)

;;; custom-lisp.el ends here
