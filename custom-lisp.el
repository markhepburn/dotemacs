;;; custom-lisp.el --- Lisp Development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;; 

;;; Code:

(autoload 'elisp-slime-nav-mode "elisp-slime-nav"
  "M-./M-, navigation for elisp" t)
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
(after "elisp-slime-nav" (diminish 'elisp-slime-nav-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slime:
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
(add-hook 'slime-repl-mode-hook (lambda ()
                                  (define-key slime-repl-mode-map
                                    (kbd "<up>") 'slime-repl-previous-input)
                                  (define-key slime-repl-mode-map
                                    (kbd "<down>") 'slime-repl-next-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; refactoring:
(autoload 'clj-refactor-mode "clj-refactor" "Refactoring support for Clojure" t)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-r")))

;;; clojurescript (build from emacs, and pop up stacktrack when
;;; there's a error):
(autoload 'cljsbuild-start "cljsbuild-mode" "Build cljs from emacs" t)
(after "cljsbuild-mode"
  (diminish 'cljsbuild-mode))

(add-hook 'clojure-mode-hook #'turn-on-eldoc-mode)

;;; ...and cider (formerly nrepl) integration:
(autoload 'cider "cider" "Connect to existing cider instance")
(autoload 'cider-jack-in "cider" "Launch a nrepl instance")
(setq cider-repl-use-pretty-printing t)
(add-hook 'cider-mode-hook (lambda ()
                             (define-key cider-repl-mode-map
                               (kbd "<up>") 'cider-repl-previous-input)
                             (define-key cider-repl-mode-map
                               (kbd "<down>") 'cider-repl-next-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See http://mumble.net/~campbell/emacs/paredit.html for a reference
;; table with examples.
(enable-minor-mode-for paredit-mode
                       '(cider
                         clojure
                         emacs-lisp
                         inferior-emacs-lisp
                         inferior-lisp
                         lisp
                         slime-repl))
(after 'paredit (diminish 'paredit-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-lisp)

;;; custom-lisp.el ends here
