
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
                                    [(up)] 'slime-repl-previous-input)
                                  (define-key slime-repl-mode-map
                                    [(down)] 'slime-repl-next-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(autoload 'clojure-jack-in "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See http://mumble.net/~campbell/emacs/paredit.html for a reference
;; table with examples.
(autoload 'paredit-mode "paredit-beta"
  "Structured editing of S-expressions" t)
(enable-minor-mode-for paredit-mode '(emacs-lisp lisp clojure inferior-lisp slime-repl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
