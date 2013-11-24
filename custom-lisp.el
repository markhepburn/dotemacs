;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(autoload 'clojure-jack-in "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

;;; ...and cider (formerly nrepl) integration:
(autoload 'cider "cider" "Connect to existing cider instance")
(autoload 'cider-jack-in "cider" "Launch a nrepl instance")
(add-hook 'cider-mode-hook (lambda ()
                             (define-key cider-repl-mode-map
                               (kbd "<up>") 'cider-repl-previous-input)
                             (define-key cider-repl-mode-map
                               (kbd "<down>") 'cider-repl-next-input)))

;;; auto-complete support for nrepl:
(when (require 'ac-nrepl nil t)
  (add-hook 'cider-mode-hook 'ac-nrepl-setup)
  (add-hook 'cider-interaction-mode-hook 'ac-nrepl-setup)
  (after "auto-complete"
    (add-to-list 'ac-modes 'cider-mode)))
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
