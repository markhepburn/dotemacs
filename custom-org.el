
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mh/org-mode-lisp-directory (concat *mh/lisp-base* "org-mode/lisp"))
(setq org-agenda-files (quote ("~/todo-apa.org")))
(add-to-list 'load-path mh/org-mode-lisp-directory)
(autoload 'org-mode "org" "Org-mode; outline on steroids" t)
(eval-after-load "org"
  '(progn
     (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
     (setq org-log-done t)
     (add-to-list 'org-agenda-files (expand-file-name "~/todo-apa.org"))
     (define-key org-mode-map  "\C-ca" 'org-agenda)
     ;; from http://orgmode.org/worg/org-hacks.php (Bernt Hansen, with my
     ;; tidy-ups)
     (defun org-reload-org (&optional source)
       "Reload compiled Org lisp files.  When invoked with non-nil argument,
loads from source files instead."
       (interactive "P")
       (let ((files-regexp
              (if source "\\.el\\'" "\\.elc\\'")))
         (mapc (lambda(f) (load f))
               (directory-files mh/org-mode-lisp-directory t files-regexp))))))
;;; Bit of a hack to work around htmlize-buffer (as called by
;;; org-write-agenda for eg) not working.  See
;;; http://www.mail-archive.com/emacs-orgmode@gnu.org/msg04365.html
;;; and
;;; http://emacsbugs.donarmstrong.com/cgi-bin/bugreport.cgi?bug=648
(eval-after-load "htmlize"
  '(progn
     (defadvice htmlize-faces-in-buffer (after org-no-nil-faces activate)
       "Make sure there are no nil faces"
       (setq ad-return-value (delq nil ad-return-value)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
