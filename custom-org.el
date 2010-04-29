
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mh/org-mode-lisp-directory (concat *mh/lisp-base* "org-mode/lisp"))
(add-to-list 'load-path mh/org-mode-lisp-directory)
(autoload 'org-mode "org" "Org-mode; outline on steroids" t)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(eval-after-load "org"
  '(progn
     (setq org-log-done t)

	 ;; Some great tips from http://orgmode.org/worg/org-customization-guide.php
	 (setq org-special-ctrl-a/e t)
	 (setq org-special-ctrl-k t)		; behaviour of this is a bit subtle
	 (setq org-completion-use-ido t)

     ;; restore default value of the tags alignment column:
     (setq org-tags-column -80)

     ;; speed navigation commands:
     (setq org-use-speed-commands t)

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

;;; function to insert my workout template in workouts.org:
(defvar mh/workout-type-history-list nil)
(defun mh/org-new-workout (type)
  (interactive
   (list
    (read-string "Workout type: " nil 'mh/workout-type-history-list)))
  (if (and type
          (not (string= type "")))
      (let ((template
             "* %s %s
** Workout:
** Time taken = 
** Heart-rate avg/max = 
** Notes:"))
        (end-of-buffer)
        (unless (bolp) (newline))
        (insert (format template
                        (format-time-string "<%Y-%m-%d %a>")
                        type))
        ;; use org to do the tags, rather than trying to read them myself:
        (outline-up-heading 1)
        (org-set-tags)
        (end-of-line 2))
    (message "Aborted (no type specified)")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
