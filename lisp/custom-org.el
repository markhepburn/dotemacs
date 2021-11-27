;;; custom-org.el --- Org-mode Customisation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;

;;; Code:

(use-package org
  ;; This prefers the manually-installed version over the existing
  ;; one, removing the need to reload for example:
  ;; https://www.reddit.com/r/emacs/comments/dv02lk/how_to_force_emacs_to_prefer_manually_installed/f7huwfy/
  :pin gnu
  :config
  (progn
    (setq org-directory (expand-file-name (file-name-as-directory "~/Nextcloud/org"))

          org-log-done t

          ;; Some great tips from http://orgmode.org/worg/org-customization-guide.php
          org-special-ctrl-a/e t
          org-special-ctrl-k t		; behaviour of this is a bit subtle
          ;; (setq org-completion-use-ido t)

          ;; I'm using org for time-tracking now; just display hours, not days:
          ;; (see http://comments.gmane.org/gmane.emacs.orgmode/77120)
          org-time-clocksum-format "%d:%02d"

          ;; Be consistent with spacing between headings, even if already
          ;; on a new line:
          org-insert-heading-respect-content t

          ;; restore default value of the tags alignment column:
          org-tags-column -80

          ;; This used to be the default I think:
          org-startup-folded t

          ;; speed navigation commands:
          org-use-speed-commands t
          org-speed-commands-user '(("S" . save-buffer))

          ;; automatically use symbols for \alpha, etc (toggle with C-c C-x \
          ;; if necessary):
          org-pretty-entities t)

    ;; Don't use agenda-cycle at the moment, so rebind C-, to my
    ;; scrolling commands:
    (add-hook 'org-mode-hook
              (lambda ()
                (local-set-key (kbd "C-,") 'scroll-up-line)))

    (add-hook 'org-mode-hook
              (lambda () (electric-indent-local-mode -1)))

    ;; Bit of a hack to work around htmlize-buffer (as called by
    ;; org-write-agenda for eg) not working.  See
    ;; http://www.mail-archive.com/emacs-orgmode@gnu.org/msg04365.html
    ;; and
    ;; http://emacsbugs.donarmstrong.com/cgi-bin/bugreport.cgi?bug=648
    (after "htmlize"
      (defadvice htmlize-faces-in-buffer (after org-no-nil-faces activate)
        "Make sure there are no nil faces"
        (setq ad-return-value (delq nil ad-return-value))))))

(use-package org-roam
  :after org
  :diminish org-roam-mode
  :hook (after-init . org-roam-setup)
  :custom (org-roam-directory (file-truename "~/Nextcloud/orgroam/"))
  :init (setq org-roam-v2-ack t)
  :bind (("C-c o f" . org-roam-node-find)
         ("C-c o l" . org-roam-buffer-toggle)
         ("C-c o i" . org-roam-node-insert)))

(use-package org-download)

(use-package htmlize)          ; belongs here as much as anywhere
(use-package ox-reveal)        ; For exporting reveal.js presentations

(use-package easy-hugo
  :custom
  (easy-hugo-basedir "~/Projects/blog.markhepburn.com/")
  (easy-hugo-postdir "content/posts"))

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
        (call-interactively 'org-set-tags-command)
        (end-of-line 2))
    (message "Aborted (no type specified)")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-org)

;;; custom-org.el ends here
