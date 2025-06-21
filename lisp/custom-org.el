;;; custom-org.el --- Org-mode Customisation  -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;

;;; Code:

(use-package org
  :ensure nil
  :config
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

        ;; Reproducting another default:
        org-adapt-indentation t

        org-datetree-add-timestamp 'inactive
        org-capture-templates
        '(("w" "Weekend" checkitem (file+function "~/Nextcloud/Notes/Weekend.org" beginning-of-buffer))
          ;; ("c" "Condense" entry (file+regexp "~/Nextcloud/Notes/condense-weekly.org" "^\* <") "* %?")
          ("c" "Condense" entry (file+olp+datetree "~/Nextcloud/Notes/condense-timetracking.org") "* %U %^{Activity}%?")
          ;; Fixme: using a nil entry-template here + :prepend t removes indentation and adds [-] for some reason:
          ("b" "Blog Queue" checkitem (file+headline "~/Nextcloud/Notes/Blog-posts.org" "Queue")))

        ;; speed navigation commands:
        org-use-speed-commands t
        org-speed-commands (add-to-list 'org-speed-commands '("S" . save-buffer) 'append)

        org-use-sub-superscripts nil    ; I find they get in the way as a default.  Set ^:t in #+options to change
        ;; automatically use symbols for \alpha, etc (toggle with C-c C-x \
        ;; if necessary):
        org-pretty-entities t)
  ;; For verb blocks, you want "#+begin_src verb :wrap src ob-verb-response"
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((verb . t)))
  ;; Bit of a hack to work around htmlize-buffer (as called by
  ;; org-write-agenda for eg) not working.  See
  ;; http://www.mail-archive.com/emacs-orgmode@gnu.org/msg04365.html
  ;; and
  ;; http://emacsbugs.donarmstrong.com/cgi-bin/bugreport.cgi?bug=648
  (after "htmlize"
    (defadvice htmlize-faces-in-buffer (after org-no-nil-faces activate)
      "Make sure there are no nil faces"
      (setq ad-return-value (delq nil ad-return-value))))
  ;; Don't use agenda-cycle at the moment, so rebind C-, to my
  ;; scrolling commands:
  :bind (:map org-mode-map
              ("C-c C-j" . consult-org-heading) ; replaces org-goto which doesn't seem that useful
              ("C-," . scroll-up-line))
  :hook (org-mode . (lambda () (electric-indent-local-mode -1))))

(use-package org-roam
  :diminish org-roam-mode
  :custom
  (org-roam-directory (file-truename "~/Nextcloud/orgroam/"))
  (org-roam-node-display-template "${title:*} ${tags:40}")
  :config (org-roam-db-autosync-enable)
  :bind (("C-c o f" . org-roam-node-find)
         ("C-c o l" . org-roam-buffer-toggle)
         ("C-c o i" . org-roam-node-insert)))

(use-package htmlize   :defer t)    ; belongs here as much as anywhere
(use-package ox-reveal :defer t) ; For exporting reveal.js presentations

(use-package easy-hugo
  :commands (easy-hugo-newpost)
  :init (setq easy-hugo-server-flags "-D")
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
