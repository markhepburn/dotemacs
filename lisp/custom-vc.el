;;; custom-vc.el --- Version Control Interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VC stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;

;;; Code:

;;; diff setup:
(setq diff-default-read-only t
      diff-font-lock-prettify t)

;; use unified diffs by default in diff-mode:
(setq diff-switches "-u")
(setq vc-svn-diff-switches '("--diff-cmd" "diff" "-x" "-u"))

;;; show changed regions in the fringe
(use-package diff-hl
  :defer 2
  :config (global-diff-hl-mode 1)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Git integration:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :init (setq
         magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)

         ;; Always fullscreen:
         magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
         )

  :config
  (require 'magit-blame)
  (setq
   ;; http://iqbalansari.github.io/blog/2014/02/22/switching-repositories-with-magit/
   ;; http://irreal.org/blog/?p=4177
   magit-repository-directories
   (mapcar (lambda (dir)
             (cons (substring dir 0 -1) 0))
           (nreverse
            (cl-remove-if-not (lambda (project)
                             (file-directory-p (concat project "/.git/")))
                           (project-known-project-roots)))))
  :custom (magit-bury-buffer-function 'magit-restore-window-configuration)

  ;; Make this a global command, not just inside a repo:
  :bind (("C-x g" . magit-status)
         ("C-c M-g" . magit-file-dispatch)))

;;; git-flow integration:
(use-package magit-gitflow
  :pin melpa-stable
  :after (magit)
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package git-messenger
  :bind (("C-x v p" . git-messenger:popup-message))
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

(use-package git-timemachine :commands git-timemachine)

(use-package github-clone :defer t)

;;; Don't add commit-message buffers to recentf list:
(after 'recentf
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG"))
;;; Don't save position in commit-message buffers either:
(after 'session
  (setq session-name-disable-regexp (concat "COMMIT_EDITMSG"
                                            "\\|"
                                            session-name-disable-regexp)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smerge enhancements:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smerge-mode
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subversion interaction:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package psvn
  :init (setq
         svn-status-track-user-input t    ; Needs this to prompt for a password!
         ;; No idea why the default behaviour is as it is (or even how to
         ;; reproduce it in cmd-line svn for that matter), but this over-rides
         ;; it with something reasonably useful (possibly replace with '("-v")
         ;; to get a list of all files changed) :
         svn-status-default-log-arguments '()
         ;; using external cmd here because psvn diff doesn't work with
         ;; colordiff, which I'm using with command-line svn:
         svn-status-default-diff-arguments '("--diff-cmd" "diff" )
         ;; default to unknown and unmodified files not displayed:
         svn-status-hide-unknown    t
         svn-status-hide-unmodified t)
  :commands (svn-status)
  :config
  (defadvice svn-status-show-svn-diff (after mh/jump-to-diff-window activate)
    "Jump to the diff window, so it can be easily navigated then closed."
    (let ((diff-window (get-buffer-window "*svn-diff*" nil)))
      (if diff-window (select-window diff-window))))
  (defadvice svn-status-show-svn-diff-for-marked-files (after mh/jump-to-diff-window activate)
    "Jump to the diff window, so it can be easily navigated then closed."
    (let ((diff-window (get-buffer-window "*svn-diff*" nil)))
      (if diff-window (select-window diff-window))))

  :bind (:map svn-status-mode-map
         ("n" . svn-status-next-line)
         ("p" . svn-status-previous-line)
         :map svn-status-diff-mode-map
         ("q" . delete-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'custom-vc)

;;; custom-vc.el ends here
