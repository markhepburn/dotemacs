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


(use-package project
  :ensure nil
  :init (setq project-vc-extra-root-markers '(".project.el" ".projectile")))

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
  ;; otherwise magit-repository-directories isn't initialised if
  ;; project.el hasn't been loaded first:
  (autoload 'project-known-project-roots "project" nil t)
  (setq
   ;; http://iqbalansari.github.io/blog/2014/02/22/switching-repositories-with-magit/
   ;; http://irreal.org/blog/?p=4177
   magit-repository-directories
   (mapcar (lambda (dir)
             (cons (substring dir 0 -1) 0))
           (nreverse
            (cl-remove-if-not (lambda (project)
                                (and
                                 (not (s-starts-with? "/docker:" project))
                                 (file-directory-p (concat project "/.git/"))))
                              (project-known-project-roots)))))
  :custom (magit-bury-buffer-function 'magit-restore-window-configuration)

  ;; Make this a global command, not just inside a repo:
  :bind (("C-x g" . magit-status)
         ("C-c M-g" . magit-file-dispatch)))

;;; Install delta from https://github.com/dandavison/delta
(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

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


(provide 'custom-vc)

;;; custom-vc.el ends here
