;;; custom-vc.el --- Version Control Interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VC stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;

;;; Code:

;;; diff setup:
(setq diff-default-read-only t)

;; use unified diffs by default in diff-mode:
(setq diff-switches "-u")
(setq vc-svn-diff-switches '("--diff-cmd" "diff" "-x" "-u"))

;;; show changed regions in the fringe:
(use-package diff-hl
  :config (global-diff-hl-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Git integration:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :pin melpa-stable
  :init (setq
         magit-set-upstream-on-push t

         ;;Trust that I'm pushing to the correct remote/branch:
         magit-push-always-verify nil

         ;; Shut upgrade-messages up:
         magit-last-seen-setup-instructions "1.4.0"

         magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)

         ;; Always fullscreen:
         magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1

         ;; replaces magit-auto-revert-mode:
         magit-revert-buffers t)

  :config (progn
            (require 'magit-blame)

            (after 'diff-hl
              (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

            ;; More magit niceties from http://whattheemacsd.com//setup-magit.el-02.html
            (defun magit-ignore-whitespace ()
              (interactive)
              (add-to-list 'magit-diff-options "-w")
              (magit-refresh))

            (defun magit-dont-ignore-whitespace ()
              (interactive)
              (setq magit-diff-options (remove "-w" magit-diff-options))
              (magit-refresh))

            (defun magit-toggle-whitespace ()
              (interactive)
              (if (member "-w" magit-diff-options)
                  (magit-dont-ignore-whitespace)
                (magit-ignore-whitespace))))

  :bind (("C-x g" . magit-status)
         :map magit-status-mode-map
         ("W" . magit-toggle-whitespace)))

;; Enable globally:
(global-magit-file-mode 1)

;;; git-flow integration:
(use-package magit-gitflow
  :pin melpa-stable
  :after (magit)
  :config (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(use-package git-messenger
  :bind (("C-x v p" . git-messenger:popup-message)))

(use-package git-timemachine)

(use-package github-clone)

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

  :config (progn

            (defadvice svn-status-show-svn-diff (after mh/jump-to-diff-window activate)
              "Jump to the diff window, so it can be easily navigated then closed."
              (let ((diff-window (get-buffer-window "*svn-diff*" nil)))
                (if diff-window (select-window diff-window))))
            (defadvice svn-status-show-svn-diff-for-marked-files (after mh/jump-to-diff-window activate)
              "Jump to the diff window, so it can be easily navigated then closed."
              (let ((diff-window (get-buffer-window "*svn-diff*" nil)))
                (if diff-window (select-window diff-window)))))

  :bind (:map svn-status-mode-map
         ("n" . svn-status-next-line)
         ("p" . svn-status-previous-line)
         :map svn-status-diff-mode-map
         ("q" . delete-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'custom-vc)

;;; custom-vc.el ends here
