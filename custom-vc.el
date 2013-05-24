;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VC stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When using diff (C-x v =); make 'q' quit buffer and close window
;; (note that this uses diff-mode-shared-map, not diff-mode-map; check
;; the source for diff-mode for why, but basically it adds
;; extra-keybindings if the buffer is read-only, using the shared-map
;; -- which then over-writes the values set in this hook):
(setq diff-default-read-only t)
(add-hook 'diff-mode-hook
		  (lambda ()
			(define-key diff-mode-shared-map "q" 'delete-window)))
;;; same for svn-diff-mode:
(add-hook 'svn-status-diff-mode-hook
		  (lambda ()
			(define-key svn-status-diff-mode-map "q" 'delete-window)))
;; Same for log-view:
(add-hook 'log-view-mode-hook
          (lambda () (define-key log-view-mode-map "q" 'delete-window)))
(add-hook 'svn-log-view-mode-hook
          (lambda () (define-key svn-log-view-mode-map "q" 'delete-window)))
;; use unified diffs by default in diff-mode:
(setq diff-switches "-u")
(setq vc-svn-diff-switches '("--diff-cmd" "diff" "-x" "-u"))
;;; show changed regions in the fringe:
(global-diff-hl-mode 1)
;;; default to unknown and unmodified files not displayed:
(setq svn-status-hide-unknown    t
      svn-status-hide-unmodified t)
(defadvice svn-status-show-svn-diff (after mh/jump-to-diff-window activate)
  "Jump to the diff window, so it can be easily navigated then closed."
  (let ((diff-window (get-buffer-window "*svn-diff*" nil)))
	(if diff-window (select-window diff-window))))
(defadvice svn-status-show-svn-diff-for-marked-files (after mh/jump-to-diff-window activate)
  "Jump to the diff window, so it can be easily navigated then closed."
  (let ((diff-window (get-buffer-window "*svn-diff*" nil)))
	(if diff-window (select-window diff-window))))

;; Git integration:
;;; Experimenting with magit mode, on the advice of many (well, @philjackson and @jamesvnc on twitter :))
(autoload 'magit-status "magit" "magit interface for git" t)
(after "magit"
  (require 'magit-svn) ; svn integration needs to be explicitly loaded now.
  (require 'magit-blame)

  ;; full screen magit-status; restores windows on exit.  From
  ;; http://whattheemacsd.com/setup-magit.el-01.html#disqus_thread
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

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
      (magit-ignore-whitespace)))

  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subversion interaction:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'psvn)
(autoload 'svn-status "psvn" "Subversion interaction mode" t)
(setq svn-status-track-user-input t)    ; Needs this to prompt for a password!
(after "psvn"
  (define-key svn-status-mode-map (kbd "n") 'svn-status-next-line)
  (define-key svn-status-mode-map (kbd "p") 'svn-status-previous-line)
  ;; No idea why the default behaviour is as it is (or even how to
  ;; reproduce it in cmd-line svn for that matter), but this over-rides
  ;; it with something reasonably useful (possibly replace with '("-v")
  ;; to get a list of all files changed) :
  (setq svn-status-default-log-arguments '())
  ;; using external cmd here because psvn diff doesn't work with
  ;; colordiff, which I'm using with command-line svn:
  (setq svn-status-default-diff-arguments '("--diff-cmd" "diff" )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

