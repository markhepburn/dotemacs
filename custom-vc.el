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
(add-to-list 'load-path (concat *mh/lisp-base* "magit"))
(autoload 'magit-status "magit" "magit interface for git" t)
(eval-after-load "magit"
  '(require 'magit-svn))                 ; svn integration needs to be explicitly loaded now.
;;; http://www.bunkus.org/blog/2009/10/an-interactive-iterative-git-blame-mode-for-emacs/
(add-to-list 'load-path (concat *mh/lisp-base* "mo-git-blame"))
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subversion interaction:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat *mh/lisp-base* "psvn"))
(require 'psvn)
(autoload 'svn-status "psvn" "Subversion interaction mode" t)
(eval-after-load "psvn"
  '(progn
     (define-key svn-status-mode-map (kbd "n") 'svn-status-next-line)
     (define-key svn-status-mode-map (kbd "p") 'svn-status-previous-line)
     ;; No idea why the default behaviour is as it is (or even how to
     ;; reproduce it in cmd-line svn for that matter), but this over-rides
     ;; it with something reasonably useful (possibly replace with '("-v")
     ;; to get a list of all files changed) :
     (setq svn-status-default-log-arguments '())
     ;; using external cmd here because psvn diff doesn't work with
     ;; colordiff, which I'm using with command-line svn:
     (setq svn-status-default-diff-arguments '("--diff-cmd" "diff" ))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

