;; I hate the (current, perhaps) look of the anti-aliased fonts used:
;;; update: nope, all good now.  Also note that
;;; mac-allow-anti-aliasing is an alias for ns-antialias-text
;(setq mac-allow-anti-aliasing nil)
(setq ns-antialias-text t)
;; use option as meta key:
;; (setq mac-command-key-is-meta nil)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'alt)

;;; integrate with OSX a bit; using cmd-` to switch frames:
(global-set-key (kbd "A-`") 'other-frame)

;;; ability to maximize emacs, even on OSX:
(add-to-list 'load-path (concat *mh/code-dir* "maxframe"))
(autoload 'maximize-frame "maxframe" "Maximize current window" t)
(defun mh/toggle-fullscreen ()
  (interactive)
  ;; pick a random var to test.  Note that because we use autoload,
  ;; mf-restore-width may not be bound yet, so check that first.
  (if (and (boundp 'mf-restore-width)
		   mf-restore-width) 
	  (restore-frame)
	(maximize-frame)))
(global-set-key (kbd "A-<return>") 'mh/toggle-fullscreen)

;;; growl interaction, just for kicks:
(defvar *growl-program* "growlnotify")
(defun growl-notify (msg &optional title)
  (let* ((msg-title (or title "Emacs"))
         (process (start-process "growl" nil *growl-program*
                                 "-a" "Emacs"
                                 "-t" msg-title)))
    (process-send-string process msg)
    (process-send-string process "\n")
    (process-send-eof process)))
