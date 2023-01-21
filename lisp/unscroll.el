;;; unscroll.el --- Allow undo of scrolling
;;; Allow undo of scrolling
;;;

;;; Commentary:

;;; Code taken from "Writing GNU Emacs Extensions" by Bob Glickstein,
;;; (O'Reilly & Assoc., 1997)

;;; Code:

(defvar unscroll-point (make-marker)
  "Cursor position for next call to \\[unscroll].")
(defvar unscroll-window-start (make-marker)
  "Window start for next call to \\[unscroll].")
(defvar unscroll-hscroll nil
  "Horizontal scroll for next call to \\[unscroll].")

(defun unscroll ()
  "Revert to last position before the start of scrolling."
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

;;; Possibly superfluous; we advice these core commands so position is
;;; remembered no matter how it is invoked, but see below for
;;; `last-command' interaction:
(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-right 'unscrollable t)

;;; These are the commands that will appear in `last-command'
(put 'scroll-up-command 'unscrollable t)
(put 'scroll-down-command 'unscrollable t)
(put 'scroll-left-command 'unscrollable t)
(put 'scroll-right-command 'unscrollable t)


(defun unscroll-maybe-remember ()
  "Conditionally remember before-scroll position.
Save window state, as long as this is the first scroll command."
  (if (not (get last-command 'unscrollable))
      (progn
	(set-marker unscroll-point (point))
	(set-marker unscroll-window-start (window-start))
	(setq unscroll-hscroll (window-hscroll)))))


(defadvice scroll-up (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-down (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-left (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-right (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))


(provide 'unscroll)

;;; unscroll.el ends here
