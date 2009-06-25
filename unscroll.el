;;; Allow undo of scrolling
;;;
;;; Code taken from "Writing GNU Emacs Extensions" by Bob Glickstein,
;;; (O'Reilly & Assoc., 1997)

(defvar unscroll-point (make-marker)
  "Cursor position for next call to \\[unscroll].")
(defvar unscroll-window-start (make-marker)
  "Window start for next call to \\[unscroll].")
(defvar unscroll-hscroll nil
  "Horizontal scroll for next call to \\[unscroll].")

;; Note: this can't be autoloaded, since it relies on advising
;; functions which won't happen until this file is loaded (ie, if this
;; is autoloaded the first time it is invoked it won't work).
(defun unscroll ()
  "Revert to last position before the start of scrolling."
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))


(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-right 'unscrollable t)


(defun unscroll-maybe-remember ()
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