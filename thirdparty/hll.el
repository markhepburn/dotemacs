;;; hll.el --- interactively toggle highlighting of lines

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Benjamin Rutt <brutt@bloomington.in.us>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides interactive highlighting of lines, useful if
;; you are viewing lines of data in a buffer with a group of people
;; and want to refer to specific lines.  The easiest function to start
;; with is `hll-toggle-line', which toggles whether the current line
;; is highlighted or not.  There is also `hll-highlight-line' and
;; `hll-unhighlight-line' which do the obvious.  There is also
;; `hll-unhighlight-buffer' which turns off hll highlighting for the
;; entire buffer.

;; Navigation between highlighted lines is provided; see
;; `hll-next-highlight' and `hll-prev-highlight'.

;; To use, place the file in your load-path and add

;; (require 'hll)

;; to your ~/.emacs.

;; This package is most useful with some keybindings.  I use the
;; following:

;; (global-set-key (kbd "C-c h h") 'hll-toggle-line)
;; (global-set-key (kbd "C-c h p") 'hll-prev-highlight)
;; (global-set-key (kbd "C-c h n") 'hll-next-highlight)
;; (global-set-key (kbd "C-c h u") 'hll-unhighlight-buffer)

;; Note:  this package probably only works on GNU Emacs.  Untested on
;; XEmacs, patches accepted.

;;; Code:

(defgroup hll nil
  "Interactively toggle highlighting of lines."
  :group 'editing)

(defcustom hll-highlight-face 'highlight
  "Face with which to highlight lines."
  :type 'face
  :group 'hll)

;; helper functions/variables

(defvar hll-overlay-list nil)
(make-variable-buffer-local 'hll-overlay-list)

;; return t if at least one element of ls1 is present in ls2
(defun hll-member-at-least-one (ls1 ls2)
  (if (or (null ls1) (null ls2))
      nil
    (or (member (car ls1) ls2)
	(hll-member-at-least-one (cdr ls1) ls2))))

;; begin user interface

(defun hll-highlight-line ()
  (interactive)
  (when (not (hll-member-at-least-one (overlays-at (point)) hll-overlay-list))
    (setq o (make-overlay (line-beginning-position)
			  (line-end-position)
			  nil nil t))
    (overlay-put o 'face hll-highlight-face)
    (overlay-put o 'evaporate t)
    (setq hll-overlay-list (cons o hll-overlay-list))))

(defun hll-unhighlight-line ()
  (interactive)
  (let (existing-overlay)
    (mapc
     (lambda (ov)
       (if (member ov hll-overlay-list)
	   (setq existing-overlay ov)))
     (overlays-at (point)))
    (when existing-overlay
      (setq hll-overlay-list (delq existing-overlay hll-overlay-list))
      (delete-overlay existing-overlay))))

(defun hll-toggle-line ()
  (interactive)
  (if (hll-member-at-least-one (overlays-at (point)) hll-overlay-list)
      (hll-unhighlight-line)
    (hll-highlight-line)))

(defun hll-unhighlight-buffer ()
  (interactive)
  (mapc
   (lambda (ov)
     (delete-overlay ov))
   hll-overlay-list)
  (setq hll-overlay-list nil))

(defun hll-prev-highlight ()
  (interactive)
  (let ((starting-point (point))
	(starting-column (current-column))
	(done nil))
    (while (not done)
      (goto-char (previous-overlay-change (line-beginning-position)))
      (beginning-of-line)
      (if (or (= (point) (point-min))
	      (hll-member-at-least-one (overlays-at (point)) hll-overlay-list))
	  (setq done t)))
    (if (= (point) (point-min))
	(goto-char starting-point)
      (move-to-column starting-column))))

(defun hll-next-highlight ()
  (interactive)
  (let ((starting-point (point))
	(starting-column (current-column))
	(done nil))
    (while (not done)
      (goto-char (next-overlay-change (line-end-position)))
      (end-of-line)
      (if (or (= (point) (point-max))
	      (hll-member-at-least-one (overlays-at (line-beginning-position))
				       hll-overlay-list))
	  (setq done t)))
    (if (= (point) (point-max))
	(goto-char starting-point)
      (move-to-column starting-column))))

(provide 'hll)
;;; hll.el ends here
