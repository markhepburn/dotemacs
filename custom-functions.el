;;; mark the line at point (main use-case is probably to mark then
;;; comment out, since killing, copying etc are already handled -- see
;;; below).  Bound to C-M-; to resemble M-; for this reason.
(defun mh/mark-line (&optional arg)
  "Mark the line under point.  Optional prefix arg marks arg
  following lines including the current line, or arg preceding
  lines including the current line if negative."
  (interactive "p")
  (if (< arg 0)
      (progn
        (setq arg (- arg))
        (previous-line (- arg 1))))
  (beginning-of-line)
  (set-mark-command nil)
  (move-end-of-line arg))
(global-set-key (kbd "C-M-;") 'mh/mark-line)

;;; From http://www.emacswiki.org/emacs/SlickCopy
;;; When kill/copy region commands are used with no region selected,
;;; operate on line instead:
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice rgrep (around rgrep-rename-previous-buffer activate compile)
  "If a previous *grep* buffer exists, offer to rename it before
running the new process."
  (interactive
   (let ((old-grep-buf (get-buffer "*grep*")))
     (if old-grep-buf
         (progn
           ;; display the old one for easier renaming!
           (if (not (get-buffer-window old-grep-buf))
               (display-buffer old-grep-buf))
           (if (y-or-n-p "Grep results buffer already exists; rename it first? ")
               (let ((new-buf-name (read-string "New name: ")))
                 (with-current-buffer old-grep-buf
                   (rename-buffer new-buf-name))))))
     ;; Sneaky: we need to match the interactive spec for the original
     ;; rgrep, but we also just want to use the original
     ;; interactivity.  Hence, this dummy list followed by
     ;; 'call-interactively (and no reference to ad-do-it)
     (list 'ignored-re 'ignored-files 'ignored-dirs)))
  (call-interactively 'ad-Orig-rgrep))

;;; utility to add hooks to enable the given minor mode for all
;;; specified major modes:
(defmacro enable-minor-mode-for (minor-mode major-mode-list)
  `(mapc (lambda (mode)
           (let ((hook (intern (concat (symbol-name mode)
                                       "-mode-hook"))))
             (add-hook hook (lambda () (,minor-mode 1)))))
         ,major-mode-list))



;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph.
;;; Takes a multi-line paragraph and makes it into a single line of text.
(defun mh/unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; As taken from "Writing GNU Emacs Extensions" (Glickstein)
(defadvice switch-to-buffer (before existing-buffer
                                    activate compile)
  "When interactive, switch to existing buffers only, unless
given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: "
                      (other-buffer)
                      (null current-prefix-arg)))))
(defadvice switch-to-buffer-other-window (before existing-buffer
                                                 activate compile)
  "When interactive, switch to existing buffers only, unless
given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: "
                      (other-buffer)
                      (null current-prefix-arg)))))
(defadvice switch-to-buffer-other-frame (before existing-buffer
                                                activate compile)
  "When interactive, switch to existing buffers only, unless
given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: "
                      (other-buffer)
                      (null current-prefix-arg)))))
;; More from Glickstein; this one in a separate file I've located (but
;; taken from his book):
(require 'unscroll)

;; functions to move point to beginning and end of window, respectively:
(defun goto-beginnning-of-window ()
  (interactive)
  (goto-char (window-start)))
(defun goto-end-of-window ()
  (interactive)
  ; not sure why this needs to be -1 or -2; works on xemacs without it:
  (goto-char (- (window-end) 2)))
;; bind to C-' and C-" respectively (note that M-r --
;; move-to-window-line -- with no arguments) moves point to center of
;; window)
(global-set-key [(control ?')] 'goto-beginnning-of-window)
(global-set-key [(control ?\")] 'goto-end-of-window)
;; See also C-l, now (?) bound to recenter-top-bottom which moves the
;; current line to the middle/top/bottom when invoked successively.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Inspired by
;;; http://emacs-fu.blogspot.com/2010/01/duplicating-lines-and-commenting-them.html,
;;; with a few tweaks:
(defun mh/duplicate-line (comment-first)
  "Duplicate the current line below; optional prefix arg comments
  the original line."
  (interactive "P")
  (beginning-of-line)
  (let ((beg (point)))
    (end-of-line)
    (let ((str (buffer-substring beg (point))))
      (when comment-first
        (comment-region beg (point)))
      (insert-string
       (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
      (forward-line -1))))
(global-set-key (kbd "C-x y") 'mh/duplicate-line)

;; http://slashusr.wordpress.com/2010/01/19/quickly-diff-the-changes-made-in-the-current-buffer-with-its-file/
(defun mh/diff-buffer-file-changes ()
  (interactive)
  (diff-buffer-with-file (current-buffer)))
;;; this over-rides 'text-scale-adjust, but that's also available on C-x C-+:
(global-set-key (kbd "C-x C-=") 'mh/diff-buffer-file-changes)

;; stole this from xemacs21:
(defun switch-to-other-buffer (arg)
  (interactive "p")
  (if (eq arg 0)
      (bury-buffer (current-buffer)))
  (switch-to-buffer
   (if (<= arg 1) (other-buffer (current-buffer))
     (nth (1+ arg) (buffer-list)))))
;; bind to C-M-l, just like in xemacs:
(global-set-key [(control meta ?l)] #'switch-to-other-buffer)

;; Scroll up/down with C-./C-,
(defun scroll-down-one-line (arg)
  (interactive "p")
  (if (eq arg 0) (setq arg 1))
  (scroll-down arg))
(defun scroll-up-one-line (arg)
  (interactive "p")
  (if (eq arg 0) (setq arg 1))
  (scroll-up arg))
(global-set-key [(control ?,)] #'scroll-up-one-line) ; C-,
(global-set-key [(control ?.)] #'scroll-down-one-line) ; C-.
;; also use C-(/) because flyspell-mode steals C-,/.
(global-set-key [(control ?\()] #'scroll-up-one-line) ; C-(
(global-set-key [(control ?\))] #'scroll-down-one-line) ;C-)
;; Move line cursor is on to top of screen:
;; Update: see also repeated use of C-l (`recenter-top-bottom`)
(defun mh/current-line-to-top (&optional distance)
  "Moves line the cursor is currently on to top of the window;
alternatively with optional prefix arg (defaults to 4) move
line to that many lines below top.  Absolute value of argument is used."
  (interactive "P")
  (if (not distance) (setq distance 4)
    (setq distance (prefix-numeric-value distance)))
  (if (> distance 0) (setq distance (- distance)))
  (save-excursion
    (forward-line distance)
    (beginning-of-line)
    (set-window-start (selected-window) (point))))
(global-set-key [(control meta ?')] #'mh/current-line-to-top)

;; Really can't believe this doesn't already exist; what am I missing?
;; Update: see also M-x how-many
(defun mh/count-words (arg)
  "Counts number of words in the buffer or, if the region is
active, in the region.  Optional prefix arg means behave similarly to
`wc -l', otherwise count words as traversed by forward-word."
  (interactive "P")
  (let ((result 0)
        (min (if mark-active (region-beginning) (point-min)))
        (max (if mark-active (region-end) (point-max))))
    (with-syntax-table (copy-syntax-table)
      (if arg (modify-syntax-entry ?- "w"))
      (save-excursion
        (goto-char min)
        (while (< (point) max)
          (if (forward-word 1)
              (incf result)))
        (message "%d word%s." result (if (> result 1) "s" ""))
        result))))


;; 
;; Courtesy of Steve Yegge, http://steve.yegge.googlepages.com/my-dot-emacs-file
;; 
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t)))) 

;; stupidity :)
(defun mh/scrum-p ()
  (interactive)
  (let* ((now (decode-time))
         (scrum-time (+ (* 60 (nth 2 now)) (nth 1 now))))
    (cond ((< scrum-time 630) (message "Scrum"))
          ((< scrum-time 900) (message "Scrum++"))
          (t (message "No more scrum today :(")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
