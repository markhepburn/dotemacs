;;; custom-functions.el --- Miscellaneous useful functions / commands
;;; Tidy-up macro from
;;; http://milkbox.net/note/single-file-master-emacs-configuration/
;;; I've been meaning to write something like this for ages; this will
;;; do as a great start!

;;; Commentary:
;; 

;;; Code:

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;;; Originally via https://web.archive.org/web/20150713053259/http://www.archivum.info/comp.emacs/2007-06/00348/Re-Ignore-%5EM-in-mixed-(LF-and-CR+LF)-line-ended-textfiles.html
;;; For interactive use, but also useful as a mode hook.
(defun ignore-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

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

(defun mh/exchange-commenting-with-other-line (&optional arg)
  "Exchange the commenting of the current line and the previous
one, such that one is commented and the other not, leaving point
on the uncommented line.  Designed to be used when experimenting
with alterations to a line, such as with `mh/duplicate-line'.
Optional non-zero prefix arg toggles with next line instead of
previous."
  (interactive "P")
  (let ((commented-re (concat "[[:space:]]*" comment-start))
        (direction (if arg 1 -1))
        current-commented
        other-commented
        successfully-toggled)
    (save-excursion
      (beginning-of-line)
      (setq current-commented (looking-at commented-re))
      (if (not (zerop (forward-line direction)))
          (message "No other line to toggle!")
        (setq other-commented (looking-at commented-re))
        (if (or (and current-commented other-commented)
                (and (not current-commented) (not other-commented)))
            (message "Exactly one line must be commented in order to toggle both.")
          ;; Do the actual toggling:
          (comment-or-uncomment-region (point-at-bol) (point-at-eol))
          (forward-line (- direction))
          (comment-or-uncomment-region (point-at-bol) (point-at-eol))
          (setq successfully-toggled t))))
    ;; If necessary, move to the uncommented line.  Note we use
    ;; `next-line' rather than `forward-line' here in order to
    ;; preserve the goal-column where possible:
    (if (and successfully-toggled other-commented)
        (next-line direction))))
(global-set-key (kbd "C-x M-;") 'mh/exchange-commenting-with-other-line)

;;; http://stackoverflow.com/a/30697761
(defun sort-lines-by-length (reverse beg end)
  "Sort lines by length."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))))))

;;; https://gist.github.com/tonini/31e349195f1a6d6d11e5
(defun mh/delete-process-at-point ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
                (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "No process at point!")))))
(define-key process-menu-mode-map (kbd "C-k") 'mh/delete-process-at-point)

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

(global-set-key (kbd "C-S-l") 'move-to-window-line-top-bottom)
;; See also C-l, now (?) bound to recenter-top-bottom which moves the
;; current line to the middle/top/bottom when invoked successively.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Inspired by
;;; http://emacs-fu.blogspot.com/2010/01/duplicating-lines-and-commenting-them.html,
;;; with a few tweaks:
;;; Update: there is a built-in `duplicate-line' now, but it doesn't do commenting.
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
      (insert
       (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
      (forward-line -1))))
(global-set-key (kbd "C-x y") 'mh/duplicate-line)
(defvar mh/duplicate-line-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "y" 'mh/duplicate-line)
    map))
(put 'mh/duplicate-line 'repeat-map 'mh/duplicate-line-repeat-map)

;;; I quite like the vi functionality of opening the next line,
;;; regardless of your position on the current line.  Deriving these
;;; from http://www.emacswiki.org/emacs/OpenNextLine, although they're
;;; not exactly tricky to write:
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))
(defun open-previous-line (arg)
  "Open a new line before the current one.
See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))
(global-set-key (kbd "S-<return>") 'open-next-line)
(global-set-key (kbd "C-S-<return>") 'open-previous-line)

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
(global-set-key (kbd "C-M-l") 'switch-to-other-buffer)

(defun mh/electric-punctuation ()
  "Tidy up whitespace around punctuation: delete any preceding
whitespace and insert one space afterwards.  Idea stolen from
  the SwiftKey android keyboard."
  (interactive)
  (when (looking-back "\s+" nil t)
    (delete-region (match-beginning 0) (match-end 0)))
  (call-interactively 'self-insert-command)
  ;; Don't insert space if we might be in a number/email/URL:
  (unless (looking-back "\\([@\\./]\\w+\\|[0-9]\\).") (just-one-space)))
(dolist (punc '(?, ?\; ?.))
  (define-key text-mode-map `[,punc] 'mh/electric-punctuation))

;;; http://www.emacswiki.org/emacs/IntegerAtPoint
(defun integer-bounds-of-integer-at-point ()
  "Return the start and end points of an integer at the current point.
The result is a paired list of character positions for an integer
   located at the current point in the current buffer.  An integer is any
   decimal digit 0 through 9 with an optional starting minus symbol
   \(\"-\")."
  (save-excursion
    (skip-chars-backward "-0123456789")
    (if (looking-at "-?[0-9]+")
        (cons (point) (match-end 0)) ; bounds of integer
      nil)))
(put 'integer 'bounds-of-thing-at-point
     'integer-bounds-of-integer-at-point)

(defun increment-number-at-point (increment)
  (interactive "p")
  (let ((numstr (thing-at-point 'integer)))
    (unless (null numstr)
      (let* ((num (string-to-number numstr))
             (incnum (+ num increment))
             (beg (beginning-of-thing 'integer))
             (end (end-of-thing 'integer)))
        (delete-region beg end)
        (insert (number-to-string incnum))))))
(global-set-key (kbd "C-x n +") 'increment-number-at-point)
(defvar increment-number-at-point-repeat-map
  (let ((map (make-sparse-keymap))
        (decrement-number-at-point (lambda ()
                                     (interactive)
                                     (setq repeat-map increment-number-at-point-repeat-map)
                                     (increment-number-at-point -1))))
    (define-key map "+" 'increment-number-at-point)
    (define-key map "=" 'increment-number-at-point)
    (define-key map "-" decrement-number-at-point)
    (define-key map "_" decrement-number-at-point)
    map))
(put 'increment-number-at-point 'repeat-map 'increment-number-at-point-repeat-map)

(defun bfn ()
  "Display the buffer's file name.  Basically short-hand for
typing M-: buffer-file-name"
  (interactive)
  (message (or buffer-file-name "Buffer is not visiting any file")))

;;
;; Courtesy of Steve Yegge, http://steve.yegge.googlepages.com/my-dot-emacs-file
;; (with tweaks for vc integration)
;;
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to
NEW-NAME.  Does the right thing if the file is under version
control"
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (if (vc-backend filename)
            (vc-rename-file filename new-name)
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR.  Does
the right thing if the file is under version control."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (vc-backend filename)
          (vc-rename-file filename newname)
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)))))

;;; Similarly, http://whattheemacsd.com/file-defuns.el-02.html
(defun delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (if (vc-backend filename)
            (vc-delete-file filename)
          (progn
            (delete-file filename)
            (kill-buffer buffer)
            (message "File '%s' successfully removed" filename)))))))

;;; https://with-emacs.com/posts/tips/quit-current-context/
(defun keyboard-quit-context+ ()
  "Quit current context.

    This function is a combination of `keyboard-quit' and
    `keyboard-escape-quit' with some parts omitted and some custom
    behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         (keyboard-quit))))

(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)

(defun mh/tag-posts (tags)
  "Apply tags (specified interactively) to the files marked in
dired, merging with existing tags.  Assumes the use of
jekyll-bootstrap, ie with the `---' delimited yaml front-matter
at the beginning."
  (interactive (list (read-from-minibuffer "Tag(s), comma-separated: ")))
  (let ((tags (split-string tags "[, ]+")))
    (labels
        ((do-tags (f)
                  (with-current-buffer (find-file-noselect f)
                    (save-excursion
                      (goto-char 0)
                      (if (re-search-forward "^tags: \\[\\([[:alnum:], ]*\\)\\]" nil t)
                          (let* ((existing-tags (split-string (match-string 1) "[, ]+"))
                                 (combined (union existing-tags tags))
                                 (new-tags (mapconcat 'identity combined ", ")))
                            ;; replace-match wasn't working for some reason:
                            (goto-char (match-beginning 1))
                            (delete-region (match-beginning 1) (match-end 1))
                            (insert new-tags))
                        (progn
                          (goto-char 0)
                          (forward-line 1)
                          (re-search-forward "^---" nil t) ; find second occurence, assuming first is on line 1
                          (goto-char (match-beginning 0))
                          (insert (format "tags: [%s]\n" (mapconcat 'identity tags ", "))))))
                    (save-buffer))))
      (mapcar #'do-tags (dired-get-marked-files)))))

;;; Archiving in case I need to write something like this again!  Was
;;; a once-off usage though.
(defun mh/posts-add-date ()
  "For all files marked in dired, assumed to have a hugo
front-matter block, add a date based on the file name."
  (interactive)
  (labels
      ((do-add-date (f)
                    (with-current-buffer (find-file-noselect f)
                      (save-excursion
                        (goto-char 0)
                        (if (not (re-search-forward "^date:" nil t))
                            (let ((date-string
                                   (substring (file-name-base (buffer-file-name)) 0 10)))
                              (forward-line 1)
                              (insert (format "date: %s\n" date-string))
                              (save-buffer)))))))
    (mapcar #'do-add-date (dired-get-marked-files))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-functions)

;;; custom-functions.el ends here
