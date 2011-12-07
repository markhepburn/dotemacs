;;; Miscellaneous customisations; mostly one-liner tweaks of
;;; appearance and functionality.


;; use font lock where possible:
(global-font-lock-mode t)
;; don't use those irritating ~ backup files:
(setq backup-inhibited t)
;; work with compressed files:
(auto-compression-mode 1)
;; update files changed on disk (mainly for use with dropbox):
(global-auto-revert-mode 1)
;; don't show toolbar:
(tool-bar-mode -1)
;; hide the menu-bar by default (accessible by C-right-click):
(menu-bar-mode -1)
;; line and column-number modes:
(line-number-mode 1)
(column-number-mode 1)
;;; next-line should go next text line (old default), not visual line (from
;;; http://bryan-murdock.blogspot.com/2009/03/emacs-next-line-changed-behavior.html
;;; originally, but things seem to have changed slightly since then):
(setq line-move-visual nil)
;;; trailing whitespace (see also M-x delete-trailing-whitespace):
(setq-default show-trailing-whitespace t)
;;; Make sure we always include a trailing newline:
(setq require-final-newline t)
;;; high-light selections:
(transient-mark-mode 1)
;;; 4-space tabs, and spaces-not-tabs:
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
;;; Default to view-mode for read-only files:
(setq view-read-only t)
;;; Single-frame ediff usage (mainly because floating windows seemed
;;; to interact badly with xmonad, even when explicitly floated):
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Cool-looking completion package:
(icomplete-mode t)
;; use ido-mode for a while (see how it gels with firefox3 location
;; bar behaviour!) (was keeping icomplete-mode for command completion; trying to use ido for both for a while..)
(when (require 'ido nil t)
  ;; This makes normal read-buffer use ido-read-buffer, etc.  I.e.,
  ;; can use ido functions with elscreen etc.
  (ido-everywhere 1)

  (setq ido-execute-command-cache nil)

  ;; Kept for posterity's sake; see below for the more complete smex
  ;; that I now use:
  (defun ido-execute-command ()
    (interactive)
    (call-interactively
     (intern
      (ido-completing-read
       "M-x "
       (progn
         (unless ido-execute-command-cache
           (mapatoms (lambda (s)
                       (when (commandp s)
                         (setq ido-execute-command-cache
                               (cons (format "%S" s) ido-execute-command-cache))))))
         ido-execute-command-cache)))))

  ;; Let's use smex instead (basically the same as above but with more
  ;; options, and crucially has the ability refresh the cache):
  (setq smex-save-file "~/.emacs.d/smex.save")
  (add-to-list 'load-path (concat *mh/lisp-base* "smex"))
  (require 'smex)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)

  (when (require 'imenu nil t)
    ;; http://chopmo.blogspot.com/2008/09/quickly-jumping-to-symbols.html
    (defun ido-goto-symbol ()
      "Will update the imenu index and then use ido to select a 
   symbol to navigate to"
      (interactive)
      (imenu--make-index-alist)
      (let ((name-and-pos '())
            (symbol-names '()))
        (flet ((addsymbols (symbol-list)
                           (when (listp symbol-list)
                             (dolist (symbol symbol-list)
                               (let ((name nil) (position nil))
                                 (cond
                                  ((and (listp symbol) (imenu--subalist-p symbol))
                                   (addsymbols symbol))
                                  
                                  ((listp symbol)
                                   (setq name (car symbol))
                                   (setq position (cdr symbol)))
                                  
                                  ((stringp symbol)
                                   (setq name symbol)
                                   (setq position (get-text-property 1 'org-imenu-marker symbol))))
                                 
                                 (unless (or (null position) (null name))
                                   (add-to-list 'symbol-names name)
                                   (add-to-list 'name-and-pos (cons name position))))))))
          (addsymbols imenu--index-alist))
        (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
               (position (cdr (assoc selected-symbol name-and-pos))))
          (goto-char position))))

    ;; Note: over-rides default binding of abbrev-prefix-mark
    (global-set-key [(meta \')] 'ido-goto-symbol))
  
  (add-hook 'ido-setup-hook
            (lambda ()
              (setq ido-enable-flex-matching t)
              ;; (global-set-key "\M-x" 'ido-execute-command)
              ))
  (ido-mode t))

;;; Visual rectangle editing: /why/ the hell is this buried in a
;;; package that makes emacs act more like windows??  Anyway:
(cua-selection-mode t)                ;; Also disables the CUA keys
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands

;;; experiment with find-file-at-point a bit (don't use their
;;; pre-configured bindings, as they will presumably over-write the
;;; ido ones!)
(autoload 'ffap "ffap" "Find file at point functionality" t)
(global-set-key (kbd "C-x M-f") 'ffap)

;;; shortcut for browse url at point:
(global-set-key (kbd "C-x M-b") 'browse-url-at-point)

;;; more specialised "opening" commands; mplayer control:
(add-to-list 'load-path (concat *mh/lisp-base* "mplayer-mode"))
(autoload 'mplayer-find-file "mplayer-mode" "Control mplayer from emacs while editing a file" t)

;;; Ignore .svn/ contents in find-grep:
;;; http://benjisimon.blogspot.com/2009/01/emacs-tip-slightly-better-find-grep.html
(setq grep-find-command 
  "find . -type f '!' -wholename '*/.svn/*' -print0 | xargs -0 -e grep -nH -e ")

;;; use some code templating (note that I'm using the bundle here; may
;;; be better later on to use the plain package and customise it):
(require 'yasnippet-bundle)

;;; autojump support (provides dired buffer of shell history options):
(add-to-list 'load-path (concat *mh/lisp-base* "j"))
(autoload 'j "j" "Autojump support (dired buffer from shell history)" t)
(global-set-key (kbd "C-x j") 'j)

;; Don't blink the cursor:
(blink-cursor-mode -1)

;;; paren-matching (investigate mic-paren mode properly at some stage):
;(show-paren-mode 1)
;;; Ok, mic-paren time is now.  Pretty happy with the defaults for
;;; now; mainly using it for off-screen notification, and because I
;;; have braces on the same line in my C code I don't need to toggle
;;; paren-open-context-backward.  May want to play with
;;; paren-match-{quoted-paren,paired-delimiter} later though.
(when (require 'mic-paren nil t)
  (paren-activate))

;; paper size:
(setq ps-paper-type 'a4)
;; Dired should recursively delete directories after asking:
(setq dired-recursive-deletes 'top
      dired-recursive-copies 'top
      dired-dwim-target t)
(add-hook 'dired-mode-hook
          (lambda ()
            ;; already available on C-xC-q (usually toggles read-only, so that works)
            ;(local-set-key (kbd "C-c C-r") 'wdired-change-to-wdired-mode)
            (when (require 'dired-x nil t)
              (setq dired-omit-files      "\\(^\\..*\\)\\|\\(CVS\\)"
                    dired-omit-extensions '("~" ".bak" ".pyc" ".elc"))
              (dired-omit-mode 1))))

;; Scroll-bars on the right please:
(if (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode 'right))
;; No startup message please:
(setq inhibit-startup-message t)
;; save a few key strokes from typing 'yes':
(fset 'yes-or-no-p 'y-or-n-p)
;; M-y to browse kill-ring:

;;; I don't know why this seemed to suddenly change; make backspace
;;; work again in isearch-mode anyway (see also C-M-w which does the
;;; same thing, and C-M-y and C-y as well):
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

;;; restore backspace behaviour in calc too; this will get disrupted
;;; with the global-set-key used below (for the autopair stuff):
(add-hook 'calc-mode-hook
          (lambda () (local-set-key (kbd "<backspace>") 'calc-pop)))

;; kill-ring selection:
(when (require 'browse-kill-ring nil t)
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-no-duplicates t) ;...and don't clog it up with duplicates
  (defadvice browse-kill-ring-insert-and-quit (after indent-region activate)
    (if (member major-mode '(emacs-lisp-mode
                             lisp-mode
                             erlang-mode
                             python-mode
                             c-mode c++-mode objc-mode
                             latex-mode plain-tex-mode))
        (let ((mark-even-if-inactive t))
          (indent-region (region-beginning) (region-end) nil)))))

;;; similar advice for 'yank and 'yank-pop:
(defadvice yank (after indent-region-for-yank activate)
  "If in a programming mode, reindent the region after yanking."
  (if (member major-mode '(emacs-lisp-mode
                           lisp-mode
                           erlang-mode
                           python-mode
                           c-mode c++-mode objc-mode
                           latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))
(defadvice yank-pop (after indent-region-for-yank-pop activate)
  "If in a programming mode, reindent the region after yanking."
  (if (member major-mode '(emacs-lisp-mode
                           lisp-mode
                           erlang-mode
                           python-mode
                           c-mode c++-mode objc-mode
                           latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

;;; Suggestion from http://www.emacswiki.org/emacs-en/KillingAndYanking
;;; Cycle backwards through the kill-ring with meta-shift-y:
(defun yank-pop-backwards ()
  (interactive)
  (yank-pop -1))
(global-set-key "\M-Y" 'yank-pop-backwards)

;; use shift-arrow to move between windows:
(windmove-default-keybindings)
;;; Let's experiment with winswitch too (makes C-xo a prefix key for
;;; hjkl switching -- less movement from the home row!)
(when (require 'win-switch nil t)
  (setq win-switch-idle-time 1)            ; Can always hit <return> to exit sooner.
  (setq win-switch-other-window-first nil) ; Rationale: either in the mode or not, no mix of functionality.
  (defun mh/win-switch-setup-keys-vistyle (&rest dispatch-keys)
    "Restore default key commands and bind global dispatch keys.
Under this setup, keys i, j, k, and l will switch windows,
respectively, up, left, down, and right, with other functionality
bound to nearby keys. The arguments DISPATCH-KEYS, if non-nil,
should be a list of keys that will be bound globally to
`win-switch-dispatch'."
    (interactive)
    (win-switch-set-keys '("h") 'left)
    (win-switch-set-keys '("k") 'up)
    (win-switch-set-keys '("j") 'down)
    (win-switch-set-keys '("l") 'right)
    (win-switch-set-keys '("n") 'next-window)
    (win-switch-set-keys '("p") 'previous-window)
    (win-switch-set-keys '("K") 'enlarge-vertically)
    (win-switch-set-keys '("J") 'shrink-vertically)
    (win-switch-set-keys '("H") 'enlarge-horizontally)
    (win-switch-set-keys '("L") 'shrink-horizontally)
    (win-switch-set-keys '(" ") 'other-frame)
    (win-switch-set-keys '(" " [return]) 'exit)
    (win-switch-set-keys '("i") 'split-horizontally)
    (win-switch-set-keys '("u") 'split-vertically)
    (win-switch-set-keys '("0") 'delete-window)
    (win-switch-set-keys '("\M-\C-g") 'emergency-exit)
    (dolist (key dispatch-keys)
      (global-set-key key 'win-switch-dispatch)))
  (mh/win-switch-setup-keys-vistyle (kbd "C-x o")))
;;; and rotate windows too:
(dolist (fn '(buf-move-up buf-move-down buf-move-left buf-move-right))
  (let ((file "buffer-move"))
    (autoload fn file "Swap buffers between windows" t)))
(global-set-key (kbd "M-g <left>")  'buf-move-left)
(global-set-key (kbd "M-g <right>") 'buf-move-right)
(global-set-key (kbd "M-g <up>")    'buf-move-up)
(global-set-key (kbd "M-g <down>")  'buf-move-down)

;; vi-like case toggle:
(when (require 'toggle-case nil t)
  (global-set-key [(control \`)]      'toggle-case)
  (global-set-key [(control ~)]       'toggle-case-backwards)
  (global-set-key [(control meta \`)] 'toggle-case-by-word)
  (global-set-key [(control meta ~)]  'toggle-case-by-word-backwards))

;; make all buffer-names unique:
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward)
  ;; (setq uniquify-separator "|")
  (setq uniquify-ignore-buffers-re "^\\*") ; Ignore *scratch*, etc
  (setq uniquify-after-kill-buffer-p t))

;;; csv support:
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; Show docs where available:
(enable-minor-mode-for eldoc-mode '(emacs-lisp lisp inferior-lisp))

;; view pdfs etc inline:
(autoload 'doc-view "doc-view" "View pdfs inline" t)

;; search through open buffers:
(autoload 'grep-buffers "grep-buffers" "Grep through open buffers" t)

;;; use ack (note that I have two versions of this command installed):
(autoload 'ack-grep "ack" "Intelligent form of grep-find" t)

;; Twitter (Used to use twit.el, but that doesn't support OAuth):
(add-to-list 'load-path (concat *mh/lisp-base* "twittering-mode"))
(autoload 'twit "twittering-mode" "Twittering mode" t)
(autoload 'twittering-update-status-interactive "twittering-mode" "Twitter status update" t)
(defalias 'twit-post 'twittering-update-status-interactive
  "Post from the minibuffer (or whatever it is set to) without invoking twit.")
(eval-after-load "twittering-mode"
  '(twittering-icon-mode))
(setq twittering-update-status-function 'twittering-update-status-from-minibuffer
      twittering-timer-interval 36000        ; I don't want auto-refresh
      twittering-use-master-password t
      twittering-url-show-status nil)
;;; By default, it assumes you have already authorised credentials by
;;; the time you try and update your status; because I'm auto-loading
;;; the update function and don't necessarily think I'll be using the
;;; main interface much, this won't be the case the first time:
(defadvice twittering-update-status-interactive (before twittering-verify-before-update activate)
  (unless (twittering-account-authorized-p)
    (twittering-verify-credentials)))

;; bind C-h a to 'apropos like in xemacs (not apropos-command as it is
;; in emacs by default)
(global-set-key [(C ?h) ?a] #'apropos)

;; Use bs-show instead of the default list-buffers:
(global-set-key [(C ?x) (C ?b)] #'bs-show)
(defadvice bs-show (around bs-show-maybe-ibuffer (arg) activate)
  "If invoked with two C-u prefixes, ie C-u C-u C-x C-b in my
  setup, invoke ibuffer instead."
  (interactive "P")
  (if (and arg
           (eq (car arg) 16))
      (ibuffer)
    ad-do-it))
(setq-default ibuffer-default-sorting-mode 'major-mode)

;; let's play with using C-w to delete words backwards:
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

;; high-light line mode is occasionally useful:
(autoload 'hll-toggle-line "hll"
  "High-light lines in buffer for clearer browsing" t)
(global-set-key (kbd "C-x t h") 'hll-toggle-line)
(global-set-key (kbd "C-x t p") 'hll-prev-highlight)
(global-set-key (kbd "C-x t n") 'hll-next-highlight)
(global-set-key (kbd "C-x t u") 'hll-unhighlight-buffer)

;;; related to that, bookmarks look very useful (like tags for source
;;; navigation, only you don't have to pop them, for eg).  I'm using
;;; C-xm as my prefix map, which defaults to send-mail which I never
;;; use.
(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto next bookmark."                t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)
(autoload 'bm-previous "bm" "List all bookmarks."                t)
(global-set-key (kbd "C-x m") (make-sparse-keymap))
(global-set-key (kbd "C-x m m") 'bm-toggle)
(global-set-key (kbd "C-x m n") 'bm-next)
(global-set-key (kbd "C-x m p") 'bm-previous)
(global-set-key (kbd "C-x m l") 'bm-show) ; l for list
(global-set-key (kbd "C-x m a") 'bm-bookmark-annotate) ; don't autload this; needs an existing bookmark to work.
(defadvice bm-toggle (around bm-toggle-create-annotation (arg) activate)
  "If given a prefix arg when creating, ask for an annotation as
  well."
  (interactive "P")
  (if arg
      (let ((bm-annotate-on-create t))
        ad-do-it)
    ad-do-it))
(defadvice bm-show (around bm-show-optionally-show-all (arg) activate)
  "With a prefix arg, show all (global) bookmarks, otherwise just
  the current buffer as normal."
  (interactive "P")
  (if arg
      (bm-show-all)
    ad-do-it))

;;; escreen; gnu-screen for emacs:
(add-to-list 'load-path (concat *mh/lisp-base* "escreen"))
(setq escreen-prefix-char (kbd "C-z"))  ;; must be done before loading!
(when (require 'escreen nil t)
  (setq escreen-one-screen-p nil)
  (escreen-install)
  (defun escreen-find-file-new-screen (filename &optional wildcards)
    (interactive
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer)))
    (escreen-create-screen)
    (find-file filename wildcards))
  (defun escreen-kill-screen-and-buffers ()
    (interactive)
    (let ((current-escreen (escreen-get-current-screen-number)))
      (if (escreen-configuration-one-screen-p)
          (error "[escreen] Only one active screen; can't kill it.")
        (dolist (data-map (escreen-configuration-data-map
                           (escreen-configuration-escreen current-escreen)))
          (kill-buffer (escreen-configuration-data-map-critical-buffer
                        (escreen-configuration-data-map-critical data-map))))
        (escreen-kill-screen current-escreen))))
  ;; adapted from http://blog.tapoueh.org/news.dim.html#%20Escreen%20integration
  (defun escreen-get-active-screen-numbers-with-emphasis ()
    "Display active screens, with the active screen emphasised."
    (interactive)
    (let ((escreens (escreen-get-active-screen-numbers))
          (emphasised ""))
      (dolist (s escreens)
        (setq emphasised
              (concat emphasised (if (= escreen-current-screen-number s)
                                   (propertize (number-to-string s)
                                               'face 'highlight)
                                   (number-to-string s))
                      " ")))
      (message "[escreen] active screens: %s" emphasised)))
  (defmacro escreen-advise-emphasis (fn)
    `(defadvice ,fn (after ,(intern (concat (symbol-name fn) "-emphasis")) activate)
       (escreen-get-active-screen-numbers-with-emphasis)))

  ;; Make the keybindings a bit more familiar:
  (escreen-advise-emphasis escreen-goto-last-screen)
  (define-key escreen-map (kbd "C-l") 'escreen-get-active-screen-numbers-with-emphasis)
  (define-key escreen-map (kbd "C-c") (escreen-advise-emphasis escreen-create-screen))
  (define-key escreen-map (kbd "C-k") (escreen-advise-emphasis escreen-kill-screen))
  (define-key escreen-map (kbd "M-k") (escreen-advise-emphasis escreen-kill-screen-and-buffers))
  (define-key escreen-map (kbd "C-n") (escreen-advise-emphasis escreen-goto-next-screen))
  (define-key escreen-map (kbd "C-p") (escreen-advise-emphasis escreen-goto-prev-screen))
  (define-key escreen-map (kbd "C-f") (escreen-advise-emphasis escreen-find-file-new-screen)))

;; Appearance: I don't mind the zenburn theme, might experiment with that occasionally:
; (autoload 'zenburn "zenburn" "Zenburn colour theme" t)

;; Use that textmate-emulation hack to insert balancing
;; quotes/brackets/tags:
;(require 'wrap-region)
;; Update -- already included in emacs!
;;; See http://code.google.com/p/autopair/ for a more sophisticated
;;; attempt; not installing yet but keep an eye on it.
(setq skeleton-pair t)
(when skeleton-pair
  (setq skeleton-autowrap t)
  ;; Don't double-up after a word (eg, the apostrophe in "I'm"):
  ;; (actually, ignore that for now -- just won't bind apostrophe to skeleton-mode)
  ;; (setq skeleton-pair-filter-function
  ;; 		(lambda () (eq (char-syntax (preceding-char)) ?w)))

  ;; http://www.emacswiki.org/emacs/AutoPairs
  ;; see also http://cmarcelo.wordpress.com/2008/04/26/a-little-emacs-experiment/
  (setq skeleton-pair-alist
        '((?\( _ ?\))
          (?[  _ ?])
          (?{  _ ?})
          (?\" _ ?\")))

  (defun autopair-insert (arg)
    (interactive "P")
    (let (pair)
      (cond
       ((assq last-command-char skeleton-pair-alist)
        (autopair-open arg))
       (t
        (autopair-close arg)))))

  (defun autopair-open (arg)
    (interactive "P")
    (let ((pair (assq last-command-char
                      skeleton-pair-alist)))
      (cond
       ((and (not mark-active)
             (eq (car pair) (car (last pair)))
             (eq (car pair) (char-after)))
        (autopair-close arg))
       (t
        (skeleton-pair-insert-maybe arg)))))

  (defun autopair-close (arg)
    (interactive "P")
    (cond
     (mark-active
      (let (pair open)
        (dolist (pair skeleton-pair-alist)
          (when (eq last-command-char (car (last pair)))
            (setq open (car pair))))
        (setq last-command-char open)
        (skeleton-pair-insert-maybe arg)))
     ((looking-at
       (concat "[ \t\n]*"
               (regexp-quote (string last-command-char))))
      (replace-match (string last-command-char))
      (indent-according-to-mode))
     (t
      (self-insert-command (prefix-numeric-value arg))
      (indent-according-to-mode))))

  (defun autopair-backspace (arg)
    (interactive "p")
    (if (eq (char-after)
            (car (last (assq (char-before) skeleton-pair-alist))))
        (and (char-after) (delete-char 1)))
    (delete-backward-char arg))

  (global-set-key [backspace] 'autopair-backspace)

  (global-set-key "("  'autopair-insert)
  (global-set-key ")"  'autopair-insert)
  (global-set-key "["  'autopair-insert)
  (global-set-key "]"  'autopair-insert)
  (global-set-key "{"  'autopair-insert)
  (global-set-key "}"  'autopair-insert)
  (global-set-key "\"" 'autopair-insert)

  (defun autopair-open-block (arg)
    (interactive "P")
    (if (looking-at "[[:space:]]*$") (just-one-space))
    (autopair-open arg)
    (newline)
    (newline-and-indent)
    (previous-line)
    (indent-according-to-mode))

  (defun autopair-close-block (arg)
    (interactive "P")
    (cond
     (mark-active
      (autopair-close arg))
     ((not (looking-back "^[[:space:]]*"))
      (newline-and-indent)
      (autopair-close arg))
     (t
      (autopair-close arg)))))

;; parse keychain-generated environment variables and set them, if
;; they exist:
;; (require 'cl)                           ; elisp has no flet by default
;; (with-temp-buffer
;;   (let ((envfile (expand-file-name (concat "~/.keychain/"
;;                                            (system-name) "-sh"))))
;;     (if (file-readable-p envfile)
;;         (flet ((get-value-by-line (lineno)
;;                   (progn
;;                     (goto-line lineno)  ; no error handling here:
;;                     (re-search-forward "=\\([[:ascii:][:digit:]_/]*?\\);")
;;                     (buffer-substring (match-beginning 1) (match-end 1)))))
;;           (insert-file-contents envfile)
;;           (setenv "SSH_AUTH_SOCK" (get-value-by-line 1))
;;           (setenv "SSH_AGENT_PID" (get-value-by-line 2))))))

;; use hippie-expand (mainly abbrev expand and dabbrev):
(global-set-key [(meta ?/)] #'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; make arrow keys work properly in comint buffers:
(add-hook 'comint-mode-hook (lambda ()
                              (define-key comint-mode-map
                                [(up)] 'comint-previous-input)))
(add-hook 'comint-mode-hook (lambda ()
                              (define-key comint-mode-map
                                [(down)] 'comint-next-input)))

;; Make sure script files are executable after save:
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; always use auto-fill mode:
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; I use octave more than obj-c in general:
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;; markdown mode:
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing markdown-formatted documents" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; open jar files as well:
(add-to-list 'auto-mode-alist '("\\.jar\\'" . archive-mode))

(eval-after-load "sql"
  '(load-library "sql-indent"))

;;; elscreen provides enough "frame" management for me:
(setq woman-use-own-frame nil)

;;; Programming modes: enable "FIXME/TODO/etc" highlighting:
(autoload 'highlight-fixmes-mode "highlight-fixmes-mode"
  "Highlighting of FIXMEs, TODOs, etc" t)
(enable-minor-mode-for
 highlight-fixmes-mode '(c python lisp LaTeX js2 haskell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'flymake nil t)
  (macrolet ((remove-alist (lst key)
                           `(setq ,lst (delq (assoc ,key ,lst) ,lst))))
   ;; doesn't seem to work for java or tex, currently:
   (remove-alist flymake-allowed-file-name-masks "\\.java\\'")
   (remove-alist flymake-allowed-file-name-masks "\\.tex\\'")
   (remove-alist flymake-allowed-file-name-masks "[0-9]+\\.tex\\'")
   ;; doesn't work with xml (needs program 'xml', not sure which that
   ;; is for a start!), and nxml provides validation anyway:
   (remove-alist flymake-allowed-file-name-masks "\\.xml\\'")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hideshow:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'hs-minor-mode "hideshow" "Enable code folding" t)
(add-to-list 'load-path (concat *mh/lisp-base* "hideshow-org"))
(autoload 'hs-org/minor-mode "hideshow-org" "Code-folding using the TAB key" t)
;; (eval-after-load "hideshow"
;;   '(progn
;;      (load-library "hideshowvis")
;;      (add-hook 'hs-minor-mode-hook 'hideshowvis-enable)))
;;; add hooks here too
(enable-minor-mode-for hs-org/minor-mode '(emacs-lisp lisp inferior-lisp))
;; (enable-minor-mode-for hs-minor-mode '(emacs-lisp lisp inferior-lisp))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compression; edit compressed kml files too:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "jka-compr"
  '(unless (member "\\.kmz\\'"
                   (mapcar (lambda (vec) (elt vec 0))
                           jka-compr-compression-info-list))
     (add-to-list 'jka-compr-compression-info-list
                  ;; Basically just copying the gzip entry:
                  ["\\.kmz\\'"
                   "compressing" "gzip" ("-c" "-q")
                   "uncompressing" "gzip" ("-c" "-d" "-q")
                   nil t "PK"])
     ;; if already enabled then toggle to get our addition recognised (note
     ;; no `auto-compression-mode' variable in xemacs 21)
     (when jka-compr-added-to-file-coding-system-alist
       (auto-compression-mode 0)
       (auto-compression-mode 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
