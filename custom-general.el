;;; Miscellaneous customisations; mostly one-liner tweaks of
;;; appearance and functionality.

;; Save point position between sessions (hat tip, http://whattheemacsd.com/init.el-03.html)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "saved.places" user-emacs-directory))

;;; Trim the modeline (http://whattheemacsd.com/init.el-04.html).  We
;;; require this early, and don't ignore errors either, because it
;;; will wind up sprinkled all through the config.
(require 'diminish)

;;; Quit emacs (??) easier:
(defalias 'sbke 'save-buffers-kill-emacs)

;; use font lock where possible:
(global-font-lock-mode t)
;;; I really should have been using this all along:
(global-subword-mode 1)
;; don't use those irritating ~ backup files:
(setq backup-inhibited t)
;; work with compressed files:
(auto-compression-mode 1)
;; update files changed on disk (mainly for use with dropbox):
(global-auto-revert-mode 1)
;;; ...and dired buffers too, and don't be chatty:
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
;; don't show toolbar:
(tool-bar-mode -1)
;; hide the menu-bar by default (accessible by C-right-click):
(menu-bar-mode -1)
;; line and column-number modes:
(line-number-mode 1)
(column-number-mode 1)
;; Don't blink the cursor:
(blink-cursor-mode -1)
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
;; (setq view-read-only t)
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

  (setq ido-enable-flex-matching t)

  ;; Display candidates vertically:
  (when (require 'ido-vertical-mode nil t)
    (ido-vertical-mode 1))

  ;; Smex: ido for M-x.
  (setq smex-save-file "~/.emacs.d/smex.save")
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
    (global-set-key (kbd "M-'") 'ido-goto-symbol))

  ;; Finally, activate:
  (ido-mode t)

  ;; Better matching:
  (when (require 'flx-ido nil t)
    (flx-ido-mode 1)
    (setq ido-use-faces nil))

  ;; ...extend its reach (see http://whattheemacsd.com//setup-ido.el-01.html):
  (when (require 'ido-ubiquitous nil t)
    (ido-ubiquitous-mode 1)

    ;; Fix ido-ubiquitous for newer packages
    (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
      `(eval-after-load ,package
         '(defadvice ,cmd (around ido-ubiquitous-new activate)
            (let ((ido-ubiquitous-enable-compatibility nil))
              ad-do-it))))
    (ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
    (ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet)))

;;; project mode:
(after 'projectile
  (projectile-global-mode)
  (diminish 'projectile-mode))

(when (require 'multiple-cursors nil t)
  (global-set-key (kbd "C-C C-C") 'mc/edit-lines)
  (global-set-key (kbd "C->")     'mc/mark-next-like-this)
  (global-set-key (kbd "C-<")     'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;;; Visual rectangle editing: /why/ the hell is this buried in a
;;; package that makes emacs act more like windows??  Anyway:
(cua-selection-mode t)                ;; Also disables the CUA keys
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands

;;; expand-region; see http://emacsrocks.com/e09.html
(autoload 'er/expand-region "expand-region" t)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; experiment with find-file-at-point a bit (don't use their
;;; pre-configured bindings, as they will presumably over-write the
;;; ido ones!)
(autoload 'ffap "ffap" "Find file at point functionality" t)
(global-set-key (kbd "C-x M-f") 'ffap)

;;; Move text up and down:
(autoload 'move-text-up   "move-text" "Shuffle text around" t)
(autoload 'move-text-down "move-text" "Shuffle text around" t)
(global-set-key (kbd "C-S-p") 'move-text-up)
(global-set-key (kbd "C-S-n") 'move-text-down)

;;; shortcut for browse url at point:
(global-set-key (kbd "C-x M-b") 'browse-url-at-point)

;;; more specialised "opening" commands; mplayer control:
(autoload 'mplayer-find-file "mplayer-mode" "Control mplayer from emacs while editing a file" t)

;;; Ignore .svn/ contents in find-grep:
;;; http://benjisimon.blogspot.com/2009/01/emacs-tip-slightly-better-find-grep.html
(setq grep-find-command
  "find . -type f '!' -wholename '*/.svn/*' -print0 | xargs -0 -e grep -nH -e ")

;;; Code templating:
(when (require 'yasnippet nil t)
  (yas-global-mode 1)
  ;; http://iany.me/2012/03/use-popup-isearch-for-yasnippet-prompt/
  (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
    (when (featurep 'popup)
      (popup-menu*
       (mapcar
        (lambda (choice)
          (popup-make-item
           (or (and display-fn (funcall display-fn choice))
               choice)
           :value choice))
        choices)
       :prompt prompt
       ;; start isearch mode immediately
       :isearch t)))
  (setq yas-prompt-functions '(yas-popup-isearch-prompt yas-no-prompt))
  ;; 't to jit-load snippets:
  (yas-load-directory (concat *mh/lisp-base* "snippets") t)
  (diminish 'yas-minor-mode))

;;; Auto-complete mode: all the cool kids are using it, and it's more
;;; active than company mode (see
;;; http://stackoverflow.com/questions/4704748/emacs-completion-autocomplete-or-company).
;;; Also, eclim can use it, and that looks quite handy.
(when (require 'auto-complete-config nil t)
  ;; no forced arrow keys, thank you:
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous)

  (ac-config-default)

  (diminish 'auto-complete-mode))

;;; paren-matching:
(setq show-paren-delay 0)
(show-paren-mode 1)

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
                    dired-omit-verbose    nil
                    dired-omit-extensions '("~" ".bak" ".pyc" ".elc"))
              (dired-omit-mode 1))))
(add-hook 'dired-mode-hook (lambda () (hl-line-mode 1)))
;;; Make dired buffer navigation a bit more friendly
;;; (http://whattheemacsd.com/setup-dired.el-02.html)
;;; Note, dired-mode-map isn't defined at load-time, so delay until dired appears:
(after 'dired
  (defun dired-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (dired-next-line (if dired-omit-mode 2 4)))
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom))

;; Scroll-bars on the right please:
(if (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode 'right))
;; No startup message please:
(setq inhibit-startup-message t)
;; save a few key strokes from typing 'yes':
(fset 'yes-or-no-p 'y-or-n-p)
;; M-y to browse kill-ring:

(when (require 'atim-unscroll nil t)
  (atim-unscroll-global-mode)
  (diminish 'atim-unscroll-mode))

(when (require 'undo-tree nil t)
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode))

;;; Alternative direction for `delete-indentation'
;;; (http://whattheemacsd.com/key-bindings.el-03.html):
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;;; I don't know why this seemed to suddenly change; make backspace
;;; work again in isearch-mode anyway (see also C-M-w which does the
;;; same thing, and C-M-y and C-y as well):
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

;;; restore backspace behaviour in calc too; this will get disrupted
;;; with the global-set-key used below (for the autopair stuff):
(add-hook 'calc-mode-hook
          (lambda () (local-set-key (kbd "<backspace>") 'calc-pop)))

;;; Can't believe I never went looking for this; great choice of
;;; keybinding too.  Hat-tip to http://irreal.org/blog/?p=1536
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;;; Temporarily enable fringe line-numbers during goto-line.
;;; Via http://whattheemacsd.com/key-bindings.el-01.html
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; kill-ring selection:
(when (require 'browse-kill-ring nil t)
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-no-duplicates t) ;...and don't clog it up with duplicates
  (defadvice browse-kill-ring-insert-and-quit (after indent-region activate)
    (if (member major-mode '(emacs-lisp-mode
                             lisp-mode
                             erlang-mode
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
                           c-mode c++-mode objc-mode
                           latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))
(defadvice yank-pop (after indent-region-for-yank-pop activate)
  "If in a programming mode, reindent the region after yanking."
  (if (member major-mode '(emacs-lisp-mode
                           lisp-mode
                           erlang-mode
                           c-mode c++-mode objc-mode
                           latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

;;; Suggestion from http://www.emacswiki.org/emacs-en/KillingAndYanking
;;; Cycle backwards through the kill-ring with meta-shift-y:
(defun yank-pop-backwards ()
  (interactive)
  (yank-pop -1))
(global-set-key (kbd "M-Y") 'yank-pop-backwards)

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
    (win-switch-set-keys '("O") 'other-frame)
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
(global-set-key (kbd "M-g h")       'buf-move-left)
(global-set-key (kbd "M-g <right>") 'buf-move-right)
(global-set-key (kbd "M-g l")       'buf-move-right)
(global-set-key (kbd "M-g <up>")    'buf-move-up)
(global-set-key (kbd "M-g k")       'buf-move-up)
(global-set-key (kbd "M-g <down>")  'buf-move-down)
(global-set-key (kbd "M-g j")       'buf-move-down)

;; vi-like case toggle:
(when (require 'toggle-case nil t)
  (global-set-key (kbd "C-`")   'joc-toggle-case)
  (global-set-key (kbd "C-~")   'joc-toggle-case-backwards)
  (global-set-key (kbd "C-M-`") 'joc-toggle-case-by-word)
  (global-set-key (kbd "C-M-~") 'joc-toggle-case-by-word-backwards))

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
(enable-minor-mode-for eldoc-mode '(emacs-lisp lisp inferior-lisp ielm))
(after 'eldoc (diminish 'eldoc-mode))

;; view pdfs etc inline:
(autoload 'doc-view "doc-view" "View pdfs inline" t)

;; search through open buffers:
(autoload 'grep-buffers "grep-buffers" "Grep through open buffers" t)

;;; use ack (note that I have two versions of this command installed):
(autoload 'ack-grep "ack" "Intelligent form of grep-find" t)

;;; Try ag (the silver-searcher) as well:
(setq ag-highlight-search t)

;; Twitter (Used to use twit.el, but that doesn't support OAuth):
(autoload 'twit "twittering-mode" "Twittering mode" t)
(autoload 'twittering-update-status-interactive "twittering-mode" "Twitter status update" t)
(defalias 'twit-post 'twittering-update-status-interactive
  "Post from the minibuffer (or whatever it is set to) without invoking twit.")
(after "twittering-mode" (twittering-icon-mode))
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
(global-set-key (kbd "C-h a") 'apropos)

;; Used to use bs-show, but the ibuffer emulation does it all and more:
(when (require 'ibuf-ext nil t)
  (global-set-key (kbd "C-x C-b") 'ibuffer-bs-show))
(setq-default ibuffer-default-sorting-mode 'major-mode)
(add-hook 'ibuffer-mode-hook (lambda () (hl-line-mode 1)))

;; let's play with using C-w to delete words backwards:
(global-set-key (kbd "C-w")     'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; high-light line mode is occasionally useful:
(autoload 'hll-toggle-line "hll"
  "High-light lines in buffer for clearer browsing" t)
(global-set-key (kbd "C-x t h") 'hll-toggle-line)
(global-set-key (kbd "C-x t p") 'hll-prev-highlight)
(global-set-key (kbd "C-x t n") 'hll-next-highlight)
(global-set-key (kbd "C-x t u") 'hll-unhighlight-buffer)
;;; and highlight the current line regardless:
(global-hl-line-mode 1)

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

;;; Winner-mode; undo for window configurations (key bindings clobber
;;; next and previous-buffer, which I never use):
(winner-mode 1)
(global-set-key (kbd "C-x <left>")  'winner-undo)
(global-set-key (kbd "C-x <right>") 'winner-redo)

;;; Replacing a lot of cruft from my skeleton-pair add-ons!
(setq sp-ignore-modes-list
      '(clojure-mode
        emacs-lisp-mode
        inferior-emacs-lisp-mode
        inferior-lisp-mode
        lisp-mode
        minibuffer-inactive-mode
        nrepl-mode
        slime-repl-mode)
      sp-base-key-bindings 'paredit)
(when (require 'smartparens-config nil t)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (diminish 'smartparens-mode))


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
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;;; Visual regexp support:
(global-set-key (kbd "C-x r q") 'vr/query-replace)

;; make arrow keys work properly in comint buffers:
(add-hook 'comint-mode-hook (lambda ()
                              (define-key comint-mode-map
                                (kbd "<up>") 'comint-previous-input)))
(add-hook 'comint-mode-hook (lambda ()
                              (define-key comint-mode-map
                                (kbd "<down>") 'comint-next-input)))

;; Make sure script files are executable after save:
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Always auto-fill in text:
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; I use octave more than obj-c in general:
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;; markdown mode:
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing markdown-formatted documents" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; open jar files as well:
(add-to-list 'auto-mode-alist '("\\.jar\\'" . archive-mode))

(after "sql"
  (load-library "sql-indent"))

;;; elscreen provides enough "frame" management for me:
(setq woman-use-own-frame nil)

;;; Programming modes: enable "FIXME/TODO/etc" highlighting.  This
;;; gets a bit messy, because it doesn't play nicely at all with
;;; web-mode, and we can't even just disable it in web-mode-hook so it
;;; has to be special-cased here:
(when (require 'fic-ext-mode nil t)
  (add-hook 'prog-mode-hook (lambda ()
                              (unless (eq major-mode 'web-mode)
                                (fic-ext-mode))))
  (diminish 'fic-ext-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compression; edit compressed kml files too:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after "jka-compr"
  (unless (member "\\.kmz\\'"
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
