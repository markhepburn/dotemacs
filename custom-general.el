;;; custom-general.el --- Miscellaneous customisations; mostly simple tweaks
;;; Miscellaneous customisations; mostly one-liner tweaks of
;;; appearance and functionality.

;;; Commentary:
;; 


;; Save point position between sessions (hat tip, http://whattheemacsd.com/init.el-03.html)
(require 'saveplace)
;;; Code:

;;; Some miscellaneous packages, no bindings or anything yet:
(use-package diminish)
(use-package flycheck)
(use-package free-keys)
(use-package pcre2el)
(use-package lively)
(use-package lorem-ipsum)
(use-package rst)
(use-package yaml-mode)
;;; Utility packages; installed as dependences anyway, mentioned as a reminder:
(use-package dash)
(use-package s)

(setq-default save-place t)
(setq save-place-file (expand-file-name "saved.places" user-emacs-directory))

;;; Quit emacs (??) easier:
(defalias 'sbke 'save-buffers-kill-emacs)

;;; http://irreal.org/blog/?p=2832
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

;;; Default to UTF-8 (mostly useful for windows, but let's make it
;;; general); https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs:
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; use font lock where possible:
(global-font-lock-mode t)
;;; I really should have been using this all along:
(add-hook 'prog-mode-hook 'subword-mode)
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
;;; Don't use the disabled-command stuff:
(setq disabled-command-function nil)
;; Don't blink the cursor:
(blink-cursor-mode -1)
;;; next-line should go next text line (old default), not visual line (from
;;; http://bryan-murdock.blogspot.com/2009/03/emacs-next-line-changed-behavior.html
;;; originally, but things seem to have changed slightly since then):
(setq line-move-visual nil)
;;; trailing whitespace (see also M-x delete-trailing-whitespace):
(setq-default show-trailing-whitespace t)
(defun mh/disable-show-ws () (setq show-trailing-whitespace nil))
(dolist (hook '(eshell-mode-hook))
  (add-hook hook 'mh/disable-show-ws))
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
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;;; project mode:
(use-package helm-projectile)
(use-package projectile
  :config
  (progn
    (projectile-global-mode 1)
    (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile
          ;; http://iqbalansari.github.io/blog/2014/02/22/switching-repositories-with-magit/
          ;; http://irreal.org/blog/?p=4177
          magit-repository-directories
          (mapcar (lambda (dir)
                    (substring dir 0 -1))
                  (nreverse
                   (remove-if-not (lambda (project)
                                    (file-directory-p (concat project "/.git/")))
                                  (projectile-relevant-known-projects))))))
  :diminish projectile-mode)

(use-package multiple-cursors
  :bind (("C-!" . mc/edit-lines)
         ("C->" . mc/mark-more-like-this-extended)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;;; Visual rectangle editing: /why/ the hell is this buried in a
;;; package that makes emacs act more like windows??  Anyway:
(cua-selection-mode t)                ;; Also disables the CUA keys
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands

;;; expand-region; see http://emacsrocks.com/e09.html
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;; Move text up and down:
(use-package move-text
  :bind (("C-S-p" . move-text-up)
         ("C-S-n" . move-text-down)))

;;; more specialised "opening" commands; mplayer control:
(use-package mplayer-mode
  :ensure nil
  :quelpa (mplayer-mode
           :fetcher github
           :repo "markhepburn/mplayer-mode"))

;;; Ignore .svn/ contents in find-grep:
;;; http://benjisimon.blogspot.com/2009/01/emacs-tip-slightly-better-find-grep.html
(setq grep-find-command
  "find . -type f '!' -wholename '*/.svn/*' -print0 | xargs -0 -e grep -nH -e ")

;;; Code templating:
(use-package yasnippet
  :config (progn
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
            (yas-load-directory (concat *mh/lisp-base* "snippets") t))
  :diminish yas/minor-mode)

;;; Company now seems more active, and in particular clojure-mode
;;; works best with company:
(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode)
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :diminish company-mode)

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
;;; Bizarrely, zip files aren't handled out-of-the-box by dired:
(after 'dired-aux
  (add-to-list 'dired-compress-file-suffixes
               '("\\.zip\\'" "" "unzip")))
(use-package peep-dired
  :init (require 'dired)
  :bind (:map dired-mode-map
              ("o" . peep-dired)))
(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; Scroll-bars on the right please:
(if (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode 'right))
;; No startup message please:
(setq inhibit-startup-message t)
;;; Ugh http://yann.hodique.info/blog/rant-obfuscation-in-emacs/
(put 'inhibit-startup-echo-area-message 'saved-value
     (setq inhibit-startup-echo-area-message (user-login-name)))
;; save a few key strokes from typing 'yes':
(fset 'yes-or-no-p 'y-or-n-p)
;; M-y to browse kill-ring:

(when (require 'atim-unscroll nil t)
  (atim-unscroll-global-mode)
  (diminish 'atim-unscroll-mode))

(use-package undo-tree
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode)

;;; Alternative direction for `delete-indentation'
;;; (http://whattheemacsd.com/key-bindings.el-03.html):
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;;; Can't believe I never went looking for this; great choice of
;;; keybinding too.  Hat-tip to http://irreal.org/blog/?p=1536
(use-package misc
  :ensure nil
  :bind ("M-Z" . zap-up-to-char))

;;; Temporarily enable fringe line-numbers during goto-line.
;;; Via http://whattheemacsd.com/key-bindings.el-01.html
(use-package nlinum
  :config (progn
            (defun goto-line-with-feedback ()
              "Show line numbers temporarily, while prompting for the line number input."
              (interactive)
              (unwind-protect
                  (progn
                    (nlinum-mode 1)
                    (call-interactively 'goto-line))
                (nlinum-mode -1)))
            (global-set-key [remap goto-line] 'goto-line-with-feedback)))

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
(use-package win-switch
  :init (setq win-switch-idle-time 1 ; Can always hit <return> to exit sooner.
              win-switch-other-window-first nil)
  :config (progn
            (defun mh/win-switch-setup-keys-vistyle (&rest dispatch-keys)
              "Restore default key commands and bind global dispatch keys.
Under this setup, keys i, j, k, and l will switch windows,
respectively, up, left, down, and right, with other functionality
bound to nearby keys.  The arguments DISPATCH-KEYS, if non-nil,
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
            (mh/win-switch-setup-keys-vistyle (kbd "C-x o"))))
;;; and rotate windows too:
(use-package buffer-move
  :bind (("M-g <left>"  . buf-move-left)
         ("M-g h"       . buf-move-left)
         ("M-g <right>" . buf-move-right)
         ("M-g l"       . buf-move-right)
         ("M-g <up>"    . buf-move-up)
         ("M-g k"       . buf-move-up)
         ("M-g <down>"  . buf-move-down)
         ("M-g j"       . buf-move-down)))

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

;;; csv-mode; smartparens mode interfers with sexp-command based
;;; navigation:
(use-package csv-mode
  :config (progn
            (smartparens-mode -1)
            (auto-fill-mode -1)))

;; Show docs where available:
(enable-minor-mode-for eldoc-mode '(emacs-lisp lisp inferior-lisp ielm))
(after 'eldoc (diminish 'eldoc-mode))

;; view pdfs etc inline:
(autoload 'doc-view "doc-view" "View pdfs inline" t)

;; search through open buffers:
(autoload 'grep-buffers "grep-buffers" "Grep through open buffers" t)

;;; use ack (note that I have two versions of this command installed):
(autoload 'ack-grep "ack" "Intelligent form of grep-find" t)

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
;;; Smartparens: awesome, but for some reason I forget I still use paredit in lisp modes
(use-package smartparens-mode
  :ensure smartparens
  :init
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
  :diminish smartparens-mode
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (diminish 'smartparens-mode))

;; use hippie-expand (mainly abbrev expand and dabbrev):
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;;; Visual regexp support:
(use-package visual-regexp
  :bind ("C-x r q" . vr/query-replace))

;; make arrow keys work properly in comint buffers:
(after 'comint
  (add-hook 'comint-mode-hook (lambda ()
                                (define-key comint-mode-map
                                  (kbd "<up>") 'comint-previous-input)))
  (add-hook 'comint-mode-hook (lambda ()
                                (define-key comint-mode-map
                                  (kbd "<down>") 'comint-next-input)))
;;; http://emacsredux.com/blog/2015/01/18/clear-comint-buffers/
  (defun comint-clear-buffer ()
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))
;;; Binding echos that of cider-nrepl:
  (define-key comint-mode-map (kbd "C-c M-o") #'comint-clear-buffer))


;; Make sure script files are executable after save:
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Always auto-fill in text:
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; I use octave more than obj-c in general:
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;; markdown mode:
(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (progn
    ;; Custom fill-break predicate to consider Liquid tags as well (since
    ;; I mostly use markdown in conjunction with Jekyll):
    (defun mh/liquid-nobreak-p ()
      (looking-back "({%[^%]*"))

    (add-hook 'markdown-mode-hook
              (lambda ()
                (add-hook 'fill-nobreak-predicate 'mh/liquid-nobreak-p)))))

;;; open jar files as well:
(add-to-list 'auto-mode-alist '("\\.jar\\'" . archive-mode))

(use-package sql-indent
  :after (sql))

;;; elscreen provides enough "frame" management for me:
(setq woman-use-own-frame nil)

;;; Programming modes: enable "FIXME/TODO/etc" highlighting.
(use-package fic-mode
  :config (add-hook 'prog-mode-hook 'fic-mode)
  :diminish fic-mode)

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

(provide 'custom-general)

;;; custom-general.el ends here
