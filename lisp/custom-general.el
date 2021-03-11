;;; custom-general.el --- Miscellaneous customisations; mostly simple tweaks
;;; Miscellaneous customisations; mostly one-liner tweaks of
;;; appearance and functionality.

;;; Commentary:
;; 

;;; Keep keys up to date:
(use-package gnu-elpa-keyring-update)

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
(use-package yaml-mode :init (add-hook 'yaml-mode-hook #'turn-off-auto-fill))
(use-package poly-ansible) ; poly-mode that combines jinja + yml mode for ansible

;;; Filename in frame title:
(setq frame-title-format
      '(buffer-file-name "%f"
                         (dired-directory dired-directory "%b")))

;;; Create dir-locals file with
;;; ((yaml-mode
;;;   (ansible-vault-password-file . "/home/notroot/.ansible-vault/custom_vault_pass")))
(defun ansible-vault-mode-maybe ()
  (when (and (derived-mode-p 'yaml-mode)
             (ansible-vault--is-vault-file))
    (ansible-vault-mode 1)))
(add-hook 'hack-local-variables-hook #'ansible-vault-mode-maybe)
(use-package ansible-vault
  :after yaml-mode)

(use-package which-key
  :demand t
  :diminish which-key-mode
  :config (which-key-mode 1))
;;; Language-server integration.  eglot is the other choice:
;;; Needs path to elixir_ls installation added to `exec-path'
(use-package lsp-mode
  ;; Add to this list as necessary; using prog-mode was too annoying:
  :hook ((dart-mode
          elixir-mode
          clojure-mode
          clojurec-mode
          clojurescript-mode) . lsp)
  :init (setq lsp-keymap-prefix "C-c C-l"
              lsp-file-watch-threshold 10000)
  :config (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  :commands lsp
  :bind ("C-c C-d" . lsp-describe-thing-at-point))
(use-package company-lsp
  :after lsp-mode
  :commands company-lsp)
(use-package helm-lsp
  :after lsp-mode
  :commands (helm-lsp-code-actions
             helm-lsp-workspace-symbol))
;;; debugger support:
(use-package dap-mode
  :after lsp-mode)
(use-package dap-elixir
  :ensure nil
  :after dap-mode)


(use-package vlf)
(use-package vlf-setup
  :ensure nil
  :after vlf)

(setq-default save-place t)
(setq save-place-file (expand-file-name "saved.places" user-emacs-directory))

;;; isearch,show counts:
(setq isearch-lazy-count t)

;;; Quit emacs (??) easier:
(defalias 'sbke 'save-buffers-kill-emacs)

;; ;;; http://irreal.org/blog/?p=2832
;; (set-fontset-font "fontset-default" nil
;;                   (font-spec :size 20 :name "Symbola"))
(set-fontset-font t 'symbol "Emoji One" nil)
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

;;; Default to UTF-8 (mostly useful for windows, but let's make it
;;; general); https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs:
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;;; Default to text-mode (in part, because smartparens behaves better there):
(setq-default major-mode 'text-mode)

;; use font lock where possible:
(global-font-lock-mode t)
;;; I really should have been using this all along:
(add-hook 'prog-mode-hook 'subword-mode)
;;; Fancy symbols bandwagon:
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
;; don't use those irritating ~ backup files:
(setq backup-inhibited t)
;; work with compressed files:
(auto-compression-mode 1)
;; update files changed on disk (mainly for use with dropbox):
(global-auto-revert-mode 1)
(diminish 'auto-revert-mode)
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
(defun mh/turn-on-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'mh/turn-on-show-trailing-whitespace))
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
(setq split-height-threshold nil)       ; Always split side-by-side if possible

(setq calc-show-banner nil)

(use-package unfill
  :bind (([remap fill-paragraph] . unfill-toggle)))

;;; Code folding:

(use-package hideshow
  :init
  (defun toggle-fold ()
    (interactive)
    (save-excursion
      (end-of-line)
      (hs-toggle-hiding)))
  ;; https://emacs.stackexchange.com/a/20925
  (defun toggle-fold-all ()
    (interactive)
    (let ((starting-ov-count (length (overlays-in (point-min) (point-max)))))
      (hs-hide-all)
      (when (equal (length (overlays-in (point-min) (point-max))) starting-ov-count)
        (hs-show-all))))
  (enable-minor-mode-for hs-minor-mode '(prog))
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
         ("M-o" . toggle-fold)
         ("M-O" . toggle-fold-all)))

;;; project mode:
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (progn
    (projectile-global-mode 1)
    (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile
          ;; http://iqbalansari.github.io/blog/2014/02/22/switching-repositories-with-magit/
          ;; http://irreal.org/blog/?p=4177
          magit-repository-directories
          (mapcar (lambda (dir)
                    (cons (substring dir 0 -1) 0))
                  (nreverse
                   (remove-if-not (lambda (project)
                                    (file-directory-p (concat project "/.git/")))
                                  (projectile-relevant-known-projects))))))
  :diminish projectile-mode)
(use-package helm-projectile
  :config (helm-projectile-on))

(use-package multiple-cursors
  :bind (("C-!" . mc/edit-lines)
         ("C->" . mc/mark-more-like-this-extended)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/toggle-cursor-on-click)))

(use-package smartscan
  :custom (smartscan-symbol-selector "symbol")
  :hook (prog-mode . smartscan-mode-turn-on))

;;; Visual rectangle editing: /why/ the hell is this buried in a
;;; package that makes emacs act more like windows??  Anyway:
(cua-selection-mode t)                ;; Also disables the CUA keys
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands

(use-package default-text-scale
  :demand t
  :config (default-text-scale-mode))

;;; expand-region; see http://emacsrocks.com/e09.html
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

;;; Move text up and down:
(use-package move-text
  :bind (("C-S-p" . move-text-up)
         ("C-S-n" . move-text-down)))

;;; visual navigation enhancements:
(use-package avy
  :bind (("M-g g" . avy-goto-line))
  :init (avy-setup-default))            ; C-' from isearch

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
(add-hook 'yas-minor-mode-hook (lambda () (yas-activate-extra-mode 'fundamental-mode)))
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode-on)
  :init (autoload 'yas-hippie-try-expand "yasnippet")
  :config
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
  (yas-load-directory (concat *mh/init-base* "snippets") t)
  :diminish (yas-minor-mode yas/minor-mode))
(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

;;; Company now seems more active, and in particular clojure-mode
;;; works best with company:
(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode)
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :diminish company-mode)
(use-package company-quickhelp
  :after (company)
  :config (add-hook 'after-init-hook 'company-quickhelp-mode))

;;; paren-matching:
(setq show-paren-delay 0)
(show-paren-mode 1)

;; paper size:
(setq ps-paper-type 'a4)

;; Dired should recursively delete directories after asking:
(use-package dired
  :ensure nil
  :init
  (setq dired-recursive-deletes 'top
        dired-recursive-copies 'top
        dired-dwim-target t)
  :config
  ;; (http://whattheemacsd.com/setup-dired.el-02.html)
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
(use-package dired-x
  :ensure nil
  :init
  (setq dired-omit-files      "\\(^\\..*\\)\\|\\(CVS\\)"
        dired-omit-verbose    nil
        dired-omit-extensions '("~" ".bak" ".pyc" ".elc"))
  :bind ([remap list-directory] . dired-jump)
  :hook (dired . dired-omit-mode))

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
(use-package dired-quick-sort
  :if (not (memq window-system '(w32)))
  :config (dired-quick-sort-setup))
;;; Tree-based file browser, to see if I use it.
;;; See also the direx-project: functions
(use-package direx
  :bind ("C-x C-j" . direx:find-directory))
(use-package dired-git-info
  :quelpa (dired-git-info
           :fetcher github
           :repo "clemera/dired-git-info")
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; Scroll-bars on the right please:
(if (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode 'right))
;; No startup message please:
(setq inhibit-startup-message t)
;;; Ugh http://yann.hodique.info/blog/rant-obfuscation-in-emacs/
;;; Disabling for now; there's some funny dependency where opening
;;; certain buffers (notably running `list-packages') attempts to save
;;; custom-file and crashes here (but for some reason this doesn't
;;; occur on windows!)
;; (put 'inhibit-startup-echo-area-message 'saved-value
;;      (setq inhibit-startup-echo-area-message (user-login-name)))
;; save a few key strokes from typing 'yes':
(fset 'yes-or-no-p 'y-or-n-p)
;; M-y to browse kill-ring:

(global-set-key (kbd "C-,") 'scroll-up-line)
(global-set-key (kbd "C-.") 'scroll-down-line)

(use-package undo-tree
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode
  :bind ("C-x u" . undo-tree-visualize))

;;; Alternative direction for `delete-indentation'
;;; (http://whattheemacsd.com/key-bindings.el-03.html):
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;;; Can't believe I never went looking for this; great choice of
;;; keybinding too.  Hat-tip to http://irreal.org/blog/?p=1536
;;; Update: trialling zop instead:
(use-package zop-to-char
  :bind (([remap zap-to-char] . zop-to-char)
         ("M-Z" . zop-up-to-char)))

;;; There isn't a default binding for this:
(global-set-key (kbd "M-K") #'kill-paragraph)

;;; Temporarily enable fringe line-numbers during goto-line.
;;; Via http://whattheemacsd.com/key-bindings.el-01.html
;;; This is now native, so we don't need nlinum though:
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (setq display-line-numbers t)
        (call-interactively 'goto-line))
    (setq display-line-numbers nil)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

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

;; use shift-arrow to move between windows:
(windmove-default-keybindings)
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
(use-package toggle-case
  :ensure nil
  :quelpa (toggle-case
           ;; Use a gist file; needed some minor formatting for package.el to be happy:
           :url "https://gist.githubusercontent.com/markhepburn/13bc70c6bdcb3a1b7951/raw/aed52d1999e030db822f8c490bee4bc0c865432d/toggle-case.el"
           :fetcher url)
  :bind (("C-`"   . toggle-case)
         ("C-~"   . toggle-case-backwards)
         ("C-M-`" . toggle-case-by-word)
         ("C-M-~" . toggle-case-by-word-backwards)))

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
;;; More helpful help:
(use-package helpful
  :bind (("C-h k" . helpful-key)
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)))

;; Used to use bs-show, but the ibuffer emulation does it all and more:
(when (require 'ibuf-ext nil t)
  (global-set-key (kbd "C-x C-b") 'ibuffer-bs-show))
(setq-default ibuffer-default-sorting-mode 'major-mode)
(add-hook 'ibuffer-mode-hook (lambda () (hl-line-mode 1)))
(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;; let's play with using C-w to delete words backwards:
(global-set-key (kbd "C-w")     'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

(use-package whole-line-or-region
  :demand t
  :diminish (whole-line-or-region-global-mode whole-line-or-region-local-mode)
  :config
  (whole-line-or-region-global-mode 1)
  (define-key whole-line-or-region-local-mode-map [remap comment-dwim] nil))

;;; Always highlight the current line:
(global-hl-line-mode 1)

;;; Winner-mode; undo for window configurations (key bindings clobber
;;; next and previous-buffer, which I never use):
(winner-mode 1)
(global-set-key (kbd "C-x <left>")  'winner-undo)
(global-set-key (kbd "C-x <right>") 'winner-redo)

;;; Replacing a lot of cruft from my skeleton-pair add-ons!
;;; Smartparens: awesome, but for some reason I forget I still use paredit in lisp modes
(use-package smartparens
  :init
  (setq sp-ignore-modes-list
        '(clojure-mode
          clojurescript-mode
          emacs-lisp-mode
          inferior-emacs-lisp-mode
          inferior-lisp-mode
          lisp-mode
          minibuffer-inactive-mode
          cider-repl-mode
          slime-repl-mode)
        sp-base-key-bindings 'paredit)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode t)
  :diminish smartparens-mode)

(use-package tags-view
  :ensure nil
  :quelpa (tags-view
           :fetcher github
           :repo "markhepburn/tags-view"))

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

;;; Text auto-wrap:
;;; This is a bit old-school, but I'm ok with that, and it's what I'm
;;; used to.  Visual-line-mode seems cool but is as wide as your
;;; window, and the hacks to fix it involve change the margin which
;;; then breaks all kinds of other modes (including magit)
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
(use-package grip-mode
  :after markdown-mode
  :init (setq grip-github-user "markhepburn"
              grip-update-after-change nil)
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

;;; open jar files as well:
(add-to-list 'auto-mode-alist '("\\.jar\\'" . archive-mode))

(use-package sql-indent
  :pin gnu
  :after (sql)
  :hook (sql-mode . sqlind-minor-mode)
  :diminish sqlind-minor-mode)

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
