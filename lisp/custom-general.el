;;; custom-general.el --- Miscellaneous customisations; mostly simple tweaks  -*- lexical-binding: t; -*-
;;; Miscellaneous customisations; mostly one-liner tweaks of
;;; appearance and functionality.

;;; Commentary:
;; 

;;; Keep keys up to date:
(use-package gnu-elpa-keyring-update)

;; Save point position between sessions (hat tip, http://whattheemacsd.com/init.el-03.html)
(require 'saveplace)
;;; Code:

(use-package try :commands (try))

;;; Some miscellaneous packages, no bindings or anything yet:
(use-package diminish :commands (diminish))
(use-package free-keys :commands (free-keys))
(use-package pcre2el :defer t)
(use-package lively :commands (lively lively-region lively-update lively-stop))
(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-list lorem-ipsum-insert-sentences lorem-ipsum-insert-paragraphs))
(use-package rst :mode ("\\.rst\\'" . rst-mode))
(use-package yaml-mode
  :mode "\\.ya?ml\\'")
(use-package poly-ansible ; poly-mode that combines jinja + yml mode for ansible
  :mode ("\\(?:_var\\|task\\)s.*\\.ya?ml\\'" . poly-ansible-mode))

(use-package emacs :ensure nil
  :custom
  (Man-notify-method 'aggressive)        ; match behaviour of woman
  :init
  (setq
   ;; Filename in frame title:
   frame-title-format '(buffer-file-name "%f"
                                         (dired-directory dired-directory "%b"))

   ;; drag and drop:
   mouse-drag-and-drop-region t
   mouse-drag-and-drop-region-cross-program t
   mouse-drag-and-drop-region-scroll-margin 2
   mouse-drag-copy-region t
   dnd-indicate-insertion-point t
   dnd-scroll-margin 2

   ;; isearch,show counts:
   isearch-lazy-count t
   ;; Avoid those .#filename that can break file-watching tools, modification timestamps etc
   save-place-file (expand-file-name "saved.places" user-emacs-directory)
   create-lockfiles nil

   ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
   x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)

   ;; Default is t, which means forward-sentence etc only recognises two spaces.
   sentence-end-double-space nil

   ;; don't use those irritating ~ backup files:
   backup-inhibited t

   default-tab-width 4

   ;; ...and dired buffers too, and don't be chatty:
   global-auto-revert-non-file-buffers t
   auto-revert-verbose nil

   ;; Don't use the disabled-command stuff:
   disabled-command-function nil

   ;; Make sure we always include a trailing newline:
   require-final-newline t

   ;; Default to view-mode for read-only files:
   view-read-only t

;;; Single-frame ediff usage (mainly because floating windows seemed
;;; to interact badly with xmonad, even when explicitly floated):
   ediff-window-setup-function 'ediff-setup-windows-plain
   ediff-split-window-function 'split-window-horizontally

   split-height-threshold nil  ; Always split side-by-side if possible

   ;; calc display:
   calc-make-windows-dedicated t
   calc-kill-line-numbering nil
   calc-show-banner nil

   ;; Ignore .svn/ contents in find-grep:
   ;; http://benjisimon.blogspot.com/2009/01/emacs-tip-slightly-better-find-grep.html
   grep-find-command "find . -type f '!' -wholename '*/.svn/*' -print0 | xargs -0 -e grep -nH -e "

   ;; paren-matching:
   show-paren-delay 0

   ;; paper size:
   ps-paper-type 'a4

   woman-use-own-frame nil

   ;; No startup message please:
   inhibit-startup-message t

   ;; save a few key strokes from typing 'yes' (note need to nil-out use-dialog-box too):
   use-short-answers t
   use-dialog-box nil
   )

  (setq-default
   buffer-file-coding-system 'utf-8
   ;; Default to text-mode (in part, because smartparens behaves better there):
   major-mode 'text-mode
   ;; spaces not tabs:
   indent-tabs-mode nil)

  ;; https://protesilaos.com/codelog/2024-12-11-emacs-diff-save-some-buffers/
  (add-to-list 'save-some-buffers-action-alist
               (list "d"
                     (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                     "show diff between the buffer and its file"))

  ;; Default to UTF-8 (mostly useful for windows, but let's make it
  ;; general); https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs:
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  (save-place-mode 1)

  ;; use font lock where possible:
  (global-font-lock-mode t)
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
  ;; Don't blink the cursor:
  (blink-cursor-mode -1)
  ;; Always highlight the current line:
  (global-hl-line-mode 1)
  ;; trailing whitespace (see also M-x delete-trailing-whitespace):
  (defun mh/turn-on-show-trailing-whitespace ()
    (setq show-trailing-whitespace t))
  ;; high-light selections:
  (transient-mark-mode 1)
  ;; Expected behaviour; delete selection when typing starts:
  (delete-selection-mode 1)
  (global-visual-wrap-prefix-mode 1)

  ;; Alternative direction for `delete-indentation'
  ;; (http://whattheemacsd.com/key-bindings.el-03.html):
  (defun join-line-fowards ()
    (interactive)
    (join-line -1))

  ;; https://mbork.pl/2016-07-25_Making_directories_on_the_fly
  (defun make-parent-directory ()
    "Make sure the directory of `buffer-file-name' exists."
    (make-directory (file-name-directory buffer-file-name) t))
  (add-hook 'find-file-not-found-functions #'make-parent-directory)

  ;; :config
  ;; disable GC while minibuffer is active for snappier performance:
  (defun mh/minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun mh/minibuffer-exit-hook ()
    (setq gc-cons-threshold (* 32 1024 1024)))

  :hook ((text-mode . visual-line-mode)
         ((prog-mode text-mode) . mh/turn-on-show-trailing-whitespace)
         (prog-mode . subword-mode)
         ;; Make sure script files are executable after save:
         (after-save . executable-make-buffer-file-executable-if-script-p)
         (minibuffer-setup . mh/minibuffer-setup-hook)
         (minibuffer-exit . mh/minibuffer-exit-hook)
         ;; Text auto-wrap:
         ;; This is a bit old-school, but I'm ok with that, and it's what I'm
         ;; used to.  Visual-line-mode seems cool but is as wide as your
         ;; window, and the hacks to fix it involve change the margin which
         ;; then breaks all kinds of other modes (including magit)
         )

  :diminish (auto-revert-mode
             visual-line-mode
             visual-wrap-prefix-mode)

  :bind
  (("C-," . scroll-up-line)
   ("C-." . scroll-down-line)
   ("C-S-s" . isearch-forward-thing-at-point)
   ;; Can't believe I never went looking for this; great choice of
   ;; keybinding too.  Hat-tip to http://irreal.org/blog/?p=1536
   ("M-Z" . zap-up-to-char)
   ;; There isn't a default binding for this:
   ("M-K" . kill-paragraph)
   ;; let's play with using C-w to delete words backwards (Yegge-inspired)
   ("C-w" . backward-kill-word)
   ("C-x C-k" . kill-region)
   ("M-j" . join-line-fowards))

  :mode
  (("\\.lua\\'" . lua-ts-mode)
   ;; Add dockerfile support to auto-mode-alist (borrowing their own regexp):
   ("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode)
   ;; open jar files as well:
   ("\\.jar\\'" . archive-mode)))

(use-package treesit
  :ensure nil
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css "https://github.com/tree-sitter/tree-sitter-css")
               (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (python "https://github.com/tree-sitter/tree-sitter-python")
               (php . ("https://github.com/tree-sitter/tree-sitter-php" "master" "php/src"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  (setq treesit-font-lock-level 3)      ; default 3; max of 4

  (dolist (mapping '((python-mode . python-ts-mode)
                     (haskell-mode . haskell-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (js-mode . js-ts-mode)
                     (javascript-mode . js-ts-mode) ; alias of js-mode, but aliases don't trigger remapping
                     ;(yaml-mode . yaml-ts-mode)    ; yaml-ts-mode is bizarely underpowered, doesn't even do indentation yet
                     ))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (mp-setup-install-grammars)

  ;;; Code folding:
  (use-package treesit-fold
    :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
    :diminish treesit-fold-mode
    :config (global-treesit-fold-mode)
    :hook ((python-ts-mode
            css-ts-mode
            tsx-tx-mode
            js-ts-mode) . treesit-fold-indicators-mode))

  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    :vc (:url "https://github.com/mickeynp/combobulate/")
    :preface
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (setq combobulate-key-prefix "C-c o")

    :hook ((python-ts-mode
            js-ts-mode
            css-ts-mode
            yaml-ts-mode
            typescript-ts-mode
            tsx-ts-mode) . #'combobulate-mode)))

(use-package activity-watch-mode
  :defer 3
  :commands (global-activity-watch-mode))

(use-package editorconfig
  :ensure nil
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

(use-package nerd-icons
  :custom (nerd-icons-font-family "CasKaydiaCove Nerd Font"))
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
(use-package nerd-icons-completion
  :config (nerd-icons-completion-mode))
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :commands (nerd-icons-corfu-formatter)
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; Create dir-locals file with
;; ((yaml-mode
;;   . ((ansible-vault--password-file . "/home/notroot/.ansible-vault/custom_vault_pass")
;;      (ansible-vault-command . "/home/notroot/.virtualenvs/venvname/bin/ansible-vault"))))
(defun ansible-vault-mode-maybe ()
  (when (and (derived-mode-p 'yaml-mode 'yaml-ts-mode)
             (ansible-vault--is-encrypted-vault-file))
    (ansible-vault-mode 1)))
(use-package ansible-vault
  :after (yaml-mode yaml-ts-mode)
  :hook (hack-local-variables . ansible-vault-mode-maybe))

;;; also, apt-install terraform-ls for lsp support
(use-package terraform-mode
  :hook (terraform-mode . outline-minor-mode))

(use-package which-key
  :ensure nil
  :defer 3
  :diminish
  :config (which-key-mode 1))
;;; Language-server integration.  eglot is the other choice:
;;; Needs path to elixir_ls installation added to `exec-path'
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  (lsp-keymap-prefix "C-c l")
  (lsp-lens-enable t)
  (lsp-file-watch-threshold 10000)
  ;; Add to this list as necessary; using prog-mode was too annoying:
  :hook (((dart-mode
           elixir-mode
           clojure-mode
           clojurec-mode
           clojurescript-mode
           csharp-mode
           terraform-mode
           typescript-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  ;; https://www.reddit.com/r/emacs/comments/ql8cyp/comment/hj2k2lh/
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  ;; https://github.com/blahgeek/emacs-lsp-booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  :hook (lsp-completion-mode . my/lsp-mode-setup-completion)
  :diminish lsp-lens-mode
  :bind ("C-c C-d" . lsp-describe-thing-at-point))
(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable nil)           ; "C-c C-l T d" to enable
  (lsp-ui-doc-position 'at-point)
  :hook (lsp-configure . (lambda () (lsp-ui-sideline-enable nil)))) ; "C-c C-l T S" to enable
;;; debugger support:
(use-package dap-mode
  :after lsp-mode)
(use-package dap-elixir
  :ensure nil
  :after dap-mode)

(use-package sudo-edit
  :commands sudo-edit)

;;; On-demand menu for calc:
(use-package casual-calc
  :after calc
  :bind (:map calc-mode-map
         ("C-o" . casual-main-menu)))

(use-package vlf :defer t)
(use-package vlf-setup
  :ensure nil
  :defer 3
  :after vlf)

;;; Quit emacs (??) easier:
(defalias 'sbke 'save-buffers-kill-emacs)

(use-package unfill
  :bind (([remap fill-paragraph] . unfill-toggle)))

(use-package hideshow
  :init
  ;; https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (and (fboundp 'tree-sitter-mode) tree-sitter-mode)
          (treesit-fold-toggle)
        (if (= level 1)
            (pcase last-command
              ('hs-cycle
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))
              ('hs-cycle-children
               ;; TODO: Fix this case. `hs-show-block' needs to be
               ;; called twice to open all folds of the parent
               ;; block.
               (save-excursion (hs-show-block))
               (hs-show-block)
               (setq this-command 'hs-cycle-subtree))
              ('hs-cycle-subtree
               (hs-hide-block))
              (_
               (if (not (hs-already-hidden-p))
                   (hs-hide-block)
                 (hs-hide-level 1)
                 (setq this-command 'hs-cycle-children))))
          (hs-hide-level level)
          (setq this-command 'hs-hide-level)))))
  (defun hs-global-cycle ()
    (interactive)
    (if (and (fboundp 'tree-sitter-mode) tree-sitter-mode)
        (pcase last-command
          ('hs-global-cycle
           (save-excursion (treesit-fold-open-all))
           (setq this-command 'treesit-fold-open-all))
          (_ (treesit-fold-close-all)))
      (pcase last-command
        ('hs-global-cycle
         (save-excursion (hs-show-all))
         (setq this-command 'hs-global-show))
        (_ (hs-hide-all)))))
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind (("C-<tab>" . hs-cycle)
         ;; FIXME: Not sure why this needs iso-lefttab rather than just tab!
         ("C-S-<iso-lefttab>" . hs-global-cycle)))

(use-package multiple-cursors
  :bind (("C-!" . mc/edit-lines)
         ("C->" . mc/mark-more-like-this-extended)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/toggle-cursor-on-click)))

(use-package smartscan
  :custom (smartscan-symbol-selector "symbol")
  :hook (prog-mode . smartscan-mode-turn-on))

(use-package default-text-scale
  ;; Now a single command bound to C-x C-M-+ etc
  :unless (fboundp 'global-text-scale-adjust)
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)
         ("C-M-0" . default-text-scale-reset)))

(use-package olivetti
  :commands olivetti-mode)

(use-package expreg
  :bind (("C-=" . expreg-expand)
         ("C-+" . expreg-contract)))

;;; Move text up and down:
(use-package move-text
  :bind (("C-S-p" . move-text-up)
         ("C-S-n" . move-text-down)))

;;; visual navigation enhancements:
(use-package avy
  :bind (("M-g g" . avy-goto-line)
         :map isearch-mode-map
         ("C-'" . avy-isearch)))            ; C-' from isearch

;;; more specialised "opening" commands; mplayer control:
(use-package mplayer-mode
  :when nil
  :vc (:url "https://github.com/markhepburn/mplayer-mode"))

;;; Code templating:
(add-hook 'yas-minor-mode-hook (lambda () (yas-activate-extra-mode 'fundamental-mode)))
(use-package yasnippet
  :defer 1
  :hook (prog-mode . yas-minor-mode-on)
  :init (autoload 'yas-hippie-try-expand "yasnippet")
  :config
  ;; 't to jit-load snippets:
  (yas-load-directory (concat *mh/init-base* "snippets") t)
  :diminish (yas-minor-mode yas/minor-mode))
(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

;;; Built-in inline completion hints:
(use-package completion-preview
  :ensure nil
  :diminish
  :hook (prog-mode comint-mode)
  :config
  (add-to-list 'completion-preview-commands 'paredit-backward-delete)
  :bind (:map completion-preview-active-mode-map
         ("M-n" . completion-preview-next-candidate)
         ("M-p" . completion-preview-prev-candidate)))

;;; Note, M-space to use orderless filtering
(use-package corfu
  :defer 1
  :init (global-corfu-mode))

;; Add extensions
(use-package cape
  :defer 1
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

;; Dired should recursively delete directories after asking:
(use-package dired
  :ensure nil
  :custom
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'top)
  (dired-listing-switches "-alh --time-style=long-iso")
  (dired-vc-rename-file t)
  (dired-dwim-target t)
  :config
  ;; (http://whattheemacsd.com/setup-dired.el-02.html)
  (defun dired-back-to-top ()
    (interactive)
    (goto-char (point-min))
    (dired-goto-next-nontrivial-file))
  (defun dired-jump-to-bottom ()
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))
  ;; https://www.n16f.net/blog/decluttering-dired-for-peace-of-mind/
  (defun mh/dired-postprocess-ls-output ()
    "Postprocess the list of files printed by the ls program when
executed by Dired."
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        ;; Go to the beginning of the next line representing a file
        (while (null (dired-get-filename nil t))
          (dired-next-line 1))
        (beginning-of-line)
        ;; Narrow to the line and process it
        (let ((start (line-beginning-position))
              (end (line-end-position)))
          (save-restriction
            (narrow-to-region start end)
            (setq inhibit-read-only t)
            (unwind-protect
                (mh/dired-postprocess-ls-line)
              (setq inhibit-read-only nil))))
        ;; Next line
        (dired-next-line 1))))
  (defun mh/dired-postprocess-ls-line ()
    "Postprocess a single line in the ls output, i.e. the information
about a single file. This function is called with the buffer
narrowed to the line."
    ;; Highlight everything but the filename
    (when (re-search-forward directory-listing-before-filename-regexp nil t 1)
      (add-text-properties (point-min) (match-end 0) '(font-lock-face shadow)))
    ;; Hide the link count
    ;; (beginning-of-line)
    ;; (when (re-search-forward " +[0-9]+" nil t 1)
    ;;   (add-text-properties (match-beginning 0) (match-end 0) '(invisible t)))
    )
  :hook (dired-after-readin . mh/dired-postprocess-ls-output)
  :bind (:map dired-mode-map
              ([remap beginning-of-buffer] . dired-back-to-top)
              ([remap end-of-buffer] . dired-jump-to-bottom)))
(use-package dired-x
  :ensure nil
  :custom
  (dired-omit-files      "\\(^\\..*\\)\\|\\(CVS\\)")
  (dired-omit-verbose    nil)
  (dired-omit-extensions '("~" ".bak" ".pyc" ".elc"))
  :bind ([remap list-directory] . dired-jump)
  :hook (dired . dired-omit-mode))

(use-package dired-preview
  :custom
  (dired-preview-display-action-alist-function
   (lambda ()
     '(display-buffer-use-least-recent-window display-buffer-pop-up-window)))
  :hook dired-mode)
(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))
(use-package dired-quick-sort
  :after dired
  :if (not (memq window-system '(w32)))
  :config (dired-quick-sort-setup))
(use-package dired-git-info
  :after dired
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;;; Ugh http://yann.hodique.info/blog/rant-obfuscation-in-emacs/
;;; Disabling for now; there's some funny dependency where opening
;;; certain buffers (notably running `list-packages') attempts to save
;;; custom-file and crashes here (but for some reason this doesn't
;;; occur on windows!)
;; (put 'inhibit-startup-echo-area-message 'saved-value
;;      (setq inhibit-startup-echo-area-message (user-login-name)))

(use-package unscroll :ensure nil :demand t)

(use-package vundo
  :custom (vundo-glyph-alist vundo-unicode-symbols)
  :bind ("C-x u" . vundo))

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

;;; rotate windows too:
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
  :init (pkg-help/url-install
           ;; Use a gist file; needed some minor formatting for package.el to be happy:
           :url "https://gist.githubusercontent.com/markhepburn/13bc70c6bdcb3a1b7951/raw/aed52d1999e030db822f8c490bee4bc0c865432d/toggle-case.el")
  :bind (("C-`"   . toggle-case)
         ("C-~"   . toggle-case-backwards)
         ("C-M-`" . toggle-case-by-word)
         ("C-M-~" . toggle-case-by-word-backwards)))

;; make all buffer-names unique:
(use-package uniquify
  :ensure nil
  :defer 2
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-ignore-buffers-re "^\\*") ; Ignore *scratch*, etc
  (uniquify-after-kill-buffer-p t))

;;; csv-mode; smartparens mode interfers with sexp-command based
;;; navigation:
(use-package csv-mode
  :mode "\\.csv\\'"
  :config
  (smartparens-mode -1))

;; Show docs where available:
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :hook ((emacs-lisp lisp inferior-lisp inferior-emacs-lisp) . eldoc-mode))

;; bind C-h a to 'apropos like in xemacs (not apropos-command as it is
;; in emacs by default)
(global-set-key (kbd "C-h a") 'apropos)
;;; More helpful help:
(use-package helpful
  :bind (("C-h k" . helpful-key)
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)))

;; Used to use bs-show, but the ibuffer emulation does it all and more:
(use-package ibuf-ext
  :ensure nil
  :custom (ibuffer-default-sorting-mode major-mode)
  :bind ("C-x C-b" . ibuffer-bs-show))
(use-package ibuffer-vc
  :commands ibuffer-vc-set-filter-groups-by-vc-root
  :hook
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(use-package whole-line-or-region
  :defer 2
  :diminish (whole-line-or-region-global-mode whole-line-or-region-local-mode)
  :config
  (whole-line-or-region-global-mode 1)
  (define-key whole-line-or-region-local-mode-map [remap comment-dwim] nil))

;;; tab-bar-history-mode is the same as winner-mode, but also respects per-tab history
;;; (key bindings clobber next and previous-buffer, which I never use)
(use-package tab-bar :ensure nil
  :init (tab-bar-history-mode 1)
  :bind (("C-x <left>" . tab-bar-history-back)
         ("C-x <right>" . tab-bar-history-forward)))

;;; Replacing a lot of cruft from my skeleton-pair add-ons!
;;; Smartparens: awesome, but for some reason I forget I still use paredit in lisp modes
(use-package smartparens-config
  :ensure smartparens
  :custom
  (sp-ignore-modes-list
   '(cider-mode
     cider-repl-mode
     clojure-mode
     clojurescript-mode
     emacs-lisp-mode
     inferior-emacs-lisp-mode
     inferior-lisp-mode
     lisp-mode
     minibuffer-inactive-mode
     nxml-mode
     slime-repl-mode
     sly-mrepl-mode))
  (sp-base-key-bindings 'paredit)
  :hook (prog-mode . smartparens-mode)
  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  :diminish smartparens-mode)

(use-package tags-view
  :when nil
  :vc (:url "https://github.com/markhepburn/tags-view"))

;;; use hippie-expand (mainly abbrev expand and dabbrev):
;;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(use-package hippie-exp :ensure nil
  :config (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  :bind ([remap dabbrev-expand] . hippie-expand))

;;; Visual regexp support:
(use-package visual-regexp
  :bind ("C-x r q" . vr/query-replace))

;; make arrow keys work properly in comint buffers:
(use-package comint
  :ensure nil
  :bind (:map comint-mode-map
              ("<up>" . comint-previous-input)
              ("<down>" . comint-next-input)))

;;; markdown mode:
(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  ;; Custom fill-break predicate to consider Liquid tags as well (since
  ;; I mostly use markdown in conjunction with Jekyll):
  (defun mh/liquid-nobreak-p ()
    (looking-back "({%[^%]*" nil nil))
  :hook (markdown-mode
         . (lambda ()
             (add-hook (make-local-variable 'fill-nobreak-predicate) 'mh/liquid-nobreak-p))))
(use-package grip-mode
  :after markdown-mode
  :custom
  (grip-github-user "markhepburn")
  (grip-update-after-change nil)
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

(use-package sql-indent
  :pin gnu
  :after sql
  :hook (sql-mode . sqlind-minor-mode)
  :diminish sqlind-minor-mode)

;;; Programming modes: enable "FIXME/TODO/etc" highlighting.
(use-package hl-todo
  :hook prog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compression; edit compressed kml files too:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package jka-compr
  :ensure nil
  :unless (member "\\.kmz\\'"
                  (mapcar (lambda (vec) (elt vec 0))
                          jka-compr-compression-info-list))
  :config
  (add-to-list 'jka-compr-compression-info-list
               ;; Basically just copying the gzip entry:
               ["\\.kmz\\'"
                "compressing" "gzip" ("-c" "-q")
                "uncompressing" "gzip" ("-c" "-d" "-q")
                nil t "PK"])
  (jka-compr-update))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-general)

;;; custom-general.el ends here
