;;; init.el --- My Emacs Initialisation Load File
;;; Overview: This file is the root of customisations; it should be
;;; kept under version control and just referred to by symlinking
;;; ~/.emacs to it.  It mostly just loads individual groups of
;;; customisations (either generic, or mostly mode- or task-specific).
;;; This makes it easier to toggle them on or off, and easier to
;;; follow for the most part.  The convention is that all files loaded
;;; from here begin with "custom-"; this is simply so that they are
;;; grouped together in the base directory (the exception is the
;;; platform-specific customisations, which for simplicity is just the
;;; platform name as reported by 'system-type).


;; http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;; Measure load-time for .emacs:
;;; Code:

(defvar *mh/load-start-time* (current-time))


;;; Commentary:
;; 

(require 'cl)

(defvar *mh/lisp-base* (file-name-directory (file-truename user-init-file))
  "Base code directory; contains free-standing code and
subdirectories of other projects.  Defaults to the directory
  containing the user's initialisation file (including following
  symlinks).")
(add-to-list 'load-path *mh/lisp-base*)

(setq el-get-dir (concat *mh/lisp-base* "el-get/"))
(setq el-get-emacswiki-base-url "http://www.emacswiki.org/emacs/download/")
(add-to-list 'load-path (concat el-get-dir "el-get"))

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

(unless (featurep 'cedet)
  (when (file-directory-p "~/.emacs.d/el-get/cedet")
    (progn
      (add-to-list 'load-path  "~/.emacs.d/el-get/cedet")
      (load-file "~/.emacs.d/el-get/cedet/cedet-devel-load.el"))))

;; Additional custom recipes, not yet in the repository:
(setq el-get-sources
      '(
        ;; Over-ride, because I want to use company now, and
        ;; auto-complete gets installed as a dependency (ess, ein,
        ;; jedi) and the el-get recipe for it turns on
        ;; global-auto-complete.  Don't do that:
        (:name auto-complete
               :type github
               :pkgname "auto-complete/auto-complete"
               :depends (popup fuzzy))
        ;; over-ride, so we get a stable marmalade version:
        (:name elnode
               :type elpa
               :depends (fakir web db s)
               :repo ("marmalade" . "http://marmalade-repo.org/packages/"))
        (:name esxml
               :type github
               :pkgname "tali713/esxml")
        (:name http-twiddle
               :type github
               :pkgname "hassy/http-twiddle")
        (:name jsx-mode
               :type github
               :pkgname "jsx/jsx-mode.el"
               :load-path "src")
        (:name mplayer-mode
               :type github
               :pkgname "markhepburn/mplayer-mode")
        (:name move-text
               :type github
               :pkgname "emacsmirror/move-text")
        (:name org-trello
               :type github
               :pkgname "ardumont/org-trello"
               :depends (elnode esxml))
        (:name sql-indent
               :type emacswiki)
        (:name tags-view
               :type github
               :pkgname "markhepburn/tags-view")
        (:name toggle-case
               :type http
               :url "http://www.cs.virginia.edu/~wh5a/personal/Emacs/toggle-case.el")))

;; My installed package list:
(setq *mh/packages*
  '(el-get
    ;distel
    ;eclim

    ag
    auctex
    buffer-move
    cedet
    cider                               ; formerly "nrepl"
    clj-refactor
    cljsbuild-mode
    clojure-mode
    color-theme-zenburn
    company-mode
    company-ghc
    csv-mode
    dash
    diff-hl
    diminish
    ein
    elisp-slime-nav
    emmet-mode
    escreen
    ess
    expand-region
    fic-ext-mode
    flycheck
    free-keys
    git-messenger
    ghc-mod
    helm
    helm-ag
    helm-swoop
    haskell-mode
    haskell-mode-exts
    http-twiddle
    jedi
    js2-mode
    json
    jsx-mode
    less-css-mode
    lively
    lorem-ipsum
    magit
    markdown-mode
    move-text
    mplayer-mode
    multiple-cursors
    org-mode
    org-trello
    paredit
    pcre2el
    pony-mode
    powerline
    projectile
    psvn
    python-django
    rainbow-mode
    rst-mode
    s
    session
    smartparens
    sql-indent
    tags-view
    toggle-case
    undo-tree
    vagrant
    vagrant-tramp
    virtualenvwrapper
    visual-regexp
    web-mode
    win-switch
    yaml-mode
    yasnippet))


(el-get-cleanup *mh/packages*)
(el-get 'sync *mh/packages*)


;;; loaded before anything else because of various macros
;;; (enable-minor-mode-for, after):
(load "custom-functions")

;;; Some settings need to be machine-specific, such as CEDET project
;;; definitions, while others are platform-specific (eg, I use
;;; maxframe on osx, but this is redundant on linux where xmonad takes
;;; care of that).  To do this, load files (if they exist)
;;; corresponding to the reported 'system-type and 'system-name
;;; (#'load will stick the ".el" on the end automatically).  Second
;;; argument 't to 'load means no error if the file doesn't exist.
(load (subst-char-in-string ?/ ?- (symbol-name system-type))
      t)
(load system-name t)       ; Assume for now it is not fully-qualified.

(load "custom-general")

(load "custom-escreen")

(load "custom-erc")

(load "custom-c")

(load "custom-lisp")

(load "custom-org")

(load "custom-haskell")

;(load "custom-erlang")

(load "custom-python")

(load "custom-latex")

(load "custom-js-web")

(load "custom-eshell")

(load "custom-vc")

(load "custom-xml")

(load "custom-helm")

;; (load "custom-stats")

(when (require 'powerline nil t)
  (powerline-default-theme))

;;; I'm a convert:
(when (require 'zenburn-theme nil t)
  (load-theme 'zenburn t))

;;; Load in customize stuff:
(setq custom-file (concat *mh/lisp-base* system-name "-variables.el"))
(load custom-file)

;;; Don't worry about disabled-command warnings:
(setq disabled-command-function nil)

;;; Set up session-saving (see https://github.com/emacs-helm/helm/issues/204):
(setq session-save-print-spec '(t nil 40000))
(add-hook 'after-init-hook 'session-initialize)

;;; company-mode completion:
(add-hook 'after-init-hook 'global-company-mode)

(message ".emacs loaded in %ds"
         (destructuring-bind (hi lo ms ps) (current-time)
           (- (+ hi lo) (+ (first *mh/load-start-time*)
                           (second *mh/load-start-time*)))))

(provide 'init)

;;; init.el ends here
