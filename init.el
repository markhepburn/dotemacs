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

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("ELPA" . "http://tromey.com/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

;; My installed package list:
(setq *mh/packages*
  '(ag
    auctex
    buffer-move
    cedet
    cider                               ; formerly "nrepl"
    clj-refactor
    cljsbuild-mode
    clojure-mode
    color-theme-zenburn
    company-ghc
    company
    csv-mode
    dash
    diff-hl
    diminish
    ;distel
    emacs-eclim
    ein
    elisp-slime-nav
    emmet-mode
    escreen
    ess
    expand-region
    fic-ext-mode
    flycheck
    free-keys
    ghc
    git-messenger
    haskell-mode
    helm
    helm-ag
    helm-swoop
    http-twiddle
    jedi
    json
    jsx-mode
    less-css-mode
    lively
    lorem-ipsum
    magit
    markdown-mode
    move-text
    ;mplayer-mode
    multiple-cursors
    org
    org-trello
    paredit
    pcre2el
    pony-mode
    powerline
    projectile
    psvn
    python-django
    rainbow-mode
    rst
    s
    session
    smartparens
    sql-indent
    ;tags-view
    ;toggle-case
    undo-tree
    vagrant
    vagrant-tramp
    virtualenvwrapper
    visual-regexp
    web-mode
    win-switch
    yaml-mode
    yasnippet))

(require 'package)

(defun mh/all-packages-installed-p ()
  (cl-every (lambda (p) (package-installed-p p))
            *mh/packages*))

(unless (mh/all-packages-installed-p)
  (message "Refreshing package database...")
  (package-refresh-contents)
  (message "Done.")
  (dolist (package *mh/packages*)
   (unless (package-installed-p package)
     (package-install package))))


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
