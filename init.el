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

;;; http://stackoverflow.com/questions/24779041/disable-warning-about-emacs-d-in-load-path
(defadvice display-warning
    (around no-warn-.emacs.d-in-load-path (type message &rest unused) activate)
  "Ignore the warning about the `.emacs.d' directory being in `load-path'."
  (unless (and (eq type 'initialization)
               (string-prefix-p "Your `load-path' seems to contain\nyour `.emacs.d' directory"
                                message t))
    ad-do-it))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")))

(setq package-pinned-packages
      '((cider . "melpa-stable")
        (clj-refactor . "melpa-stable")
        (magit . "melpa-stable")
        (magit-gitflow . "melpa-stable")))


(require 'package)
(package-initialize)

(unless (package-installed-p 'use-package)
  (progn
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(require 'use-package)
(setq use-package-always-ensure t)

;; My installed package list:
;;; TODO: quelpa-use-package for my own repos
(setq *mh/packages*
  '(auctex
    cedet
    company-ghc
    company-jedi
    dash
    ;distel
    ein
    eshell-git-prompt
    ess
    fic-mode
    flycheck
    ghc
    haskell-mode
    helm
    helm-ag
    helm-descbinds
    helm-swoop
    jedi
    markdown-mode
    ;mplayer-mode
    multiple-cursors
    org-plus-contrib
    org-trello
    pcre2el
    python-django
    s
    session
    smartparens
    sql-indent
    ;tags-view
    ;toggle-case
    virtualenvwrapper))

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
;;; definitions, while others are platform-specific (eg, I used to use
;;; maxframe on osx, but this is redundant on linux where xmonad takes
;;; care of that).  To do this, load files (if they exist)
;;; corresponding to the reported 'system-type and 'system-name
;;; (#'load will stick the ".el" on the end automatically).  Second
;;; argument 't to 'load means no error if the file doesn't exist.
(load (subst-char-in-string ?/ ?- (symbol-name system-type))
      t)
(load system-name t)       ; Assume for now it is not fully-qualified.

(load "custom-general")

(load "custom-god-mode")

(load "custom-escreen")

(load "custom-erc")

;(load "custom-c")

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

(use-package powerline
  :config (powerline-default-theme))

;;; Zenburn is life:
(use-package zenburn-theme)

;;; Load in customize stuff:
(setq custom-file (concat *mh/lisp-base* system-name "-variables.el"))
(load custom-file)

;;; Don't worry about disabled-command warnings:
(setq disabled-command-function nil)

;;; Set up session-saving (see https://github.com/emacs-helm/helm/issues/204):
(setq session-save-print-spec '(t nil 40000))
(add-hook 'after-init-hook 'session-initialize)

(message ".emacs loaded in %ds"
         (destructuring-bind (hi lo ms ps) (current-time)
           (- (+ hi lo) (+ (first *mh/load-start-time*)
                           (second *mh/load-start-time*)))))

(provide 'init)

;;; init.el ends here
