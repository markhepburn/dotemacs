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

(defvar *mh/init-base*  (file-name-directory (file-truename user-init-file)))
(defvar *mh/lisp-base* (concat *mh/init-base* "lisp/")
  "Base code directory; contains free-standing code and
subdirectories of other projects.  Defaults to the lisp
subdirectory of the location containing the user's initialisation
file (including following symlinks).")
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
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(require 'package)
(package-initialize)

(unless (package-installed-p 'use-package)
  (message "Installing use-package first...")
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package)
  (message "Installing use-package first... Done."))

(require 'use-package)
(setq use-package-always-ensure t)

;;; toggle-case
(use-package quelpa-use-package
  :init (setq quelpa-update-melpa-p nil))


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

;;; Load all custom-* files (except for -functions, already loaded above):
(dolist (custom-file (directory-files *mh/lisp-base* nil "custom-.*" t))
  (unless (string= "custom-functions" custom-file)
    (load custom-file)))

(use-package powerline
  :config (powerline-default-theme))

;;; Zenburn is life:
(use-package zenburn-theme)

;;; Load in customize stuff:
(setq custom-file (concat *mh/lisp-base* system-name "-variables.el"))
(load custom-file nil)

;;; Don't worry about disabled-command warnings:
(setq disabled-command-function nil)

;;; Set up session-saving (see https://github.com/emacs-helm/helm/issues/204):
(use-package session
  :init (setq session-save-print-spec '(t nil 40000))
  :config (add-hook 'after-init-hook 'session-initialize))

(message ".emacs loaded in %ds"
         (destructuring-bind (hi lo ms ps) (current-time)
           (- (+ hi lo) (+ (first *mh/load-start-time*)
                           (second *mh/load-start-time*)))))

(provide 'init)

;;; init.el ends here
