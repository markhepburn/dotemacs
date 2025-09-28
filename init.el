;; -*- lexical-binding: t -*-
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

;;; Once upon a time this was inspired by the following post, but now
;;; use-package does most of the efficiency heavy-lifting:
;;; http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;;; Code:

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq pgtk-use-im-context-on-new-connection nil
      native-comp-async-report-warnings-errors 'silent
      file-name-handler-alist nil
      message-log-max 16384
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold gc-cons-threshold-original
                   gc-cons-percentage gc-cons-percentage-original)
             (load "secure-settings.el.gpg" t)
             ;; (require 'secure-settings "secure-settings.el.gpg" t)
             (garbage-collect)) t)

;;; Commentary:
;;

(require 'cl-macs)

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

(setq package-native-compile t
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t)

(cl-defun pkg-help/url-install (&key url name)
  "Install a package from a remote URL, by downloading to a
temporary file and using `package-install-file'"
  (let* ((pac-name (or name
                       (file-name-base url)))
         (tmp-file (make-temp-file (concat "pkg-help-file-install-" pac-name))))
    (unless (package-installed-p (intern pac-name))
      (url-copy-file url tmp-file 'ok-if-already-exists)
      (package-install-file tmp-file)
      (delete-file tmp-file))))

;;; Utility packages; load here before other customisations that may use them.
(use-package dash :demand t)
(use-package s    :demand t)
(use-package f    :demand t)
(use-package seq  :defer t :pin gnu)

;;; I have no desire to have proportional fonts in my modeline!
(set-face-attribute 'mode-line-active nil :inherit 'mode-line)
(set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (set-face-attribute 'mode-line-active nil :inherit 'mode-line)
            (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)))

;; (load "secure-settings.el.gpg" t)

;;; loaded before anything else because of various macros
;;; (enable-minor-mode-for, after):
(require 'custom-functions)

;;; Some settings need to be machine-specific, such as CEDET project
;;; definitions, while others are platform-specific (eg, I used to use
;;; maxframe on osx, but this is redundant on linux where xmonad takes
;;; care of that). To do this, load files (if they exist)
;;; corresponding to the reported 'system-type and 'system-name.
;;; Second argument 't means no error if the file doesn't exist.
(require (intern (subst-char-in-string ?/ ?- (symbol-name system-type))) nil t)
(require (intern (system-name)) nil t)

;;; Load all custom-* files (except for -functions, already loaded above):
(let ((excluded-files
       '("custom-functions.el"          ; loaded above
         "custom-latex.el"              ; Don't use this most of the time; enable when necessary
         "custom-hydras.el"             ; Experiment with the built-in + repeat-mode
         )))
  (dolist (custom-file (directory-files *mh/lisp-base* nil "custom-.*" nil))
    (unless (-contains? excluded-files custom-file)
      (require (intern (f-base custom-file)) nil t))))

(use-package powerline
  :config (powerline-default-theme))

;;; Zenburn is life:
(use-package zenburn-theme
  :config (if (daemonp)
              (cl-labels ((load-zenburn (frame)
                                     (with-selected-frame frame
                                       (load-theme 'zenburn t))
                                     (remove-hook 'after-make-frame-functions #'load-zenburn)))
                (add-hook 'after-make-frame-functions #'load-zenburn))
            (load-theme 'zenburn t)))

;;; Load in customize stuff:
(setq custom-file (concat *mh/lisp-base* (system-name) "-variables.el"))
(load custom-file t)

;;; Don't worry about disabled-command warnings:
(setq disabled-command-function nil)

;;; Set up session-saving (see https://github.com/emacs-helm/helm/issues/204):
(use-package session
  :init
  (setq session-save-print-spec '(t nil 40000)
        session-globals-exclude '(consult--buffer-history
                                  vertico-repeat-history))
  :hook (after-init . session-initialize))
(use-package recentf
  :hook (after-init . recentf-mode))

(message ".emacs loaded in %s" (emacs-init-time))

(provide 'init)

;;; init.el ends here
