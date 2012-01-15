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

;;; TODO: I really need to stick to a consistent form of defining key
;;; sequences (ie, pick one of strings, vectors, #'kbd,..).  Somewhere
;;; I know I've seen an article describing them all, and which one(s)
;;; you should use for maximum portability, etc.  If only I could find
;;; that again...
;;; Ok, it's this one: http://www.nongnu.org/emacs-tiny-tools/keybindings/
;;; see also: http://www.emacswiki.org/emacs/KeyBindingNotation


;; http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;; Measure load-time for .emacs:
(defvar *mh/load-start-time* (current-time))

(require 'cl)

(defvar *mh/lisp-base* (file-name-directory (file-truename user-init-file))
  "Base code directory; contains free-standing code and
  subdirectories of other projects.  Defaults to the directory
  containing the user's initialisation file (including following
  symlinks).")
(add-to-list 'load-path *mh/lisp-base*)

(defvar *mh/thirdparty-lisp* (concat *mh/lisp-base* "thirdparty/")
  "Directory containing most third-party code, both single files
  and project directories such as git submodules.")
(add-to-list 'load-path *mh/thirdparty-lisp*)

(defvar *mh/thirdparty-special* (concat *mh/lisp-base* "thirdparty-special/")
  "Directory containing third-party directories that require
special treatment; generally, they will have a subdirectory
containing the code, or a special file that must be autoloaded")

;;; Now add every directory under the thirdparty/ dir to the path as
;;; well (thirdparty-special requires manual handling):
(dolist (entry (directory-files *mh/thirdparty-lisp*))
  (let ((dir (expand-file-name entry *mh/thirdparty-lisp*)))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

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

;;; loaded before custom-general because of enable-minor-mode-for
;;; macro, which is used in -general:
(load "custom-functions")

(load "custom-general")

(load "custom-lisp")

(load "custom-org")

(load "custom-c")

(load "custom-haskell")

(load "custom-erlang")

(load "custom-python")

(load "custom-latex")

(load "custom-js-web")

(load "custom-vc")

(load "custom-xml")

(load "custom-stats")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-view-style (quote (("^a5$" "xdvi %d -paper a5") ("^landscape$" "xdvi %d -paper a4r -s 4") ("." "xdvi %d") ("prosper" "open -a /Applications/Preview.app %d"))))
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(message ".emacs loaded in %ds"
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) (+ (first *mh/load-start-time*)
                           (second *mh/load-start-time*)))))
