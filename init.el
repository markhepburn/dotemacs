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
(defvar *mh/load-start-time* (current-time))

(require 'cl)

(defvar *mh/lisp-base* (file-name-directory (file-truename user-init-file))
  "Base code directory; contains free-standing code and
  subdirectories of other projects.  Defaults to the directory
  containing the user's initialisation file (including following
  symlinks).")
(add-to-list 'load-path *mh/lisp-base*)

(setq el-get-dir (concat *mh/lisp-base* "el-get/"))

(add-to-list 'load-path (concat el-get-dir "el-get"))

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

;; Additional custom recipes, not yet in the repository:
(setq el-get-sources
      '((:name ag
               :type github
               :pkgname "Wilfred/ag.el"
               :features ag)
        ;; See https://github.com/dimitri/el-get/issues/1120
        (:name magit
               :website "https://github.com/magit/magit#readme"
               :description "It's Magit! An Emacs mode for Git."
               :type github
               :pkgname "magit/magit"
               :depends (cl-lib))))

;; My installed package list:
(setq *mh/packages*
  '(el-get
    ;distel
    ;eclim

    ag
    auctex
    autopair
    cedet
    diminish
    ess
    magit
    org-mode
    pony-mode
    smex
    zenburn-theme
    zencoding-mode))


(el-get-cleanup *mh/packages*)
(el-get 'sync *mh/packages*)



(defvar *mh/thirdparty-lisp* (concat *mh/lisp-base* "thirdparty/")
  "Directory containing most third-party code, both single files
  and project directories such as git submodules.")
(add-to-list 'load-path *mh/thirdparty-lisp*)

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

(load "custom-c")

(load "custom-lisp")

(load "custom-org")

(load "custom-haskell")

;(load "custom-erlang")

(load "custom-python")

(load "custom-latex")

(load "custom-js-web")

(load "custom-vc")

(load "custom-xml")

;; (load "custom-stats")

;;; I'm a convert:
(when (require 'zenburn-theme nil t)
  (load-theme 'zenburn t))

;;; Load in customize stuff:
(setq custom-file (concat *mh/lisp-base* system-name "-variables.el"))
(load custom-file)

(message ".emacs loaded in %ds"
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) (+ (first *mh/load-start-time*)
                           (second *mh/load-start-time*)))))
