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
(setq el-get-emacswiki-base-url "http://www.emacswiki.org/emacs/download/")
(add-to-list 'load-path (concat el-get-dir "el-get"))

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(unless (featurep 'cedet)
  (when (file-directory-p "~/.emacs.d/el-get/cedet")
    (progn
      (add-to-list 'load-path  "~/.emacs.d/el-get/cedet")
      (load-file "~/.emacs.d/el-get/cedet/cedet-devel-load.el"))))

;; Additional custom recipes, not yet in the repository:
(setq el-get-sources
      '((:name ag
               :type github
               :pkgname "Wilfred/ag.el"
               :features ag)
        (:name atim-unscroll
               :type emacswiki)
        (:name cl-lib
	       :type emacsmirror
	       :pkgname "cl-lib")
        ;; override; current recipe times out:
        (:name csv-mode
               :type emacswiki)
        ;; Fork of zencoding, supporting the new Emmet functionality:
        (:name emmet-mode
               :type github
               :pkgname "smihica/emmet-mode")
        (:name flycheck
               :type github
               :pkgname "lunaryorn/flycheck"
               :depends (s dash)
               :post-init (add-hook 'after-init-hook 'global-flycheck-mode))
        (:name flx
               :type github
               :pkgname "lewang/flx"
               :post-init (progn
                            (flx-ido-mode 1)
                            (setq ido-use-faces nil)))
        (:name git-messenger
               :type github
               :pkgname "syohex/emacs-git-messenger"
               :post-init (global-set-key (kbd "C-x v p") 'git-messenger:popup-message))
        (:name grep-buffers
               :type emacswiki)
        (:name grizzl
               :type github
               :pkgname "d11wtq/grizzl")
        (:name http-twiddle
               :type github
               :pkgname "hassy/http-twiddle")
        (:name less-css-mode
               :type github
               :pkgname "purcell/less-css-mode")
        (:name mplayer-mode
               :type github
               :pkgname "markhepburn/mplayer-mode")
        (:name move-text
               :type github
               :pkgname "emacsmirror/move-text")
        (:name pcre2el
               :type github
               :pkgname "joddie/pcre2el")
        (:name projectile
               :description "Project navigation and management library for Emacs"
               :type github
               :pkgname "bbatsov/projectile"
               :depends (dash grizzl s)
               :features projectile)
        (:name sql-indent
               :type emacswiki)
        (:name tags-view
               :type github
               :pkgname "markhepburn/tags-view")
        (:name toggle-case
               :type http
               :url "http://www.cs.virginia.edu/~wh5a/personal/Emacs/toggle-case.el")
        (:name unbound
               :type emacswiki)
        (:name visual-regexp
               :type github
               :pkgname "benma/visual-regexp.el"
	       :depends (cl-lib)
               :features (visual-regexp)
               :post-init (global-set-key (kbd "C-x r q") 'vr/query-replace))
        (:name win-switch
               :type github
               :pkgname "genovese/win-switch"
               :compile "win-switch.el")
        (:name zenburn-theme
               :type github
               :pkgname "bbatsov/zenburn-emacs"
               :post-init (add-to-list 'custom-theme-load-path
                                       default-directory))))

;; My installed package list:
(setq *mh/packages*
  '(el-get
    ;distel
    ;eclim

    auto-complete
    auto-complete-rst
    ac-nrepl
    ag
    auctex
    browse-kill-ring
    buffer-move
    cedet
    clojure-mode
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
    flx
    flycheck
    git-messenger
    grep-buffers
    grizzl
    haskell-mode
    haskell-mode-exts
    http-twiddle
    ido-ubiquitous
    js2-mode
    json
    less-css-mode
    lively
    lorem-ipsum
    magit
    markdown-mode
    move-text
    mplayer-mode
    multiple-cursors
    nrepl
    nrepl-ritz
    org-mode
    paredit
    pcre2el
    pony-mode
    projectile
    psvn
    python                              ; fgallina version
    rainbow-mode
    rst-mode
    s
    smartparens
    smex
    sql-indent
    tags-view
    toggle-case
    undo-tree
    unbound
    visual-regexp
    win-switch
    yaml-mode
    yasnippet
    zenburn-theme))


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

(load "custom-erc")

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
