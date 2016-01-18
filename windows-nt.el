;;; Automatically handle ssh-agent interaction (hooks into magit):
;;; (warning: might need to disable the "start" script in the git-bin
;;; path; haven't come across issues arising from doing that yet)
(unless (package-installed-p 'ssh-agency)
  (package-refresh-contents)
  (package-install 'ssh-agency))

;;; Don't display ^M in mixed-line-endings buffers
;;; Via http://stackoverflow.com/questions/730751/hiding-m-in-emacs
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;; Get shell stuff working properly (such as find-name-dired, etc):
(setq mh/git-bash-bin "C:/Program Files/Git/bin")

(add-to-list 'exec-path mh/git-bash-bin)
(setenv "PATH" (concat mh/git-bash-bin path-separator (getenv "PATH")))

;;; We'll use regular cmd for now, with a proper path (above)
;; (setq shell-file-name (concat mh/git-bash-bin "/" "bash.exe")
;;       explicit-shell-file-name shell-file-name)

(setq projectile-indexing-method 'alien
      projectile-enable-caching t)

;;; See http://חנוך.se/diary/how_to_enable_GnuTLS_for_Emacs_24_on_Windows/index.en.html
;;; Essentially, need to provide the gnutls dlls, and drop the cert bundle in ~/.emacs.d
(setq-default gnutls-trustfiles (list (concat *mh/lisp-base* "ca-bundle.crt")))
