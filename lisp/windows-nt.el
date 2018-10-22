;;; Automatically handle ssh-agent interaction (hooks into magit):
;;; (warning: might need to disable the "start" script in the git-bin
;;; path; haven't come across issues arising from doing that yet)
(use-package ssh-agency)

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

;;; Try and speed up git; see https://lists.gnu.org/archive/html/emacs-devel/2018-06/msg00667.html
(setq w32-pipe-read-delay 0
      w32-pipe-buffer-size 16384)

;;; Use eshell in preference most of the time, and shell runs cmd.exe,
;;; but if bash is needed for something
;;; (http://caiorss.github.io/Emacs-Elisp-Programming/Emacs_On_Windows.html):
(defun run-bash ()
  (interactive)
  (let ((shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe"))
    (shell "*bash*")))

;;; Now we can M-x pyvenv-workon (in conjunction with mkvirtualenv)
(setenv "WORKON_HOME" "C:/Users/mark_2/Envs")

;;; We'll use regular cmd for now, with a proper path (above)
;; (setq shell-file-name (concat mh/git-bash-bin "/" "bash.exe")
;;       explicit-shell-file-name shell-file-name)

(setq projectile-indexing-method 'alien
      projectile-enable-caching nil
      projectile-git-submodule-command nil)

;;; See http://חנוך.se/diary/how_to_enable_GnuTLS_for_Emacs_24_on_Windows/index.en.html
;;; Essentially, need to provide the gnutls dlls, and drop the cert bundle in ~/.emacs.d
(setq-default gnutls-trustfiles (list (concat *mh/init-base* "ca-bundle.crt")))

(use-package powershell
  :pin melpa)
