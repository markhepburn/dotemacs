;;; Automatically handle ssh-agent interaction (hooks into magit):
;;; (warning: might need to disable the "start" script in the git-bin
;;; path; haven't come across issues arising from doing that yet)
(use-package ssh-agency)
;;; Hack!  Powershell module posh-sshagent's Start-SshAgent sets
;;; gitconfig's core.sshCommand, to a C:\Program Files... path, which
;;; promptly breaks git-bash.  So, set this environment variable
;;; everywhere git-bash's git is used, which will override the
;;; core.sshCommand settings, and they should all be able to co-exist.
(setenv "GIT_SSH_COMMAND" "ssh")

;; (set-face-font 'default "Consolas-10")
(set-frame-font "Cascadia Code PL-9")
(use-package ligature
  :quelpa (ligature
           :fetcher github
           :repo "mickeynp/ligature.el")
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; nxml doesn't derive from prog-mode:
  (ligature-set-ligatures 'nxml-mode '("<!--" "-->" "</"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "||=" "||>" ;"***"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;; https://github.com/sabof/org-bullets/issues/11#issuecomment-439228372
(setq inhibit-compacting-font-caches t)

;;; Don't display ^M in mixed-line-endings buffers
;;; Via http://stackoverflow.com/questions/730751/hiding-m-in-emacs
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;; Get shell stuff working properly (such as find-name-dired, etc):
(setq mh/git-bash-dir "C:/Program Files/Git")

(add-to-list 'exec-path (expand-file-name "~/w64devkit/bin"))
(add-to-list 'exec-path (f-join mh/git-bash-dir "bin"))
(add-to-list 'exec-path (f-join (getenv "userprofile") "bin"))
(add-to-list 'exec-path (f-join mh/git-bash-dir "usr" "bin"))
(setenv "PATH" (concat (f-join mh/git-bash-dir "bin") path-separator (getenv "PATH")))

(defun uuidgen ()
  (interactive)
  (insert
   (replace-regexp-in-string
    "\n$" "" (shell-command-to-string "pwsh.exe -Command [guid]::NewGuid().toString()"))))

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
(setenv "WORKON_HOME" (f-join (getenv "userprofile") "Envs"))

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


(provide 'windows-nt)
