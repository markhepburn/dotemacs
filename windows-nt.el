;;; Set variables for ssh interaction, assuming they exist in a file ~/.ssh/agent.env.
;;; See https://help.github.com/articles/working-with-ssh-key-passphrases
(defun mh/parse-sshagent-env ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "~/.ssh/agent.env"))
    (dolist (varname '("SSH_AUTH_SOCK" "SSH_AGENT_PID"))
      (goto-char 0)
      (re-search-forward (concat varname "=\\([^;]+\\)"))
      (setenv varname (match-string 1)))))

;;; Don't display ^M in mixed-line-endings buffers
;;; Via http://stackoverflow.com/questions/730751/hiding-m-in-emacs
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;; Get shell stuff working properly (such as find-name-dired, etc):
(setq mh/git-bash-bin "C:/Program Files (x86)/Git/bin")

(push mh/git-bash-bin exec-path)
(setenv "PATH" (concat mh/git-bash-bin ";" (getenv "PATH")))

(setq shell-file-name (concat mh/git-bash-bin "/" "bash.exe")
      explicit-shell-file-name shell-file-name)
