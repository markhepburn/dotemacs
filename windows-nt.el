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
