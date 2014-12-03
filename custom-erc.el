;;; custom-erc.el --- ERC Customisations

;;; Commentary:
;; ERC Customisations

;;; Code:

;;; Ignore all the "xxx has quit: timeout" etc messages:
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#clojure"
                        "#clojure-au"
                        "#emacs"
                        "#geodjango"
                        "#openlayers")))

(setq erc-server "irc.freenode.net"
      erc-nick   "markhepburn"
      erc-port   6697)                  ; 6697 for TLS

(after 'erc
  (when (require 'erc-services nil t)
    (load "erc-creds")
    (setq erc-prompt-for-nickserv-password nil)
    (erc-services-mode 1))

  (when (featurep 'helm)

    (defun erc-helm-buffer-list ()
      (mapcar 'buffer-name (erc-buffer-list)))

    (defvar helm-source-erc-channel-list
      '((name . "ERC Channels")
        (volatile)
        (delayed)
        (candidates-process . helm-erc-buffer-list)
        (action . helm-switch-to-buffer)))

   (defun erc-helm-switch-buffer ()
     "Use helm to select an active ERC buffer.
Replaces erc-iswitchb, which doesn't work for me."
     (interactive)
     (helm :sources '(helm-source-erc-channel-list)
           :buffer "*helm-erc-channels*"))

   (define-key erc-mode-map (kbd "C-c C-b") 'erc-helm-switch-buffer)))

(provide 'custom-erc)

;;; custom-erc.el ends here
