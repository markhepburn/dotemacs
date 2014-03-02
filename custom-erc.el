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

  (defun erc-ido-switch-buffer ()
    "Use ido to switch between active ERC buffers.
Replaces erc-iswitchb, which isn't working for me at the moment."
    (interactive)
    (switch-to-buffer
     (ido-completing-read "Channel: "
                          (mapcar 'buffer-name (erc-buffer-list))
                          nil t)))

  (define-key erc-mode-map (kbd "C-c C-b") 'erc-ido-switch-buffer))

(provide 'custom-erc)

;;; custom-erc.el ends here
