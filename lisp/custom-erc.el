;;; custom-erc.el --- ERC Customisations

;;; Commentary:
;; ERC Customisations

;;; Credentials are contained in secure-settings.el.gpg
;;; It should contain code to set `erc-nickserv-passwords', eg
;; (setq erc-nickserv-passwords
;;       '((freenode (("markhepburn" . "my-password")))))

;;; Code:

(use-package erc
  :init
  (setq erc-server "irc.libera.chat" ; "irc.freenode.net"
        erc-nick   "markhepburn"
        erc-user-full-name "Mark Hepburn"
        erc-port   6697                 ; 6697 for TLS

        erc-autojoin-channels-alist
        '(("irc.libera.chat" "#clojure"
                             ;; "#clojure-au"
                             "#elixir"
                             "#emacs"
                             ;; "#django-geo"
                             "#lobsters"
                             ;; "#openlayers"
			     ))

        ;; Ignore all the "xxx has quit: timeout" etc messages:
        erc-hide-list '("JOIN" "PART" "QUIT"))

  :commands start-erc
  :config
  (defun start-erc ()
    (interactive)
    (erc-tls :server erc-server
             :port erc-port
             :nick erc-nick
             :client-certificate `(,(expand-file-name "~/.ssl/irc.key")
                                   ,(expand-file-name "~/.ssl/irc.cer")))))

(use-package erc-services
  :ensure nil
  :after erc
  :config
  (load "erc-creds" t nil)
  (setq erc-prompt-for-nickserv-password nil)

  (when (boundp 'dbus-compiled-version)
    (add-to-list 'erc-modules 'notifications)
    (erc-update-modules))

  (erc-services-mode 1))


(provide 'custom-erc)

;;; custom-erc.el ends here
