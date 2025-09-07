;;; custom-erc.el --- ERC Customisations  -*- lexical-binding: t; -*-

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
        '((Libera.Chat "#emacs" "#lobsters"))
        ;; Others: #clojure, #clojure-au; #elixir, #openlayers, #django-geo

        ;; Ignore all the "xxx has quit: timeout" etc messages:
        erc-hide-list '("JOIN" "PART" "QUIT"))

  :commands start-erc
  :custom (erc-buffer-display 'buffer)
  :config
  (setopt erc-modules
          (seq-union '(keep-place) erc-modules))
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
  (setq erc-prompt-for-nickserv-password nil)

  (when (boundp 'dbus-compiled-version)
    (add-to-list 'erc-modules 'notifications)
    (erc-update-modules))

  (erc-services-mode 1))

(use-package erc-goodies
  :ensure nil
  :hook (erc-join . erc-keep-place-indicator-enable))

(setq auth-sources '(default
                      "secrets:session"
                      "secrets:Login"
                      "~/.authinfo.gpg"))


(provide 'custom-erc)

;;; custom-erc.el ends here
