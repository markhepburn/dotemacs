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
  (setq erc-server "irc.freenode.net"
        erc-nick   "markhepburn"
        erc-port   6697                 ; 6697 for TLS

        erc-autojoin-channels-alist
        '(("freenode.net" "#clojure"
                          "#clojure-au"
                          "#emacs"
                          "#geodjango"
                          "#openlayers"))

        ;; Ignore all the "xxx has quit: timeout" etc messages:
        erc-hide-list '("JOIN" "PART" "QUIT")))

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
