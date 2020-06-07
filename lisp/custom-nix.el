;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; custom-c.el --- Customisations for Nix package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(use-package nix-mode
  :mode "\\.nix\\'")
(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))

;;; Loads dir-locals.nix (by default)
;;; See https://blog.jethro.dev/posts/nix_buffer_emacs/ for sample usage:
(use-package nix-buffer)

(provide 'custom-nix)

;;; custom-nix.el ends here
