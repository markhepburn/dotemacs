;;; custom-haskell.el --- Haskell development customisation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set up some path stuff first:

;;; Commentary:
;;

;;; Code:

(use-package haskell-ts-mode
  :mode "\\.hs\\'")

(use-package lsp-haskell
  :after (haskell-ts-mode))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-haskell)

;;; custom-haskell.el ends here
