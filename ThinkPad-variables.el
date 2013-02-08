;;; This is to separate the auto-written gunk that customize produces
;;; from init.el.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-style (quote (("^a5$" "xdvi %d -paper a5") ("^landscape$" "xdvi %d -paper a4r -s 4") ("." "xdvi %d") ("prosper" "open -a /Applications/Preview.app %d"))))
 '(haskell-notify-p t)
 '(haskell-process-type (quote cabal-dev))
 '(haskell-tags-on-save t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
