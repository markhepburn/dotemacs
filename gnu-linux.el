
;;; See: https://bugs.launchpad.net/ubuntu/+source/emacs-snapshot/+bug/291399 for the need for this:
(set-frame-parameter nil 'font-backend '(xft x))
(set-default-font "Consolas-11")
;(set-default-font "Inconsolata-11")
;(set-default-font "Bitstream Vera Sans Mono-10")
;(set-default-font "Monospace-10")


;; In X-windows, play nicely with the clipboard:
(setq x-select-enable-clipboard t)
(global-set-key [(C ?y)] #'x-clipboard-yank)
