;;; gnu-linux.el --- Linux-specific Customisations

;;; See: https://bugs.launchpad.net/ubuntu/+source/emacs-snapshot/+bug/291399 for the need for this:

;;; Commentary:
;; 

;;; Code:

(set-frame-parameter nil 'font-backend '(xft x))
;;; this is now actually set in ~/.Xresources (otherwise, it doesn't
;;; seem to get set correctly when the daemon starts up); relevant
;;; line:
;;; emacs.font:-microsoft-Consolas-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1
;;; or
;;; emacs.font:Ubuntu Mono:pixelsize=16:antialias=true
;;; May not need this now.
;(set-frame-font "Ubuntu Mono-12")
;(set-frame-font "Inconsolata-11")
;(set-frame-font "Bitstream Vera Sans Mono-10")
;(set-frame-font "Monospace-10")

;;; font-lock for apt sources:
(add-to-list 'auto-mode-alist '("sources\\.list\\'" . conf-mode))

;;; Make sure that the default browser is used by browse-url*:
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "x-www-browser")

;;; Projectile: on linux, we can use fd (https://github.com/sharkdp/fd)
(setq projectile-generic-command "fd . -0")

;;; only use org-agenda mode in Linux:
;; (eval-after-load "org"
;;   '(progn
;;      (add-to-list 'org-agenda-files (expand-file-name "~/todo-apa.org"))
;;      (define-key org-mode-map  "\C-ca" 'org-agenda)))

;; In X-windows, play nicely with the clipboard:
(setq x-select-enable-clipboard t)
(global-set-key (kbd "C-y") 'x-clipboard-yank)

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key [(control return)] 'semantic-ia-complete-symbol)
            (local-set-key (kbd "C-c ?") 'semantic-ia-complete-symbol-menu)
            (local-set-key (kbd "C-c >") 'semantic-complete-analyze-inline)
            (local-set-key (kbd "C-c p") 'semantic-analyze-proto-impl-toggle)

            (local-set-key "." 'semantic-complete-self-insert)
            (local-set-key ">" 'semantic-complete-self-insert)

            (ede-minor-mode 1)))

(add-to-list 'exec-path (expand-file-name "~/Projects/elixir-ls"))

(provide 'gnu-linux)

;;; gnu-linux.el ends here
