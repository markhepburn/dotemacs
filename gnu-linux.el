
;;; See: https://bugs.launchpad.net/ubuntu/+source/emacs-snapshot/+bug/291399 for the need for this:
(set-frame-parameter nil 'font-backend '(xft x))
;;; this is now actually set in ~/.Xresources (otherwise, it doesn't
;;; seem to get set correctly when the daemon starts up); relevant
;;; line:
;;; emacs.font:-microsoft-Consolas-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1
;;; May not need this now.
(set-default-font "Consolas-11")
;(set-default-font "Inconsolata-11")
;(set-default-font "Bitstream Vera Sans Mono-10")
;(set-default-font "Monospace-10")

;;; font-lock for apt sources:
(add-to-list 'auto-mode-alist '("sources\\.list\\'" . conf-mode))

;;; Make sure that the default browser is used by browse-url*:
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "x-www-browser")

;;; only use org-agenda mode in Linux:
;; (eval-after-load "org"
;;   '(progn
;;      (add-to-list 'org-agenda-files (expand-file-name "~/todo-apa.org"))
;;      (define-key org-mode-map  "\C-ca" 'org-agenda)))

;;; twit.el: use a bit more in Linux (have tweetie on the mac), so
;;; include a few linux-specific customisations:
(setq twit-user-image-dir (expand-file-name "~/.twit.el/icons"))
(setq twit-show-user-images t)
(defun mh/twit-notify-tweet ()
  (if twit-show-user-images
      (let* ((user  (cadr twit-last-tweet))
             (tweet (caddr twit-last-tweet))
             (img-re (concat
                      "^["
                      ;; manually construct case-insensitive RE:
                      (mapconcat (lambda (c)
                                   (list (downcase c) (upcase c)))
                                 (string-to-list user)
                                 "][")
                      "]-"))
             (img
              (or (car-safe (directory-files twit-user-image-dir t img-re))
                  (concat twit-user-image-dir "/twitter.png"))))
        (shell-command (concat "notify-send -i " img
                               " \"" user "\" \""
                               (replace-regexp-in-string "\"" "\\\\\"" tweet) "\"")))))
(add-hook 'twit-new-tweet-hook 'mh/twit-notify-tweet)

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
