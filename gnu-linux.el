
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

;;; only use org-agenda mode in Linux:
(add-to-list 'org-agenda-files (expand-file-name "~/todo-apa.org"))
(define-key org-mode-map  "\C-ca" 'org-agenda)

;;; twit.el: use a bit more in Linux (have tweetie on the mac), so
;;; include a few linux-specific customisations:
(setq twit-user-image-dir (expand-file-name "~/.twit.el/icons"))
(setq twit-show-user-images t)
(add-hook 'twit-new-tweet-hook
          (lambda ()
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
                                         (replace-regexp-in-string "\"" "\\\\\"" tweet) "\""))))))

;; In X-windows, play nicely with the clipboard:
(setq x-select-enable-clipboard t)
(global-set-key [(C ?y)] #'x-clipboard-yank)

;; Need a better way of doing this, but it'll work for now:
(eval-after-load "cedet"
  '(progn
     (ede-cpp-root-project "Atlantis"
			   :name "Atlantis"
			   :file "~/Projects/atlantis/Project.ede"
			   :include-path '("/atassess/include"
					   "/atecology/include"
					   "/ateconomic/include"
					   "/atFileConvert/include"
					   "/atlantismain"
					   "/atlantismain/include"
					   "/atlantisUtil/include"
					   "/atlink"
					   "/atlink/include"
					   "/atmanage/include"
					   "/atphysics/include"
					   "/sjwlib/include")
			   :system-include-path '("/usr/include"
						  "/usr/local/include"
						  "/usr/include/libxml2"
						  "/usr/include/nanohttp-1.0"
						  "/usr/include/libcsoap-1.0"))
     (ede-cpp-root-project "Atlantis-integration"
			   :name "Atlantis-integration"
			   :file "~/Projects/atlantis-integration-dev/Project.ede"
			   :include-path '("/atassess/include"
					   "/atecology/include"
					   "/ateconomic/include"
					   "/atFileConvert/include"
					   "/atlantismain"
					   "/atlantismain/include"
					   "/atlantisUtil/include"
					   "/atlink/include"
					   "/atmanage/include"
					   "/atphysics/include"
					   "/sjwlib/include")
			   :system-include-path '("/usr/include"
						  "/usr/local/include"
						  "/usr/include/libxml2"
						  "/usr/include/nanohttp-1.0"
						  "/usr/include/libcsoap-1.0"))))
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key [(control return)] 'semantic-ia-complete-symbol)
            (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
            (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
            (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)

            (local-set-key "." 'semantic-complete-self-insert)
            (local-set-key ">" 'semantic-complete-self-insert)

            (ede-minor-mode 1)))
