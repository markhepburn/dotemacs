
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
					   "/atlink"
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

            (ede-minor-mode 1)

;;;             (local-set-key "\C-c,n" 'senator-next-tag)
;;;             (local-set-key "\C-c,p" 'senator-previous-tag)
            
;;;             (local-set-key "\C-c,u" 'senator-go-to-up-reference)
;;;             (local-set-key "\C-c,-" 'senator-fold-tag)
;;;             (local-set-key "\C-c,+" 'senator-unfold-tag)

;;;             (local-set-key "\C-c,\C-w" 'senator-kill-tag)
;;;             (local-set-key "\C-c,\C-y" 'senator-yank-tag)
;;;             (local-set-key "\C-c,\M-w" 'senator-copy-tag)
            ))
