;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C mode:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Use the same basic indent as the default tab-width:
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "k&r")
            (setq c-basic-offset tab-width)))

;;; let's experiment with cedet:
;;; http://xtalk.msk.su/~ott/en/writings/emacs-devenv/EmacsCedet.html
;;; (url seems to be about a much newer version than included with ubuntu atm, so using CVS)
;; (require 'cedet)
(load-file (concat *mh/lisp-base* "cedet/common/cedet.el"))
;; (semantic-load-enable-minimum-features)
(semantic-load-enable-gaudy-code-helpers)
(require 'semantic-ia)
(semantic-add-system-include "/usr/local/include")
(semantic-add-system-include "/usr/include")
;; Need a better way of doing this, but it'll work for now:
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
                                             "/usr/include/libcsoap-1.0"))
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
(autoload 'gtags-mode "gtags" "GNU Global source navigation" t)
(add-hook 'c-mode-common-hook (lambda () (gtags-mode 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
