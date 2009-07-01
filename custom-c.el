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
;;; make 'q' close the window in symref results:
(add-hook 'semantic-symref-results-mode-hook
		  '(lambda () (local-set-key "q" 'delete-window)))
(autoload 'gtags-mode "gtags" "GNU Global source navigation" t)
(add-hook 'c-mode-common-hook (lambda () (gtags-mode 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
