;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C mode:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Use the same basic indent as the default tab-width:
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "k&r")
            (setq c-basic-offset tab-width)))

;;; use the new IDE-like gdb multi-window interface:
(setq gdb-many-windows t)

;;; support for skeleton-pair like functionality (note that keys need
;;; to be bound especially, because c-mode and friends bind ?\( etc to
;;; electric-...)  autopair-* are defined in custom-general.el
(when (and (fboundp 'autopair-insert)
           (fboundp 'autopair-open-block)
           (fboundp 'autopair-close-block))
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (local-set-key "(" 'autopair-insert)
               (local-set-key ")" 'autopair-insert)
               (local-set-key "{" 'autopair-open-block)
               (local-set-key "}" 'autopair-close-block))))

;;; advise c-beginning-of-defun so it sets mark before jumping (why
;;; doesn't it already do this??)
(defadvice c-beginning-of-defun (before c-push-mark-before-bod activate)
  "Save point before jumping to the beginning of the defun, so
  you can easily jump back."
  (push-mark))
(defadvice c-end-of-defun (before c-push-mark-before-eod activate)
  "Save point before jumping to the end of the defun, so you can
  easily jump back."
  (push-mark))

;;; let's experiment with cedet:
;;; http://xtalk.msk.su/~ott/en/writings/emacs-devenv/EmacsCedet.html
;;; (url seems to be about a much newer version than included with ubuntu atm, so using CVS)
;; (require 'cedet)
(load-file (concat *mh/thirdparty-special* "cedet/common/cedet.el"))
;; (semantic-load-enable-minimum-features)
(semantic-load-enable-gaudy-code-helpers)
(require 'semantic-ia)
(semantic-add-system-include "/usr/local/include")
(semantic-add-system-include "/usr/include")
;;; make 'q' close the window in symref results:
(add-hook 'semantic-symref-results-mode-hook
		  '(lambda () (local-set-key "q" 'delete-window)))
(autoload 'gtags-mode "gtags" "GNU Global source navigation" t)
(define-key senator-prefix-map "v" 'semantic-decoration-include-visit)
(add-hook 'c-mode-common-hook (lambda () (gtags-mode 1)))
;;; tags-view:
(autoload 'tv-view-history "tags-view" "Navigate and manipulate TAGS stack" t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java mode (going to play with eclim to see if that's a viable
;;; eclipse alternative)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat *mh/thirdparty-special* "emacs-eclim"))
(add-to-list 'load-path (concat *mh/thirdparty-special* "emacs-eclim/vendor"))
(when (require 'eclim nil t)
  (setq eclim-executable "/opt/eclipse/eclipse-java-indigo/eclim")
  (setq eclim-auto-save t)
  ;; completion too, assuming it's available:
  (when (require 'auto-complete-config nil t)
    (require 'ac-emacs-eclim-source)
    (add-hook 'eclim-mode-hook
              (lambda () (add-to-list 'ac-sources 'ac-source-emacs-eclim))))
  (global-eclim-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
