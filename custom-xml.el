
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xml:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxml-mode should be auto-loaded automatically I think:
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))
(fset 'xml-mode 'nxml-mode)
(add-hook 'nxml-mode-hook
          ;; alternatively, see variable nxml-sexp-element-flag
          (lambda () (local-set-key (kbd "C-c M-f") 'nxml-forward-balanced-item)))
(setq nxhtml-global-minor-mode t)
(setq nxhtml-global-validation-header-mode t)
(setq nxhtml-skip-welcome t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
