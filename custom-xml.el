
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xml:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxml-mode should be auto-loaded automatically I think:
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))
(fset 'xml-mode 'nxml-mode)
;;; Hook(s):
(defun mh/nxml-enable-hs ()
  "Functions to run when in nxml mode."
  (setq nxml-sexp-element-flag t)
  (eval-after-load "hideshow"
    (let ((nxml-mode-hs-info '(nxml-mode ("^\\s-*\\(<[^/].*>\\)\\s-*$" 1)
                                         "^\\s-*</.*>\\s-*$")))
      (when (not (member nxml-mode-hs-info hs-special-modes-alist))
        (setq hs-special-modes-alist
              (cons nxml-mode-hs-info hs-special-modes-alist))
        t)))
  (hs-minor-mode 1))

(add-hook 'nxml-mode-hook 'mh/nxml-enable-hs)
;; (add-hook 'nxml-mode-hook
;;           ;; alternatively, see variable nxml-sexp-element-flag
;;           ;; Actually, C-M-n/p does what I wanted this for I think!
;;           (lambda () (local-set-key (kbd "C-c M-f") 'nxml-forward-balanced-item)))
;;; General setup:
(setq nxhtml-global-minor-mode t)
(setq nxhtml-global-validation-header-mode t)
(setq nxhtml-skip-welcome t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
