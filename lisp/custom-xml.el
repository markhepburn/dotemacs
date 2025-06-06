;;; custom-xml.el --- XML Editing  -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xml:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxml-mode should be auto-loaded automatically I think:

;;; Commentary:
;;

;;; Code:

(use-package nxml-mode
  :ensure nil
  ;; (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "kml" "kmz") t) "\\'")
  :mode "\\.\\(km[lz]\\|r\\(?:ng\\|ss\\)\\|s\\(?:ch\\|vg\\)\\|x\\(?:ml\\|s\\(?:d\\|lt\\)\\)\\)\\'"
  :init
  (setq nxhtml-global-minor-mode t
        nxhtml-global-validation-header-mode t
        nxhtml-skip-welcome t)
  :config
  (fset 'xml-mode 'nxml-mode)
  ;; Hook(s):
  (defun mh/nxml-enable-hs ()
    "Functions to run when in nxml mode."
    (setq nxml-sexp-element-flag t)
    (after "hideshow"
      (let ((nxml-mode-hs-info '(nxml-mode ("^\\s-*\\(<[^/].*>\\)\\s-*$" 1)
                                           "^\\s-*</.*>\\s-*$")))
        (when (not (member nxml-mode-hs-info hs-special-modes-alist))
          (setq hs-special-modes-alist
                (cons nxml-mode-hs-info hs-special-modes-alist))
          t)))
    (hs-minor-mode 1))
  :hook (nxml-mode . mh/nxml-enable-hs))

(provide 'custom-xml)

;;; custom-xml.el ends here
