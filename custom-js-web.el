;;; custom-js-web.el --- Javascript and Web development customisation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript (and Web) stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Experiment: now that flycheck+jshint provides error checking, try
;;; the built in js-mode for a bit (it's already in auto-mode-alist):
;;; Code:

(setq js-indent-level 2)

(use-package json-reformat)
(use-package restclient
  ;; Work-around: the emacs version I'm using doesn't bundle
  ;; json-pretty-print-buffer, used by restclient-mode.  So, implement
  ;; it using json-reformat:
  :config (when (require 'json-reformat nil t)
            (defun json-pretty-print-buffer ()
              (json-reformat-region (point-min) (point-max)))))

;;; From http://whattheemacsd.com//setup-html-mode.el-05.html
;;; after deleting a tag, indent properly (I didn't use
;;; sgml-delete-tag, but it's on C-c C-d)
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

(use-package web-mode
  :mode "\\.html?\\'")

;;; emmet (zencoding) shortcuts for html generation:
(use-package emmet-mode
  :after (web-mode)
  :init (setq-default emmet-indentation 2)
  :config (progn
            (add-hook 'web-mode-hook 'emmet-mode)
            (add-hook 'css-mode-hook 'emmet-mode))
  :bind (:map emmet-mode-keymap
         ("C-j" . nil) ;; reclaim C-j keybinding from emmet!
         ("M-<return>" . emmet-expand-line)))

;;; Interactive django mode (virtualenv and fabric integration, etc):
;;; (can't remember the relative merits of either; work it out later):
(use-package pony-mode)
(use-package python-django)

;;; Make css colour definitions the colour they represent:
(use-package rainbow-mode
  :after (css-mode)
  :config (add-hook 'css-mode-hook 'rainbow-turn-on))

;;; use c-style indentation in css:
(setq cssm-indent-function 'cssm-c-style-indenter)

;;; Now using LessCSS, using its own derived mode:
(use-package less-css-mode
  :after (less-css-mode)
  :config (add-hook 'less-css-mode-hook 'rainbow-turn-on))

;;; JSX (React):
(use-package jsx-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Useful commands:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(defun url-encode-region (start end)
  "Replace a region with the same contents, only URL encoded."
  (interactive "r")
  (let ((text (url-hexify-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(provide 'custom-js-web)

;;; custom-js-web.el ends here
