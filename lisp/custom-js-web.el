;;; custom-js-web.el --- Javascript and Web development customisation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript (and Web) stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Experiment: now that flycheck+jshint provides error checking, try
;;; the built in js-mode for a bit (it's already in auto-mode-alist):
;;; Code:

(setq js-indent-level 2)

(use-package restclient
  ;; Work-around: the emacs version I'm using doesn't bundle
  ;; json-pretty-print-buffer, used by restclient-mode.  So, implement
  ;; it using json-reformat:
  :commands (restclient-mode)
  :config (when (require 'json-reformat nil t)
            (defun json-pretty-print-buffer ()
              (json-reformat-region (point-min) (point-max)))))
(use-package restclient-helm
  :after (restclient-mode))

;;; Via https://writequit.org/articles/working-with-logs-in-emacs.html,
;;; which has lots of tips:
(use-package json-navigator
  :commands (json-navigator-navigate-after-point
             json-navigator-navigate-region))

;;; From http://whattheemacsd.com//setup-html-mode.el-05.html
;;; after deleting a tag, indent properly (I didn't use
;;; sgml-delete-tag, but it's on C-c C-d)
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

(use-package web-mode
  :init (setq
         web-mode-markup-indent-offset 2
         ;; CSS offset indentation
         web-mode-code-indent-offset 2
         ;; Script offset indentation (for JavaScript, Java, PHP, etc.)
         web-mode-css-indent-offset 2
         ;; HTML content indentation
         web-mode-indent-style 2)
  :mode ("\\.html?\\'" "\\.tsx\\'")
  ;; :config
  ;; (defun setup-tide-tsx ()
  ;;   (when (string-equal "tsx" (file-name-extension buffer-file-name))
  ;;     (tide-setup)))
  ;; :hook (web-mode . setup-tide-tsx)
  )

;;; emmet (zencoding) shortcuts for html generation:
(use-package emmet-mode
  :after (web-mode)
  :init (setq-default emmet-indentation 2)
  :hook (web-mode css-mode)
  :bind (:map emmet-mode-keymap
         ("C-j" . nil) ;; reclaim C-j keybinding from emmet!
         ("M-<return>" . emmet-expand-line)))

;;; Make css colour definitions the colour they represent:
(use-package rainbow-mode
  :after (css-mode)
  :hook (css-mode . rainbow-turn-on))

;;; use c-style indentation in css:
(setq cssm-indent-function 'cssm-c-style-indenter)

;;; Now using LessCSS, using its own derived mode:
(use-package less-css-mode
  :mode "\\.less\\'"
  :hook (less-css-mode . rainbow-turn-on))

;;; JS (note, jsx-mode is for typed-js, use js-jsx-mode for React):
(use-package tide :disabled)

(use-package typescript-mode
  :mode "\\.tsx\\'")

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
