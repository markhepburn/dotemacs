;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript (and Web) stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Experiment: now that flycheck+jshint provides error checking, try
;;; the built in js-mode for a bit (it's already in auto-mode-alist):
(setq js-indent-level 2)

;;; mozrepl integration
;;; (http://people.internetconnection.net/2009/02/interactive-html-development-in-emacs/):
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(after "moz"
  (require 'moz)
  (require 'json)
  (defun moz-update (&rest ignored)
    "Update the remote mozrepl instance"
    (interactive)
    (comint-send-string (inferior-moz-process)
                        (concat "content.document.body.innerHTML="
                                (json-encode (buffer-string)) ";")))
  (defun moz-enable-auto-update ()
    "Automatically update the remote mozrepl when this buffer changes"
    (interactive)
    (add-hook 'after-change-functions 'moz-update t t))
  (defun moz-disable-auto-update ()
    "Disable automatic mozrepl updates"
    (interactive)
    (remove-hook 'after-change-functions 'moz-update t)))

;;; basic html (and inherited by django-html); don't auto-fill:
(add-hook 'html-mode-hook (lambda () (auto-fill-mode -1)))

;;; From http://whattheemacsd.com//setup-html-mode.el-05.html
;;; after deleting a tag, indent properly (I didn't use
;;; sgml-delete-tag, but it's on C-c C-d)
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

;;; zencoding shortcuts for html generation:
(autoload 'zencoding-mode "zencoding-mode" "Zencoding HTML generation shortcuts" t)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(setq-default zencoding-indentation 2)
;;; reclaim C-j keybinding from zencoding!
(after "zencoding-mode"
  (define-key zencoding-mode-keymap (kbd "C-j") nil)
  (define-key zencoding-mode-keymap (kbd "M-<return>") 'zencoding-expand-line))

;;; django templates:
(autoload 'django-html-mode "django-html-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . django-html-mode))

;;; Interactive django mode (virtualenv and fabric integration, etc):
;; Loading now then plugs it in to the related major-modes:
(require 'pony-mode nil t)

;;; Make css colour definitions the colour they represent:
(autoload 'rainbow-turn-on "rainbow-mode" nil t)
(add-hook 'css-mode-hook 'rainbow-turn-on)

;;; use c-style indentation in css:
(setq cssm-indent-function 'cssm-c-style-indenter)
(add-hook 'css-mode-hook
          (lambda () (local-set-key "{" 'autopair-open-block)))

;;; Now using LessCSS, using it's own derived mode:
(when (require 'less-css-mode nil t)
  (add-hook 'less-css-mode-hook 'rainbow-turn-on))

;; (add-to-list 'load-path (expand-file-name "~/elisp/mmm-mode-0.4.8"))
;; ;; MMM-Mode
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)

;; ;; Set up an mmm group for fancy html editing
;; (mmm-add-group
;;  'fancy-html
;;  '(;; (html-php-embedded
;; ;;     :submode php-mode
;; ;;     :face mmm-output-submode-face
;; ;;     :front "<[?]php"
;; ;;     :back "[?]>")
;; ;;    (html-css-embedded
;; ;;     :submode css-mode
;; ;;     :face mmm-declaration-submode-face
;; ;;     :front "<style\[^>\]*>"
;; ;;     :back "</style>")
;; ;;    (html-css-attribute
;; ;;     :submode css-mode
;; ;;     :face mmm-declaration-submode-face
;; ;;     :front "\\bstyle=\\s-*\""
;; ;;     :back "\"")
;;    (html-javascript-embedded
;;     :submode javascript-mode
;;     :face mmm-code-submode-face
;;     :front "<script\[^>\]*>"
;;     :back "</script>")
;;    (html-javascript-attribute
;;     :submode javascript-mode
;;     :face mmm-code-submode-face
;;     :front "\\bon\\w+=\\s-*\""
;;     :back "\"")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
