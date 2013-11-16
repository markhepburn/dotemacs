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

;;; emmet (zencoding) shortcuts for html generation:
(autoload 'emmet-mode "emmet-mode"
  "Emmet (Zencoding) HTML generation shortcuts" t)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(setq-default emmet-indentation 2)
;;; reclaim C-j keybinding from emmet!
(after "emmet-mode"
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (define-key emmet-mode-keymap (kbd "M-<return>") 'emmet-expand-line))

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

;;; Now using LessCSS, using its own derived mode:
(when (require 'less-css-mode nil t)
  (add-hook 'less-css-mode-hook 'rainbow-turn-on))
