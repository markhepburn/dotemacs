;;; custom-tabs.el --- Customisations for tab-bar-mode  -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(use-package tab-bar
  :init
  (setq tab-bar-show 1                  ; hide when only one tab
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-function #'tab-bar-tab-name-all
        tab-bar-new-tab-to 'rightmost)
  :config
  (require 'consult)
  (defun tab-bar-consult-buffer ()
    (interactive)
    (let ((selected (consult--multi consult-buffer-sources
                                    :require-match
                                    (confirm-nonexistent-file-or-buffer)
                                    :prompt "Switch to (other tab): "
                                    :history 'consult--buffer-history
                                    :sort nil
                                    :state nil)))
      (message "DEBUG: %s %s" (car selected) (bufferp (car selected)))
      (when (car selected)
        (switch-to-buffer-other-tab (car selected)))))
  :bind (:map tab-prefix-map
              ;; use C-x t T to toggle actually displaying the tab-bar:
              ("T" . toggle-frame-tab-bar)
              ;; C-z C-z to emulate "toggle between most recent tab" (not circulate in order)
              ("C-z" . tab-recent)
              ;; View (/select) list of tabs. See also C-zRET tab-bar-select-tab-by-name
              ("C-l" . tab-list)
              ;; Next/previous tabs:
              ("C-n" . tab-next)
              ("C-p" . tab-previous)
              ;; Override C-z b to use consult-buffer:
	      ("b" . tab-bar-consult-buffer))
  :bind-keymap ("C-z" . tab-prefix-map))

(provide 'custom-tabs)
