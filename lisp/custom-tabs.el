;;; custom-tabs.el --- Customisations for tab-bar-mode  -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(use-package tab-bar
  :ensure nil
  :init
  (setq tab-bar-show 1                  ; hide when only one tab
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-function #'tab-bar-tab-name-all
        tab-bar-new-tab-to 'rightmost)
  (tab-bar-history-mode 1)
  :config
  ;; This was tricky to get right; overriding
  ;; `consult--buffer-display' in `consult-buffer' doesn't actually
  ;; create a new tab (not 100% sure why) and using `consult--multi'
  ;; directly doesn't check the source type (won't work for buffers in
  ;; the recent list but not currently open). Instead, create a buffer
  ;; and close if we quit out:
  (require 'consult)
  (defun tab-bar-consult-buffer ()
    (interactive)
    (tab-bar-new-tab)
    (condition-case nil
        (consult-buffer)
      ((quit error) (tab-bar-close-tab))))
  ;; tab-bar-history-mode is the same as winner-mode, but also respects per-tab history
  ;; (key bindings clobber next and previous-buffer, which I never use)
  :bind (("C-x <left>" . tab-bar-history-back)
         ("C-x <right>" . tab-bar-history-forward)
         :map tab-prefix-map
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
