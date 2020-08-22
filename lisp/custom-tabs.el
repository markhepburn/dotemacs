;;; custom-tabs.el --- Customisations for tab-bar-mode

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
  ;; use C-x t T to toggle actually displaying the tab-bar:
  (define-key tab-prefix-map (kbd "T") #'tab-bar-mode)
  ;; C-z C-z to emulate "toggle between most recent tab" (not circulate in order)
  (define-key tab-prefix-map (kbd "C-z") #'tab-recent)
  :bind-keymap ("C-z" . tab-prefix-map))

