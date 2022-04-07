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
  :bind (:map tab-prefix-map
              ;; use C-x t T to toggle actually displaying the tab-bar:
              ("T" . toggle-frame-tab-bar)
              ;; C-z C-z to emulate "toggle between most recent tab" (not circulate in order)
              ("C-z" . tab-recent)
              ;; View (/select) list of tabs. See also C-zRET tab-bar-select-tab-by-name
              ("C-l" . tab-list)
              ;; Next/previous tabs:
              ("C-n" . tab-next)
              ("C-p" . tab-previous))
  :bind-keymap ("C-z" . tab-prefix-map))

