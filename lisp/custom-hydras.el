;;; custom-hydras.el --- hydra setup

;;; Commentary:
;; A few hydras, mostly cribbed from existing examples

;;; Code:

(use-package hydra)


;;; win-switch emulation (lighter-weight, and I have hydra installed
;;; as a dependency anyway):

(defhydra hydra-windows (:color red
                         :timeout 1.5
                         :hint nil)
  "
  ^Move^   ^Size^          ^Add/Delete^
--------------------------------------------
[_h_] ←   [_H_] Wider     [_-_] Split Horiz.
[_j_] ↓   [_J_] Shorter   [_|_] Split Vert.
[_k_] ↑   [_K_] Taller    [_0_] Delete
[_l_] →   [_L_] Narrower  [_1_] Delete Others
         [_+_] Balance
"
  ;; windows movement:
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ;; Resizing:
  ("K" enlarge-window)
  ("J" shrink-window)
  ("H" enlarge-window-horizontally)
  ("L" shrink-window-horizontally)
  ("+" balance-windows)
  ;; Adding/Deleting:
  ("-" split-window-horizontally)
  ("|" split-window-vertically)
  ("0" delete-window)
  ("1" delete-other-windows)
  ("u" winner-undo)
  ;; Quit:
  ("<return>" nil)
  ("q" nil))

(defun mh/maybe-hydra-windows (arg)
  (interactive "P")
  (if (or arg (> (count-windows) 2))
      (call-interactively 'hydra-windows/body)
    (call-interactively 'other-window)))

(global-set-key (kbd "C-x o") 'mh/maybe-hydra-windows)


(provide 'custom-hydras)

;;; custom-hydras.el ends here
