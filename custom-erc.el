(defun erc-ido-switch-buffer ()
  "Use ido to switch between active ERC buffers.
Replaces erc-iswitchb, which isn't working for me at the moment."
  (interactive)
  (switch-to-buffer
   (ido-completing-read "Channel: "
                        (mapcar 'buffer-name (erc-buffer-list))
                        nil t)))

(define-key erc-mode-map (kbd "C-c C-b") 'erc-ido-switch-buffer)
