;;; custom-escreen.el --- Customisations for escreen
;;; escreen; gnu-screen for emacs:

;;; Commentary:
;; 

;;; Code:

(setq escreen-prefix-char (kbd "C-z"))  ;; must be done before loading!
(when (require 'escreen nil t)
  (setq escreen-one-screen-p nil)
  (escreen-install)
  (defun escreen-find-file-new-screen (filename &optional wildcards)
    (interactive
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer)))
    (escreen-create-screen)
    (find-file filename wildcards))
  (defun escreen-dired-new-screen (directory)
    (interactive
     (list
      (ido-read-directory-name "Dired[new screen]: ")))
    (escreen-create-screen)
    (dired (expand-file-name directory)))
  (defun escreen-kill-screen-and-buffers ()
    (interactive)
    (let ((current-escreen (escreen-get-current-screen-number)))
      (if (escreen-configuration-one-screen-p)
          (error "[escreen] Only one active screen; can't kill it")
        (dolist (data-map (escreen-configuration-data-map
                           (escreen-configuration-escreen current-escreen)))
          (kill-buffer (escreen-configuration-data-map-critical-buffer
                        (escreen-configuration-data-map-critical data-map))))
        (escreen-kill-screen current-escreen))))
  (defun escreen-open-buffer-new-screen (buf)
    (interactive "B")
    (escreen-create-screen)
    (switch-to-buffer buf))
  ;; adapted from http://blog.tapoueh.org/news.dim.html#%20Escreen%20integration
  (defun escreen-get-active-screen-numbers-with-emphasis ()
    "Display active screens, with the active screen emphasised."
    (interactive)
    (let ((escreens (escreen-get-active-screen-numbers))
          (emphasised ""))
      (dolist (s escreens)
        (setq emphasised
              (concat emphasised (if (= escreen-current-screen-number s)
                                     (propertize (number-to-string s)
                                                 'face 'highlight)
                                   (number-to-string s))
                      " ")))
      (message "[escreen] active screens: %s" emphasised)))
  (defmacro escreen-advise-emphasis (fn)
    `(defadvice ,fn (after ,(intern (concat (symbol-name fn) "-emphasis")) activate)
       (escreen-get-active-screen-numbers-with-emphasis)))

  ;; Make the keybindings a bit more familiar:
  (escreen-advise-emphasis escreen-goto-last-screen)
  (define-key escreen-map (kbd "b")   'escreen-open-buffer-new-screen)
  (define-key escreen-map (kbd "d")   'escreen-dired-new-screen)
  (define-key escreen-map (kbd "C-l") 'escreen-get-active-screen-numbers-with-emphasis)
  (define-key escreen-map (kbd "C-c") (escreen-advise-emphasis escreen-create-screen))
  (define-key escreen-map (kbd "C-k") (escreen-advise-emphasis escreen-kill-screen))
  (define-key escreen-map (kbd "M-k") (escreen-advise-emphasis escreen-kill-screen-and-buffers))
  (define-key escreen-map (kbd "C-n") (escreen-advise-emphasis escreen-goto-next-screen))
  (define-key escreen-map (kbd "C-p") (escreen-advise-emphasis escreen-goto-prev-screen))
  (define-key escreen-map (kbd "C-f") (escreen-advise-emphasis escreen-find-file-new-screen)))

(provide 'custom-escreen)

;;; custom-escreen.el ends here
