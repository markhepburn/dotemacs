;;; custom-escreen.el --- Customisations for escreen
;;; escreen; gnu-screen for emacs:

;;; Commentary:
;;

;;; Code:

(use-package escreen
  :demand t                            ; otherwise loading is deferred
  :init (progn
          (setq escreen-prefix-char (kbd "C-z"))

          (defun escreen-find-file-new-screen (filename &optional wildcards)
            (interactive
             (find-file-read-args "Find file: "
                                  (confirm-nonexistent-file-or-buffer)))
            (escreen-create-screen)
            (find-file filename wildcards))
          (defun escreen-dired-new-screen (directory)
            (interactive
             (list
              (read-directory-name "Dired[new screen]: ")))
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
                                                         'face 'success)
                                           (number-to-string s))
                              " ")))
              (message "[escreen] active screens: %s" emphasised)))
          (defmacro escreen-advise-emphasis (fn)
            `(defadvice ,fn (after ,(intern (concat (symbol-name fn) "-emphasis")) activate)
               (escreen-get-active-screen-numbers-with-emphasis))))

  :config (progn
            (setq escreen-one-screen-p nil)
            (escreen-install)
            (escreen-advise-emphasis escreen-create-screen)
            (escreen-advise-emphasis escreen-goto-last-screen)
            (escreen-advise-emphasis escreen-kill-screen)
            (escreen-advise-emphasis escreen-kill-screen-and-buffers)
            (escreen-advise-emphasis escreen-goto-next-screen)
            (escreen-advise-emphasis escreen-goto-prev-screen)
            (escreen-advise-emphasis escreen-find-file-new-screen))

  :bind (:map escreen-map
         ("b"   . escreen-open-buffer-new-screen)
         ("d"   . escreen-dired-new-screen)
         ("C-l" . escreen-get-active-screen-numbers-with-emphasis)
         ("C-c" . escreen-create-screen)
         ("C-k" . escreen-kill-screen)
         ("M-k" . escreen-kill-screen-and-buffers)
         ("C-n" . escreen-goto-next-screen)
         ("C-p" . escreen-goto-prev-screen)
         ("C-f" . escreen-find-file-new-screen)))

(provide 'custom-escreen)
;;; custom-escreen.el ends here
