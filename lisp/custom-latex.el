;;; custom-latex.el --- LaTeX usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load AucTeX:

;;; Commentary:
;;

;;; Code:

(use-package tex-site
  :ensure auctex
  :defer t
  :mode ("\\.text\\'" . latex-mode)
  :config
  (setq
   LaTeX-ignore-comment-regexp "%[^ a-zA-Z]"
   LaTeX-right-comment-regexp "%[^ a-zA-Z]"
   ;; plug reftex into auctex:
   reftex-plug-into-AUCTeX t
   ;; This might be better as buffer-local, but leave it for now:
   reftex-guess-label-type nil
   TeX-save-query nil)

  ;; Function to move point to the next \item:
  (defun mh/LaTeX-find-next-item (count)
    "Move point forward to the next occurence of `\\item'.
Optional prefix arg count finds the count next occurence.
Negative argument searches backwards."
    (interactive "p")
    (if (not (re-search-forward "\\\\item[ \t]*" nil t count))
        (message "No more \\items found.")))
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (define-key LaTeX-mode-map (kbd "C-c i") 'mh/LaTeX-find-next-item)))

  ;; Function to move point to the next \section (or subsection, etc):
  (defun mh/LaTeX-find-next-section (count)
    "Move point forward to the next section or subsection.
Optional prefix arg count finds the count next occurence.
Negative argument searches backwards."
    (interactive "p")
    (if (not (re-search-forward "\\\\\\(sub\\)*section\{" nil t count))
        (message "No more sections found.")))
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (define-key LaTeX-mode-map (kbd "C-c s") 'mh/LaTeX-find-next-section)))

  (add-hook 'LaTeX-mode-hook
            (lambda () (TeX-global-PDF-mode 1)))

  ;; turn on reftex:
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (reftex-mode t)))


  ;; Modify TeX-insert-braces to place braces around region, if active:
  (defadvice TeX-insert-braces (around braces-around-region
                                       activate compile)
    "If the region is active, insert braces around region;
otherwise behave as normal."
    (if mark-active
        (progn
          (if (> (point) (mark)) (exchange-point-and-mark))
          (insert TeX-grop)
          (goto-char (mark))
          (insert TeX-grcl))
      ad-do-it)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-latex)

;;; custom-latex.el ends here
