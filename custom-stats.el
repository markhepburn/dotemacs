;;; Set-up and configuration for stats stuff, mainly ESS (for R)

(add-to-list 'load-path (concat *mh/lisp-base* "ess/lisp"))
(require 'ess-site)

(setq ess-eval-visibly-p nil) ; otherwise C-c C-r (eval-region) takes
                              ; forever
(setq ess-ask-for-ess-directory nil) ; otherwise you are prompted each
                                     ; time you start an interactive R
                                     ; session
(require 'ess-eldoc) ; to show function arguments while you are typing
                     ; them

(eval-after-load "ess-mode"
  '(progn 
     ;; http://www.sigmafield.org/2009/10/01/r-object-tooltips-in-ess/
     ;; Tidied up a fair bit (redunant progns removed, using with-temp-buffer)
     (setq ess-R-object-tooltip-alist
           '((numeric    . "summary")
             (factor     . "table")
             (integer    . "summary")
             (lm         . "summary")
             (other      . "str")))

     (defun ess-R-object-tooltip ()
       "Get info for object at point, and display it in a tooltip."
       (interactive)
       (let ((objname (current-word))
             (curbuf (current-buffer)))
         (if objname
             (with-temp-buffer
               (ess-command (concat "class(" objname ")\n")  (current-buffer) )
               (let ((bs (buffer-string)))
                 (if (not(string-match "\(object .* not found\)\|unexpected" bs))
                     (let* ((objcls (buffer-substring
                                     (+ 2 (string-match "\".*\"" bs))
                                     (- (point-max) 2)))
                            (myfun (cdr (assoc-string objcls
                                                      ess-R-object-tooltip-alist))))
                       (unless myfun
                         (setq myfun
                               (cdr(assoc-string "other"
                                                 ess-R-object-tooltip-alist))))
                       (ess-command (concat myfun "(" objname ")\n") (current-buffer))
                       (let ((bs (buffer-string)))
                         (set-buffer curbuf)
                         (tooltip-show-at-point bs 0 30)))))))))

     (define-key ess-mode-map (kbd "C-c .") 'ess-R-object-tooltip)))