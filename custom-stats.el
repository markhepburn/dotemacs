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
             (curbuf (current-buffer))
             (tmpbuf (get-buffer-create "**ess-R-object-tooltip**")))
         (if objname
             (progn
               (ess-command (concat "class(" objname ")\n")  tmpbuf )
               (set-buffer tmpbuf)
               (let ((bs (buffer-string)))
                 (if (not(string-match "\(object .* not found\)\|unexpected" bs))
                     (let* ((objcls (buffer-substring
                                     (+ 2 (string-match "\".*\"" bs))
                                     (- (point-max) 2)))
                            (myfun (cdr(assoc-string objcls
                                                     ess-R-object-tooltip-alist))))
                       (progn
                         (if (eq myfun nil)
                             (setq myfun
                                   (cdr(assoc-string "other"
                                                     ess-R-object-tooltip-alist))))
                         (ess-command (concat myfun "(" objname ")\n") tmpbuf)
                         (let ((bs (buffer-string)))
                           (progn
                             (set-buffer curbuf)
                             (tooltip-show-at-point bs 0 30)))))))))
         (kill-buffer tmpbuf)))

     (define-key ess-mode-map (kbd "C-c .") 'ess-R-object-tooltip)))