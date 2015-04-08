;;; custom-python.el --- Python Development Customisation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set up for ipython:

;;; Commentary:
;; 

;;; Code:

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(defun mh/python-mode-jedi-setup ()
  "Include a .dir-locals.el file with
    ((nil . ((jedi/venv-name . \"venv-name\"))))
  in it.  Including one in the virtualenv itself
  enables navigating through lib source as well."
  (hack-local-variables)
  (when (boundp 'jedi/venv-name)
    (venv-workon jedi/venv-name))

  (setq jedi:setup-keys      t
        jedi:use-shortcuts   t
        jedi:complete-on-dot t)
  (jedi:setup)
  (require 'company-jedi nil t)
  (jedi-mode 1))

(after "python"
  (when (require 'jedi nil t)
    (add-hook 'python-mode-hook 'mh/python-mode-jedi-setup))

  (add-hook 'python-mode-hook
            (lambda () (imenu-add-to-menubar "Declarations")))
  (add-hook 'python-mode-hook
            (lambda () (semantic-decoration-mode -1)))

  ;; Advice ':' so typing ':)' results in in '):', eg for function
  ;; definitions:
  (defadvice python-indent-electric-colon (around python-electric-colon-autoplace (arg)
                                                  activate)
    (if (and (looking-at ")")
             (not arg))
        (progn
          (move-end-of-line nil)
          ad-do-it
          (newline-and-indent))
      ad-do-it)))

;;; ipython-notebook integration:
(after "ein"
  (setq ein:use-auto-complete t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-python)

;;; custom-python.el ends here
