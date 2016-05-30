;;; custom-python.el --- Python Development Customisation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set up for ipython:

;;; Commentary:
;;

;;; Code:

;;; Include a .dir-locals.el file with
;;; ((nil . ((python-shell-virtualenv-path . \"/path/to/venv\")))) in
;;; it.  Pip-install jedi, possibly also importmagic.  Flake8 doesn't
;;; work on windows.
(use-package elpy
  :pin melpa-stable
  :init
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")
  ;; May need to setenv WORKON_HOME:
  (defalias 'workon 'pyvenv-workon)
  :config
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules))

;;; ipython-notebook integration:
(use-package ein
  :init (setq ein:use-auto-complete t))

;(use-package virtualenvwrapper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-python)

;;; custom-python.el ends here
