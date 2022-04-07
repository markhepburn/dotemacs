;;; custom-python.el --- Python Development Customisation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set up for ipython:

;;; Commentary:
;;

;;; Code:

(use-package elpy
  :init
  (advice-add 'python-mode :before #'elpy-enable)
  (setq elpy-rpc-backend "jedi"
        elpy-django-server-command "runserver_plus")
  ;; May need to setenv WORKON_HOME:
  (defalias 'workon 'pyvenv-workon)
  :hook (python-mode . lsp)
  :config
  (pyvenv-tracking-mode 1)           ; use pyvenv-workon in dir-locals
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules)
  :bind (:map python-mode-map
         ("C-c ." . elpy-goto-definition)))

;;; pipx install jedi-language-server:
(use-package lsp-jedi
  :after lsp-mode
  :demand t ; Need this, because nothing is autoloaded so otherwise it doesn't get loaded
  :config
  (add-to-list 'lsp-disabled-clients 'pyls))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-python)

;;; custom-python.el ends here
