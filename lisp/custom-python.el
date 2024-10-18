;;; custom-python.el --- Python Development Customisation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set up for ipython:

;;; Commentary:
;;

;;; Code:

;; (use-package elpy
;;   :init
;;   (advice-add 'python-mode :before #'elpy-enable)
;;   (setq elpy-rpc-backend "jedi"
;;         elpy-django-server-command "runserver_plus")
;;   ;; May need to setenv WORKON_HOME:
;;   (defalias 'workon 'pyvenv-workon)
;;   :hook (python-mode . lsp)
;;   :config
;;   (pyvenv-tracking-mode 1)           ; use pyvenv-workon in dir-locals
;;   (delete 'elpy-module-highlight-indentation elpy-modules)
;;   (delete 'elpy-module-flymake elpy-modules)
;;   :bind (:map python-mode-map
;;          ("C-c ." . elpy-goto-definition)))

;;; pipx install jedi-language-server:
;; (use-package lsp-jedi
;;   :after lsp-mode
;;   :config
;;   (add-to-list 'lsp-disabled-clients 'pyls))

;;; Include a .dir-locals.el setting lsp-pyright-venv-directory to the venv name.
;;; See also lsp-pyright-venv-path, which is the location of your venvs -- set per-host
(use-package lsp-pyright
  :demand t
  :after (lsp-mode python-mode python-ts-mode)
  :init (setq lsp-pyright-log-level "warning"
              ;; https://docs.basedpyright.com/
              lsp-pyright-langserver-command "basedpyright"))

(use-package python-mode
  :ensure nil
  :hook (python-mode . lsp))

(use-package python-ts-mode
  :ensure nil
  :hook (python-ts-mode . lsp))

(use-package pyvenv
  :init (setenv "WORKON_HOME" "~/.virtualenvs/")
  :config
  (add-hook 'pyvenv-pre-activate-hooks
            (lambda ()
              ;; need to set path to venv, not base dir of all venvs:
              (setq lsp-pyright-venv-path python-shell-virtualenv-root
                    lsp-pyright-venv-directory pyvenv-workon)))
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1)
  ;; Set correct Python interpreter
  ;; (setq pyvenv-post-activate-hooks
  ;;       (list (lambda ()
  ;;               (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  ;; (setq pyvenv-post-deactivate-hooks
  ;;       (list (lambda ()
  ;;               (setq python-shell-interpreter "python3"))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-python)

;;; custom-python.el ends here
