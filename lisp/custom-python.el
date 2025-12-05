;;; custom-python.el --- Python Development Customisation  -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set up for ipython:

;;; Commentary:
;;

;;; Code:

(use-package flymake-ruff
  :when (executable-find "ruff")
  :hook (python-base-mode . flymake-ruff-load))

;;; Ref https://ddavis.io/blog/python-emacs-4/
(use-package python-base-mode
  :ensure nil
  :hook (python-base-mode . mh/python-init)
  :init
  (defun mh/python-init ()
    (let* ((project (project-current))
           (project-root (when project (project-root project)))
           (venv-path (when project-root
                        (locate-dominating-file default-directory ".venv"))))
      (message "project-root: %s | venv-path: %s" project-root venv-path)
      (cond
       ;; First look for a .venv directory
       ((and venv-path (file-directory-p venv-path))
        (make-local-variable 'pyvenv-virtual-env)
        ;; FIXME: not sure this is right; does it need to be the full path, or the path containing the venv?
        (let ((active-venv-path (file-name-concat venv-path ".venv")))
          (message "Activating %s" active-venv-path)
          (pyvenv-activate active-venv-path)))
       ;; Else, if pyvenv-workon is set (probably via .dir-locals.el) activate that:
       ((and (not venv-path) (boundp 'pyvenv-workon) pyvenv-workon)
        (message "Working-on %s" pyvenv-workon)
        (make-local-variable 'pyvenv-virtual-env)
        (pyvenv-workon pyvenv-workon))))))

(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/.virtualenvs/")
  (set (make-variable-buffer-local 'pyvenv-virtual-env) nil)
  :commands (pyvenv-activate pyvenv-workon))

;;; sample dape configuration for docker remote server
;;; (run program with "-m debugpy --listen 0.0.0.0:5678")
;; `(debugpy-docker-attach
;;        modes (python-mode python-ts-mode)
;;        host "localhost"
;;        port 5678
;;        :request "attach"
;;        :type "python"
;;        :pathMappings [(:localRoot "/home/mark/Condense/imas-seachange/backend/"
;;                        :remoteRoot "/app")]
;;        :justMyCode nil
;;        :showReturnValue t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-python)

;;; custom-python.el ends here
