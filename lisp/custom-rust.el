;;; custom-rust.el -- Programming using Rust  -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;;; Based off https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/

(use-package rustic
  :init
  (setq rustic-lsp-client 'eglot)
  :config
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  :mode ("\\.rust\\'" . rustic-mode)
  :hook (rustic-mode . mh/rustic-mode-hook))

(defun mh/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(provide 'custom-rust)

;;; custom-rust.el ends here
