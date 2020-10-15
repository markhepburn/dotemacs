;;; custom-c.el --- Customisations for c-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C mode:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Use the same basic indent as the default tab-width:
;;; Code:

(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "k&r")
            (setq c-basic-offset tab-width)))

;;; use the new IDE-like gdb multi-window interface:
(setq gdb-many-windows t)

;;; advise c-beginning-of-defun so it sets mark before jumping (why
;;; doesn't it already do this??)
(defadvice c-beginning-of-defun (before c-push-mark-before-bod activate)
  "Save point before jumping to the beginning of the defun, so
you can easily jump back."
  (push-mark))
(defadvice c-end-of-defun (before c-push-mark-before-eod activate)
  "Save point before jumping to the end of the defun, so you can
easily jump back."
  (push-mark))

;;; let's experiment with cedet:
;;; http://xtalk.msk.su/~ott/en/writings/emacs-devenv/EmacsCedet.html
;;; Note that we need to load this early in init.el, or the built-in
;;; cedet is loaded first and we get a conflict.  This config taken
;;; from Alex Ott's sample at https://gist.github.com/3930120

(use-package cedet
  :disabled t
  :config
  ;; select which submodes we want to activate
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
                                        ;(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

  ;; Activate semantic
  (semantic-mode 1)

  ;; customisation of modes
  (defun alexott/cedet-hook ()
    (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
    (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
    ;;
    (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
    (local-set-key "\C-c=" 'semantic-decoration-include-visit)

    (local-set-key "\C-cj" 'semantic-ia-fast-jump)
    (local-set-key "\C-cq" 'semantic-ia-show-doc)
    (local-set-key "\C-cs" 'semantic-ia-show-summary)
    (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
  (add-hook 'c-mode-common-hook 'alexott/cedet-hook)
  (add-hook 'lisp-mode-hook 'alexott/cedet-hook)
  (add-hook 'scheme-mode-hook 'alexott/cedet-hook)
  (add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
  (add-hook 'erlang-mode-hook 'alexott/cedet-hook)

  (defun alexott/c-mode-cedet-hook ()
    (local-set-key "\C-ct" 'eassist-switch-h-cpp)
    (local-set-key "\C-xt" 'eassist-switch-h-cpp)
    (local-set-key "\C-ce" 'eassist-list-methods)
    (local-set-key "\C-c\C-r" 'semantic-symref))
  (add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t)

  (when (cedet-ectag-version-check t)
    (semantic-load-enable-primary-ectags-support))

  (semantic-add-system-include "/usr/local/include")
  (semantic-add-system-include "/usr/include")
;;; make 'q' close the window in symref results:
  (add-hook 'semantic-symref-results-mode-hook
            '(lambda () (local-set-key "q" 'delete-window)))
  (autoload 'gtags-mode "gtags" "GNU Global source navigation" t)
  (add-hook 'c-mode-common-hook (lambda () (gtags-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C-sharp stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package omnisharp
  ;; :hook csharp
  :after company
  :config
  (add-to-list 'company-backends #'company-omnisharp))

(defun mh/csharp-setup ()
  (company-mode)
  (omnisharp-mode)
  (flycheck-mode)

  (c-set-style "ellemtel")
  (setq indent-tabs-mode nil
        c-syntactic-indentation t
        c-basic-offset 4
        truncate-lines t
        tab-width 4
        evil-shift-width 4))

(use-package csharp-mode
  :after omnisharp
  :config (add-hook 'csharp-mode-hook #'mh/csharp-setup)
  :bind (:map csharp-mode-map
              ("M-." . omnisharp-find-implementations)
              ("." . omnisharp-add-dot-and-auto-complete)
              ("C-S-<space>" . omnisharp-auto-complete)
              ("C-c r r" . omnisharp-run-code-action-refactoring)
              ("C-c C-c" . recompile)))

;;; Default prefix C-c C-n:
(use-package dotnet
  :hook (csharp-mode . dotnet-mode))

;;; Transient interface; C-c n:
(use-package sharper
  :bind
  ("C-c n" . sharper-main-transient))

;; This emacs package requires the omnisharp-roslyn server program. Emacs will manage connection to the server as a subprocess.
;; The easiest/default way to install the server is to invoke M-x omnisharp-install-server and follow instructions on minibufer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-c)

;;; custom-c.el ends here
