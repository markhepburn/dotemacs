;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C mode:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Use the same basic indent as the default tab-width:
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "k&r")
            (setq c-basic-offset tab-width)))

;;; use the new IDE-like gdb multi-window interface:
(setq gdb-many-windows t)

;;; support for skeleton-pair like functionality (note that keys need
;;; to be bound especially, because c-mode and friends bind ?\( etc to
;;; electric-...)  autopair-* are defined in custom-general.el
(when (and (fboundp 'autopair-insert)
           (fboundp 'autopair-open-block)
           (fboundp 'autopair-close-block))
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (local-set-key "(" 'autopair-insert)
               (local-set-key ")" 'autopair-insert)
               (local-set-key "{" 'autopair-open-block)
               (local-set-key "}" 'autopair-close-block))))

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
(add-to-list 'load-path (concat el-get-dir "cedet/contrib/"))
;; select which submodes we want to activate
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

;; Activate semantic
(semantic-mode 1)

;; load contrib library
(require 'eassist)

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

;; ;; SRecode
;; (global-srecode-minor-mode 1)
;; ;; EDE
;; (global-ede-mode 1)
;; (ede-enable-generic-projects)

;; Setup JAVA....
(require 'cedet-java)

(semantic-add-system-include "/usr/local/include")
(semantic-add-system-include "/usr/include")
;;; make 'q' close the window in symref results:
(add-hook 'semantic-symref-results-mode-hook
		  '(lambda () (local-set-key "q" 'delete-window)))
(autoload 'gtags-mode "gtags" "GNU Global source navigation" t)
(add-hook 'c-mode-common-hook (lambda () (gtags-mode 1)))
;;; tags-view:
(autoload 'tv-view-history "tags-view" "Navigate and manipulate TAGS stack" t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java mode (going to play with eclim to see if that's a viable
;;; eclipse alternative)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'eclim nil t)
  (setq eclim-executable "/opt/eclipse/eclipse-java-indigo/eclim")
  (setq eclim-auto-save t)
  ;; completion too, assuming it's available:
  (when (require 'auto-complete-config nil t)
    (require 'ac-emacs-eclim-source)
    (add-hook 'eclim-mode-hook
              (lambda () (add-to-list 'ac-sources 'ac-source-emacs-eclim))))
  (global-eclim-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
