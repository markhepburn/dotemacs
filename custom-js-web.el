;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript (and Web) stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; never mind all that, use Yegge's awesome mode:
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-auto-indent-flag nil)         ; must be set /before/ loading
(eval-after-load "js2"
  '(when (require 'js-comint nil t)
     (setq inferior-js-program-command "rhino")
     (add-hook 'js2-mode-hook '(lambda () 
                                 (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                                 (local-set-key "\C-\M-x"  'js-send-last-sexp-and-go)
                                 (local-set-key "\C-cb"    'js-send-buffer)
                                 (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                                 (local-set-key "\C-cl"    'js-load-file-and-go)))))

;;; mozrepl integration
;;; (http://people.internetconnection.net/2009/02/interactive-html-development-in-emacs/):
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(eval-after-load "moz"
  '(progn
     (require 'moz)
     (require 'json)
     (defun moz-update (&rest ignored)
       "Update the remote mozrepl instance"
       (interactive)
       (comint-send-string (inferior-moz-process)
                           (concat "content.document.body.innerHTML="
                                   (json-encode (buffer-string)) ";")))
     (defun moz-enable-auto-update ()
       "Automatically update the remote mozrepl when this buffer changes"
       (interactive)
       (add-hook 'after-change-functions 'moz-update t t))
     (defun moz-disable-auto-update ()
       "Disable automatic mozrepl updates"
       (interactive)
       (remove-hook 'after-change-functions 'moz-update t))))

;;; django templates:
(autoload 'django-html-mode "django-html-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . django-html-mode))

;; (add-to-list 'load-path (expand-file-name "~/elisp/mmm-mode-0.4.8"))
;; ;; MMM-Mode
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)

;; ;; Set up an mmm group for fancy html editing
;; (mmm-add-group
;;  'fancy-html
;;  '(;; (html-php-embedded
;; ;;     :submode php-mode
;; ;;     :face mmm-output-submode-face
;; ;;     :front "<[?]php"
;; ;;     :back "[?]>")
;; ;;    (html-css-embedded
;; ;;     :submode css-mode
;; ;;     :face mmm-declaration-submode-face
;; ;;     :front "<style\[^>\]*>"
;; ;;     :back "</style>")
;; ;;    (html-css-attribute
;; ;;     :submode css-mode
;; ;;     :face mmm-declaration-submode-face
;; ;;     :front "\\bstyle=\\s-*\""
;; ;;     :back "\"")
;;    (html-javascript-embedded
;;     :submode javascript-mode
;;     :face mmm-code-submode-face
;;     :front "<script\[^>\]*>"
;;     :back "</script>")
;;    (html-javascript-attribute
;;     :submode javascript-mode
;;     :face mmm-code-submode-face
;;     :front "\\bon\\w+=\\s-*\""
;;     :back "\"")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
