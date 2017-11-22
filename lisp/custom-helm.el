;;; Taken from http://tuhdo.github.io/helm-intro.html as a starting-point.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: helm              ;;
;;                            ;;
;; GROUP: Convenience -> Helm ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :init
  (progn
    (setq
     ;; must set before helm-config,  otherwise helm use default
     ;; prefix "C-x c", which is inconvenient because you can
     ;; accidentially press "C-x C-c"
     helm-command-prefix-key "C-c h"

     helm-google-suggest-use-curl-p t
     helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
     helm-quick-update t  ; do not display invisible candidates
     helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
     helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
     helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

     helm-split-window-default-side 'other ;; open helm buffer in another window
     helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window

     helm-candidate-number-limit 200 ; limit the number of displayed canidates
     helm-M-x-requires-pattern 0   ; show all candidates when set to 0
     helm-boring-file-regexp-list
     '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$" "\\.tc$") ; do not show these files in helm buffer
     helm-ff-file-name-history-use-recentf t
     helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
     ido-use-virtual-buffers t      ; Needed in helm-buffers-list
     helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non--nil useful in helm-mini that lists buffers
     ))
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c h m" . helm-man-woman)
         ("C-c h f" . helm-find)
         ("C-c h l" . helm-locate)
         ("C-c h o" . helm-occur)
         ("C-c h r" . helm-resume)
         ("C-c m" . helm-all-mark-rings)

         ;; Helm-mode tweaks:
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; rebihnd tab to do persistent action
         ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
         ("C-z" . helm-select-action) ; list actions using C-z
         ("C-x 2" . helm-select-2nd-action)
         ("C-x 3" . helm-select-3rd-action)
         ("C-x 4" . helm-select-4rd-action)

         ;; Grep integration:
         :map helm-grep-mode-map
         ("<return>" . helm-grep-mode-jump-other-window)
         ("n" . helm-grep-mode-jump-other-window-forward)
         ("p" . helm-grep-mode-jump-other-window-backward))

  :config
  (progn
    (require 'helm-config)
    (require 'helm-eshell)
    (require 'helm-files)
    (require 'helm-grep)

    (setq helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                              '(picture-mode artist-mode)))

    ;; Something funky going on, but can't :bind to help-command (it's
    ;; either a sparse keymap or a function; weird)
    (define-key 'help-command (kbd "C-f") 'helm-apropos)
    (define-key 'help-command (kbd "r") 'helm-info-emacs)

    ;; use helm to list eshell history
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

    ;; Save current position to mark ring
    (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

   (helm-mode 1))

  :demand t
  :diminish (helm--minor-mode helm-mode))



;;; Helm-ag:
(use-package helm-ag
  :init (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case"
              helm-ag-command-option "--all-text"
              helm-ag-thing-at-point 'symbol)
  :bind (("M-g ." . helm-ag)
         ("M-g ," . helm-ag-pop-stack)
         ("M-g s" . helm-ag-project-root)))

;;; helm-swoop:
(use-package helm-swoop
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch)))

(use-package helm-descbinds
  :config (helm-descbinds-mode))

;;; A few enhancements:
(use-package helm-ext
  :config
  (helm-ext-ff-enable-skipping-dots t)
  (helm-ext-minibuffer-enable-header-line-maybe t))
