;;; custom-eshell.el --- Customisations for eshell
;;; Don't know how much I'll use eshell, but try setting it up anyway.

;;; Taken from http://www.emacswiki.org/emacs/EshellCompletion

;;; Commentary:
;; 

;;; Code:

(defun ac-pcomplete ()
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (flet ((insert-and-inherit (&rest args)))
    ;; this code is stolen from `pcomplete' in pcomplete.el
    (let* (tramp-mode ;; do not automatically complete remote stuff
           (pcomplete-stub)
           (pcomplete-show-list t) ;; inhibit patterns like * being deleted
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions))
           (beg (pcomplete-begin))
           ;; note, buffer text and completion argument may be
           ;; different because the buffer text may bet transformed
           ;; before being completed (e.g. variables like $HOME may be
           ;; expanded)
           (buftext (buffer-substring beg (point)))
           (arg (nth pcomplete-index pcomplete-args)))
      ;; we auto-complete only if the stub is non-empty and matches
      ;; the end of the buffer text
      (when (and (not (zerop (length pcomplete-stub)))
                 (or (string= pcomplete-stub ; Emacs 23
                              (substring buftext
                                         (max 0
                                              (- (length buftext)
                                                 (length pcomplete-stub)))))
                     (string= pcomplete-stub ; Emacs 24
                              (substring arg
                                         (max 0
                                              (- (length arg)
                                                 (length pcomplete-stub)))))))
        ;; Collect all possible completions for the stub. Note that
        ;; `candidates` may be a function, that's why we use
        ;; `all-completions`.
        (let* ((cnds (all-completions pcomplete-stub candidates))
               (bnds (completion-boundaries pcomplete-stub
                                            candidates
                                            nil
                                            ""))
               (skip (- (length pcomplete-stub) (car bnds))))
          ;; We replace the stub at the beginning of each candidate by
          ;; the real buffer content.
          (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                  cnds))))))

(defvar ac-source-pcomplete
  '((candidates . ac-pcomplete)))

;; (add-hook 'eshell-mode-hook #'(lambda () (setq ac-sources '(ac-source-pcomplete))))
;; (add-to-list 'ac-modes 'eshell-mode)

(defun eshell-completing-history ()
  (interactive)
  (insert
   (completing-read "Eshell history: "
                    (delete-dups
                     (ring-elements eshell-history-ring)))))

(add-hook 'eshell-mode-hook
          (lambda () (local-set-key (kbd "M-r") 'eshell-completing-history)))

;;; Apparently in a development version; lets add it now:
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;;; type "ls" then open files by clicking / hitting <return>
;;; http://caiorss.github.io/Emacs-Elisp-Programming/Eshell.html
(eval-after-load "em-ls"
  '(progn
     (defun ted-eshell-ls-find-file-at-point (point)
       "RET on Eshell's `ls' output to open files."
       (interactive "d")
       (find-file (buffer-substring-no-properties
                   (previous-single-property-change point 'help-echo)
                   (next-single-property-change point 'help-echo))))

     (defun pat-eshell-ls-find-file-at-mouse-click (event)
       "Middle click on Eshell's `ls' output to open files.
 From Patrick Anderson via the wiki."
       (interactive "e")
       (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
       (defvar ted-eshell-ls-keymap map))

     (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
       "Eshell's `ls' now lets you click or RET on file names to open them."
       (add-text-properties 0 (length ad-return-value)
                            (list 'help-echo "RET, mouse-2: visit this file"
                                  'mouse-face 'highlight
                                  'keymap ted-eshell-ls-keymap)
                            ad-return-value)
       ad-return-value)))

;;; smart display (allow easier editing of command lines
;;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
;;; (Removed for now; didn't like it that much)
(require 'eshell)

;; (use-package eshell-git-prompt
;;   :config (eshell-git-prompt-use-theme 'powerline))

;;; https://ekaschalk.github.io/post/custom-eshell/
(use-package all-the-icons)

(defun set-icon-fonts (CODE-FONT-ALIST)
  "Utility to associate many unicode points with specified fonts."
  (--each CODE-FONT-ALIST
    (-let (((font . codes) it))
      (--each codes
        (set-fontset-font t `(,it . ,it) font)))))

(defun initialise-icon-fonts ()
  (when window-system
    (set-icon-fonts
     '(("fontawesome"
        ;;                         
        #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

       ("all-the-icons"
        ;;    
        #xe907 #xe928)

       ("github-octicons"
        ;;                          
        #xf091 #xf059 #xf076 #xf075 #xe192  #xf016)

       ("symbol regular"
        ;; 𝕊    ⨂      ∅      ⟻    ⟼     ⊙      𝕋       𝔽  ; 
        #x1d54a #x2a02 #x2205 #x27fb #x27fc #x2299 #x1d54b #x1d53d #xe100
        ;; 𝔹    𝔇       𝔗
        #x1d539 #x1d507 #x1d517)))
    (remove-hook 'focus-in-hook #'initialise-icon-fonts)))
;;; on linux, we start emacs as a daemon... meaning initialisation
;;; code runs before X is present.  So to initalise our fonts, we need
;;; to wait until we have a window-system, but obvious-looking
;;; candidates such as `before-make-frame-hook' don't get called on
;;; the first frame.  The new-ish focus hooks are our only option, so
;;; we also use remove-hook to ensure we only run it once.
(add-hook 'focus-in-hook #'initialise-icon-fonts)

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

(defmacro esh-section (NAME ICON FORM &rest PROPS)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  `(setq ,NAME
         (lambda () (when ,FORM
                      (-> ,ICON
                          (concat esh-section-delim ,FORM)
                          (with-face ,@PROPS))))))

(defun esh-acc (acc x)
  "Accumulator for evaluating and concatenating esh-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (concat acc esh-sep it))
    acc))

(defun esh-prompt-func ()
  "Build `eshell-prompt-function'"
  (concat esh-header
          (-reduce-from 'esh-acc "" eshell-funcs)
          "\n"
          eshell-prompt-string))

;; Separator between esh-sections
(setq esh-sep "  ")  ; or " | "

;; Separator between an esh-section icon and form
(setq esh-section-delim "")

;; Eshell prompt header
(setq esh-header "\n┌─")  ; or "\n "

;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
;; your login, these can be the same.
(setq eshell-prompt-regexp "└─> ")   ; or "└─> "
(setq eshell-prompt-string "└─> ")   ; or "└─> "

(esh-section esh-dir
             "\xf07c"  ;  (faicon folder)
             (abbreviate-file-name (eshell/pwd))
             '(:foreground "gold" :bold ultra-bold :underline t))

(esh-section esh-git
             "\xe907"  ;  (git icon)
             (magit-get-current-branch)
             '(:foreground "pink"))

(esh-section esh-python
             "\xe928"  ;  (python icon)
             pyvenv-virtual-env-name)

(esh-section esh-clock
             "\xf017"  ;  (clock icon)
             (format-time-string "%H:%M" (current-time))
             '(:foreground "forest green"))

;; Below I implement a "prompt number" section
(setq esh-prompt-num 0)
(add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
(advice-add 'eshell-send-input :before
            (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))

(esh-section esh-num
             "\xf0c9"  ;  (list icon)
             (number-to-string esh-prompt-num)
             '(:foreground "brown"))

;; Choose which eshell-funcs to enable
(setq eshell-funcs (list esh-dir esh-git esh-python esh-clock esh-num))

;; Enable the new eshell prompt
(setq eshell-prompt-function 'esh-prompt-func)

(provide 'custom-eshell)

;;; custom-eshell.el ends here
