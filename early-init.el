;;; early-init.el --- Early initialisation  -*- lexical-binding: t -*-

;;; Commentary:
;; Run before init.el, before package and UI initialization happens.
;; Most taken from, with comments, from https://github.com/DiamondBond/emacs/blob/master/early-init.el

;;; Code:

;; Garbage Collection
;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later.
(setq gc-cons-percentage-original gc-cons-percentage
      gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Silence compiler warnings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((comp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local cl-functions))

;; Warning: with 3, the compiler is free to perform dangerous optimizations.
;;; https://github.com/karthink/repeat-help/issues/4 -- O3 breaks repeat-help (at least)
(setq-default native-comp-speed 2)

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq ;native-comp-jit-compilation nil  ;; disable for now
      native-comp-jit-compilation-deny-list nil)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup nil)


;; Initialise installed packages
(setq package-enable-at-startup t)

;; Allow loading from the package cache
(defvar package-quickstart)
(setq package-quickstart t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(provide 'early-init)

;;; early-init.el ends here
