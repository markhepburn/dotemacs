;;; custom-dart-flutter.el -- Modes for mobile development using flutter and dart  -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(use-package dart-mode
  ;; Optional
  :hook (dart-mode . flutter-test-mode))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload)))

;;; Placeholder for any eglot customisation

(provide 'custom-dart-flutter)

;;; custom-dart-flutter.el ends here
