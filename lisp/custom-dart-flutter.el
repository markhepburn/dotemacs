;;; custom-dart-flutter.el -- Modes for mobile development using flutter and dart

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

(use-package lsp-dart
  :after lsp-mode
  :hook (dart-mode . lsp)
  ;; :init
  ;; (dap-register-debug-template "Flutter :: Custom debug"
  ;;                              (list :flutterPlatform "x86_64"
  ;;                                    :program "lib/main_debug.dart"
  ;;                                    :args '("--flavor" "customer_a")))
  )

(provide 'custom-dart-flutter)

;;; custom-dart-flutter.el ends here
