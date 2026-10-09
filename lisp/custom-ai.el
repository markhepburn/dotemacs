;;; custom-ai.el --- Integrations for current trendy LLM AI tooling -*- lexical-binding: t; -*-



;;; Commentary:
;;

;;; Code:

(use-package gptel
  :custom (gptel-custom-mode #'markdown-ts-mode)
  :bind ("C-c g" . gptel)
  :config
  (setq
   gptel-model 'gemini-flash-latest
   gptel-default-mode #'markdown-ts-mode
   gptel-backend (gptel-make-gemini "Gemini"
                                    :key gptel-api-key
                                    :stream t)))


;;; Need to explicitly install deps:
(use-package shell-maker)
(use-package acp)
(use-package agent-shell
  :diminish
  :bind ("C-c A" . agent-shell)
  :config
  (setq ;; Gemini:
        agent-shell-google-authentication
        (agent-shell-google-make-authentication
         :api-key (lambda () gemini-api-key))
        ;; Claude Code:
        agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)))

;;; Maybe later; Emacs integration:
;;; https://github.com/steveyegge/efrit?tab=readme-ov-file


;;; See also: https://github.com/copilot-emacs/copilot.el

(provide 'custom-ai)

;;; custom-ai.el ends here
