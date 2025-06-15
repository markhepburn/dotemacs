;;; custom-ai.el --- Integrations for current trendy LLM AI tooling -*- lexical-binding: t; -*-



;;; Commentary:
;; 

;;; Code:

(use-package gptel
  :bind ("C-c g" . gptel)
  :config
  (setq
   gptel-model 'gemini-2.5-pro-exp-03-25
   gptel-backend (gptel-make-gemini "Gemini"
                                    :key gptel-api-key
                                    :stream t)))


(use-package aider
  :init
  (setq aider-args '("--model" "gemini"))
  :bind ("C-c a" . aider-transient-menu)
  :config
  (if (not (getenv "GEMINI_API_KEY"))
      (setenv "GEMINI_API_KEY" gemini-api-key)))


;;; See also: https://github.com/copilot-emacs/copilot.el

(provide 'custom-ai)

;;; custom-ai.el ends here
