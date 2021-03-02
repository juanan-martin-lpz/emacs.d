;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)


(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))

(require-package 'hydra)

(use-package treemacs)
(require-package 'treemacs-all-the-icons)
(require-package 'treemacs-magit)
(require-package 'treemacs-projectile)
(require-package 'treemacs-persp)
(require-package 'treemacs-perspective)


(require-package 'lsp-ui)
(require-package 'helm-lsp)
(require-package 'lsp-ivy)
(require-package 'lsp-treemacs)

(require-package 'lsp-java)
;;(require-package 'dap-mode)
;;(require-package 'dap-java)

(require-package 'company-emacs-eclim)
(company-emacs-eclim-setup)

(add-hook 'java-mode-hook #'lsp)


(provide 'init-lsp)

;;; init-lsp.el ends here
