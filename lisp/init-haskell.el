(use-package attrap
  :straight t
)

(use-package dante
  :straight t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode)
  :config
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))
  )

(when (maybe-require-package 'reformatter)
    (reformatter-define hindent
      :program "hindent"
      :lighter " Hin")

    (use-package ormolu
    :hook (haskell-mode . ormolu-format-on-save-mode)
    :bind
    (:map haskell-mode-map
      ("C-c r" . ormolu-format-buffer)))

    (defalias 'hindent-mode 'hindent-on-save-mode)

    (reformatter-define ormolu
      :program "ormolu"
      :lighter " Orm"))

  (with-eval-after-load 'haskell-mode
    (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
    (define-key haskell-mode-map (kbd "C-o") 'open-line))

  (provide 'init-haskell)