(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode))
  ;(setq company-minimum-prefix-length 1)
  ;(setq company-selection-wrap-around t)
  (add-hook 'after-init-hook 'global-company-mode))
