;; Set eshell to load a custom ~/bin/eshell bash script loading zh
;; (setenv "ESHELL" (expand-file-name "~/bin/eshell"))

(use-package eshell
  :bind (("\C-c C-e" . term/clear)
         ("\C-c e" . term/clear))
  :config
  ;; (add-hook 'eshell-mode-hook 'eshell-bookmark-setup)
  (setf async-shell-command-buffer 'rename-buffer))


(add-to-list 'exec-path "/usr/local/bin")


;; Use bash in emacs
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
