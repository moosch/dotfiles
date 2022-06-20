(package-initialize)
(setenv "LANG" "en_US.UTF-8")
(setq default-directory "~/")
(add-to-list 'load-path "~/.emacs.d/addons/")
(defvar is-osx (string-equal "darwin" (symbol-name system-type)))


;; Modernise selection behaviour
;; (use-package delsel
;;   :ensure nil
;;   :config (delete-selection-mode +1))

;; Enable column numbers
(use-package simple
  :ensure nil
  :config (column-number-mode +1))

;; Auto-refresh buffer when changed outside emacs
(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))


