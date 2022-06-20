;; Assuming usage with dart-mode
(use-package dart-mode
  :custom
  (dart-sdk-path (concat (getenv "HOME") "/development/flutter/bin/cache/dark-sdk/")
   dart-format-on-save t))

(use-package hover
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-z" . #'hover-run-or-hot-reload)
              ("C-M-x" . #'hover-run-or-hot-restart)
              ("C-M-p" . #'hover-take-screenshot'))
  :init
  (setq hover-flutter-sdk-path (concat (getenv "HOME") "/development/flutter/bin/flutter")
        hover-command-path (concat (getenv "GOPATH") "/bin/hover") ; remove if `hover` is already in $PATH
        hover-hot-reload-on-save t
        hover-screenshot-path (concat (getenv "HOME") "/Pictures"
        hover-screenshot-prefix "flutter-hover-"
        hover-observatory-uri "http://localhost:50300"
        hover-clear-buffer-on-hot-restart t)))

