(setq package-enable-at-startup nil)
(setq native-comp-async-report-warnings-errors 'silent)

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))
