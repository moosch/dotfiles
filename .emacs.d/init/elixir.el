(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))

;; Elixir https://alchemist.readthedocs.io/en/latest/installation/
(unless (package-installed-p 'alchemist)
  (package-install 'alchemist))
