(deftheme moosch-theme
  "Created 2021-10-01.")

(custom-theme-set-variables
 'moosch-theme
 '(ansi-color-faces-vector [default default default italic underline success warning error])
 '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://stable.melpa.org/packages/")))
 '(package-selected-packages '(which-key elixir-mode slime-theme slime multiple-cursors use-package paredit column-marker ac-slime auto-complete markdown-mode)))

(provide-theme 'moosch-theme)
