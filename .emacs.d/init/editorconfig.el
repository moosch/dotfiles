(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Save sessions
(desktop-save-mode 1)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))


(setq standard-indent 2)

;; I'm not sure this is really working
(setq whitespace-style '(face tabs tab-mark spaces space-mark trailing lines-tail))

;; Show full path in the title bar.
(setq-default frame-title-format "%b (%f)")

;; Always wrap lines
(global-visual-line-mode 1)

;; Highlight current line
; (global-hl-line-mode 1)

;; Set colors to distinguish between active and inactive windows
(set-face-attribute 'mode-line nil :background "SlateGray1")
(set-face-attribute 'mode-line-inactive nil :background "grey93")

;; Show keybindings cheatsheet
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))


;(require 'column-marker)

(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode t)
(setq column-number-mode t)

;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 2 characters wide.
(setq-default tab-width 2)

;; Indentation setting for various languages.
(setq tab-width 2)
(setq standard-indent 2)
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq lua-indent-level 2)
;; (setq lisp-indent-level 2)

;; Never use tabs, use spaces instead.
;; (setq tab-width 2)
;; (setq js-indent-level 2)
;; (setq css-indent-offset 2)
;; (setq c-basic-offset 2)
;; (setq-default indent-tabs-mode nil)
;; (setq-default c-basic-offset 2)
;; (setq-default tab-width 2)
;; (setq-default c-basic-indent 2)
