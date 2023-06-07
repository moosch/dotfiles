;;------------------------------------------------
;; General commands
;;------------------------------------------------
;;; Code:
;; To evaluate and load an expression use C-M-x


;; C-h o     describe-symbol
;; C-Mx i    completion-at-point



;;------------------------------------------------
;; Packages
;;------------------------------------------------
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nognu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode)






;;------------------------------------------------
;; Theming
;;------------------------------------------------
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))
;;  (load-theme 'doom-acario-dark))

;; Tell Emacs where to find some custom themes
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; (load-theme 'wombat t)
;;(load-theme 'wheatgrass t)
;;(load-theme 'nord t)
;;(load-theme 'zenburn t)
;;(load-theme 'hc-zenburn t)
;;(load-theme 'modus-vivendi t)
;;(load-theme 'gruvbox t)
;;(load-theme 'spacemacs-dark t)
;;(load-theme 'badger t)

;; (set-face-attribute 'default nil :font "Fira Code Retina" :height 130)


;; Customizations
;; (set-face-background 'default "#222")
;; (set-face-background 'cursor "#c96")
;; (set-face-background 'isearch "#c60")
;; (set-face-foreground 'isearch "#eee")
;; (set-face-background 'lazy-highlight "#960")
;; (set-face-foreground 'lazy-highlight "#ccc")
;; (set-face-foreground 'font-lock-comment-face "#fc0")
;; (set-face-attribute 'region nil :background "#0c947f" :foreground "#ffffff")





;;------------------------------------------------
;; MacOSX Friendly
;;------------------------------------------------
(defvar is-osx (string-equal "darwin" (symbol-name system-type)))

;; Make ESC quit prompts
;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Use ESC as universal get me out of here command
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Things you'd expect from macOS app.
(global-set-key (kbd "s-s") 'save-buffer)             ;; save
(global-set-key (kbd "s-S") 'write-file)              ;; save as
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ;; quit
(global-set-key (kbd "s-a") 'mark-whole-buffer)       ;; select all


;;------------------------------------------------
;; Making Emacs Linux friendly
;;------------------------------------------------
;; Set Alt key as "super"
(setq win32-lwindow-modifier 'super)






;;------------------------------------------------
;; General startup stuff
;;------------------------------------------------
;; Don't show splash screen on startup
;; Flash when the bell rings
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq echo-keystrokes 0.1)
(setq echo-keystrokes 0.1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq cursor-in-non-selected-windows t)
(setq sentence-end-double-space nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq help-window-select t)
(setq visible-bell -1)

;; Different backup directory
(setq backup-directory-alist '(("." . "~/.saves")))

(show-paren-mode 1)

;; Delete selection on type to replace
(delete-selection-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(delete-selection-mode 1)          ; Delete selected text when typing
(global-unset-key (kbd "s-p"))     ; Don't print








;;------------------------------------------------
;; Which Key
;;------------------------------------------------
;; C-h brings up minibuffer of commands
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.1))





;;------------------------------------------------
;; Highlight line
;;------------------------------------------------
(global-hl-line-mode +1)





;;------------------------------------------------
;; Modeline
;;------------------------------------------------
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))





;;------------------------------------------------
;; Icons and Fonts
;;------------------------------------------------
;; Need to run M+x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)







;;------------------------------------------------
;; IDO
;;------------------------------------------------
;; (setq ido-everywhere t)
;; C-x b to search buffer list
;; (setq ido-enable-flex-matching t)
(ido-mode t)







;;------------------------------------------------
;; Projectile
;;------------------------------------------------
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1))








;------------------------------------------------
;; Dashboard
;;------------------------------------------------
(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-items '((recents . 10)
			    (projects . 5)))
    (setq dashboard-show-shortcuts nil)
    (setq dashboard-center-content nil)
    (setq dashboard-banner-logo-title "Code to hell")
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    ;(setq dashboard-startup-banner "~/.emacs/gnu.png")
    )
  :config
  (dashboard-setup-startup-hook))







;;------------------------------------------------
;; Tide
;;------------------------------------------------
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))






;;------------------------------------------------
;; Ivy
;;------------------------------------------------
(use-package ivy
  :diminish
  :bind (
	 ("C-s" . swiper)
	 ("s-f" . swiper-isearch)
	 ("M-x" . counsel-M-x)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ;("C-l" . ivy-alt-done)
         ;("C-j" . ivy-next-line)
         ;("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ;("C-k" . ivy-previous-line)
         ;("C-l" . ivy-done)
         ;("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ;("C-k" . ivy-previous-line)
					;("C-d" . ivy-reverse-i-search-kill)
	 )
  :config
  (ivy-rich-mode 1))

;(use-package ivy-rich
;  :init
;  (ivy-rich-mode 1))

;; Set minibuffer colors to distinguish between active and inactive windows
(set-face-attribute 'mode-line nil :background "#4682c2")
(set-face-attribute 'mode-line-inactive nil :background "#444")






;;------------------------------------------------
;; Flx
;;------------------------------------------------
(use-package flx ;; Improves sorting of fuzzy-matched results
  :defer t
  :init (setq ivy-flx-limit 10000))






;;------------------------------------------------
;; Council
;;------------------------------------------------
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ;("C-x C-f" . counsel-find-file)
         ;("s-x s-f" . counsel-find-file)
	 ("s-o" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^







;;------------------------------------------------
;; Rainbow Delimiters
;;------------------------------------------------
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)) ; prog-mode is the base mode for all programming languages




;;------------------------------------------------
;; Windmove
;;------------------------------------------------
;; Go to other windows easily with one keystroke Cmd-something.
(global-set-key (kbd "s-1") (kbd "C-x 1"))  ;; Cmd-1 kill other windows (keep 1)
(global-set-key (kbd "s-2") (kbd "C-x 2"))  ;; Cmd-2 split horizontally
(global-set-key (kbd "s-3") (kbd "C-x 3"))  ;; Cmd-3 split vertically
(global-set-key (kbd "s-0") (kbd "C-x 0"))  ;; Cmd-0...
(global-set-key (kbd "s-w") (kbd "C-x 0"))  ;; ...and Cmd-w to close current window

;; Move between windows with Control-Command-Arrow and with =Cmd= just like in iTerm.
(use-package windmove
  :config
  ; (global-set-key (kbd "<C-s-left>")  'windmove-left)  ;; Ctrl+Cmd+left go to left window
  (global-set-key (kbd "s-[")  'windmove-left)         ;; Cmd+[ go to left window

  ; (global-set-key (kbd "<C-s-right>") 'windmove-right) ;; Ctrl+Cmd+right go to right window
  (global-set-key (kbd "s-]")  'windmove-right)        ;; Cmd+] go to right window

  ; (global-set-key (kbd "<C-s-up>")    'windmove-up)    ;; Ctrl+Cmd+up go to upper window
  (global-set-key (kbd "s-{")  'windmove-up)           ;; Cmd+Shift+[ go to upper window

  ; (global-set-key (kbd "<C-s-down>")  'windmove-down)  ;; Ctrl+Cmd+down go to down window
  (global-set-key (kbd "s-}")  'windmove-down))        ;; Cmd+Shift+] got to down window


;; Enable winner mode to quickly restore window configurations
(winner-mode 1)
(global-set-key (kbd "M-s-[") 'winner-undo)
(global-set-key (kbd "M-s-]") 'winner-redo)







;;------------------------------------------------
;; Centaur Tabs
;;------------------------------------------------
(use-package centaur-tabs
  :ensure t
  :config
  (setq centaur-tabs-set-bar 'under
	centaur-tabs-set-icons t
	centaur-tabs-gray-out-icons 'buffer
	centaur-tabs-height 24
	centaur-tabs-set-modified-marker t
	centaur-tabs-modified-marker "*")
  (centaur-tabs-mode t))
;; Switch tabs C-super-<right> ? centaur-tabs-forwatd
(global-set-key (kbd "M-s-<right>") 'centaur-tabs-forward-tab)
(global-set-key (kbd "M-s-<left>") 'centaur-tabs-backward-tab)









;;------------------------------------------------
;; Company - complete anything
;;------------------------------------------------
(use-package company
  :ensure t
  :init
  :config
  (setq company-idle-delay 0.1
	company-global-modes '(not org-mode)
	company-minimum-prefix-length 2
	company-selection-wrap-around t
	company-global-modes '(not erc-mode message-mode eshell-mode shell-mode))
  (add-hook 'after-init-hook 'global-company-mode))





;;------------------------------------------------
;; Flycheck
;;------------------------------------------------
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (bind-key "M-n" 'flycheck-next-error flycheck-mode-map)
  (bind-key "M-p" 'flycheck-previous-error flycheck-mode-map))

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c-mode-hook
	  (lambda () (setq flycheck-gcc-include-path
			   (list (expand-file-name "~/bin/glfw-3.3.8/include/")
				 (expand-file-name "~/bin/vulkan/1.3.224.1/x86_64/include/")
				 (expand-file-name "~/bin/includes/glm-0.9.9.8/glm/")
				 (expand-file-name "~/bin/includes/cglm/include/")
				 (expand-file-name "~/bin/glad/")
				 (expand-file-name "/usr/include/")))))










;;------------------------------------------------
;; Multiple Cursors
;;------------------------------------------------
;; Multiple cursors. Similar to Sublime or VS Code.
(use-package multiple-cursors)
;; (use-package multiple-cursors
;;   :config
;;   (setq mc/always-run-for-all 1)
;;   (global-set-key (kbd "s-M-<up>") 'mc/mark-previous-lines)
;;   (global-set-key (kbd "s-M-<down>") 'mc/mark-next-lines)
;;   (global-set-key (kbd "s-d") 'mc/mark-next-like-this)        ;; Cmd+d select next occurrence of region
;;   (global-set-key (kbd "s-D") 'mc/mark-all-dwim)              ;; Cmd+Shift+d select all occurrences
;;   ;; (global-set-key (kbd "M-s-d") 'mc/edit-beginnings-of-lines) ;; Alt+Cmd+d add cursor to each line in   region
;;   (define-key mc/keymap (kbd "<return>") nil))

(setq mc/always-run-for-all 1)
(global-set-key (kbd "s-M-<up>") 'mc/mark-previous-lines)
(global-set-key (kbd "s-M-<down>") 'mc/mark-next-lines)
(global-set-key (kbd "s-l") 'mc/mark-next-like-this-word)
(global-set-key (kbd "s-M-L") 'mc/mark-all-dwim)
(global-set-key (kbd "M-d") 'mc/mark-next-word-like-this)







;;------------------------------------------------
;; Use my bash env
;;------------------------------------------------
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))








;;------------------------------------------------
;; JSON
;;------------------------------------------------
(use-package json-mode
  :ensure t)







;;------------------------------------------------
;; MaGit
;;------------------------------------------------
(use-package magit
  :ensure t
  :bind (
	 ("C-x g" . magit-status)))







;;------------------------------------------------
;; LSP Modes
;;------------------------------------------------
(setq lsp-log-io nil) ;; Don't log everything = speed
(setq lsp-keymap-prefix "C-c l")
(setq lsp-restart 'auto-restart)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-show-code-actions t)

(use-package lsp-mode
  :ensure t
  :hook (
	 (web-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands lsp-deferred)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)






(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	  (funcall (cdr my-pair)))))






;;------------------------------------------------
;; PrettierJS
;;------------------------------------------------
(use-package prettier-js
  :ensure t)
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))
			     (enable-minor-mode
                              '("\\.tsx?\\'" . prettier-js-mode))))






;;------------------------------------------------
;; C Mode
;;------------------------------------------------
(setq c-default-style "bsd")







;;------------------------------------------------
;; Web Mode
;;------------------------------------------------
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :commands web-mode)






;;------------------------------------------------
;; Editing
;;------------------------------------------------

;; Delete trailing spaces and add new line in the end of a file on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;; Linear undo and redo.
(use-package undo-fu)
(global-set-key (kbd "s-z") 'undo-fu-only-undo)
(global-set-key (kbd "s-Z") 'undo-fu-only-redo)


;; Move-text lines around with meta-up/down.
(use-package move-text
  :config
  (move-text-default-bindings))


;; Comment line or region.
(global-set-key (kbd "s-/") 'comment-line)

;; Visually find and replace text
(use-package visual-regexp
  :config
  (define-key global-map (kbd "M-s-f") 'vr/replace)
  (define-key global-map (kbd "s-r") 'vr/replace))  ;; Cmd+r find and replace

;(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
;(global-set-key (kbd "s-<left>") 'smarter-move-beginning-of-line)

;; Kill line with CMD-Backspace. Note that thanks to Simpleclip, killing doesn't rewrite the system clipboard.
;; Kill one word with Alt+Backspace.
;; Kill forward word with Alt-Shift-Backspace.
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)
(global-set-key (kbd "M-S-<backspace>") 'kill-word)

;; Use Cmd for movement and selection.
(global-set-key (kbd "s-<right>") (kbd "C-e"))        ;; End of line
(global-set-key (kbd "S-s-<right>") (kbd "C-S-e"))    ;; Select to end of line
(global-set-key (kbd "s-<left>") (kbd "M-m"))         ;; Beginning of line (first non-whitespace character)
(global-set-key (kbd "S-s-<left>") (kbd "M-S-m"))     ;; Select to beginning of line

(global-set-key (kbd "s-<up>") 'beginning-of-buffer)  ;; First line
(global-set-key (kbd "s-<down>") 'end-of-buffer)      ;; Last line

;; Copy & Paste
(global-set-key (kbd "s-c") (kbd "M-w")) ;; Copy
(global-set-key (kbd "s-x") (kbd "C-w")) ;; Cut
(global-set-key (kbd "s-v") (kbd "C-y")) ;; Paste

;; Close buffer
(global-set-key (kbd "s-w") (kbd "C-x C-k"))
(global-set-key (kbd "s-k") (kbd "C-x C-k"))

;; Save buffer
(global-set-key (kbd "s-s") (kbd "C-x C-s"))

;; Find file
(global-set-key (kbd "M-o") 'counsel-find-file)

;; Find in directory
(global-set-key (kbd "s-F") 'grep-find)


;; Copy current line to next line and move cursor down
(defun my-duplicate-line-down(comment-first)
    "Duplicate the current line."
    (interactive "P")
    (let ((line-text (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
      (save-excursion
        (if comment-first
            (progn
              (comment-line 1)
              (move-beginning-of-line 1)
              (open-line 1))
          (move-end-of-line 1)
          (open-line 1)
          (forward-char))
        (insert line-text))
      (next-line)))

;; Copy current line to previous line and move cursor up
(defun my-duplicate-line-up(comment-first)
    "Duplicate the current line."
    (interactive "P")
    (let ((line-text (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
      (save-excursion
        (if comment-first
            (progn
              (comment-line 1)
              (move-beginning-of-line 1)
              (open-line 1))
          (move-end-of-line 1)
          (open-line 1)
          (forward-char))
        (insert line-text))))

(global-set-key (kbd "M-S-<down>") 'my-duplicate-line-down)
(global-set-key (kbd "M-S-<up>") 'my-duplicate-line-up)


;; Thanks to Bozhidar Batsov
;; http://emacsredux.com/blog/2013/]05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


;; Always wrap lines
(global-visual-line-mode 1)


;; Show stray whitespace.
(setq-default show-trailing-whitespace 1)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 2 characters wide.
(setq-default tab-width 2)

;; Indentation setting for various languages.
(setq tab-width 4)
(setq standard-indent 4)
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq css-indent-offset 2)
(setq lua-indent-level 4)
;; (setq lisp-indent-level 2)

(indent-tabs-mode -1)



;;------------------------------------------------
;; Load custom EmacsLisp files
;;------------------------------------------------
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/config")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))


;;(load-user-file "init/early.el")











;;------------------------------------------------
;; Generated config
;;------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" default))
 '(package-selected-packages
   '(go-mode web-mode prettier-js ripgrep dashboard ido-grid-mode exec-path-from-shell move-text project-explorer which-key tree-mode projectile visual-regexp flymake-lua yasnippet lua-mode ido-vertical-mode sly treeview badger-theme slime json-mode undo-fu blamer amx paredit company-box doom-modeline dumb-jump magit zenburn-theme tide command-log-mode luarocks hc-zenburn-theme neotree dired-sidebar multiple-cursors use-package rjsx-mode editorconfig shell-pop flx helpful lsp-ui gruvbox-theme tree-sitter ivy-rich flymake counsel yaml-mode doom-themes smex nord-theme rainbow-delimiters helm-lsp ccls typescript-mode alchemist color-theme-sanityinc-tomorrow auto-complete lsp-ivy all-the-icons spacemacs-theme dap-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
