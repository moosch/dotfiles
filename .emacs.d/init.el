;;------------------------------------------------
;; General commands
;;------------------------------------------------
;;; Code:
;; To evaluate and load an expression use C-M-x


;; C-h o     describe-symbol
;; C-Mx i    completion-at-point

;;------------------------------------------------
;; Themes
;;------------------------------------------------

;; Tell Emacs where to find some custom themes
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'wombat t)
;;(load-theme 'wheatgrass t)
;;(load-theme 'nord t)
;;(load-theme 'zenburn t)
;;(load-theme 'hc-zenburn t)
;;(load-theme 'modus-vivendi t)
;;(load-theme 'gruvbox t)
;;(load-theme 'spacemacs-dark t)
;;(load-theme 'badger t)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 130)


;; Customizations
(set-face-background 'default "#222")
(set-face-background 'cursor "#c96")
(set-face-background 'isearch "#c60")
(set-face-foreground 'isearch "#eee")
(set-face-background 'lazy-highlight "#960")
(set-face-foreground 'lazy-highlight "#ccc")
(set-face-foreground 'font-lock-comment-face "#fc0")
(set-face-attribute 'region nil :background "#0c947f" :foreground "#ffffff")


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
        (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode)



;;------------------------------------------------
;; General Setup
;;------------------------------------------------
(defvar is-osx (string-equal "darwin" (symbol-name system-type)))

;; Make ESC quit prompts
;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Use ESC as universal get me out of here command
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Don't show splash screen on startup
;; Flash when the bell rings
(setq inhibit-startup-message t
      cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows
      echo-keystrokes 0.1               ; Show keystrokes right away, don't show the message in the scratch buffer
      initial-scratch-message nil       ; Empty scratch buffer
      initial-major-mode 'org-mode      ; Org mode by default
      sentence-end-double-space nil     ; Sentences should end in one space, come on!
      confirm-kill-emacs 'y-or-n-p      ; y and n instead of yes and no when quitting
      help-window-select t              ; Select help window so it's easy to quit it with 'q'
      visible-bell t)

(fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(delete-selection-mode 1)          ; Delete selected text when typing
(global-unset-key (kbd "s-p"))     ; Don't print


;; Things you'd expect from macOS app.
(global-set-key (kbd "s-s") 'save-buffer)             ;; save
(global-set-key (kbd "s-S") 'write-file)              ;; save as
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ;; quit
(global-set-key (kbd "s-a") 'mark-whole-buffer)       ;; select all


;; Toggle toolbar
(tool-bar-mode -1)
;; Toggle scroll bar
(scroll-bar-mode 1)
;; Toggle menubar
(menu-bar-mode 1)
;; Column numbers
(column-number-mode)
;; Toggle line numbers
(global-display-line-numbers-mode 1)
;; Toggle highlight cursor line
;;(hl-line-mode 1)
(global-hl-line-mode 1)
(set-face-underline 'hl-line nil)
(set-face-attribute 'hl-line nil :inherit nil :background "#333")
;; Toggle cursor blink
(blink-cursor-mode -1)
(show-paren-mode 1)

(indent-tabs-mode -1)

;; Line numbers hooks
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((t nil)))
 '(whitespace-empty ((t nil)))
 '(whitespace-line ((t nil)))
 '(whitespace-newline ((t (:foreground "grey9" :weight normal))))
 '(whitespace-space ((t (:foreground "grey9"))))
 '(whitespace-tab ((t (:foreground "gray9"))))
 '(whitespace-trailing ((t nil))))

;; Custom stuff from HandmadeHero Stream
(defun post-load-stuff ()
  (interactive)
  ;(maximum-frame)
  (set-background-color "#161616")
  ;(set-foreground-color "burlywood")
  (set-cursor-color "#39ccc5"))

(add-hook 'window-setup-hook 'post-load-stuff t)

;; Don't bother with auto save and backups.
;; (setq auto-save-default nil)
(setq make-backup-files nil)

;; Move file to trash instead of removing.
(setq-default delete-by-moving-to-trash t)

;; Revert (update) buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

;; Save sessions
(desktop-save-mode 1)

;; Show full path in the title bar.
(setq-default frame-title-format "%b (%f)")

;; stop emacs saving temp files in the directory hierarchy
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; Default popup navigator
;;(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)


(require 'ido)
(setq ido-ignore-buffers '("^\*Messages\*"))

;;------------------------------------------------
;; Tree Sitter
;;------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elisp-tree-sitter/core")
(add-to-list 'load-path "~/.emacs.d/elisp-tree-sitter/lisp")
(add-to-list 'load-path "~/.emacs.d/elisp-tree-sitter/langs")
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)

(global-tree-sitter-mode)



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
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


;; Set minibuffer colors to distinguish between active and inactive windows
(set-face-attribute 'mode-line nil :background "#4682c2")
(set-face-attribute 'mode-line-inactive nil :background "#444")


;;------------------------------------------------
;; Which Key
;;------------------------------------------------
;; C-h brings up minibuffer of commands
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.2))

;;------------------------------------------------
;; Flx
;;------------------------------------------------
(use-package flx ;; Improves sorting of fuzzy-matched results
  :defer t
  :init (setq ivy-flx-limit 10000))


;;------------------------------------------------
;; Smex
;;------------------------------------------------
;(use-package smex ;; Adds M-x recent command sorting for counsel-M-x
;  :defer 1
;  :after counsel)

;;------------------------------------------------
;; AMX - newer version of smex
;;------------------------------------------------
(use-package amx
	     :ensure t
	     :after ivy
	     :custom
	     (amx-backend 'auto)
	     (amx-save-file "~/.emacs.d/amx-items")
	     (amx-history-length 100)
	     (amx-show-key-bindings nil)
	     :config
	     (amx-mode 1))


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
;; Projectile
;;------------------------------------------------
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects") ;;Customize as fit
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired)) ;; First thing to happen when you load up Projectile




;;------------------------------------------------
;; Magit - Git UI
;;------------------------------------------------
(use-package magit
  ;;:commands (magit-status magit-get-current-branch)
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;;------------------------------------------------
;; Helpful
;;------------------------------------------------
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-function-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . hpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;;------------------------------------------------
;; Rainbow Delimiters
;;------------------------------------------------
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)) ; prog-mode is the base mode for all programming languages




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


(setq standard-indent 2)

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
(setq tab-width 2)
(setq standard-indent 2)
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq lua-indent-level 2)
;; (setq lisp-indent-level 2)






;;------------------------------------------------
;; NeoTree
;;------------------------------------------------
(use-package neotree
  :config
  (setq neo-window-width 32
        neo-create-file-auto-open t
        neo-banner-message nil
        neo-show-updir-line t
        neo-window-fixed-size nil
        neo-vc-integration nil
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-show-hidden-files t
        neo-mode-line-type 'none
        neo-auto-indent-point t)
  (setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
  (setq neo-hidden-regexp-list '("venv" "\\.pyc$" "~$" "\\.git" "__pycache__" ".DS_Store"))
  (global-set-key (kbd "s-B") 'neotree-toggle))           ;; Cmd+Shift+b toggle tree






;;------------------------------------------------
;; Company - complete anything.
;; The popup window when typing
;;------------------------------------------------
(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode))
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-global-modes '(not erc-mode message-mode eshell-mode shell-mode))
  (add-hook 'after-init-hook 'global-company-mode))


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


;; M-s-<up>
;; M-s-<down>


;;------------------------------------------------
;; ivy-posframe - central box popup for commands
;;------------------------------------------------
;(use-package ivy-posframe
;	     :ensure t
;	     :delight
;	     :custom
;	     (ivy-posframe-height-alist
;	       '((swiper . 20)
;		 (t . 10)))
;	     (ivy-posframe-display-function-alist
;	       '((complete-symbol . ivy-posframe-display-at-point)
;		 (counsel-describe-function . nil) ; turn off for search types
;		 (counsel-describe-variable . nil)
;		 (swiper . nil) ; turn off for swiper search
;		 (swiper-isearch . nil)
;		 (t . ivy-posframe-display-at-frame-center)))
;	     :config
;	     (ivy-posframe-mode 1))




;;------------------------------------------------
;; Making Emacs MacOSX friendly
;;------------------------------------------------
;;(when is-osx
;; (setq mac-command-modifier 'meta)
;;  (setenv "PATH" (concat "/opt/local/bin:/opt/local/sbin:" (getenv "PATH"))))
;; Set Cmd to be "super"
;;(setq mac-right-command-modifier 'super)
;;(setq mac-command-modifier 'super)

;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
;;(setq mac-right-option-modifier 'nil)


;;------------------------------------------------
;; Making Emacs Linux friendly
;;------------------------------------------------
;; Set Alt key as "super"
(setq win32-lwindow-modifier 'super)
;; Set Windows key as "Meta"



;;------------------------------------------------
;; Flycheck & Flymake
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
;; Flycheck
;;------------------------------------------------
;(use-package flycheck
;  :ensure t
;  :config
;  (bind-key "M-n" 'flycheck-next-error flycheck-mode-map)
;  (bind-key "M-p" 'flycheck-previous-error flycheck-mode-map))



;;------------------------------------------------
;; LSP Mode
;;------------------------------------------------
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file sumbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))
;; (setq lsp-ui-sideline-enable t)
;; (setq lsp-ui-sideline-show-hover t)
;; (setq lsp-ui-peek-mode t)
;; (setq lsp-ui-peek-enable t)


;;------------------------------------------------
;; LSP Treemacs
;;------------------------------------------------
(use-package lsp-treemacs
  :after lsp)

;;------------------------------------------------
;; Helm LSP
;;------------------------------------------------

;;------------------------------------------------
;; LSP Ivy
;;------------------------------------------------
(use-package lsp-ivy)

;;------------------------------------------------
;; LSP Company
;;------------------------------------------------
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

; The popup box
(use-package company-box
  :hook (company-mode . company-box-mode))


;;------------------------------------------------
;; LSP - Modes
;;------------------------------------------------
;;(use-package typescript-mode
;;  :mode "\\.ts\\"
;;  :hook (typescript-mode . lsp-deferred)
;;  :config
;;  (setq typescript-indent-level 2))

;; (use-package lua-mode
;;  :ensure t)

;; (use-package lua-mode))
;; (add-hook 'lua-mode-hook 'lsp)
;; (setq lsp-enable-semantic-highlighting t)



;;------------------------------------------------
;; Lua Mode
;;------------------------------------------------

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  ;; :hook (lua-mode . lsp-deferred)
  :init
  (setq lua-indent-level 2)
  (setq lua-indent-string-contents t)
  (setq lua-indent-nested-block-content-align nil)
  (setq lua-indent-close-paren-align nil))

;; (defun lua-at-most-one-indent (old-function &rest arguments)
;;   (let ((old-res (apply old-function arguments)))
;;     (if (> old-res lua-indent-level) lua-indent-level old-res)))

;; (advice-add #'lua-calculate-indentation-block-modifier
;;             :around #'lua-at-most-one-indent)

(setq c-basic-offset 2)



;;------------------------------------------------
;; GLSL Mode
;;------------------------------------------------
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))


;;------------------------------------------------
;; Blamer - git lens
;;------------------------------------------------
(use-package blamer
  :ensure t
  :bind (("s-i" . blamer-show-commit-info)
         ("C-c i" . ("s-i" . blamer-show-posframe-commit-info)))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))


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

;;(load-user-file "config/c.el")

(load-user-file "igrep/igrep.el")
(autoload 'igrep "igrep"
  "*Run `grep` PROGRAM to match REGEX in FILES..." t)
(autoload 'igrep-find "igrep"
  "*Run `grep` via `find`..." t)
(autoload 'igrep-visited-files "igrep"
  "*Run `grep` ... on all visited files." t)
(autoload 'dired-do-igrep "igrep"
  "*Run `grep` on the marked (or next prefix ARG) files." t)
(autoload 'dired-do-igrep-find "igrep"
  "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
(autoload 'Buffer-menu-igrep "igrep"
  "*Run `grep` on the files visited in buffers marked with '>'." t)
(autoload 'igrep-insinuate "igrep"
  "Define `grep' aliases for the corresponding `igrep' commands." t)

(autoload 'egrep "igrep"
  "*Run `egrep`..." t)
(autoload 'fgrep "igrep"
  "*Run `fgrep`..." t)
(autoload 'agrep "igrep"
  "*Run `agrep`..." t)
(autoload 'grep-find "igrep"
  "*Run `grep` via `find`..." t)
(autoload 'egrep-find "igrep"
  "*Run `egrep` via `find`..." t)
(autoload 'fgrep-find "igrep"
  "*Run `fgrep` via `find`..." t)
(autoload 'agrep-find "igrep"
  "*Run `agrep` via `find`..." t)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(blamer tree-sitter glsl-mode lua-mode company-mode company magit projectile color-theme-sanityinc-tomorrow badger-theme spacemacs-theme hc-zenburn-theme zenburn-theme nord-theme gruvbox-theme helpful smex flx doom-modeline ivy-rich rainbow-delimiters command-log-mode yasnippet which-key visual-regexp use-package undo-fu treeview tree-mode sly slime shell-pop project-explorer pfuture neotree multiple-cursors move-text luarocks hydra flycheck exec-path-from-shell editorconfig dumb-jump dired-sidebar counsel cfrs auto-complete alchemist ace-window))
 '(safe-local-variable-values '((comment-fill-column . 80))))
