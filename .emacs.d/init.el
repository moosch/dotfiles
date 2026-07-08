;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrate use-package with straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Redirect backup and auto-save files to /tmp instead of cluttering working dirs
(make-directory "/tmp/emacs-backups" t)
(make-directory "/tmp/emacs-autosaves" t)
(setq backup-directory-alist `(("." . "/tmp/emacs-backups")))
(setq auto-save-file-name-transforms `((".*" "/tmp/emacs-autosaves/" t)))
(setq create-lockfiles nil)

;; Auto-revert buffers when files change on disk
(global-auto-revert-mode 1)
(setq auto-revert-use-notify nil)

;; Show relative path to working dir in modeline
(defun my/modeline-buffer-path ()
  (if-let* ((file (buffer-file-name))
            (root (or (project-root (project-current)) default-directory))
            (rel  (file-relative-name file root)))
      rel
    (buffer-name)))

(setq-default mode-line-buffer-identification
              '(:eval (propertize (my/modeline-buffer-path)
                                  'face 'mode-line-buffer-id)))

;; Scrolling
(pixel-scroll-precision-mode 1)  ; smooth trackpad/wheel scrolling in GUI
(xterm-mouse-mode 1)              ; mouse support in terminal

;; Hide line-wrap continuation indicators
(set-display-table-slot standard-display-table 'wrap ?\s)
(setq fringe-indicator-alist
      (assq-delete-all 'continuation fringe-indicator-alist))
(global-set-key (kbd "M-<up>")   #'scroll-down-line)
(global-set-key (kbd "M-<down>") #'scroll-up-line)

;; General settings
(setq use-short-answers t)
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(electric-indent-mode -1)
(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

(setq native-comp-async-report-warnings-errors 'silent)

(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer
             1 nil
             (lambda ()
               (dolist (buf '("*scratch*" "*Messages*" "*Async-native-compile-log*"))
                 (when (get-buffer buf)
                   (kill-buffer buf)))
               ;; Only kill straight-process if no process is still running in it
               (when-let ((buf (get-buffer "*straight-process*")))
                 (unless (get-buffer-process buf)
                   (kill-buffer buf)))))))

;; Dired
(use-package dired
  :straight nil
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t))

(use-package dired-subtree
  :after dired
  :config
  (defun my/dired-open-or-expand ()
    "Open file, or expand/collapse directory inline as a tree."
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (if (and file (file-directory-p file))
          (dired-subtree-toggle)
        (dired-find-file))))
  (defun my/dired-create-file ()
    "Prompt for a filename, create it in the current dired directory, then refresh."
    (interactive)
    (let* ((dir  (dired-current-directory))
           (name (read-string "New file name: "))
           (file (expand-file-name name dir)))
      (write-region "" nil file nil 'silent)
      (revert-buffer)
      (dired-goto-file file)))

  (defun my/dired-delete-file ()
    "Delete file or directory at point after y/n confirmation."
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (when (and file
                 (y-or-n-p (format "Delete %s? " (file-name-nondirectory file))))
        (if (file-directory-p file)
            (delete-directory file t)
          (delete-file file))
        (revert-buffer))))

  (defun my/dired-move-file ()
    "Move file or directory at point, prompting for destination."
    (interactive)
    (let* ((file (dired-get-filename nil t))
           (dest (read-file-name
                  (format "Move %s to: " (file-name-nondirectory file))
                  (dired-current-directory))))
      (when file
        (rename-file file dest)
        (revert-buffer))))

  (defun my/dired-rename-file ()
    "Rename file or directory at point in-place."
    (interactive)
    (let* ((file     (dired-get-filename nil t))
           (name     (file-name-nondirectory file))
           (new-name (read-string (format "Rename %s to: " name) name))
           (new-file (expand-file-name new-name (file-name-directory file))))
      (when (and file (not (string= name new-name)))
        (rename-file file new-file)
        (revert-buffer)
        (dired-goto-file new-file))))

  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map
      (kbd "RET") #'my/dired-open-or-expand
      (kbd "^")   #'dired-up-directory)))

;; which-key
(use-package which-key
  :custom
  (which-key-idle-delay 0.2)
  :config
  (which-key-mode))

;; Theme
(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (custom-set-faces
   '(hl-line ((t (:background "#282828" :extend t))))
   '(region  ((t (:background "#303540"))))))

;; Tree-sitter
(use-package treesit
  :straight nil
  :config
  (setq treesit-font-lock-level 4)
  (setq treesit-language-source-alist
        '((go         "https://github.com/tree-sitter/tree-sitter-go"         "v0.25.0")
          (gomod      "https://github.com/camdencheek/tree-sitter-go-mod"    "v1.1.0")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.25.0")
          (python     "https://github.com/tree-sitter/tree-sitter-python"     "v0.25.0")
          (c          "https://github.com/tree-sitter/tree-sitter-c"          "v0.24.2")
          (cpp        "https://github.com/tree-sitter/tree-sitter-cpp"        "v0.23.4")
          (rust       "https://github.com/tree-sitter/tree-sitter-rust"       "v0.24.2")
          (json       "https://github.com/tree-sitter/tree-sitter-json"       "v0.24.8")
          (bash       "https://github.com/tree-sitter/tree-sitter-bash"       "v0.25.1")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
          (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
          (html       "https://github.com/tree-sitter/tree-sitter-html"       "v0.23.2")
          (heex       "https://github.com/phoenixframework/tree-sitter-heex"  "v0.9.0")
          (elixir     "https://github.com/elixir-lang/tree-sitter-elixir"     "v0.3.5")
          (yaml       "https://github.com/ikatyang/tree-sitter-yaml"          "v0.5.0")))
  (add-to-list 'auto-mode-alist '("\\.go\\'"  . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'"   . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'"   . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'"  . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'"   . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'"  . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

  ;; Auto-install any grammars not yet compiled (runs once in background at startup)
  (dolist (grammar treesit-language-source-alist)
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

(use-package elixir-ts-mode)

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

;; Per-language indent settings
(dolist (hook '(c-ts-mode-hook c++-ts-mode-hook))
  (add-hook hook (lambda () (setq indent-tabs-mode nil tab-width 4))))
(add-hook 'go-ts-mode-hook
          (lambda ()
            (setq indent-tabs-mode t tab-width 4)
            (add-hook 'before-save-hook #'gofmt nil t)
            (evil-local-set-key 'insert (kbd "TAB") (lambda () (interactive) (insert "\t")))))
(add-hook 'elixir-ts-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil)
            (setq-local tab-width 2)
            (setq-local elixir-ts-indent-offset 2)))
(dolist (hook '(js-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
  (add-hook hook (lambda () (setq indent-tabs-mode nil tab-width 2))))

(use-package odin-mode
  :straight (:host github :repo "mattt-b/odin-mode"))

(use-package terraform-mode
  :hook (terraform-mode . eglot-ensure)
  :custom
  (terraform-indent-level 2))

;; Ensure language server binaries are findable
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))
(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(add-to-list 'exec-path (expand-file-name "~/Software/elixir-ls"))
(add-to-list 'exec-path (expand-file-name "~/Software/node/bin"))

;; Eldoc — disable automatic minibuffer popup; use SPC k to show on demand
(use-package eldoc
  :straight nil
  :custom
  (eldoc-idle-delay 1e10)            ; effectively never auto-display
  (eldoc-echo-area-use-multiline-p nil))

;; LSP
(use-package eglot
  :straight nil
  :hook
  ((go-ts-mode          . eglot-ensure)
   (c-ts-mode           . eglot-ensure)
   (elixir-ts-mode      . eglot-ensure)
   (js-ts-mode          . eglot-ensure)
   (typescript-ts-mode  . eglot-ensure)
   (tsx-ts-mode         . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(elixir-ts-mode . ("elixir-ls")))
  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("terraform-ls" "serve")))
  (add-to-list 'eglot-server-programs
               `((js-ts-mode typescript-ts-mode tsx-ts-mode)
                 . ("typescript-language-server" "--stdio"
                    "--tsserver-path"
                    ,(string-trim (shell-command-to-string
                                   "sed -n 's|.*cmd-shim-target=\\(.*\\)/bin/tsserver|\\1|p' $(which tsserver)"))))))

;; In-buffer completion
(use-package corfu
  :custom
  (corfu-auto t)           ; show popup automatically
  (corfu-auto-delay 0.2)   ; slight delay to avoid noise while typing fast
  (corfu-auto-prefix 2)    ; start after 2 characters
  (corfu-preselect 'first) ; first candidate is pre-selected
  :bind
  (:map corfu-map
   ("<up>"     . corfu-previous)
   ("<down>"   . corfu-next)
   ("TAB"      . corfu-insert)
   ("<tab>"    . corfu-insert)
   ("RET"      . corfu-insert)
   ("<return>" . corfu-insert)
   ("<escape>" . corfu-quit))
  :init
  (global-corfu-mode)
  :config
  ;; Dismiss popup when exiting insert mode (e.g. ESC with no popup visible)
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit))

(use-package corfu-terminal
  :after corfu
  :config
  ;; Child frames don't work in terminal — use corfu-terminal as the popup renderer
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; Minibuffer completion
(use-package vertico
  :config
  (vertico-mode)
  (define-key vertico-map (kbd "<escape>") #'abort-minibuffers))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  ;; Treat "/" as an orderless separator so "src/main.go" matches "src/**/main.go"
  (defun my/orderless-slash-dispatcher (pattern _index _total)
    (when (string-match-p "/" pattern)
      (cons 'orderless-regexp
            (mapconcat #'regexp-quote (split-string pattern "/") ".*"))))
  (add-to-list 'orderless-style-dispatchers #'my/orderless-slash-dispatcher))

(use-package consult
  :custom
  (consult-preview-key 'any)
  :config
  (defun my/project-find-file ()
    "Fuzzy-find a file in the current project with live preview.
Supports NAME:LINE syntax — preview scrolls to LINE and RET opens at LINE."
    (interactive)
    (require 'project)
    (let* ((pr    (or (project-current nil) (cons 'transient default-directory)))
           (files (project-files pr))
           (jump-line nil)
           (file-preview (consult--file-preview))
           (state
            (lambda (action cand)
              (when (eq action 'preview)
                (let ((input (minibuffer-contents-no-properties)))
                  (setq jump-line
                        (when (string-match ":\\([0-9]+\\)\\'" input)
                          (string-to-number (match-string 1 input))))))
              (funcall file-preview action cand)
              (when (and (eq action 'preview) cand jump-line (> jump-line 0))
                (when-let* ((buf (find-buffer-visiting cand))
                            (win (get-buffer-window buf)))
                  (with-selected-window win
                    (goto-char (point-min))
                    (forward-line (1- jump-line)))))))
           (orderless-style-dispatchers
            (cons (lambda (pat _idx _total)
                    (when (string-match "\\`\\(.+\\):[0-9]+\\'" pat)
                      (cons 'orderless-regexp
                            (regexp-quote (match-string 1 pat)))))
                  orderless-style-dispatchers))
           (selected
            (consult--read files
                           :prompt "Find file: "
                           :category 'file
                           :state state
                           :require-match t)))
      (find-file selected)
      (when (and jump-line (> jump-line 0))
        (goto-char (point-min))
        (forward-line (1- jump-line))
        (recenter)))))

;; Ivy/Counsel — used only for buffer switching (preview + C-k to kill)
(use-package ivy
  :config
  ;; Don't enable ivy-mode globally — keep vertico/consult for everything else
  (setq ivy-use-virtual-buffers nil
        ivy-count-format "(%d/%d) "))

(use-package counsel
  :after ivy)

;; Undo
(use-package undo-fu)

;; Clipboard (terminal)
(use-package xclip
  :config
  (xclip-mode 1))

;; Evil mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-toggle-key "")
  (setq evil-esc-delay 0)
  (setq evil-undo-system 'undo-fu)
  (setq evil-search-module 'evil)
  (setq evil-ex-search-persistent-highlight t)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "<escape>") #'evil-ex-nohighlight)
  (define-key evil-visual-state-map (kbd "<escape>") #'evil-normal-state)

  ;; Visual paste: restore kill-ring after paste so clipboard is not overwritten
  (defun my/evil-visual-paste (count)
    "Paste over visual selection without overwriting the clipboard."
    (interactive "p")
    (let ((saved (car kill-ring)))
      (evil-paste-after count)
      (when saved (kill-new saved t))))
  (evil-define-key 'visual 'global "p" #'my/evil-visual-paste))

(use-package drag-stuff
  :after evil
  :config
  (drag-stuff-global-mode 1)
  (evil-define-key '(normal visual) 'global
    (kbd "S-<up>")   #'drag-stuff-up
    (kbd "S-<down>") #'drag-stuff-down))

;; Git
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (custom-set-faces
   '(diff-hl-insert ((t (:background "#73c936" :foreground "#73c936"))))
   '(diff-hl-change ((t (:background "#cc8c3c" :foreground "#cc8c3c"))))
   '(diff-hl-delete ((t (:background "#f43841" :foreground "#f43841"))))))

(use-package magit
  :config
  (defun my/magit-blame-toggle ()
    (interactive)
    (if (bound-and-true-p magit-blame-mode)
        (magit-blame-quit)
      (call-interactively #'magit-blame-addition))))

;; AI agent shell
(use-package agent-shell
  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)))

;; Code folding (tree-sitter aware)
(use-package treesit-fold
  :straight (:host github :repo "emacs-tree-sitter/treesit-fold")
  :hook (prog-mode . treesit-fold-mode)
  :config
  (defun my/treesit-fold-toggle-dwim ()
    "Toggle fold for the function at point.
Works from the function header, body, or closing brace/end by always
targeting the function's body block directly.

Handles standard languages (block/compound_statement) and Elixir,
where def/defp are `call' nodes whose body is a `do_block' child."
    (interactive)
    (let* ((block-types '("block" "statement_block" "body"
                          "compound_statement" "do_block"))
           (func-types  '("function_declaration" "method_declaration"
                          "function_definition"  "method_definition"
                          "arrow_function"       "function_expression"
                          "function_item"))
           (node      (treesit-node-at (point)))
           (func-node (treesit-parent-until
                       node
                       (lambda (n)
                         (or (member (treesit-node-type n) func-types)
                             ;; Elixir: def/defp/defmacro are `call' nodes
                             ;; whose body is a do_block child
                             (and (equal (treesit-node-type n) "call")
                                  (seq-find (lambda (c)
                                              (equal (treesit-node-type c) "do_block"))
                                            (treesit-node-children n t)))))
                       t))
           (body-node (when func-node
                        (seq-find (lambda (c) (member (treesit-node-type c) block-types))
                                  (treesit-node-children func-node t)))))
      (if body-node
          (save-excursion
            (goto-char (treesit-node-start body-node))
            (treesit-fold-toggle))
        (treesit-fold-toggle)))))

;; Leader key
(use-package general
  :after evil
  :config
  (general-evil-setup)

  (general-define-key "<escape>" #'keyboard-quit)

  (general-define-key
    :states 'normal
    "U" #'evil-redo)

  (general-create-definer leader!
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")

  (leader!
    "SPC" '(execute-extended-command :wk "M-x")

    "f"   '(:ignore t :wk "files")
    "ff"  '(my/project-find-file :wk "find file")
    "fg"  '(consult-ripgrep :wk "grep project")
    "fr"  '(consult-recent-file :wk "recent files")
    "fs"  '(save-buffer :wk "save file")
    "fd"  '(dired :wk "dired")

    "d"   '(:ignore t :wk "dired")
    "dn"  '(my/dired-create-file  :wk "new file")
    "dd"  '(my/dired-delete-file  :wk "delete")
    "dm"  '(my/dired-move-file    :wk "move")
    "dr"  '(my/dired-rename-file  :wk "rename")

    "b"   '(:ignore t :wk "buffers")
    "bb"  '(counsel-switch-buffer :wk "switch buffer")
    "bk"  '(kill-this-buffer :wk "kill buffer")

    "s"   '(:ignore t :wk "search")
    "sf"  '(consult-find :wk "find file by name")
    "sg"  '(consult-ripgrep :wk "grep project")

    "|"   '(split-window-right :wk "split vertical")
    "-"   '(split-window-below :wk "split horizontal")

    "<left>"  '(windmove-left :wk "window left")
    "<right>" '(windmove-right :wk "window right")
    "<up>"    '(windmove-up :wk "window up")
    "<down>"  '(windmove-down :wk "window down")

    "w"   '(:ignore t :wk "windows")
    "wk"  '(delete-window :wk "close window")
    "wo"  '(delete-other-windows :wk "close others")

    "p"   '(my/project-find-file :wk "find project file")

    "a"   '(agent-shell :wk "agent shell")

    "c"   '(my/treesit-fold-toggle-dwim :wk "toggle fold")

    "<"   '(evil-jump-backward :wk "jump back")
    ">"   '(evil-jump-forward  :wk "jump forward")

    "k"   '(eldoc-doc-buffer :wk "docs")

    "g"   '(:ignore t :wk "git/goto")
    "gg"  '(magit-status :wk "git status")
    "gb"  '(my/magit-blame-toggle :wk "git blame")
    "gd"  '(xref-find-definitions :wk "definition")
    "gr"  '(xref-find-references :wk "references"))

  ;; Override SPC c in visual mode to comment/uncomment the selection
  ;; (normal mode keeps the fold-toggle binding above)
  (general-define-key
    :states 'visual
    :keymaps 'override
    :prefix "SPC"
    "c" '(comment-or-uncomment-region :wk "comment region"))

  ;; gc: comment current line (normal) or selection (visual)
  (general-define-key
    :states 'normal
    :keymaps 'override
    "gc" #'comment-line)
  (general-define-key
    :states 'visual
    :keymaps 'override
    "gc" #'comment-or-uncomment-region))
