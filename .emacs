;; .emacs --- Anselm's emacs config
;; Commentary: My modern emacs config using evil, straight, gpt.

;;; Code:

;; straight.el for package management (https://github.com/radio-software/straight.el)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(straight-use-package 'org)


;; ====================================
;; CORE SETTINGS & PERFORMANCE
;; ====================================

;; Start screen
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; Garbage collection settings
(setq gc-cons-threshold (* 50 1000 1000)) ; 50MB
(setq read-process-output-max (* 1024 1024)) ; 1MB

(use-package pg
  :straight (:host github :repo "emarsden/pg-el"))

(use-package pgmacs
  :straight (:host github :repo "emarsden/pgmacs"))


;; Startup Performance Monitoring
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; File handling
(setq
 make-backup-files nil           ; No backup~ files
 auto-save-default nil           ; No #autosave# files
 undo-tree-auto-save-history nil ; No ~undo-tree~ files
 create-lockfiles nil)           ; No .# files

;; Error handling
(setq debug-on-error nil)
(setq native-comp-async-report-warnings-errors nil)

;; Basic UX improvements
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)      ; Auto-refresh buffers
(savehist-mode 1)                ; Remember minibuffer history
(setq confirm-kill-processes nil) ;; Don't wait for confirmation if there are running processes
(setq enable-recursive-minibuffers t)

;; ====================================
;; CORE FUNCTIONALITY
;; ====================================

;; =================
;; System Path
;; =================
(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize))

;; =================
;; Minibuffer & Completion
;; =================

;; Insert paths into minibuffer prompts
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; Completion backend
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :init
  (vertico-mode))

;; Minibuffer actions
(use-package embark
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; ====================================
;; EVIL MODE & NAVIGATION
;; ====================================

(use-package evil
  :after undo-tree
  :init
  (setq evil-want-integration t ;; This is optional since it's already set to t by default.
        evil-want-keybinding nil
        evil-symbol-word-search t  ;; Makes evil-search-word- look for word
        evil-undo-system 'undo-tree
        evil-search-module 'evil-search)
  ;; code folding
  :bind
  (:map evil-normal-state-map ("za" . hs-toggle-hiding))
  :config
  (evil-mode 1))


(global-subword-mode t)

(use-package undo-tree
  :demand t
  :diminish 'undo-tree-mode
  :init (global-undo-tree-mode))

(use-package evil-collection
  :after evil
  :diminish 'evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package evil-goggles
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; increment/decrement numbers like in vim
(use-package evil-numbers
  :after evil
  :bind (("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)))

;; surround  with symbol
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; File navigation
(use-package treemacs
  :defer t
  :config
  (progn
    (treemacs-set-width 50)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
    ))


(use-package treemacs-evil
  :after (treemacs evil)
  :config (evil-treemacs-state t))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))


;; Show breadcrumbs
(use-package breadcrumb
  :config (breadcrumb-mode))

;; ====================================
;; UI CONFIGURATION
;; ====================================
;; Basic UI elements
(tool-bar-mode -1)                        ; Disable toolbar
(scroll-bar-mode -1)                      ; Disable visible scrollbar
(set-fringe-mode 10)                      ; Set margin fringe size
(tooltip-mode -1)                         ; Disable tooltips
(show-paren-mode t)                       ; Show matching parentheses
(setq ns-pop-up-frames nil)               ; Open files in same frame
(setq-default frame-title-format "%b (%f)"); Show full path in title bar

;; Theme configuration
(load-theme 'modus-vivendi t)             ; Set default theme

;; Line numbers configuration
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(column-number-mode t)
(setq display-line-numbers-width nil)

;; Default to 4 spaces per tab
(setq-default tab-width 4 indent-tabs-mode nil)

;; Ensure line numbers width adjusts to buffer size
(defun display-line-numbers-equalize ()
  "Equalize the width of line numbers based on buffer size."
  (setq display-line-numbers-width
        (length (number-to-string (line-number-at-pos (point-max))))))
(add-hook 'find-file-hook 'display-line-numbers-equalize)

;; Modeline configuration
(use-package nerd-icons)

(use-package doom-modeline
  :init
  (setq doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-modal-icon t
        doom-modeline-persp-name t
        doom-modeline-persp-icon t
        doom-modeline-minor-modes nil
        doom-modeline--battery-status t)
  :config
  (doom-modeline-mode 1))

;; Buffer highlighting
(defun advise-dimmer-config-change-handler ()
  "Advise to only force process if no predicate is truthy."
  (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                         dimmer-prevent-dimming-predicates)))
    (unless ignore
      (when (fboundp 'dimmer-process-all)
        (dimmer-process-all t)))))

(defun corfu-frame-p ()
  "Check if the buffer is a corfu frame buffer."
  (string-match-p "\\` \\*corfu" (buffer-name)))

(defun dimmer-configure-corfu ()
  "Convenience settings for corfu users."
  (add-to-list
   'dimmer-prevent-dimming-predicates
   #'corfu-frame-p))

(use-package dimmer
  :custom
  (dimmer-fraction 0.4)
  :config
  (advice-add
   'dimmer-config-change-handler
   :override 'advise-dimmer-config-change-handler)
  (dimmer-configure-corfu)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-which-key)
  (dimmer-mode t))


;; Terminal colors support
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package xterm-color
  :init
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

;; Window management
(use-package golden-ratio
  :after evil
  :diminish 'golden-ratio-mode
  :custom
  (golden-ratio-exclude-modes '(treemacs-mode))
  :config
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-down
                  evil-window-up)))
  (golden-ratio-mode))

;; Mode line improvements
(use-package mode-line-bell
  :init
  (setq visible-bell 1)
  :config
  (mode-line-bell-mode 1))

;; Better annotations in minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;; Add icons to annotations
(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

;; Shell colors
(add-hook 'shell-mode-hook
          (lambda ()
            (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

;; Font configuration
(defun set-font-height (height)
  "Set the font height to HEIGHT."
  (interactive "nEnter font height: ")
  (set-face-attribute 'default nil :height height))

(set-font-height 125)

;; Search highlighting
(use-package evil-anzu
  :after evil)

(use-package anzu
  :diminish 'anzu-mode
  :config
  (global-anzu-mode))

;; Diminish minor modes from modeline
(use-package diminish)

;; Key hint system
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; ====================================
;; DEVELOPMENT TOOLS
;; ====================================

;; Declaratively make HTTP requests
(use-package restclient)

(use-package json-ts-mode
      :mode ("\\.jsonl?\\'" "\\.bubble\\'"))

;; =================
;; Version Control
;; =================
(use-package magit
  :diminish 'smerge-mode
  :bind ("C-x g" . magit-status))

(use-package forge
  :after magit)

(use-package treemacs-magit
  :after (treemacs magit))
(setq auth-sources '("~/.authinfo"))

(setq vc-follow-symlinks t) ;; don't warn of symlinks

;; =================
;; LSP Support
;; =================

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc)
  (define-key eglot-mode-map (kbd "C-c d") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-c x") 'xref-find-references)
  (define-key eglot-mode-map (kbd "C-c i") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c c") 'eglot-find-declaration)
  (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c =") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c M-r") 'eglot-reconnect)
  (define-key eglot-mode-map (kbd "C-c m") 'eglot-imenu)

  ;; Use pylsp as primary (rename, completion, mypy via plugin)
  ;; and ruff as secondary (fast linting + ruff-specific code actions)
  (add-to-list 'eglot-server-programs
               '(python-base-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs
               '(python-base-mode . ("ruff" "server")) t) ; append

  ;; Workspace configuration for all servers
  (setq-default eglot-workspace-configuration
                '((pylsp
                   (plugins
                    (ruff (enabled . :json-false))  ; disable ruff inside pylsp, we run it standalone
                    (mypy (enabled . t))
                    (pycodestyle (enabled . :json-false))
                    (pyflakes (enabled . :json-false))
                    (flake8 (enabled . :json-false))))
                  (typescript
                   (tsserver
                    (maxTsServerMemory . 8192)))
                  (eslint
                   (execArgv . ["--max_old_space_size=8192"])))))

;; Python: start eglot + format on save
(add-hook 'python-base-mode-hook
          (lambda ()
            (eglot-ensure)
            (add-hook 'after-save-hook 'eglot-format nil t)))

(add-hook 'clojure-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)

(use-package eglot-booster
  :straight ( eglot-booster :type git :host nil :repo "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

;; Prevent eglot from spamming messages
(defun stop-spamming-pls-2 (orig-fun &rest args)
  "Stop eglot from spamming the echo area"
  (if (not (string-equal (nth 1 args) "$/progress"))
      (apply orig-fun args)))
(advice-add 'eglot-handle-notification :around #'stop-spamming-pls-2)


;; =================
;; Code Completion
;; =================
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Completion after two characters
  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode))


;; Add icons to completion options
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; =================
;; Code Formatting
;; =================
(use-package format-all
  :demand t
  :diminish format-all-mode
  :config
  (add-hook 'python-mode 'format-all-mode 'format-all-ensure-formatter)
  (add-hook 'js-mode 'format-all-mode 'format-all-ensure-formatter)
  (add-hook 'web-mode 'format-all-mode 'format-all-ensure-formatter)
  (add-hook 'ess-r-mode-hook 'format-all-mode 'format-all-ensure-formatter)
  (add-hook 'c-mode-common-hook 'format-all-mode 'format-all-ensure-formatter)
  (add-hook 'emacs-lisp-mode 'format-all-mode 'format-all-ensure-formatter))

;; =================
;; Snippets
;; =================
(use-package yasnippet
  :after yasnippet-snippets
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;; =================
;; AI Assistance
;; =================
(use-package pimacs :straight (:host github :repo "ananthakumaran/pimacs.el"))

(defun load-secret-key-from-file (file-path)
  "Load the secret key from the specified FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (string-trim (buffer-string))))

(use-package le-gpt
  :after evil
  :bind (("M-C-g" . le-gpt-chat)
         ("M-C-n" . le-gpt-complete-at-point)
         ("M-C-t" . le-gpt-transform-region)
         ;; if you use consult
         ("C-c C-s" . le-gpt-consult-buffers))
  :config
  (setq le-gpt-api-type 'anthropic)
  (setq le-gpt-model "claude-opus-4-7")
  (setq le-gpt-python-path "/Users/anselm/.venvs/le-gpt/bin/python")
  (setq le-gpt-max-tokens 10000)

  (setq le-gpt-openai-key (load-secret-key-from-file "~/.secrets/OPENAIKEY"))
  (setq le-gpt-anthropic-key (load-secret-key-from-file "~/.secrets/ANTHROPICKEY"))

  (evil-define-key 'normal le-gpt-buffer-list-mode-map
    (kbd "RET") #'le-gpt-buffer-list-open-buffer
    (kbd "d") #'le-gpt-buffer-list-mark-delete
    (kbd "u") #'le-gpt-buffer-list-unmark
    (kbd "x") #'le-gpt-buffer-list-execute
    (kbd "gr") #'le-gpt-buffer-list-refresh
    (kbd "/") #'le-gpt-buffer-list-filter
    (kbd "C-c C-s") #'le-gpt-consult-buffers
    (kbd "q") #'quit-window))



;; =================
;; Language Support
;; =================

;; Typescript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(setq js-indent-level 2)
;; make sure that eglot formats with two spaces for typescript
(setq typescript-indent-level 2)
(add-hook 'typescript-ts-mode-hook (lambda () (setq-local typescript-indent-level 2)))
(add-hook 'tsx-ts-mode-hook (lambda () (setq-local typescript-indent-level 2)))

(use-package nvm
  :straight (:host github :repo "rejeep/nvm.el"))


;; Elisp
(add-hook 'emacs-lisp-mode-hook (lambda ()
                             ;; aligns with LSP config
                             (local-set-key (kbd "C-c d") 'xref-find-definitions)))

;; Python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt"
      python-shell-completion-native-enable t)

(use-package pyvenv
  :init
  (pyvenv-tracking-mode))

(defun start-django-shell ()
  "Start Django shell if manage.py exists."
  (interactive)
  (let* ((root-dir (vc-root-dir))
         (manage-py-file (concat root-dir "manage.py")))
    (if (file-exists-p manage-py-file)
        (run-python (concat manage-py-file " shell") t)
      (message (concat "manage.py doesn't exist in " root-dir)))))

(defun run-python-with-autoreload ()
  "Start Python REPL with autoreload enabled."
  (run-python)
  (python-shell-send-string "%load_ext autoreload")
  (python-shell-send-string "%autoreload 0"))

;; Clojure
(use-package clojure-mode)
(use-package cider
  :init
  (setq cider-auto-jump-to-error nil))

;; C/C++
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c m")
                           (lambda()
                             (compile "make")))))

;; Markdown
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; Docker
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; K8s
(use-package kubernetes)
(use-package kubernetes-evil)

;; Terraform
(use-package terraform-mode)

;; YAML
(use-package yaml-pro
  :hook (yaml-ts-mode . yaml-pro-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

;; CSV
(defun csv-open-link-at-point()
  (interactive)
  (let* ((start (save-excursion (re-search-backward ",")))
         (end (save-excursion (re-search-forward ",")))
         (link (buffer-substring-no-properties  (+ start 1) (- end 1))))
    (browse-url link)))

(use-package csv-mode
  :bind (("C-c C-o" . csv-open-link-at-point)))

;; Debugger
(use-package dap-mode
  :config
  (require 'dap-python)
  (require 'dap-node)
  :custom
  (dap-python-debugger "debugpy")
  :init
  (dap-mode 1)
  (dap-ui-mode 1))

(defun dap-python--pyenv-executable-find (command)
  (executable-find command))

;; ====================================
;; PRODUCTIVITY TOOLS
;; ====================================

;; =================
;; Org Mode
;; =================

(use-package ekg
  :config
  (require 'ekg-auto-save)
  (add-hook 'ekg-capture-mode-hook #'ekg-auto-save-mode)
  (add-hook 'ekg-edit-mode-hook #'ekg-auto-save-mode))

(evil-define-key 'normal ekg-notes-mode-map
  "A" 'ekg-notes-any-tags
  "B" 'ekg-notes-select-and-browse-url
  "a" 'ekg-notes-any-note-tags
  "b" 'ekg-notes-browse
  "c" 'ekg-notes-create
  "d" 'ekg-notes-delete
  "g" 'ekg-notes-refresh
  "k" 'ekg-notes-kill
  "n" 'ekg-notes-next ;; or "j"
  "o" 'ekg-notes-open
  "p" 'ekg-notes-previous ;; or "k"
  "q" 'kill-buffer-and-window
  "t" 'ekg-notes-tag)

;; Enable variable pitch fonts in Org mode
(setq org-agenda-files '("~/org/"))

;; Hide emphasis markers (bold, italic, etc.)
(setq org-hide-emphasis-markers t)

;; Set default font faces
(custom-theme-set-faces
 'user
 '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-document-title ((t (:height 1.5 :weight bold)))))

;; Set pretty entities (e.g., \alpha → α)
(setq org-pretty-entities t)

;; UTF8 bullets instead of *
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package evil-org
  :after org
  :diminish 'evil-org-mode
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode)
         (evil-org-mode . (lambda ()
                            (evil-org-set-key-theme
                             '(navigation todo insert textobjects additional)))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(setq org-latex-prefer-user-labels t)

;; add an execute function for python for org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (emacs-lisp . t)))

;; =================
;; Text Editing
;; =================
(use-package move-text
  :config
  (move-text-default-bindings))

;; =================
;; Search & Navigation
;; =================
(use-package ctrlf
  :config
  (ctrlf-mode +1))

(use-package consult
  :bind (("C-c C-j" . consult-imenu)
         ("C-x b" . consult-buffer)
         ("C-c s" . consult-ripgrep)
         ("C-x C-b" . consult-buffer)))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  :bind (:map grep-mode-map
              ("C-c C-p" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit)))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-x"))
  (persp-state-default-file (expand-file-name "perspective-state" user-emacs-directory))
  :init
  (persp-mode)
  :config
  (define-key perspective-map (kbd "g") nil)
  ;; perspective.el has a built-in consult source
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

;; =================
;; Writing
;; =================
(use-package ispell
  :commands (ispell-word
             ispell-region
             ispell-buffer))


;; =================
;; Document Viewing
;; =================
;; Set continuous scrolling for PDFs
(setq doc-view-continuous t)

;; =================
;; Terminal
;; =================
(use-package vterm
  ;; avoid compiling vterm at startup
  :defer t)


;; Do not echo commands in comint
(setq comint-process-echoes t)

;; =================
;; Project Management
;; =================
(use-package projectile
  :diminish 'projectile-mode
  :bind (("C-c f" . projectile-find-file))
  :config
  (projectile-mode +1))

;; =================
;; Session Management
;; =================
;; Reload buffers from previous session
(desktop-save-mode 1)

;; =================
;; Custom Functions
;; =================
(defun buffer-backed-by-file-p (buffer)
  "Check if BUFFER is backed by a file."
  (let ((backing-file (buffer-file-name buffer)))
    (if (buffer-modified-p buffer)
        t
      (if backing-file
          (file-exists-p (buffer-file-name buffer))
        t))))

(defun kill-removed-buffers ()
  "Kill buffers whose files have been removed."
  (interactive)
  (let ((to-kill (-remove 'buffer-backed-by-file-p (buffer-list))))
    (mapc 'kill-buffer to-kill)
    (message "Killed %s buffers" (length to-kill))))

(defun copy-file-path-of-buffer ()
  "Copy the current buffer's file path to clipboard."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message (concat "Yanked " file-name " to clipboard"))
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(global-set-key (kbd "C-c C-y f") 'copy-file-path-of-buffer)

;; ====================================
;; THINGS TO CHECKOUT
;; ====================================
;; (use-package meow) ;; Replacement for evil
(use-package indent-bars
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1))
  (indent-bars-highlight-current-depth '(:blend 0.4))
  (indent-bars-display-on-blank-lines t)
  (indent-bars-no-descend-lists 'skip) ; prevent extra bars in nested lists + skip intermediate bars
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed; check the wiki
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	  if_statement with_statement while_statement)
	 (typescript function_declaration method_definition arrow_function
	  class_declaration for_statement for_in_statement
	  if_statement while_statement do_statement switch_statement
	  try_statement)
	 (tsx function_declaration method_definition arrow_function
	  class_declaration for_statement for_in_statement
	  if_statement while_statement do_statement switch_statement
	  try_statement)))
  ;; Note: wrap likely not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  
  :hook ((python-base-mode yaml-mode typescript-ts-mode typescript-tsx-mode tsx-ts-mode) . indent-bars-mode)
  )
(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))
;; (use-package combobulate) ;; code editing based on tree-sitter

;; Keep Custom's machine-local state out of this tracked file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(provide '.emacs)
;;; .emacs ends here
