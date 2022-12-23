;;; .emacs --- Anselm's emacs config
;;; Commentary:

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
      (eval-pr
       int-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(setq straight-use-package-by-default t) ;; use with use-package
(straight-use-package 'use-package)

;; NAVIGATION

;; evil-mode
(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))

(use-package evil
  :after undo-tree
  :init
  (setq evil-want-integration t ;; This is optional since it's already set to t by default.
        evil-want-keybinding nil
        evil-symbol-word-search t  ;; Makes evil-search-word- look for word
        evil-undo-system 'undo-tree
        evil-search-module 'evil-search)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :diminish 'evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package evil-goggles
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))


(use-package treemacs
  :demand t
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
    (treemacs)))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-evil
  :after (treemacs evil)
  :config (evil-treemacs-state t))

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

(use-package exec-path-from-shell
  :demand t
  :init
  (add-to-list 'exec-path "/usr/local/bin")
  :config
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path-from-shell-variables "LSP_USE_PLISTS"))

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :init
  (vertico-mode))

(use-package embark
  :ensure t

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

(use-package consult
  :bind (("C-c C-j" . consult-imenu)
         ("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; project management
(use-package projectile
  :diminish 'projectile-mode
  :bind (("C-c f" . projectile-find-file))
  :config
  (projectile-mode +1))


;; UI CONFIG

;; themes
(use-package doom-themes
  :disabled t
  )

(consult-theme 'modus-vivendi)


;; general
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(tooltip-mode -1)

(setq-default frame-title-format "%b (%f)");; Show full path in the title bar.

;; enable disabling displaying of modes
(use-package diminish)


;; fancy modeline
(use-package doom-modeline
  :init
  (setq doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-modal-icon t
        doom-modeline-persp-name t
        doom-modeline-persp-icon t
        doom-modeline-minor-modes t
        doom-modeline--battery-status t)
  :config (doom-modeline-mode 1))

;; relative line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(column-number-mode t)
(setq display-line-numbers-width nil)

(defun display-line-numbers-equalize ()
  "Equalize The width"
  (setq display-line-numbers-width (length (number-to-string (line-number-at-pos (point-max))))))
(add-hook 'find-file-hook 'display-line-numbers-equalize)

;;highlight current buffer
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


(use-package vterm)
;; ansi colors in compilation mode and shells
(use-package xterm-color
  :init
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))

  (advice-add 'compilation-filter :around #'my/advice-compilation-filter)
  )

;; Enable richer annotations
(use-package marginalia
  :init
  (marginalia-mode))

;; Show matching parantheses
(show-paren-mode t)


;; UX CONFIG

;; always open frames in same window
(setq ns-pop-up-frames nil)

(use-package move-text
  :config
  (move-text-default-bindings)
  )

;; make sure to use zsh colors in shell
(add-hook 'shell-mode-hook
          (lambda ()
            (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

;; do not echo commands in conint
(setq comint-process-echoes t)


(use-package origami
  :config
  (global-origami-mode))

;; mode-line-bell
;; change annoying ring with flashing minibuffer
(use-package mode-line-bell
  :init
  (setq visible-bell 1)
  :config
  (mode-line-bell-mode 1))

;; answer yes-or-no w/ y or n
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package golden-ratio
  :after evil
  :diminish 'golden-ratio-mode
  :config
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-down
                  evil-window-up)))
  (golden-ratio-mode))

;; show number of matches when searching
(use-package evil-anzu
  :after evil)

;; show number of hits when searching
(use-package anzu
  :diminish 'anzu-mode
  :config
  (global-anzu-mode))

(setq vc-follow-symlinks t) ;; don't warn of symlinks


;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(use-package restclient)
;; PROGRAMMING GENERAL

;; python

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(defun start-django-shell ()
  (interactive)
  (let* ((root-dir (vc-root-dir))
         (manage-py-file (concat root-dir "manage.py")))
    (if (file-exists-p manage-py-file)
        (run-python (concat manage-py-file " shell"))
      (message (concat "manage.py doesn't exist in " root-dir)))))

(use-package pyvenv)
;; LSP
(use-package lsp-mode
  :diminish (eldoc-mode lsp-lens-mode)
  :demand t
  :custom
  (lsp-completion-provider :none)
  (lsp-use-plists t)
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-pydocstyle-enabled nil)
  (lsp-pylsp-plugins-pylint-enabled t)
  (lsp-pylsp-plugins-flake8-enabled nil)
  :init
  (evil-define-key 'normal lsp-mode-map (kbd "`") lsp-command-map)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :config
  (add-to-list 'lsp-enabled-clients 'ts-ls)
  (add-to-list 'lsp-enabled-clients 'clojure-lsp)
  (add-to-list 'lsp-enabled-clients 'clangd)
  (add-to-list 'lsp-enabled-clients 'pylsp)
  :hook ((clojure-mode . lsp)
         (python-mode . lsp)
         (java-mode . lsp)
         (web-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my/lsp-mode-setup-completion))
  :commands lsp)

(use-package dap-mode
  :config
  (require 'dap-python)
  :custom
  (dap-python-debugger "debugpy")
  :init
  (dap-mode 1)
  (dap-ui-mode 1))

(use-package dap-python
  :after dap-mode)

(defun dap-python--pyenv-executable-find (command)
    (executable-find command))

(setq lsp-completion-provider :none)

(use-package consult-lsp)

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-py
  :custom
  (lsp-py-flake8-enabled nil)
  (lsp-py-pyflakes-enabled nil)
  (lsp-py-pydocstyle-enabled nil)
  (lsp-py-pycodestyle-enabled nil)
  (lsp-py-pylint-enabled t)
  (lsp-py-jedi-completion-enabled t)
  (lsp-py-jedi-definition-enabled t)
  (lsp-py-jedi-hover-enabled t)
  (lsp-py-jedi-references-enabled t)
  :straight (lsp-py :type git :host nil :repo "https://github.com/AnselmC/lsp-py.git"))

(use-package lsp-ui
  :demand t
  :after lsp-mode
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-delay 2.0
        lsp-ui-doc-position 'at-point
        lsp-ui-peek-enable t)

  :bind (:map lsp-ui-peek-mode-map
              ("C-j" . lsp-ui-peek--select-next)
              ("C-k" . lsp-ui-peek--select-prev))
  :commands lsp-ui-mode)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))


;; elisp
(add-hook 'emacs-lisp-mode (lambda ()
                             (local-set-key (kbd "C-c d") 'xref-find-definitions)))



;; ability to use multiple terminal sessions
(use-package multi-term)

;; code completion

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu

  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Completion after two characters
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; nDisable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  :bind
  ;; Configure SPC for separator insertion
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode))


;; auto-formatting
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

;; git
(use-package magit
  :diminish smerge-mode
  :bind ("C-x g" . magit-status))

(use-package forge
  :after magit)



;; code error checking
(flymake-mode-off)
(diminish 'flymake-mode)
(use-package flycheck
  :init (global-flycheck-mode))

;; yasnippet
(use-package yasnippet
  :after yasnippet-snippets
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets) ;; actual snippets

;; default to 4 spaces per tab
(setq-default tab-width 4 indent-tabs-mode nil)

(use-package tree-sitter
  :diminish tree-sitter-mode
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :after tree-sitter)



;; PROGRAMMING LANGUAGE CONFIG

(use-package clojure-mode)
(use-package cider
  :init
  (setq cider-auto-jump-to-error nil))

(use-package paredit)

;; python stuff

;;(use-package elpy
;;  :bind (("C-c d" . 'elpy-goto-definition)
;;         ("C-c w" . 'elpy-goto-definition-other-window))
;;  :init
;;  (progn
;;    (elpy-enable)
;;    (setenv "WORKON_HOME" "~/miniconda3/envs/")
;;    (setq elpy-rpc-timeout 5000
;;          elpy-rpc-virtualenv-path 'current
;;          python-shell-interpreter "ipython"
;;          python-shell-interpreter-args "-i --simple-prompt")
;;    (add-hook 'elpy-mode-hook (lambda ()
;;                                (add-hook 'before-save-hook
;;                                          'elpy-black-fix-code nil t)))))
(defun run-python-with-autoreload ()
  (run-python)
  (python-shell-send-string "%load_ext autoreload")
  (python-shell-send-string "%autoreload 0"))

;; conda
(use-package conda
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  :config
  (conda-env-initialize-interactive-shells))

;; import sorting
(use-package py-isort
  :config
  (add-hook 'before-save-hook 'py-isort-before-save))

(use-package sphinx-doc
  :init
  (setq sphinx-doc-include-types nil) ;; set to t to include type annotations
  :hook ((python-mode-hook . sphinx-doc-mode)))

;; EIN (Emacs IPython Notebook)
(use-package ein)



;; c++
(add-hook 'c-mode-common-hook #'clang-format+-mode)
(setq clang-format-style "google")
(add-hook 'c-mode-common-hook (lambda ()
                                (local-set-key (kbd "C-c m") (lambda()
                                                               (compile "make")))))

;; html
(add-hook 'html-mode-hook (lambda()
                            (add-hook 'before-save-hook 'indent-region nil t)))

;; markdown
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; docker
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; Web
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.tsx?\\'" . web-mode)))

;; R
(use-package ess-site
  :straight ess
  :config
  (if (not (executable-find "R"))
      (setq inferior-R-program-name "/usr/local/bin/R"))
  (setq ess-use-flymake nil) ;; disable Flymake
  (add-hook 'ess-r-mode-hook (lambda()
                               'electric-layout-mode)))

;; CSV
(defun csv-open-link-at-point()
  (interactive)
  (let* ((start (save-excursion (re-search-backward ",")))
         (end (save-excursion (re-search-forward ",")))
         (link (buffer-substring-no-properties  (+ start 1) (- end 1))))
    (browse-url link)))
(use-package csv-mode
  :bind (("C-c C-o" . csv-open-link-at-point))
  )

;; YAML
(use-package yaml-mode
  :mode (
	     "\\.yaml\\'"
	     "\\.yml\\'"
	     ))


;; SQL
;;(use-package sqlformat
;;  :config
;;  (progn
;;    (add-hook 'sql-mode-hook 'sqlformat-on-save-mode)))


;; PRODUCTIVITY

;; org-mode
;;(let* ((variable-tuple
;;        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;              ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;       (base-font-color     (face-foreground 'default nil 'default))
;;       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;;
;;  (let* ((variable-tuple
;;          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;         (base-font-color     (face-foreground 'default nil 'default))
;;         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;;
;;    (custom-theme-set-faces
;;     'user
;;     `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;     `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;     `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;     `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))
;;
;;  (custom-theme-set-faces
;;   'user
;;   `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;   `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;   `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;   `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.0))))
;;   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.05))))
;;   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.1))))
;;   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.25))))
;;   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))
;;(custom-theme-set-faces
;; 'user
;; '(variable-pitch ((t (:family "ETBembo" :height 120 :weight thin))))
;; '(fixed-pitch ((t ( :family "Fira Code Retina" :height 120)))))
;;(add-hook 'org-mode-hook 'variable-pitch-mode)
;;(add-hook 'org-mode-hook 'visual-line-mode)
;;(custom-theme-set-faces
;; 'user
;; '(org-block ((t (:inherit fixed-pitch))))
;; '(org-code ((t (:inherit (shadow fixed-pitch)))))
;; '(org-document-info ((t (:foreground "dark orange"))))
;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;; '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
;; '(org-link ((t (:foreground "royal blue" :underline t))))
;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-property-value ((t (:inherit fixed-pitch))) t)
;; '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;; '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;; '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c C-l" . org-insert-link))

  :custom
  (org-hide-emphasis-markers t)
  :config
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (C . t)))
  ;; Use minted
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (require 'ox-latex)
  (add-to-list 'org-latex-classes
               '("koma-book"
                 "\\documentclass{scrbook}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; #+LaTeX_CLASS: beamer in org files
  (unless (boundp 'org-export-latex-classes)
    (setq org-export-latex-classes nil))
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass\[presentation\]\{beamer\}"
                 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

  (setq org-latex-listings 'minted
        org-log-done t
        org-latex-prefer-user-labels t
        ;; fontify title etc.
        org-src-fontify-natively t
        ;; remove colored hyperlinks
        org-latex-with-hyperref nil
        ;; Add the shell-escape flag
        org-latex-pdf-process '(
                                "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "bibtex %b"
                                "makeglossaries %b"
                                "makeindex %b"
                                "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                )
        ;; table caption below table
        org-latex-caption-above nil
        ;; Sample minted options.
        org-latex-minted-options '(
                                   ("frame" "lines")
                                   ("fontsize" "\\scriptsize")
                                   ("xleftmargin" "\\parindent")
                                   ("linenos" "")
                                   )

        org-latex-table-scientific-notation nil
        ;; enable latex highlighting in org-mode
        org-highlight-latex-and-related '(latex script entities)
        ;; set python3
        org-babel-python-command "python3"
        )
  )
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(use-package org-ref
  :config
  (setq org-ref-bibliography-notes "~/Documents/private/uni/notes.org"
        org-ref-default-bibliography '("~/Documents/private/uni/bib/bamot.bib")
        org-ref-pdf-directory "~/Zotero/storage")
  )

(use-package evil-org
  :after org
  :diminish 'evil-org-mode
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode)
         (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional)))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(setq org-latex-prefer-user-labels t)

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/roam_notes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

;; fuzzy search on bib
;;(use-package helm-bibtex
;;  :after helm
;;  :config
;;  (setq bibtex-completion-bibliography "~/Documents/private/uni/bib/bamot.bib"
;;        bibtex-completion-library-path "~/Zotero/storage"
;;        bibtex-completion-pdf-field "file"
;;        bibtex-completion-notes-path "~/Documents/private/uni/notes.org")
;;  )
;; fix bug when viewing pdfs with linum-mode
;; (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))

;; spell-checking
(use-package ispell
  :commands (ispell-word
             ispell-region
             ispell-buffer))

(use-package le-thesaurus
  :straight (le-thesaurus :type git :host nil :repo "https://github.com/AnselmC/le-thesaurus.el.git"))

(use-package flyspell
  :disabled t
  :config (flyspell-mode t))


(use-package flyspell-correct
  :config
  (setq flyspell-correct-interface #'flyspell-correct-dummy))

;; set continuous scrolling for pdfs
(setq doc-view-continuous t)


;; MISC
(setq
 make-backup-files nil  ; stop creating backup~ files
 auto-save-default nil  ; stop creating #autosave# files
 undo-tree-auto-save-history nil ; stop creating ~undo-tree~ files
 create-lockfiles nil)  ; stop creating .# files

;; Revert (update) buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

;; Remember last minibuffer commands
(savehist-mode 1)

;; CUSTOM
(defun set-font-height (height)
  (set-face-attribute 'default nil :height height))

(set-font-height 145)

(defun buffer-backed-by-file-p (buffer)
  (let ((backing-file (buffer-file-name buffer)))
    (if (buffer-modified-p buffer)
        t
      (if backing-file
          (file-exists-p (buffer-file-name buffer))
        t))))

(defun kill-removed-buffers ()
  (interactive)
  (let ((to-kill (-remove 'buffer-backed-by-file-p (buffer-list))))
    (mapc 'kill-buffer to-kill)
    (message "Killed %s buffers" (length to-kill))))


(defun copy-file-path-of-buffer ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message (concat "Yanked " file-name " to clipboard"))
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(global-set-key (kbd "C-c C-y f") 'copy-file-path-of-buffer)

;; reload buffers etc from previous session
(desktop-save-mode nil)



(provide '.emacs)
;;; .emacs ends here
