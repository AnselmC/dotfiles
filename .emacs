;;; .emacs --- Anselm's emacs config
;;; Commentary:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-eighties))
 '(custom-safe-themes
   '("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default))
 '(elpy-syntax-check-command "pylint")
 '(helm-minibuffer-history-key "M-p")
 '(org-agenda-files '("~/todo.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Code:
;; straight.el for package management (https://github.com/raxod502/straight.el)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
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
  :config
  ;;(global-evil-visualstar-mode)
  (evil-collection-init))

(use-package treemacs
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

;; surround visually selected area with symbol
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package exec-path-from-shell
  :init (add-to-list 'exec-path "/usr/local/bin")
  :config
  (exec-path-from-shell-initialize))

(use-package selectrum
  :bind (("C-M-r" . selectrum-repeat)
         :map selectrum-minibuffer-map
         ("C-r" . selectrum-select-from-history)
         ("C-j" . selectrum-next-candidate)
         ("C-k" . selectrum-previous-candidate))
  :custom-face
  (selectrum-current-candidate ((t (:background "DodgerBlue2"))))
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :after selectrum
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package consult
  :bind (("C-c C-j" . consult-imenu)
         ("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer)))


;; project management
(use-package projectile
  :diminish projectile-mode
  :bind (("C-c f" . projectile-find-file))
  :config
  (projectile-mode +1))


;; UI CONFIG

;; themes
(use-package doom-themes
  :defer t)
(dolist (i custom-enabled-themes)
  (disable-theme i)) ;; first disable all currently enabled themes
(load-theme 'doom-snazzy t)
(doom-themes-visual-bell-config)

(use-package color-theme-sanityinc-tomorrow
  :defer t
  :config
  ;;(color-theme-sanityinc-tomorrow-eighties)
  )


;; general
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(tooltip-mode -1)

(setq-default frame-title-format "%b (%f)");; Show full path in the title bar.

(use-package diminish) ;; enable disabling displaying of modes

;; display completion at poin
(use-package posframe)

(use-package mini-frame
  :after (selectrum posframe)
  :config
  (progn
    (mini-frame-mode +1)
    (setq selectrum-display-action nil)
    (setq mini-frame-show-parameters
          (lambda ()
            (let* ((info (posframe-poshandler-argbuilder))
                   (posn (posframe-poshandler-point-bottom-left-corner info))
                   (left (car posn))
                   (top (cdr posn)))
              `((left . ,left)
                (top . ,top)
                (width . 0.4)
                (height . 1)))))))


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

;; relative linenumbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(column-number-mode t)
(setq display-line-numbers-width nil)

(defun display-line-numbers-equalize ()
  "Equalize The width"
  (setq display-line-numbers-width (length (number-to-string (line-number-at-pos (point-max))))))
(add-hook 'find-file-hook 'display-line-numbers-equalize)

;;highlight current buffer
(use-package dimmer
  :init
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (setq dimmer-fraction 0.4)
  :config
  (dimmer-mode t)
  )

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

;; render emojis, i.e. :smile:
(use-package emojify
  :hook (after-init . global-emojify-mode))

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
  :diminish golden-ratio-mode
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
  :diminish anzu-mode
  :config
  (global-anzu-mode))

(setq vc-follow-symlinks t) ;; don't warn of symlinks


;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; PROGRAMMING GENERAL

;; elisp
(add-hook 'emacs-lisp-mode (lambda ()
                             (local-set-key (kbd "C-c d") 'xref-find-definitions)))

;; ability to use multiple terminal sessions
(use-package multi-term)

;; code completion
(use-package company
  :config
  (global-company-mode))

;; auto-formatting
(use-package format-all
  :config
  (add-hook 'js-mode 'format-all-mode)
  (add-hook 'ess-r-mode-hook 'format-all-mode)
  (add-hook 'c-mode-common-hook 'format-all-mode)
  (add-hook 'emacs-lisp-mode 'format-all-mode))

;; git
(use-package magit
  :bind ("C-x g" . magit-status))

;; code error checking
(flymake-mode-off)
(use-package flycheck
  :config (global-flycheck-mode))

(use-package flycheck-inline
  :after flycheck
  :hook ((flycheck-mode-hook . flycheck-inline-mode)))


;; yasnippet
(use-package yasnippet
  :after yasnippet-snippets
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets) ;; actual snippets

;; default to 4 spaces per tab
(setq-default tab-width 4 indent-tabs-mode nil)


;; PROGRAMMING LANGUAGE CONFIG

(use-package clojure-mode)
(use-package cider)
(use-package paredit)

;; python stuff

(use-package elpy
  :bind (("C-c d" . 'elpy-goto-definition)
         ("C-c w" . 'elpy-goto-definition-other-window))
  :init
  (progn
    (elpy-enable)
    (setq elpy-rpc-timeout 5000
          elpy-rpc-virtualenv-path 'current
          python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt")
    (add-hook 'elpy-mode-hook (lambda ()
                                (add-hook 'before-save-hook
                                          'elpy-black-fix-code nil t)))))
(defun run-python-with-autoreload ()
  (run-python)
  (python-shell-send-string "%load_ext autoreload")
  (python-shell-send-string "%autoreload 0"))

;; conda
(use-package conda
  :init
  (setq conda-env-home-directory (expand-file-name "~/miniconda3"))
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

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
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
(use-package csv-mode)

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
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c C-l" . org-insert-link))
  :config
  (setq org-hide-emphasis-markers t)
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

(use-package org-ref
  :config
  (setq org-ref-bibliography-notes "~/Documents/private/uni/notes.org"
        org-ref-default-bibliography '("~/Documents/private/uni/bib/bamot.bib")
        org-ref-pdf-directory "~/Zotero/storage")
  )

(use-package evil-org
  :after org
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

(use-package thesaurus
  :straight (thesaurus :type git :host github :repo "AnselmC/thesaurus.el"))

(use-package flyspell
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
 create-lockfiles nil)  ; stop creating .# files

;; Revert (update) buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

(use-package elpy
  :bind (("C-c d" . 'elpy-goto-definition)
         ("C-c w" . 'elpy-goto-definition-other-window))
  :init
  (progn
    (elpy-enable)
    (setenv "WORKON_HOME" "~/miniconda3/envs/")
    (setq elpy-rpc-timeout 5000
          elpy-rpc-virtualenv-path 'current)
    (add-hook 'elpy-mode-hook (lambda ()
                                (add-hook 'before-save-hook
                                          'elpy-black-fix-code nil t)))))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; CUSTOM
(defun set-font-height (height)
  (set-face-attribute 'default nil :height height))


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
(desktop-save-mode 1)

(provide '.emacs)
;;; .emacs ends here
