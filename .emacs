;;; .emacs --- Anselm's emacs config
;;; Commentary:

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-eighties))
 '(custom-safe-themes
   '("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default))
 '(exec-path
   '("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin"))
 '(helm-minibuffer-history-key "M-p")
 '(selectrum-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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
  :init (global-undo-tree-mode))

(use-package evil
  :after undo-tree
  :init
  (setq evil-want-integration t ;; This is optional since it's already set to t by default.
        evil-want-keybinding nil
        evil-symbol-word-search t  ;; Makes evil-search-word- look for word
        evil-undo-system 'undo-tree)
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
  :after (treemacs evil))


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

(use-package exec-path-from-shell)

(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :after selectrum
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;; fuzzy search
;;(use-package helm
;;  :after exec-path-from-shell
;;  :bind (("C-x C-f" . helm-find-files)
;;         ("M-x" . helm-M-x)
;;         ("C-x C-b" . helm-mini)
;;         ("C-x b" . helm-buffers-list))
;;  :init
;;  ;; center buffer frame (https://www.reddit.com/r/emacs/comments/jj269n/display_helm_frames_in_the_center_of_emacs/)
;;  (defun my-helm-display-frame-center (buffer &optional resume)
;;    "Display `helm-buffer' in a separate frame which centered in
;;parent frame."
;;    (if (not (display-graphic-p))
;;        ;; Fallback to default when frames are not usable.
;;        (helm-default-display-buffer buffer)
;;      (setq helm--buffer-in-new-frame-p t)
;;      (let* ((parent (selected-frame))
;;             (frame-pos (frame-position parent))
;;             (parent-left (car frame-pos))
;;             (parent-top (cdr frame-pos))
;;             (width (/ (frame-width parent) 2))
;;             (height (/ (frame-height parent) 3))
;;             tab-bar-mode
;;             (default-frame-alist
;;               (if resume
;;                   (buffer-local-value 'helm--last-frame-parameters
;;                                       (get-buffer buffer))
;;                 `((parent . ,parent)
;;                   (width . ,width)
;;                   (height . ,height)
;;                   (undecorated . ,helm-use-undecorated-frame-option)
;;                   (left-fringe . 0)
;;                   (right-fringe . 0)
;;                   (tool-bar-lines . 0)
;;                   (line-spacing . 0)
;;                   (desktop-dont-save . t)
;;                   (no-special-glyphs . t)
;;                   (inhibit-double-buffering . t)
;;                   (tool-bar-lines . 0)
;;                   (left . ,(+ parent-left (/ (* (frame-char-width parent) (frame-width parent)) 4)))
;;                   (top . ,(+ parent-top (/ (* (frame-char-width parent) (frame-height parent)) 6)))
;;                   (title . "Helm")
;;                   (vertical-scroll-bars . nil)
;;                   (menu-bar-lines . 0)
;;                   (fullscreen . nil)
;;                   (visible . ,(null helm-display-buffer-reuse-frame))
;;                   ;; (internal-border-width . ,(if IS-MAC 1 0))
;;                   )))
;;             display-buffer-alist)
;;        (set-face-background 'internal-border (face-foreground 'default))
;;        (helm-display-buffer-popup-frame buffer default-frame-alist))
;;      (helm-log-run-hook 'helm-window-configuration-hook)))
;;  (setq helm-display-function 'my-helm-display-frame-center)
;;  (setq helm-show-completion-display-function #'my-helm-display-frame-center)
;;  :config
;;  (exec-path-from-shell-initialize)
;;  (helm-mode 1)
;;  )




;; project management
(use-package projectile)


;; UI CONFIG

;; general
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Show full path in the title bar.
(setq-default frame-title-format "%b (%f)")

;; display completion at point
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
                (top . ,top)))))))


;; color-scheme
(use-package color-theme-sanityinc-tomorrow
  :config
  (color-theme-sanityinc-tomorrow-eighties))
;; fancy mode
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


;; show matching parantheses
(show-paren-mode t)


;; UX CONFIG

(use-package move-text
  :config
  (move-text-default-bindings)
  )

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
  :config
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-down
                  evil-window-up)))
  (golden-ratio-mode))

;; PROGRAMMING GENERAL

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
  (add-hook 'js-mode 'format-all-mode)
  (add-hook 'ess-r-mode-hook 'format-all-mode)
  (add-hook 'c-mode-common-hook 'format-all-mode)
  (add-hook 'emacs-lisp-mode 'format-all-mode))

;; git
(use-package magit
  :bind ("C-x g" . magit-status))

;; code error checking
(use-package flycheck
  :config (global-flycheck-mode))

;; yasnippet
(use-package yasnippet-snippets) ;; actual snippets
(use-package yasnippet
  :after yasnippet-snippets
  :config
  (yas-global-mode 1))

;; default to 4 spaces per tab
(setq-default tab-width 4 indent-tabs-mode nil)


;; PROGRAMMING LANGUAGE CONFIG

;; python stuff
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

;; R
(use-package ess-site
  :straight ess
  :config
  (if (not (executable-find "R"))
      (setq inferior-R-program-name "/usr/local/bin/R"))
  (setq ess-use-flymake nil) ;; disable Flymake
  (add-hook 'ess-r-mode-hook (lambda()
                               'electric-layout-mode)))

;; YAML
(use-package yaml-mode
  :mode (
	     "\\.yaml\\'"
	     "\\.yml\\'"
	     ))


;; SQL
(use-package sqlformat
  :init
  (progn
    (add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
    (define-key sql-mode-map (kbd "C-c C-f") 'sqlformat)))



;; PRODUCTIVITY

;; org-mode
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c C-l" . org-insert-link))
  :config
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

(setq org-latex-prefer-user-labels t)

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
             ispell-buffer)
  :config
  (let ((hunspell-file (executable-find "hunspell"))
        (dict-path '(("en_US" "~/Library/Spelling/en_US.aff"))))
    (setq ispell-really-hunspell hunspell-file
          ispell-program-name hunspell-file
          ispell-dictionary "en_US"
          ispell-dictionary-alist dict-path)))

(use-package thesaurus
  :straight (thesaurus :type git :host github :repo "AnselmC/thesaurus.el"))

(use-package flyspell)

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

(add-to-list 'load-path "~/.emacs.d/custom")
(require 'sphinx-doc)
(require 'org-extensions)
(require 'elpy-extensions)
(require 'miscellaneous)
(add-to-list 'load-path "~/.emacs.d/custom/translatel")
(require 'translat)



;; reload buffers etc from previous session
(desktop-save-mode 1)

(provide '.emacs)
;;; .emacs ends here
