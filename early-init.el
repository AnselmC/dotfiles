;;; early-init.el --- Loaded before init and the first frame -*- lexical-binding: t -*-

;;; Commentary:
;; Symlinked to ~/.emacs.d/early-init.el.  Keep this minimal: things that
;; must happen before package init and the initial frame.

;;; Code:

;; straight.el handles packages
(setq package-enable-at-startup nil)

;; Defer GC until startup is done; .emacs restores a sane threshold
;; in `emacs-startup-hook'.
(setq gc-cons-threshold most-positive-fixnum)

;; Disable UI elements before the first frame renders (cheaper than
;; toggling the modes later in .emacs).
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
