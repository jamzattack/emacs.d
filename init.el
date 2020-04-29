;; -*- lexical-binding: t; -*-

;; Temporarily disable GC
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prefer newer files rather than old byte-compiled ones
(setq load-prefer-newer t)

;; Move custom-file to the lisp directory
(setq custom-file (expand-file-name "lisp/custom.el"
				    user-emacs-directory))

;; Add the lisp directory to `load-path'
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Define a function that loads my config file
(defun config-load (&optional tangle)
  "Load my config file.
If config.el exists, load it.  Otherwise tangle config.org and
then load config.el."
  (interactive "P")
  (if tangle
      (let ((org (expand-file-name "config.org" user-emacs-directory)))
	(require 'org)
	(org-babel-tangle-file org)
	(load "config" t))
    (unless (load "config" t)
      (config-load t)))
  (load-file custom-file))

;; Load config.org
(config-load)

;; Revert GC settings
(setq gc-cons-threshold 800000
      gc-cons-percentage 0.1)
