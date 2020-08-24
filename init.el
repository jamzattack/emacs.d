;; -*- lexical-binding: t; -*-

(when (boundp 'comp-deferred-compilation)
  (setq comp-deferred-compilation t))

(with-eval-after-load 'epg-config
  (setq epg-pinentry-mode 'loopback))

;; Temporarily disable GC
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Settings for graphical instance
(setq x-wait-for-event-timeout nil	; remove frame creation delay
      frame-resize-pixelwise t)		; frames resize by pixel

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

Load \"config.el\".  If \"config.el\" doesn't exist, or if prefix
arg TANGLE is non-nil, tangle \"config.org\" first."
  (interactive "P")
  (if tangle
      (let ((org (expand-file-name "config.org" user-emacs-directory)))
	(require 'org)
	(org-babel-tangle-file org)
	(load "config" t))
    (unless (load "config" t)
      (config-load t))))

;; Enable all local variables temporarily, for `desktop-read'
(let ((enable-local-variables :all))
  ;; Load custom.el
  (when (file-exists-p custom-file)
    (load-file custom-file))
  ;; Load config.org
  (config-load))

;; Revert GC settings
(setq gc-cons-threshold 800000
      gc-cons-percentage 0.1)
