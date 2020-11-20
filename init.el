;; -*- lexical-binding: t; -*-

(when (boundp 'comp-deferred-compilation)
  (setq comp-deferred-compilation t))

(with-eval-after-load 'epg-config
  (setq epg-pinentry-mode 'loopback))

;; Settings for graphical instance
(setq x-wait-for-event-timeout nil	; remove frame creation delay
      frame-resize-pixelwise t)		; frames resize by pixel

;; Prefer newer files rather than old byte-compiled ones
(setq load-prefer-newer t)

;; Move custom-file to the lisp directory
(setq custom-file (locate-user-emacs-file "lisp/custom-file.el"))

;; Add the lisp directory to `load-path'
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;; Define a function that loads my config file
(defun config-load (&optional tangle)
  "Load my config file.

Load \"config.el\".  If \"config.el\" doesn't exist, or if prefix
arg TANGLE is non-nil, tangle \"config.org\" first."
  (interactive "P")
  (if tangle
      (let ((org (locate-user-emacs-file "config.org")))
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
