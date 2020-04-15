;; -*- lexical-binding: t; -*-
;; Default to lexical binding
(setq-default lexical-binding t)

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
  (when (file-exists-p custom-file)
    (load custom-file))
  (if tangle
      (let ((org (expand-file-name "config.org" user-emacs-directory)))
	(require 'org)
	(org-babel-tangle-file org)
	(load "config" t))
    (unless (load "config" t)
      (config-load t))))

;; Load config.org
(config-load)
