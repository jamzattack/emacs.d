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
(defun config-load (&optional recompile)
  "Load my config file.
If config.elc exists, load it. 
If config.el exists, load it, then compile it.
If config.org exists, tangle it, load it, compile it.
If prefix arg RECOMPILE is non-nil, delete config.elc and
config.el, then repeat."
  (interactive "P")
  (let* ((dir (expand-file-name "lisp/" user-emacs-directory))
	 (el (expand-file-name "config.el" dir))
	 (elc (expand-file-name "config.elc" dir))
	 (org (expand-file-name "config.org" user-emacs-directory)))
    (cond (recompile
	   (delete-file elc)
	   (delete-file el)
	   (config-load))
	  ((file-exists-p elc)
	   (when (featurep 'config)
	     (unload-feature 'config t))
	   (require 'config elc))
	  ((file-exists-p el)
	   (when (featurep 'config)
	     (unload-feature 'config t))
	   (byte-compile-file el)
	   (config-load))
	  ((file-exists-p org)
	   (require 'org)
	   (org-babel-tangle-file org)
	   (config-load))
	  (t (user-error "file \"%s\" not found" org)))))

;; Load config.org
(config-load)
