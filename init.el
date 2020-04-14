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
If config.el exists, load it.
If config.org exists, tangle it and then load it.
If prefix arg TANGLE is non-nil, tangle config.org even if
config.el exists."
  (interactive "P")
  (let* ((dir (expand-file-name "lisp/" user-emacs-directory))
	 (el (expand-file-name "config.el" dir))
	 (org (expand-file-name "config.org" user-emacs-directory)))
    (cond (tangle
	   (delete-file el)
	   (config-load))
	  ((file-exists-p el)
	   (load-file el))
	  ((file-exists-p org)
	   (require 'org)
	   (org-babel-tangle-file org)
	   (config-load))
	  (t (user-error "file \"%s\" not found" org)))
    (when (file-exists-p custom-file)
      (load custom-file))))

;; Load config.org
(config-load)
