;; Prefer newer files rather than old byte-compiled ones.
(setq load-prefer-newer t)

;; add the lisp directory to `load-path'
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Move custom settings to another file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Define a function that loads my config file
(defun config-load (&optional recompile)
  "Load my config file.
If config.elc exists, load it. 
If config.el exists, load it, then compile it.
If config.org exists, tangle it, load it, compile it.
If arg RECOMPILE is non-nil, or if called interactively, do the last one"
  (interactive '(t))
  (let* ((dir (expand-file-name "lisp/" user-emacs-directory))
	 (el (expand-file-name "config.el" dir))
	 (elc (expand-file-name "config.elc" dir))
	 (org (expand-file-name "config.org" user-emacs-directory)))
    (cond ((file-exists-p elc)
	   (progn (when recompile
		    (delete-file elc)
		    (delete-file el)
		    (config-load))
		  (load-file elc)))
	  ((file-exists-p el)
	   (progn (load-file el)
		  (byte-compile-file el)))
	  ((file-exists-p org)
	   (progn (org-babel-tangle-file org)
		  (config-load))))))

(config-load)

(load custom-file)

;; Theme
(load-theme 'custom t)

(provide 'init)
