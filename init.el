;; Prefer newer files rather than old byte-compiled ones.
(setq load-prefer-newer t)

;; add the lisp directory to `load-path'
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Move custom settings to another file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Define a function that loads my config file
(defun config-load ()
  (interactive)
  (let* ((dir (expand-file-name "lisp/" user-emacs-directory))
	 (el (expand-file-name "config.el" dir))
	 (elc (expand-file-name "config.elc" dir))
	 (org (expand-file-name "config.org" user-emacs-directory)))
    (cond ((file-exists-p elc) (load-file elc))
	  ((file-exists-p el) (load-file el))
	  ((file-exists-p org)
	   (progn (org-babel-tangle-file org)
		  (config-load))))))

(config-load)

(load custom-file)

;; Theme
(load-theme 'custom t)

(provide 'init)
