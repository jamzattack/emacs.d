;; Prefer newer files rather than old byte-compiled ones.
(setq load-prefer-newer t)

;; add the lisp directory to `load-path'
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Move custom settings to another file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Load my real config, then the custom file
(require 'config.el)
(load custom-file)

;; Theme
(load-theme 'custom t)

(provide 'init)
