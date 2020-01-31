;; Move custom settings to another file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Load my real config, then the custom file
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'config.el)
(load custom-file)

;; Theme
(load-theme 'custom t)

(provide 'init)
