;; clear C-z to use it as a prefix
(global-unset-key (kbd "C-z"))

;; Move custom settings to another file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Load my real config, then the custom file
(load-file (concat user-emacs-directory "config.el"))
(load custom-file)

;; Theme
(load-theme 'custom t)

(provide 'init)
