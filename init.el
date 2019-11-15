;; Turn this directory into a variable
(setq emacs-config-directory (expand-file-name "~/.emacs.d/"))
;; This loads my config.org file
(org-babel-load-file (concat emacs-config-directory "config.org"))
;; This loads my fonts settings
(global-set-key (kbd "C-z f") (lambda nil (interactive)
                                    (load-file (concat emacs-config-directory "config/fonts.el"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function 'eww-browse-url)
 '(dired-listing-switches "-alh")
 '(exwm-input-prefix-keys
   '([f8]
     ""
     [menu]
     "" "" ""
     [134217848]
     [134217824]
     [134217766]
     [134217786]))
 '(exwm-workspace-number 4)
 '(exwm-workspace-switch-create-limit 4)
 '(global-company-mode t)
 '(package-selected-packages
   '(exwm-surf openwith fish-completion god-mode slime vterm desktop-environment haskell-mode htmlize ox-gfm geiser flycheck-lilypond flycheck pdf-tools xelb exwm xterm-color xclip which-key use-package try transmission switch-window smex rainbow-mode rainbow-delimiters pinentry mingus lorem-ipsum ido-vertical-mode elpher counsel company base16-theme))
 '(show-paren-mode t))

(provide 'init)
