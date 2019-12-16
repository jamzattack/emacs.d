;; Turn this directory into a variable
(global-unset-key (kbd "C-z"))

(add-to-list 'load-path (concat user-emacs-directory
				"lisp"))
(add-to-list 'load-path (concat user-emacs-directory
				"lisp/sane-lilypond"))
(add-to-list 'load-path (concat user-emacs-directory
				"lisp/lilypond-mode"))

;; This loads my config.org file
(org-babel-load-file (concat user-emacs-directory "config.org"))
(load-theme 'custom t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote eww-browse-url))
 '(custom-safe-themes
   (quote
    ("be01510ef83ecc304ee0d0aa9b4981868ef9a0bfb165a411f949b240f04a7ff3" default)))
 '(desktop-environment-screenlock-command "i3lock -c ffffd0 -n")
 '(dimmer-fraction 0.2)
 '(epg-pinentry-mode (quote loopback) t)
 '(exwm-input-prefix-keys
   (quote
    ([XF86AudioMute]
     [XF86AudioLowerVolume]
     [XF86AudioRaiseVolume]
     [134217761]
     [f8]
     ""
     [menu]
     "" "" ""
     [134217848]
     [134217824]
     [134217766]
     [134217786])))
 '(exwm-workspace-number 1)
 '(exwm-workspace-switch-create-limit 2)
 '(god-exempt-major-modes nil)
 '(god-exempt-predicates nil)
 '(package-selected-packages
   (quote
    (helpful helm-descbinds helm-smex pdf-tools frame-mode helm-notmuch helm-system-packages helm desktop-environment elfeed notmuch dimmer exwm dim system-packages base16-theme bash-completion fish-completion god-mode slime vterm haskell-mode htmlize ox-gfm geiser flycheck-lilypond flycheck xelb xterm-color xclip use-package try transmission switch-window smex rainbow-mode rainbow-delimiters pinentry mingus lorem-ipsum elpher)))
 '(show-paren-mode t))

(provide 'init)

