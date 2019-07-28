;; This loads my config.org file
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(base16-distinct-fringe-background nil)
 '(browse-url-browser-function (quote eww-browse-url))
 '(byte-compile-verbose nil)
 '(custom-enabled-themes (quote (base16-bright)))
 '(custom-safe-themes
   (quote
    ("5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" default)))
 '(dired-listing-switches "-alh")
 '(dired-use-ls-dired (quote unspecified))
 '(eshell-ls-use-in-dired nil nil (em-ls))
 '(exwm-input-prefix-keys
   (quote
    ("" "" ""
     [134217848]
     [134217824]
     [134217766]
     [134217786]
     [8388724])))
 '(exwm-workspace-number 4)
 '(font-lock-global-modes (quote (not speedbar-mode)))
 '(ido-vertical-mode t)
 '(line-number-mode t)
 '(notmuch-address-internal-completion (quote (received nil)))
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "university" :query "university"))))
 '(package-selected-packages
   (quote
    (geiser powerline flycheck-lilypond flycheck pdf-tools xelb exwm xterm-color xclip which-key use-package try transmission switch-window smex rainbow-mode rainbow-delimiters pinentry notmuch mingus lorem-ipsum ido-vertical-mode elpher elfeed counsel company base16-theme)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch-serif ((t (:height 1.2 :foundry "PfEd" :family "Terminus (TTF)")))))

(provide 'init)

;;; init.el ends here
