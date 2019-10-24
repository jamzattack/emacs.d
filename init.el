;; This loads my config.org file
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote eww-browse-url))
 '(dired-listing-switches "-alh")
 '(exwm-input-prefix-keys
   (quote
    ("" "" ""
     [134217848]
     [134217824]
     [134217766]
     [134217786]
     [134217761]
     [8388724])))
 '(exwm-workspace-number 4)
 '(global-company-mode t)
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
    (moe-theme haskell-mode auto-complete auto-comp htmlize ox-gfm geiser powerline flycheck-lilypond flycheck pdf-tools xelb exwm xterm-color xclip which-key use-package try transmission switch-window smex rainbow-mode rainbow-delimiters pinentry notmuch mingus lorem-ipsum ido-vertical-mode elpher elfeed counsel company base16-theme)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch-serif ((t (:height 1.2 :foundry "PfEd" :family "Terminus (TTF)")))))

(provide 'init)

;;; init.el ends here
