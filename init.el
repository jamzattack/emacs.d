;; This loads my config.org file
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(base16-distinct-fringe-background nil)
 '(base16-highlight-mode-line "box")
 '(custom-enabled-themes (quote (base16-woodland)))
 '(custom-safe-themes
   (quote
    ("8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" default)))
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
    (pinentry mingus xterm-color flycheck-lilypond transmission notmuch rainbow-delimiters base16-theme company-shell company auto-complete rainbow-mode ##))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse ((t (:background "white")))))

(provide 'init)

;;; init.el ends here
