;; This loads my config.org file
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(browse-url-browser-function 'eww-browse-url)
 '(custom-enabled-themes '(moe-dark))
 '(dired-listing-switches "-alh")
 '(exwm-input-prefix-keys
   '("" "" "" ""
     [134217848]
     [134217824]
     [134217766]
     [134217786]
     [134217761]
     [8388724]))
 '(exwm-workspace-number 4)
 '(exwm-workspace-switch-create-limit 4)
 '(global-company-mode t)
 '(notmuch-address-internal-completion '(received nil))
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "university" :query "university")))
 '(package-selected-packages
   '(vterm desktop-environment moe-theme haskell-mode auto-complete auto-comp htmlize ox-gfm geiser powerline flycheck-lilypond flycheck pdf-tools xelb exwm xterm-color xclip which-key use-package try transmission switch-window smex rainbow-mode rainbow-delimiters pinentry notmuch mingus lorem-ipsum ido-vertical-mode elpher elfeed counsel company base16-theme))
 '(show-paren-mode t))


(provide 'init)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
