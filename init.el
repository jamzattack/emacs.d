(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'base16-theme)
  (package-refresh-contents)
  (package-install 'base16-theme))

;; setting environment variables
(setenv "NOTMUCH_CONFIG" (expand-file-name "~/.config/notmuch-config"))
(setenv "PASSWORD_STORE_DIR" (expand-file-name "~/.local/share/password-store/"))
(setenv "PATH" (concat (getenv "PATH") ":/home/jamzattack.scripts"))
(setq exec-path (append exec-path '("/home/jamzattack/.scripts")))

;; some simple non-annoyances
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'save-buffers-kill-emacs 'delete-frame)
(setq make-backup-files nil)
(setq auto-save-default nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(base16-distinct-fringe-background nil)
 '(base16-highlight-mode-line "box")
 '(blink-cursor-mode 1)
 '(column-number-mode t)
 '(company-idle-delay 0)
 '(custom-enabled-themes (quote (base16-woodland)))
 '(custom-safe-themes
   (quote
    ("8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" default)))
 '(display-time-mode t)
 '(menu-bar-mode nil)
 '(nil nil t)
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
    (pinentry mingus xterm-color flycheck-lilypond transmission notmuch rainbow-delimiters base16-theme company-shell company auto-complete rainbow-mode ##)))
 '(scroll-bar-mode nil)
 '(set-cursor-color "#d35c5c")
 '(set-default-font "monospace-9")
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#1d1910"))))
 '(mouse ((t (:background "white")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; PACKAGES

(use-package pinentry
             :ensure t
             :config

;; Allow emacs pinentry
(setq epa-pinentry-mode 'loopback)
(pinentry-start) 2
;; My own settings
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(global-font-lock-mode 1)

(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
(add-hook 'LilyPond-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck '(require 'flycheck-lilypond))

(set-frame-font "monospace-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; easier (I think) moving around windows
(global-set-key (kbd "C-x M-p") 'windmove-up)
(global-set-key (kbd "C-x M-n") 'windmove-down)
(global-set-key (kbd "C-x M-b") 'windmove-left)
(global-set-key (kbd "C-x M-f") 'windmove-right)
(global-set-key (kbd "<M-tab>") 'other-window)

; windows interactively
(defun v-resize (key)
  "interactively resize the window"  
  (interactive "cHit p/n/b/f to resize") 
  (cond                                  
   ((eq key (string-to-char "n"))                      
    (enlarge-window 1)             
    (call-interactively 'v-resize)) 
   ((eq key (string-to-char "p"))                      
    (enlarge-window -1)             
    (call-interactively 'v-resize)) 
   ((eq key (string-to-char "b"))                      
    (enlarge-window-horizontally -1)             
    (call-interactively 'v-resize)) 
   ((eq key (string-to-char "f"))                      
    (enlarge-window-horizontally 1)            
    (call-interactively 'v-resize)) 
   (t (push key unread-command-events))))
(global-set-key (kbd "C-c +") 'v-resize)

; colour highlighting for hex codes
(global-set-key (kbd "C-c h") 'rainbow-mode)
 
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c f") 'flymake-mode)
(global-set-key (kbd "C-c w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-c y") 'clipboard-yank)
(setq visible-cursor t)

;; MAXIMUM OVERDRIVE
(setq disabled-command-function nil)
;;

(provide 'init)

;;; init.el ends here
