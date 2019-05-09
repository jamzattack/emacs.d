(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; setting environment variables
(setenv "NOTMUCH_CONFIG" "/home/jamzattack/.config/notmuch-config")
(setenv "PASSWORD_STORE_DIR" "/home/jamzattack/.local/share/password-store/")
(setenv "PATH" (concat (getenv "PATH") ":/home/jamzattack.scripts"))
(setq exec-path (append exec-path '("/home/jamzattack/.scripts")))

;; some simple non-annoyances
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(base16-distinct-fringe-background nil)
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
 '(package-selected-packages
   (quote
    (mingus xterm-color flycheck-lilypond transmission notmuch rainbow-delimiters base16-theme company-shell company auto-complete rainbow-mode ##)))
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
 '(mouse ((t (:background "white")))))

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

; colour highlighting for hex codes
(global-set-key (kbd "C-c h") 'rainbow-mode)
 
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c f") 'flymake-mode)
(global-set-key (kbd "C-c w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-c y") 'clipboard-yank)
(setq visible-cursor t)

;;

(provide 'init)

;;; init.el ends here
