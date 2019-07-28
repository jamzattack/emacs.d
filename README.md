
# Table of Contents

1.  [Packages](#orgc500f33)
    1.  [Startup](#org6fff18e)
        1.  [Repositories and initialisation](#org750f95f)
        2.  [Use-package](#org252b846)
        3.  [Base16-theme](#org68beaa3)
        4.  [exwm](#orgf6bd89b)
    2.  [Installed Packages](#org1019584)
        1.  [Pinentry](#org7bf5d85)
        2.  [Completion](#org6957343)
    3.  ["Applications"](#org7372484)
        1.  [Appearance](#org483536e)
        2.  [Quality of life](#org293d07d)
        3.  [Not really useful](#orgba816dc)
        4.  [To be confirmed](#org9546deb)
        5.  [PDF-tools](#org7c552e2)
2.  [Environment Variables](#org75f844c)
3.  [Fixing defaults](#org0d428a4)
    1.  [Miscellaneous stuff](#orgee0421f)
        1.  [No more pesky extra files, other basics](#org7d5d660)
        2.  [Enable all the features, because what's the point in having less?](#org10d4919)
    2.  [Aesthetic stuff](#orgad761e3)
        1.  [GUI ugliness](#org6a59af4)
        2.  [Font and cursor](#orgb5c9196)
    3.  [Tabs](#org24c3047)
    4.  [Buffers/input](#org7cad837)
        1.  [ido-mode](#org32742d8)
        2.  [ibuffer](#org20b8968)
4.  [Custom functions](#orgcd5c751)
    1.  [Resizing windows](#orga1b7f17)
    2.  [Go to config file](#org88e65bf)
    3.  [Reloading config](#org8ca3297)
    4.  [Programming](#orgb25c35b)
        1.  [Compiling](#org23d2fb8)
        2.  [Compiling in emacs via lambda](#org1df6509)
        3.  [Notification bar replacement](#org010e7e1)
        4.  [Opening Output](#orgb201709)
    5.  [Email](#orgaa9f147)
    6.  [dmenu](#org2617eee)
        1.  [dmenu launcher](#orgdfc7f53)
        2.  [dmenuhandler](#orga56d33c)
        3.  [pdf-opener](#orgf4c6089)
        4.  [video-opener](#orgc530903)
    7.  [Other&#x2026;](#org35bf3d1)
        1.  [Mouse](#org0dd4084)
5.  [Major mode hooks and variables](#org88c68f2)
    1.  [Lilypond mode](#orge5c3a2c)
    2.  [Electric pairs](#orga6ec6cd)
    3.  [Org Mode](#org10d2a35)
    4.  [M-x compile hooks](#org3c8c518)
        1.  [Groff](#org8e8b3f4)
        2.  [C](#org7dd537b)
        3.  [LiLyPond](#org70aad47)
        4.  [LaTeX](#orgd5df58f)
6.  [Keybindings](#org6a1cddf)
    1.  [Miscellaneous](#org9425ec7)
        1.  [Line numbers](#org32fb029)
        2.  [Spelling correction](#org70dfc0b)
        3.  [Line wrap](#org014a6ec)
        4.  [Mouse](#org703ee2d)
        5.  [client](#org60bc810)
    2.  [Clipboard](#org0526919)
    3.  [Moving between windows](#org26b2104)
    4.  [Config file](#org074cc8c)
        1.  [Visit config file](#org15b5a31)
        2.  [Reload config file](#org45bc18b)
    5.  [General WM stuff](#org0386657)
        1.  [Information](#orga4da5e2)
        2.  [dmenu scripts](#org4c52e1e)
    6.  [Programming/Typesetting](#orgd0f3187)
7.  [Mode-line](#org454b4f7)
8.  [Email](#org70d09df)


<a id="orgc500f33"></a>

# Packages


<a id="org6fff18e"></a>

## Startup


<a id="org750f95f"></a>

### Repositories and initialisation

Enable melpa archive and making sure packages work

    (require 'package)
    (setq package-enable-at-startup nil)
    (add-to-list 'package-archives
                 '("melpa" . "https://melpa.org/packages/")
                 '("gnu" . "https://elpa.gnu.org/packages/"))
    (package-initialize)


<a id="org252b846"></a>

### Use-package

Install use-package if not installed

    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))


<a id="org68beaa3"></a>

### Base16-theme

Gotta have that HIGH-CONTRAST theme

    (unless (package-installed-p 'base16-theme)
      (package-refresh-contents)
      (package-install 'base16-theme))
    (setq base16-distinct-fringe-background nil)
    (setq base16-theme-256-color-source "terminal")
    (load-theme 'base16-bright t)


<a id="orgf6bd89b"></a>

### exwm

Emacs X Window Manager

    (use-package exwm
      :ensure t
      :config
      (require 'exwm-config)
      (exwm-config-default))


<a id="org1019584"></a>

## Installed Packages


<a id="org7bf5d85"></a>

### Pinentry

This package lets emacs be used for gpg authentication

    (use-package pinentry
      :ensure t
      :init
      (setq epa-pinentry-mode 'loopback)
      (pinentry-start))


<a id="org6957343"></a>

### Completion

1.  Company

    A very good package for auto-completion
    \#+BEGIN<sub>SRC</sub> emacs-lisp
      (use-package company
        :ensure t
        :init
        (add-hook 'after-init-hook 'global-company-mode)
        :config
        (setq company-idle-delay 0))
    \#+END<sub>SRC</sub>\*

2.  Geiser

    A scheme backend
    
        (use-package geiser
          :ensure t)


<a id="org7372484"></a>

## "Applications"

1.  Mingus

    A nice mpd front-end in emacs
    (I couldn't get EMMS working with mopidy)
    
        (use-package mingus
          :ensure t)

2.  Notmuch

    A simple email client, with emphasis on searching
    
        (use-package notmuch
          :ensure t
          :config (setq notmuch-archive-tags '("-unread" "-inbox")))

3.  Transmission

    An emacs front-end for the transmission bittorrent daemon
    
        (use-package transmission
          :ensure t)

4.  Elfeed

        (use-package elfeed
          :ensure t
          :config (load-file (expand-file-name "~/.emacs.d/feeds.el")))


<a id="org483536e"></a>

### Appearance

1.  Rainbow-mode

    This package highlights hex colours
    (also install xterm-color to use in a terminal emulator)
    
        (use-package rainbow-mode
          :ensure t
          :config
          (global-set-key (kbd "C-c h") 'rainbow-mode))

2.  xterm-color

    Allows a terminal emulator to use 256 colors
    
        (use-package xterm-color
          :ensure t)

3.  Rainbow-delimiters

    Minor mode that highlights parentheses well
    
        (use-package rainbow-delimiters
          :ensure t
          :init
          (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


<a id="org293d07d"></a>

### Quality of life

1.  Smex

    smex integrates "M-x" with `ido`
    
        (use-package smex
          :ensure t
          :init (smex-initialize)
          :bind
          ("M-x" . smex))

2.  Ido-vertical

    This package enables a vertical mode for the `ido` function
    
        (use-package ido-vertical-mode
          :ensure t
          :init
          (ido-vertical-mode 1))
        (setq ido-vertical-define-keys 'C-n-and-C-p-only)

3.  Which-key

    Shows what your keys do
    
        (use-package which-key
          :ensure t
          :init (which-key-mode)) 

4.  Try

    Allows you to try other packages without committing
    
        (use-package try
          :ensure t)


<a id="orgba816dc"></a>

### Not really useful

1.  Lorem Ipsum

    A 'Lorem ipsum' generator
    
        (use-package lorem-ipsum
          :ensure t)


<a id="org9546deb"></a>

### To be confirmed

1.  Elpher - gopher client

        (use-package elpher
          :ensure t)

2.  xclip - enable use of X11 clipboard in terminal

        (use-package xclip
          :ensure t)

3.  exwm-surf - allows emacsy control of surf

        ;; (use-package exwm-surf
        ;;   :ensure t
        ;;   :config
        ;;   (setq exwm-surf-history-file "/home/jamzattack/.surf/history")
        ;;   (setq exwm-surf-bookmark-file "/home/jamzattack/.surf/bookmarks")
        ;;   (add-hook 'exwm-manage-finish-hook 'exwm-surf-init))


<a id="org7c552e2"></a>

### PDF-tools

Majorly increases performance when viewing pdfs as a file

    (use-package pdf-tools
      :ensure t
      :init
      (pdf-tools-install))


<a id="org75f844c"></a>

# Environment Variables

Setting path, email and password variables

    (setenv "NOTMUCH_CONFIG" (expand-file-name "~/.config/notmuch-config"))
    (setenv "PASSWORD_STORE_DIR" (expand-file-name "~/.local/share/password-store/"))
    (setenv "PATH" (concat (getenv "PATH") ":/home/jamzattack/.local/bin"))
    (setq exec-path (append exec-path '("/home/jamzattack/.local/bin")))


<a id="org0d428a4"></a>

# Fixing defaults


<a id="orgee0421f"></a>

## Miscellaneous stuff


<a id="org7d5d660"></a>

### No more pesky extra files, other basics

    (defalias 'yes-or-no-p 'y-or-n-p)
    (setq make-backup-files nil)
    (setq auto-save-default nil)


<a id="org10d4919"></a>

### Enable all the features, because what's the point in having less?

    (setq disabled-command-function nil)


<a id="orgad761e3"></a>

## Aesthetic stuff


<a id="org6a59af4"></a>

### GUI ugliness

Disable all the wasteful bars

    (scroll-bar-mode -1)
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (fringe-mode 1)


<a id="orgb5c9196"></a>

### Font and cursor

Use system monospace font, red non-blinking cursor

    (set-cursor-color "red")
    (blink-cursor-mode -1)


<a id="org24c3047"></a>

## Tabs

Tabs are 4 spaces wide

    (setq-default indent-tabs-mode nil)
    (setq tab-width 4)


<a id="org7cad837"></a>

## Buffers/input


<a id="org32742d8"></a>

### ido-mode

ido-mode is much better than the default for switching
buffers and going to files.

    (setq ido-enable-flex-matching nil)
    (setq ido-create-new-buffer 'always)
    (setq ido-everywhere t)
    (ido-mode 1)


<a id="org20b8968"></a>

### ibuffer

ibuffer is also a lot better than the default
(plus it has colours)

    (global-set-key (kbd "C-x C-b") 'ibuffer)


<a id="orgcd5c751"></a>

# Custom functions


<a id="orga1b7f17"></a>

## Resizing windows

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


<a id="org88e65bf"></a>

## Go to config file

Visit your config file. Bound to "C-c e" in `Keybindings` section.

    (defun config-visit ()
      "Go to your config.org"
      (interactive)
      (find-file "~/.emacs.d/config.org"))


<a id="org8ca3297"></a>

## Reloading config

Reloads this config file. Bound to "C-c r" in Keybindings section.

    (defun config-reload ()
      "Reloads ~/.emacs.d/config.org at runtime"
      (interactive)
      (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))


<a id="orgb25c35b"></a>

## Programming


<a id="org23d2fb8"></a>

### Compiling

Compile the file associate with current buffer.

    (defun generic-compiler ()
      "Runs my own compile script on the file associated with the
    current buffer. Works with:
    lilypond, groff (ms, mom), c, tex, python, and go"
      (interactive)
      (shell-command (concat "compiler "
                             (buffer-file-name) " &"))
      (bury-buffer "*Async Shell Command*"))


<a id="org1df6509"></a>

### Compiling in emacs via lambda

    (require 'compile)
    (defun my-compiler-command ()
      "A simple lambda to set compile-command"
      (lambda ()
        (set (make-local-variable 'compile-command)
             (format "compiler %s" buffer-file-name))))


<a id="org010e7e1"></a>

### Notification bar replacement

    (defun notibar ()
      "Brings up a notification with the following information:
    Date
    Time
    Memory used
    Disk available
    Brightness level
    Volume level
    Battery level"
      (interactive)
      (shell-command "notibar"))


<a id="orgb201709"></a>

### Opening Output

    (defun opout ()
      "Opens a pdf file of the same name as the current file"
      (interactive)
      (find-file-other-window (concat
                               (file-name-sans-extension buffer-file-name)
                               ".pdf")))


<a id="orgaa9f147"></a>

## Email

    (defun mailsync ()
      "Downloads new mail and adds it to the notmuch database"
      (interactive)
      (shell-command "mbsync -a && notmuch new &" "*mailsync*"))


<a id="org2617eee"></a>

## dmenu


<a id="orgdfc7f53"></a>

### dmenu launcher

    (defun dmenu_recency ()
      "Launch a program with dmenu"
      (interactive)
      (start-process "dmenu_recency" nil
                     "dmenu_recency"))


<a id="orga56d33c"></a>

### dmenuhandler

    (defun dmenuhandler ()
      "Choose how to handle the url in X11 clipboard"
      (interactive)
      (start-process "dmenuhandler" nil
                     "dmenuhandler"))


<a id="orgf4c6089"></a>

### pdf-opener

    (defun pdf-opener ()
      "Select a .pdf or .ps file to view in zathura"
      (interactive)
      (start-process "pdf-opener" nil
                     "pdf-opener"))


<a id="orgc530903"></a>

### video-opener

    (defun video-opener ()
      "Select a downloaded video to watch via dmenu and mpv"
      (interactive)
      (start-process "video-opener" nil
                     "video-opener"))


<a id="org35bf3d1"></a>

## Other&#x2026;


<a id="org0dd4084"></a>

### Mouse

    (defun mousetoggle ()
      "Toggles touchpad on my laptop"
      (interactive)
      (shell-command "mousetoggle")
      (message "touchpad input toggled"))


<a id="org88c68f2"></a>

# Major mode hooks and variables


<a id="orge5c3a2c"></a>

## Lilypond mode

Use lilypond mode for .ly files
(taken from lilypond.org)

    (autoload 'LilyPond-mode "lilypond-mode")
    (setq auto-mode-alist
          (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
    (setq LilyPond-pdf-command "zathura")


<a id="orga6ec6cd"></a>

## Electric pairs

Auto-add parentheses

    (setq electric-pair-pairs '(
                                (?\( . ?\))
                                ))

    (add-hook 'prog-mode-hook (electric-pair-mode t))


<a id="org10d2a35"></a>

## Org Mode

    (add-hook 'org-mode-hook 'org-indent-mode)
    (setq org-src-window-setup 'current-window)
    (setq org-src-tab-acts-natively t)
    (setq org-ellipsis " ")


<a id="org3c8c518"></a>

## M-x compile hooks

The function ´my-compiler-command´ is defined above.  Simply changes
variable 'compiler-command.


<a id="org8e8b3f4"></a>

### Groff

    (add-hook 'nroff-mode-hook
              (lambda ()
                (set (make-local-variable 'compile-command)
                     (format "groff -ms -Tpdf %s > %s" 
                             (shell-quote-argument buffer-file-name)
                             (concat (file-name-sans-extension
                                      (shell-quote-argument
                                       buffer-file-name)) ".pdf")))))


<a id="org7dd537b"></a>

### C

    (add-hook 'c-mode-hook
              (lambda ()
                (set (make-local-variable 'compile-command)
                     (format "compiler %s" buffer-file-name))))


<a id="org70aad47"></a>

### LiLyPond

    (add-hook 'LilyPond-mode-hook
              (lambda ()
                (set (make-local-variable 'compile-command)
                     (format "lilypond %s" buffer-file-name))))


<a id="orgd5df58f"></a>

### LaTeX

    (add-hook 'latex-mode-hook
              (lambda ()
                (set (make-local-variable 'compile-command)
                     (format "pdflatex %s" buffer-file-name))))

Somewhat related, overrides latex-mode keybinding that interferes with
my compile key "C-c C-m".

    (add-hook 'latex-mode-hook
              (lambda ()
                (local-unset-key (kbd "C-c C-m"))))


<a id="org6a1cddf"></a>

# Keybindings


<a id="org9425ec7"></a>

## Miscellaneous


<a id="org32fb029"></a>

### Line numbers

    (global-set-key (kbd "C-c n") 'display-line-numbers-mode)


<a id="org70dfc0b"></a>

### Spelling correction

    (global-set-key (kbd "C-c s") 'flyspell-mode)


<a id="org014a6ec"></a>

### Line wrap

    (global-set-key (kbd "C-c l") 'toggle-truncate-lines)


<a id="org703ee2d"></a>

### Mouse

    (global-set-key (kbd "s-t \\") 'mousetoggle)


<a id="org60bc810"></a>

### client

    (global-set-key (kbd "C-x C-c") 'delete-frame)


<a id="org0526919"></a>

## Clipboard

    (global-set-key (kbd "C-c w") 'clipboard-kill-ring-save)
    (global-set-key (kbd "C-c y") 'clipboard-yank)


<a id="org26b2104"></a>

## Moving between windows

    (global-set-key (kbd "s-p") 'windmove-up)
    (global-set-key (kbd "s-n") 'windmove-down)
    (global-set-key (kbd "s-b") 'windmove-left)
    (global-set-key (kbd "s-f") 'windmove-right)
    (global-set-key (kbd "<M-tab>") 'other-window)


<a id="org074cc8c"></a>

## Config file

Both defined in the Custom Functions section


<a id="org15b5a31"></a>

### Visit config file

    (global-set-key (kbd "C-c e") 'config-visit)


<a id="org45bc18b"></a>

### Reload config file

    (global-set-key (kbd "C-c r") 'config-reload)


<a id="org0386657"></a>

## General WM stuff


<a id="orga4da5e2"></a>

### Information

    (global-set-key (kbd "s-t b") 'battery)
    (global-set-key (kbd "s-t s-b") 'notibar)


<a id="org4c52e1e"></a>

### dmenu scripts

All of these are bound to functions written in 'Custom functions'

    (global-set-key (kbd "s-t d") 'dmenu_recency)
    (global-set-key (kbd "s-t P") 'pdf-opener)
    (global-set-key (kbd "s-t V") 'video-opener)
    (global-set-key (kbd "s-t D") 'dmenuhandler)


<a id="orgd0f3187"></a>

## Programming/Typesetting

    (global-set-key (kbd "C-c C-m") 'compile)
    (global-set-key (kbd "C-c p") 'opout)


<a id="org454b4f7"></a>

# Mode-line

Just some basic extra stuff in the mode-line.
I don't want anything fancy.

    (column-number-mode t)
    (display-time-mode t)
    (setq display-time-24hr-format 1)


<a id="org70d09df"></a>

# Email

    (setq send-mail-function 'sendmail-send-it
          sendmail-program "/usr/bin/msmtp"
          mail-specify-envelope-from t
          message-sendmail-envelope-from 'header
          mail-envelope-from 'header)

