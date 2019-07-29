- [Packages](#orga3fd7d8)
  - [Startup](#org3e9ae4a)
    - [Repositories and initialisation](#org63ae400)
    - [Use-package](#org6617b5d)
    - [Base16-theme](#org636c4c2)
    - [exwm](#org11a5f40)
  - [Installed Packages](#org5fed59a)
    - [Pinentry](#org78c7b78)
    - [Org](#org0c24984)
    - [Completion](#orgffbea8e)
    - ["Applications"](#orgc2b2f4c)
    - [Appearance](#orgfa738b8)
    - [Quality of life](#org9be62c5)
    - [Not really useful](#org0d4fdf2)
    - [To be confirmed](#orgaefa3d4)
    - [PDF-tools](#orga1cc1b5)
- [Environment Variables](#org446a5fb)
- [Fixing defaults](#orgfa30881)
  - [Miscellaneous stuff](#orgb4c5540)
    - [No more pesky extra files, other basics](#orgf7a4b6b)
    - [Enable all the features, because what's the point in having less?](#org765814c)
  - [Aesthetic stuff](#orge0ce14f)
    - [GUI ugliness](#org755c880)
    - [Font and cursor](#org0c75f94)
  - [Tabs](#orgea43c04)
  - [Buffers/input](#org28a8af0)
    - [ido-mode](#orge90dfcd)
    - [ibuffer](#org63cb5a8)
- [Custom functions](#org1e35a72)
  - [Resizing windows](#org73de44b)
  - [Go to config file](#org6c13e14)
  - [Reloading config](#org374a11f)
  - [Programming](#orgdd2e7ea)
    - [Compiling](#org2f0b97d)
    - [Opening Output](#org4dfc2fb)
  - [Email](#org6d7cb5b)
  - [WM stuff](#org56beae0)
    - [Notification bar replacement](#org7febf5e)
    - [dmenu](#org2debdf0)
    - [Other&#x2026;](#org93813b8)
- [Major mode hooks and variables](#orgc6766ef)
  - [Lilypond mode](#org1111109)
  - [Electric pairs](#org7619958)
  - [Org Mode](#org36e3793)
  - [M-x compile hooks](#org83e610a)
    - [Groff](#org8c0e4ed)
    - [C](#org2614a8f)
    - [LiLyPond](#org891be9e)
    - [LaTeX](#org229a124)
- [Keybindings](#org67057eb)
  - [Miscellaneous](#org8893709)
    - [Line numbers](#orgae7a582)
    - [Spelling correction](#org8f5e42c)
    - [Line wrap](#org05d38b3)
    - [Mouse](#org61c58ae)
    - [client](#org8b4a43e)
  - [Clipboard](#org25e30e0)
  - [Moving between windows](#orgbbebc08)
  - [Config file](#org25ca5e6)
    - [Visit config file](#org82c4329)
    - [Reload config file](#orga5058b3)
  - [General WM stuff](#org32749d7)
    - [Information](#org96a71d9)
    - [dmenu scripts](#org08f2cbe)
  - [Programming/Typesetting](#org9e4d0b8)
- [Mode-line](#org12f4496)
- [Email](#orgb081e5f)


<a id="orga3fd7d8"></a>

# Packages


<a id="org3e9ae4a"></a>

## Startup


<a id="org63ae400"></a>

### Repositories and initialisation

Enable melpa archive and making sure packages work

```emacs-lisp
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)
```


<a id="org6617b5d"></a>

### Use-package

Install use-package if not installed

```emacs-lisp
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
```


<a id="org636c4c2"></a>

### Base16-theme

Gotta have that HIGH-CONTRAST theme

```emacs-lisp
(unless (package-installed-p 'base16-theme)
  (package-refresh-contents)
  (package-install 'base16-theme))
(setq base16-distinct-fringe-background nil)
(setq base16-theme-256-color-source "terminal")
(load-theme 'base16-bright t)
```


<a id="org11a5f40"></a>

### exwm

Emacs X Window Manager

```emacs-lisp
(use-package exwm
  :ensure t
  :config
  (require 'exwm-config)
  (exwm-config-default))
```


<a id="org5fed59a"></a>

## Installed Packages


<a id="org78c7b78"></a>

### Pinentry

This package lets emacs be used for gpg authentication

```emacs-lisp
(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
```


<a id="org0c24984"></a>

### Org

1.  Github markdown

    ```emacs-lisp
    (use-package ox-gfm
      :ensure t)
    ```

2.  Html export

    ```emacs-lisp
    (use-package htmlize
      :ensure t)
    ```


<a id="orgffbea8e"></a>

### Completion

1.  Company

    A very good package for auto-completion
    
    ```emacs-lisp
    (use-package company
      :ensure t
      :init
      (global-company-mode)
      :config
      (setq company-idle-delay 0))
    ```

2.  Geiser

    A scheme backend
    
    ```emacs-lisp
    (use-package geiser
      :ensure t)
    ```


<a id="orgc2b2f4c"></a>

### "Applications"

1.  Mingus

    A nice mpd front-end in emacs (I couldn't get EMMS working with mopidy)
    
    ```emacs-lisp
    (use-package mingus
      :ensure t)
    ```

2.  Notmuch

    A simple email client, with emphasis on searching
    
    ```emacs-lisp
    (use-package notmuch
      :ensure t
      :config (setq notmuch-archive-tags '("-unread" "-inbox")))
    ```

3.  Transmission

    An emacs front-end for the transmission bittorrent daemon
    
    ```emacs-lisp
    (use-package transmission
      :ensure t)
    ```

4.  Elfeed

    ```emacs-lisp
    (use-package elfeed
      :ensure t
      :config (load-file (expand-file-name "~/.emacs.d/feeds.el")))
    ```


<a id="orgfa738b8"></a>

### Appearance

1.  Rainbow-mode

    This package highlights hex colours (also install xterm-color to use in a terminal emulator)
    
    ```emacs-lisp
    (use-package rainbow-mode
      :ensure t
      :config
      (global-set-key (kbd "C-c h") 'rainbow-mode))
    ```

2.  xterm-color

    Allows a terminal emulator to use 256 colors
    
    ```emacs-lisp
    (use-package xterm-color
      :ensure t)
    ```

3.  Rainbow-delimiters

    Minor mode that highlights parentheses well
    
    ```emacs-lisp
    (use-package rainbow-delimiters
      :ensure t
      :init
      (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
    ```


<a id="org9be62c5"></a>

### Quality of life

1.  Smex

    smex integrates "M-x" with `ido`
    
    ```emacs-lisp
    (use-package smex
      :ensure t
      :init (smex-initialize)
      :bind
      ("M-x" . smex))
    ```

2.  Ido-vertical

    This package enables a vertical mode for the `ido` function
    
    ```emacs-lisp
    (use-package ido-vertical-mode
      :ensure t
      :init
      (ido-vertical-mode 1))
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)
    ```

3.  Which-key

    Shows what your keys do
    
    ```emacs-lisp
    (use-package which-key
      :ensure t
      :init (which-key-mode)) 
    ```

4.  Try

    Allows you to try other packages without committing
    
    ```emacs-lisp
    (use-package try
      :ensure t)
    ```


<a id="org0d4fdf2"></a>

### Not really useful

1.  Lorem Ipsum

    A 'Lorem ipsum' generator
    
    ```emacs-lisp
    (use-package lorem-ipsum
      :ensure t)
    ```


<a id="orgaefa3d4"></a>

### To be confirmed

1.  Elpher - gopher client

    ```emacs-lisp
    (use-package elpher
      :ensure t)
    ```

2.  xclip - enable use of X11 clipboard in terminal

    ```emacs-lisp
    (use-package xclip
      :ensure t)
    ```

3.  exwm-surf - allows emacsy control of surf

    ```emacs-lisp
    ;; (use-package exwm-surf
    ;;   :ensure t
    ;;   :config
    ;;   (setq exwm-surf-history-file "/home/jamzattack/.surf/history")
    ;;   (setq exwm-surf-bookmark-file "/home/jamzattack/.surf/bookmarks")
    ;;   (add-hook 'exwm-manage-finish-hook 'exwm-surf-init))
    ```


<a id="orga1cc1b5"></a>

### PDF-tools

Majorly increases performance when viewing pdfs as a file

```emacs-lisp
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install))
```


<a id="org446a5fb"></a>

# Environment Variables

Setting path, email and password variables

```emacs-lisp
(setenv "NOTMUCH_CONFIG" (expand-file-name "~/.config/notmuch-config"))
(setenv "PASSWORD_STORE_DIR" (expand-file-name "~/.local/share/password-store/"))
(add-hook 'after-init-hook
          (lambda ()
            (setenv "PATH" (concat (getenv "PATH") ":/home/jamzattack/.local/bin/"))
            (push "/home/jamzattack/.local/bin/" exec-path)))
```


<a id="orgfa30881"></a>

# Fixing defaults


<a id="orgb4c5540"></a>

## Miscellaneous stuff


<a id="orgf7a4b6b"></a>

### No more pesky extra files, other basics

```emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
```


<a id="org765814c"></a>

### Enable all the features, because what's the point in having less?

```emacs-lisp
(setq disabled-command-function nil)
```


<a id="orge0ce14f"></a>

## Aesthetic stuff


<a id="org755c880"></a>

### GUI ugliness

Disable all the wasteful bars

```emacs-lisp
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode 1)
```


<a id="org0c75f94"></a>

### Font and cursor

Use system monospace font, red non-blinking cursor

```emacs-lisp
(set-cursor-color "red")
(blink-cursor-mode -1)
```


<a id="orgea43c04"></a>

## Tabs

Tabs are 4 spaces wide

```emacs-lisp
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
```


<a id="org28a8af0"></a>

## Buffers/input


<a id="orge90dfcd"></a>

### ido-mode

ido-mode is much better than the default for switching buffers and going to files.

```emacs-lisp
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)
```


<a id="org63cb5a8"></a>

### ibuffer

ibuffer is also a lot better than the default (plus it has colours)

```emacs-lisp
(global-set-key (kbd "C-x C-b") 'ibuffer)
```


<a id="org1e35a72"></a>

# Custom functions


<a id="org73de44b"></a>

## Resizing windows

```emacs-lisp
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
```


<a id="org6c13e14"></a>

## Go to config file

Visit your config file. Bound to "C-c e" in `Keybindings` section.

```emacs-lisp
(defun config-visit ()
  "Go to your config.org"
  (interactive)
  (find-file "~/.emacs.d/config.org"))
```


<a id="org374a11f"></a>

## Reloading config

Reloads this config file. Bound to "C-c r" in Keybindings section.

```emacs-lisp
(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
```


<a id="orgdd2e7ea"></a>

## Programming


<a id="org2f0b97d"></a>

### Compiling


<a id="org4dfc2fb"></a>

### Opening Output

```emacs-lisp
(defun opout ()
  "Opens a pdf file of the same name as the current file"
  (interactive)
  (find-file-other-window (concat
                           (file-name-sans-extension buffer-file-name)
                           ".pdf")))
```


<a id="org6d7cb5b"></a>

## Email

```emacs-lisp
(defun mailsync ()
  "Downloads new mail and adds it to the notmuch database"
  (interactive)
  (shell-command "mbsync -a && notmuch new &" "*mailsync*"))
```


<a id="org56beae0"></a>

## WM stuff


<a id="org7febf5e"></a>

### Notification bar replacement

```emacs-lisp
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
  (call-process "notibar"))
```


<a id="org2debdf0"></a>

### dmenu

1.  dmenu launcher

    ```emacs-lisp
    (defun dmenu_recency ()
      "Launch a program with dmenu"
      (interactive)
      (start-process "dmenu_recency" nil
                     "dmenu_recency"))
    ```

2.  dmenuhandler

    ```emacs-lisp
    (defun dmenuhandler ()
      "Choose how to handle the url in X11 clipboard"
      (interactive)
      (start-process "dmenuhandler" nil
                     "dmenuhandler"))
    ```

3.  pdf-opener

    ```emacs-lisp
    (defun pdf-opener ()
      "Select a .pdf or .ps file to view in zathura"
      (interactive)
      (start-process "pdf-opener" nil
                     "pdf-opener"))
    ```

4.  video-opener

    ```emacs-lisp
    (defun video-opener ()
      "Select a downloaded video to watch via dmenu and mpv"
      (interactive)
      (start-process "video-opener" nil
                     "video-opener"))
    ```


<a id="org93813b8"></a>

### Other&#x2026;

1.  Mouse

    ```emacs-lisp
    (defun mousetoggle ()
      "Toggles touchpad on my laptop"
      (interactive)
      (shell-command "mousetoggle")
      (message "touchpad input toggled"))
    ```


<a id="orgc6766ef"></a>

# Major mode hooks and variables


<a id="org1111109"></a>

## Lilypond mode

Use lilypond mode for .ly files (taken from lilypond.org)

```emacs-lisp
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
(setq LilyPond-pdf-command "zathura")
```


<a id="org7619958"></a>

## Electric pairs

Auto-add parentheses

```emacs-lisp
(setq electric-pair-pairs '(
                            (?\( . ?\))
                            ))
```

```emacs-lisp
(add-hook 'prog-mode-hook (electric-pair-mode t))
```


<a id="org36e3793"></a>

## Org Mode

```emacs-lisp
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-src-window-setup 'current-window)
(setq org-src-tab-acts-natively t)
(setq org-ellipsis " ")
```


<a id="org83e610a"></a>

## M-x compile hooks


<a id="org8c0e4ed"></a>

### Groff

```emacs-lisp
(add-hook 'nroff-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "groff -ms -Tpdf %s > %s" 
                         (shell-quote-argument buffer-file-name)
                         (concat (file-name-sans-extension
                                  (shell-quote-argument
                                   buffer-file-name)) ".pdf")))))


```


<a id="org2614a8f"></a>

### C

```emacs-lisp
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "compiler %s" buffer-file-name))))
```


<a id="org891be9e"></a>

### LiLyPond

```emacs-lisp
(add-hook 'LilyPond-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "lilypond %s" buffer-file-name))))
```


<a id="org229a124"></a>

### LaTeX

```emacs-lisp
(add-hook 'latex-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "pdflatex %s" buffer-file-name))))
```

Somewhat related, overrides latex-mode keybinding that interferes with my compile key "C-c C-m".

```emacs-lisp
(add-hook 'latex-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-c C-m"))))
```


<a id="org67057eb"></a>

# Keybindings


<a id="org8893709"></a>

## Miscellaneous


<a id="orgae7a582"></a>

### Line numbers

```emacs-lisp
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
```


<a id="org8f5e42c"></a>

### Spelling correction

```emacs-lisp
(global-set-key (kbd "C-c s") 'flyspell-mode)
```


<a id="org05d38b3"></a>

### Line wrap

```emacs-lisp
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
```


<a id="org61c58ae"></a>

### Mouse

```emacs-lisp
(global-set-key (kbd "s-t \\") 'mousetoggle)
```


<a id="org8b4a43e"></a>

### client

```emacs-lisp
(global-set-key (kbd "C-x C-c") 'delete-frame)
```


<a id="org25e30e0"></a>

## Clipboard

```emacs-lisp
(global-set-key (kbd "C-c w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-c y") 'clipboard-yank)
```


<a id="orgbbebc08"></a>

## Moving between windows

```emacs-lisp
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-n") 'windmove-down)
(global-set-key (kbd "s-b") 'windmove-left)
(global-set-key (kbd "s-f") 'windmove-right)
(global-set-key (kbd "<M-tab>") 'other-window)
```


<a id="org25ca5e6"></a>

## Config file

Both defined in the Custom Functions section


<a id="org82c4329"></a>

### Visit config file

```emacs-lisp
(global-set-key (kbd "C-c e") 'config-visit)
```


<a id="orga5058b3"></a>

### Reload config file

```emacs-lisp
(global-set-key (kbd "C-c r") 'config-reload)
```


<a id="org32749d7"></a>

## General WM stuff


<a id="org96a71d9"></a>

### Information

```emacs-lisp
(global-set-key (kbd "s-t b") 'battery)
(global-set-key (kbd "s-t s-b") 'notibar)
```


<a id="org08f2cbe"></a>

### dmenu scripts

All of these are bound to functions written in 'Custom functions'

```emacs-lisp
(global-set-key (kbd "s-t d") 'dmenu_recency)
(global-set-key (kbd "s-t P") 'pdf-opener)
(global-set-key (kbd "s-t V") 'video-opener)
(global-set-key (kbd "s-t D") 'dmenuhandler)
```


<a id="org9e4d0b8"></a>

## Programming/Typesetting

```emacs-lisp
(global-set-key (kbd "C-c C-m") 'compile)
(global-set-key (kbd "C-c p") 'opout)
```


<a id="org12f4496"></a>

# Mode-line

Just some basic extra stuff in the mode-line. I don't want anything fancy.

```emacs-lisp
(column-number-mode t)
(display-time-mode t)
(setq display-time-24hr-format 1)
```


<a id="orgb081e5f"></a>

# Email

```emacs-lisp
(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
```