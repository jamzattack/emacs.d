- [Packages](#orgad6610d)
  - [Startup](#org7aedcac)
    - [Repositories and initialisation](#orgb158679)
    - [Use-package](#orgdc157c8)
    - [Theme](#org8e6736e)
  - [Installed Packages](#orga51a2c2)
    - [Pinentry](#orgc575a23)
    - [Org](#orgb6701c3)
    - [Window management](#org8381cf0)
    - [Completion](#orgbf9a678)
    - ["Applications"](#org18b8089)
    - [Appearance](#org152bb00)
    - [god-mode](#org5a8d1c6)
    - [Quality of life](#orgcdd7774)
    - [Not really useful](#org007e256)
    - [To be confirmed](#orgf520a75)
    - [PDF-tools](#org3e46121)
- [Stuff to do when loading](#orgc4d7694)
  - [Environment Variables](#orgf7edea5)
- [Fixing defaults](#orgea15fbb)
  - [Miscellaneous stuff](#orgab52fd8)
    - [No more pesky extra files, other basics](#org6d99efd)
    - [Enable all the features, because what's the point in having less?](#orgfd2ef5d)
  - [Aesthetic stuff](#org9a9ead7)
    - [GUI ugliness](#org8ac5632)
    - [Font and cursor](#org63525a8)
  - [Tabs](#org0da4da7)
  - [Buffers/input](#org11fd25e)
    - [ido-mode](#orgaf9c312)
    - [ibuffer](#org6b97a8b)
- [Custom functions](#org160a618)
  - [Resizing windows](#org6498ad7)
  - [Go to config file](#org606359a)
  - [Ido](#org2699408)
    - [Bookmarks](#org2e0084f)
  - [Reloading config](#org1a5696b)
  - [Programming](#org905bccd)
    - [Opening Output](#org6ade26b)
  - [Email](#orgf6d4716)
  - [WM stuff](#org6ca9fd4)
    - [Notification bar replacement](#orgd33c5f2)
    - [dmenu](#orge8d3fd4)
    - [Other&#x2026;](#org89ff23a)
- [Major mode hooks and variables](#org0ad1c0b)
  - [Lilypond mode](#org9b8105c)
  - [Electric pairs](#org908ab48)
  - [Org Mode](#org0048f58)
  - [M-x compile hooks](#org53b93f8)
    - [Groff](#org3d9b192)
    - [C](#org0d208ff)
    - [LiLyPond](#org319cf4d)
    - [LaTeX](#orgfa7a42e)
- [Keybindings](#org56044be)
  - [Interaction with Emacs](#orgac817a3)
    - [ido-bookmark-jump (custom function)](#org0d8c3ab)
    - [Terminal functionality](#orgd6a40bf)
  - [Config](#org9602673)
  - [General WM stuff](#orgf8953f8)
    - [System information](#org4a49d14)
    - [dmenu scripts](#org001d391)
  - [Programming/Typesetting](#orgddb4927)
  - [Miscellaneous](#orgba8fd86)
    - [Line numbers](#orgea38b0c)
    - [Spelling correction](#org5c0d89e)
    - [Line wrap](#org7c4e4a8)
    - [Mouse](#orgb01b340)
- [Mode-line](#org4f79fab)
- [Email](#org4ad40cb)


<a id="orgad6610d"></a>

# Packages


<a id="org7aedcac"></a>

## Startup


<a id="orgb158679"></a>

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


<a id="orgdc157c8"></a>

### Use-package

Install use-package if not installed

```emacs-lisp
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
```


<a id="org8e6736e"></a>

### Theme

1.  moe-theme

    ```emacs-lisp
    (use-package moe-theme
      :ensure t
      :config
      (require 'moe-theme-switcher)
      (setq moe-theme-switch-by-sunrise-and-sunset t)
      (moe-theme-set-color 'purple))
    ```

2.  base16

    Base16 is a nice theme framework, but moe-theme is my new best friend.
    
    ```emacs-lisp
    (unless (package-installed-p 'base16-theme)
      (package-refresh-contents)
      (package-install 'base16-theme))
    (setq base16-distinct-fringe-background nil)
    (setq base16-theme-256-color-source "colors")
    ;; (load-theme 'base16-bright t)
    ```


<a id="orga51a2c2"></a>

## Installed Packages


<a id="orgc575a23"></a>

### Pinentry

This package lets emacs be used for gpg authentication

```emacs-lisp
(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
```


<a id="orgb6701c3"></a>

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


<a id="org8381cf0"></a>

### Window management

1.  EXWM - Emacs X Window Manager

    Manipulate X windows as emacs buffers.
    
    ```emacs-lisp
    (use-package exwm
      :ensure t
      :config
      (require 'exwm-config)
      (exwm-config-default))
    ```

2.  Golden ratio

    This package resizes windows according to the golden ratio. The focused window is larger
    
    ```emacs-lisp
    (use-package golden-ratio
      :ensure t
      :config (golden-ratio-mode))
    ```


<a id="orgbf9a678"></a>

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


<a id="org18b8089"></a>

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
      :config                                                   
      (if (file-exists-p "~/.emacs.d/feeds.el")                 
          (load-file (expand-file-name "~/.emacs.d/feeds.el"))))
    ```


<a id="org152bb00"></a>

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


<a id="org5a8d1c6"></a>

### god-mode

```emacs-lisp
(use-package god-mode
  :ensure t
  :config
  (global-set-key (kbd "<left>") 'god-local-mode)
  (global-set-key (kbd "<right>") 'god-local-mode)
  (god-mode))
```


<a id="orgcdd7774"></a>

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

2.  Which-key

    Shows what your keys do
    
    ```emacs-lisp
    (use-package which-key
      :ensure t
      :init (which-key-mode)
      :config (which-key-enable-god-mode-support))
    ```

3.  Try

    Allows you to try other packages without committing
    
    ```emacs-lisp
    (use-package try
      :ensure t)
    ```


<a id="org007e256"></a>

### Not really useful

1.  Lorem Ipsum

    A 'Lorem ipsum' generator
    
    ```emacs-lisp
    (use-package lorem-ipsum
      :ensure t)
    ```


<a id="orgf520a75"></a>

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


<a id="org3e46121"></a>

### PDF-tools

Majorly increases performance when viewing pdfs as a file

```emacs-lisp
;; (use-package pdf-tools
;;   :ensure t
;;   :init
;;   (pdf-tools-install))
```


<a id="orgc4d7694"></a>

# Stuff to do when loading


<a id="orgf7edea5"></a>

## Environment Variables

Setting path, email and password variables

```emacs-lisp
(setenv "NOTMUCH_CONFIG" (expand-file-name "~/.config/notmuch-config"))
(setenv "PASSWORD_STORE_DIR" (expand-file-name "~/.local/share/password-store/"))
(add-hook 'after-init-hook
          (lambda ()
            (setenv "PATH" (concat (getenv "PATH") ":/home/jamzattack/.local/bin/"))
            (push "/home/jamzattack/.local/bin/" exec-path)))
```


<a id="orgea15fbb"></a>

# Fixing defaults


<a id="orgab52fd8"></a>

## Miscellaneous stuff


<a id="org6d99efd"></a>

### No more pesky extra files, other basics

```emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
```


<a id="orgfd2ef5d"></a>

### Enable all the features, because what's the point in having less?

```emacs-lisp
(setq disabled-command-function nil)
```


<a id="org9a9ead7"></a>

## Aesthetic stuff


<a id="org8ac5632"></a>

### GUI ugliness

Disable all the wasteful bars

```emacs-lisp
(when (window-system)
  (scroll-bar-mode -1)
  (fringe-mode 1))
(menu-bar-mode -1)
(tool-bar-mode -1)

```


<a id="org63525a8"></a>

### Font and cursor

Use system monospace font, red non-blinking cursor

```emacs-lisp
(set-cursor-color "red")
(blink-cursor-mode -1)
```


<a id="org0da4da7"></a>

## Tabs

Tabs are 4 spaces wide

```emacs-lisp
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
```


<a id="org11fd25e"></a>

## Buffers/input


<a id="orgaf9c312"></a>

### ido-mode

ido-mode is much better than the default for switching buffers and going to files.

```emacs-lisp
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)
```


<a id="org6b97a8b"></a>

### ibuffer

ibuffer is also a lot better than the default (plus it has colours)

```emacs-lisp
(global-set-key (kbd "C-x C-b") 'ibuffer)
```


<a id="org160a618"></a>

# Custom functions


<a id="org6498ad7"></a>

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


<a id="org606359a"></a>

## Go to config file

Visit your config file. Bound to "C-c e" in `Keybindings` section.

```emacs-lisp
(defun config-visit ()
  "Go to your config.org"
  (interactive)
  (find-file "~/.emacs.d/config.org"))
```


<a id="org2699408"></a>

## Ido


<a id="org2e0084f"></a>

### Bookmarks

```emacs-lisp
(defun ido-bookmark-jump ()
  "An ido wrapper for `bookmark-jump'. Designed for interactive
use, so just use `bookmark-jump' in elisp."
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump
   (ido-completing-read "Bookmark: " bookmark-alist)))
```


<a id="org1a5696b"></a>

## Reloading config

Reloads this config file. Bound to "C-c r" in Keybindings section.

```emacs-lisp
(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
```


<a id="org905bccd"></a>

## Programming


<a id="org6ade26b"></a>

### Opening Output

```emacs-lisp
(defun opout ()
  "Opens a pdf file of the same name as the current file"
  (interactive)
  (find-file-other-window (concat
                           (file-name-sans-extension buffer-file-name)
                           ".pdf")))
```


<a id="orgf6d4716"></a>

## Email

```emacs-lisp
(defun mailsync ()
  "Downloads new mail and adds it to the notmuch database"
  (interactive)
  (shell-command "mbsync -a && notmuch new &" "*mailsync*"))
```


<a id="org6ca9fd4"></a>

## WM stuff


<a id="orgd33c5f2"></a>

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


<a id="orge8d3fd4"></a>

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


<a id="org89ff23a"></a>

### Other&#x2026;

1.  Mouse

    ```emacs-lisp
    (defun mousetoggle ()
      "Toggles touchpad on my laptop"
      (interactive)
      (shell-command "mousetoggle")
      (message "touchpad input toggled"))
    ```


<a id="org0ad1c0b"></a>

# Major mode hooks and variables


<a id="org9b8105c"></a>

## Lilypond mode

Use lilypond mode for .ly files (taken from lilypond.org)

```emacs-lisp
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
(setq LilyPond-pdf-command "zathura")
```


<a id="org908ab48"></a>

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


<a id="org0048f58"></a>

## Org Mode

```emacs-lisp
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-src-window-setup 'current-window)
(setq org-src-tab-acts-natively t)
(setq org-ellipsis " ")
```


<a id="org53b93f8"></a>

## M-x compile hooks


<a id="org3d9b192"></a>

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


<a id="org0d208ff"></a>

### C

```emacs-lisp
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "compiler %s" buffer-file-name))))
```


<a id="org319cf4d"></a>

### LiLyPond

```emacs-lisp
(add-hook 'LilyPond-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "lilypond %s" buffer-file-name))))
```


<a id="orgfa7a42e"></a>

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


<a id="org56044be"></a>

# Keybindings


<a id="orgac817a3"></a>

## Interaction with Emacs


<a id="org0d8c3ab"></a>

### ido-bookmark-jump (custom function)

Open a bookmark with the default keybinding `C-x r b`, but with ido

```emacs-lisp
(global-set-key (kbd "C-x r b") 'ido-bookmark-jump)
```


<a id="orgd6a40bf"></a>

### Terminal functionality

Rebinding some useful keys that can't be used in a terminal.

```emacs-lisp
(unless (window-system)
  ;; Comments -- C-x C-;
  (global-set-key (kbd "C-c ;") 'comment-line)
  ;; Indentation -- C-M-\"
  (global-set-key (kbd "C-c \\") 'indent-region))
```


<a id="org9602673"></a>

## Config

```emacs-lisp
(global-set-key (kbd "C-c r") 'config-reload)
```


<a id="orgf8953f8"></a>

## General WM stuff


<a id="org4a49d14"></a>

### System information

Built-in battery function with `s-t b`. Custom notification script with `s-t s-b`.

```emacs-lisp
(global-set-key (kbd "s-t b") 'battery)
(global-set-key (kbd "s-t s-b") 'notibar)
```


<a id="org001d391"></a>

### dmenu scripts

I still have some use for dmenu, despite only using emacs&#x2026; All commands are prefixed with `s-t`

| d | enter commands into dmenu       |
| P | select a pdf to open with emacs |
| V | select a video to open with mpv |
| D | choose what to do with a URL    |

```emacs-lisp
(global-set-key (kbd "s-t d") 'dmenu_recency)
(global-set-key (kbd "s-t P") 'pdf-opener)
(global-set-key (kbd "s-t V") 'video-opener)
(global-set-key (kbd "s-t D") 'dmenuhandler)
```


<a id="orgddb4927"></a>

## Programming/Typesetting

Bind emacs compile to `C-c C-m`. This allows 2 rapid presses of `C-m` or `RET` to skip the prompt.

"opout" is a script to open the output of a file (e.g. TeX, LilyPond).

```emacs-lisp
(global-set-key (kbd "C-c C-m") 'compile)
(global-set-key (kbd "C-c p") 'opout)
```


<a id="orgba8fd86"></a>

## Miscellaneous


<a id="orgea38b0c"></a>

### Line numbers

```emacs-lisp
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
```


<a id="org5c0d89e"></a>

### Spelling correction

```emacs-lisp
(global-set-key (kbd "C-c s") 'flyspell-mode)
```


<a id="org7c4e4a8"></a>

### Line wrap

```emacs-lisp
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
```


<a id="orgb01b340"></a>

### Mouse

```emacs-lisp
(global-set-key (kbd "s-t \\") 'mousetoggle)
```


<a id="org4f79fab"></a>

# Mode-line

Just some basic extra stuff in the mode-line. I don't want anything fancy.

```emacs-lisp
(column-number-mode t)
(display-time-mode t)
(setq display-time-24hr-format 1)
```


<a id="org4ad40cb"></a>

# Email

email settings

```emacs-lisp
(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
```