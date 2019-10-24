- [Packages](#org601d93b)
  - [Startup](#org40f08b9)
    - [Repositories and initialisation](#org14f743c)
    - [Use-package](#orgfb09fad)
  - [Installed Packages](#org249163a)
    - [Pinentry](#orgace4365)
    - [Org](#orgd22786d)
    - [Window management](#orgc8538ec)
    - [Completion](#orgc6d79db)
    - ["Applications"](#org07c96e1)
    - [Appearance](#org9a578b1)
    - [god-mode](#org89e30a2)
    - [Quality of life](#org07505a2)
    - [Not really useful](#org54fc236)
    - [To be confirmed](#orgfb6c648)
    - [PDF-tools](#org00c732f)
- [Stuff to do when loading](#org5ce17b6)
  - [Environment Variables](#orgf216447)
- [Fixing defaults](#orgf901a42)
  - [Miscellaneous stuff](#org6065595)
    - [No more pesky extra files, other basics](#orgd45ed50)
    - [Enable all the features, because what's the point in having less?](#orgc83d9ae)
  - [Aesthetic stuff](#orgb3d7671)
    - [GUI ugliness](#org78e2a5b)
    - [Font and cursor](#orgc6b1659)
  - [Tabs](#orgf29a6cb)
  - [Buffers/input](#orgf44209d)
    - [ido-mode](#org1df3a39)
    - [ibuffer](#org394fe9b)
  - [desktop-save](#org244b17e)
- [Custom functions](#org825eeaa)
  - [Resizing windows](#org68dfad7)
  - [Go to config file](#org509453f)
  - [Ido](#org2be9116)
    - [Bookmarks](#orgf20b3f5)
  - [Reloading config](#orga710085)
  - [Programming](#org23bc956)
    - [Opening Output](#org2ce3883)
  - [Email](#orgb967afd)
  - [WM stuff](#org2049944)
    - [Notification bar replacement](#org211aba2)
    - [dmenu](#orgd75a08b)
    - [Other&#x2026;](#org304bfaf)
- [Major mode hooks and variables](#orgdff939f)
  - [Lilypond mode](#org0bfe126)
  - [Electric pairs](#orge0bd8e6)
  - [Org Mode](#orge26d1ec)
  - [M-x compile hooks](#org9694d01)
    - [Groff](#orgab9d80b)
    - [C](#orgdeaa4ac)
    - [LiLyPond](#orgaa7bd09)
    - [LaTeX](#orgd7c0558)
- [Keybindings](#org2347ee4)
  - [Interaction with Emacs](#orgabd655a)
    - [ido-bookmark-jump (custom function)](#orgaa2193e)
    - [Terminal functionality](#orgdd1d158)
  - [Config](#org387df35)
  - [General WM stuff](#orgbdbaa98)
    - [System information](#orgf396b61)
    - [dmenu scripts](#org4009176)
  - [Programming/Typesetting](#orgb4dbadd)
  - [Miscellaneous](#org266bce4)
    - [Line numbers](#org6b38ab0)
    - [Spelling correction](#org5550f7f)
    - [Line wrap](#org8437f21)
    - [Mouse](#org6902eae)
- [Mode-line](#orgc527321)
- [Email](#orgcad4fbe)


<a id="org601d93b"></a>

# Packages


<a id="org40f08b9"></a>

## Startup


<a id="org14f743c"></a>

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


<a id="orgfb09fad"></a>

### Use-package

Install use-package if not installed

```emacs-lisp
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
```


<a id="org249163a"></a>

## Installed Packages


<a id="orgace4365"></a>

### Pinentry

This package lets emacs be used for gpg authentication

```emacs-lisp
(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
```


<a id="orgd22786d"></a>

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


<a id="orgc8538ec"></a>

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


<a id="orgc6d79db"></a>

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


<a id="org07c96e1"></a>

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


<a id="org9a578b1"></a>

### Appearance

1.  Theme

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

2.  Rainbow-mode

    This package highlights hex colours (also install xterm-color to use in a terminal emulator)
    
    ```emacs-lisp
    (use-package rainbow-mode
      :ensure t
      :config
      (global-set-key (kbd "C-c h") 'rainbow-mode))
    ```

3.  xterm-color

    Allows a terminal emulator to use 256 colors
    
    ```emacs-lisp
    (use-package xterm-color
      :ensure t)
    ```

4.  Rainbow-delimiters

    Minor mode that highlights parentheses well
    
    ```emacs-lisp
    (use-package rainbow-delimiters
      :ensure t
      :init
      (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
    ```


<a id="org89e30a2"></a>

### god-mode

```emacs-lisp
(use-package god-mode
  :ensure t
  :config
  (global-set-key (kbd "<left>") 'god-local-mode)
  (global-set-key (kbd "<right>") 'god-local-mode)
  (god-mode))
```


<a id="org07505a2"></a>

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


<a id="org54fc236"></a>

### Not really useful

1.  Lorem Ipsum

    A 'Lorem ipsum' generator
    
    ```emacs-lisp
    (use-package lorem-ipsum
      :ensure t)
    ```


<a id="orgfb6c648"></a>

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


<a id="org00c732f"></a>

### PDF-tools

Majorly increases performance when viewing pdfs as a file

```emacs-lisp
;; (use-package pdf-tools
;;   :ensure t
;;   :init
;;   (pdf-tools-install))
```


<a id="org5ce17b6"></a>

# Stuff to do when loading


<a id="orgf216447"></a>

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


<a id="orgf901a42"></a>

# Fixing defaults


<a id="org6065595"></a>

## Miscellaneous stuff


<a id="orgd45ed50"></a>

### No more pesky extra files, other basics

```emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
```


<a id="orgc83d9ae"></a>

### Enable all the features, because what's the point in having less?

```emacs-lisp
(setq disabled-command-function nil)
```


<a id="orgb3d7671"></a>

## Aesthetic stuff


<a id="org78e2a5b"></a>

### GUI ugliness

Disable all the wasteful bars

```emacs-lisp
(when (window-system)
  (scroll-bar-mode -1)
  (fringe-mode 1))
(menu-bar-mode -1)
(tool-bar-mode -1)

```


<a id="orgc6b1659"></a>

### Font and cursor

Use system monospace font, red non-blinking cursor

```emacs-lisp
(set-cursor-color "red")
(blink-cursor-mode -1)
```


<a id="orgf29a6cb"></a>

## Tabs

Tabs are 4 spaces wide

```emacs-lisp
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
```


<a id="orgf44209d"></a>

## Buffers/input


<a id="org1df3a39"></a>

### ido-mode

ido-mode is much better than the default for switching buffers and going to files.

```emacs-lisp
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)
```


<a id="org394fe9b"></a>

### ibuffer

ibuffer is also a lot better than the default (plus it has colours)

```emacs-lisp
(global-set-key (kbd "C-x C-b") 'ibuffer)
```


<a id="org244b17e"></a>

## desktop-save

```emacs-lisp
(desktop-save-mode t)
```


<a id="org825eeaa"></a>

# Custom functions


<a id="org68dfad7"></a>

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


<a id="org509453f"></a>

## Go to config file

Visit your config file. Bound to "C-c e" in `Keybindings` section.

```emacs-lisp
(defun config-visit ()
  "Go to your config.org"
  (interactive)
  (find-file "~/.emacs.d/config.org"))
```


<a id="org2be9116"></a>

## Ido


<a id="orgf20b3f5"></a>

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


<a id="orga710085"></a>

## Reloading config

Reloads this config file. Bound to "C-c r" in Keybindings section.

```emacs-lisp
(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
```


<a id="org23bc956"></a>

## Programming


<a id="org2ce3883"></a>

### Opening Output

```emacs-lisp
(defun opout ()
  "Opens a pdf file of the same name as the current file"
  (interactive)
  (find-file-other-window (concat
                           (file-name-sans-extension buffer-file-name)
                           ".pdf")))
```


<a id="orgb967afd"></a>

## Email

```emacs-lisp
(defun mailsync ()
  "Downloads new mail and adds it to the notmuch database"
  (interactive)
  (shell-command "mbsync -a && notmuch new &" "*mailsync*"))
```


<a id="org2049944"></a>

## WM stuff


<a id="org211aba2"></a>

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


<a id="orgd75a08b"></a>

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


<a id="org304bfaf"></a>

### Other&#x2026;

1.  Mouse

    ```emacs-lisp
    (defun mousetoggle ()
      "Toggles touchpad on my laptop"
      (interactive)
      (shell-command "mousetoggle")
      (message "touchpad input toggled"))
    ```


<a id="orgdff939f"></a>

# Major mode hooks and variables


<a id="org0bfe126"></a>

## Lilypond mode

Use lilypond mode for .ly files (taken from lilypond.org)

```emacs-lisp
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
(setq LilyPond-pdf-command "zathura")
```


<a id="orge0bd8e6"></a>

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


<a id="orge26d1ec"></a>

## Org Mode

```emacs-lisp
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-src-window-setup 'current-window)
(setq org-src-tab-acts-natively t)
(setq org-ellipsis " ")
```


<a id="org9694d01"></a>

## M-x compile hooks


<a id="orgab9d80b"></a>

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


<a id="orgdeaa4ac"></a>

### C

```emacs-lisp
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "compiler %s" buffer-file-name))))
```


<a id="orgaa7bd09"></a>

### LiLyPond

```emacs-lisp
(add-hook 'LilyPond-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "lilypond %s" buffer-file-name))))
```


<a id="orgd7c0558"></a>

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


<a id="org2347ee4"></a>

# Keybindings


<a id="orgabd655a"></a>

## Interaction with Emacs


<a id="orgaa2193e"></a>

### ido-bookmark-jump (custom function)

Open a bookmark with the default keybinding `C-x r b`, but with ido

```emacs-lisp
(global-set-key (kbd "C-x r b") 'ido-bookmark-jump)
```


<a id="orgdd1d158"></a>

### Terminal functionality

Rebinding some useful keys that can't be used in a terminal.

```emacs-lisp
(unless (window-system)
  ;; Comments -- C-x C-;
  (global-set-key (kbd "C-c ;") 'comment-line)
  ;; Indentation -- C-M-\"
  (global-set-key (kbd "C-c \\") 'indent-region))
```


<a id="org387df35"></a>

## Config

```emacs-lisp
(global-set-key (kbd "C-c r") 'config-reload)
```


<a id="orgbdbaa98"></a>

## General WM stuff


<a id="orgf396b61"></a>

### System information

Built-in battery function with `s-t b`. Custom notification script with `s-t s-b`.

```emacs-lisp
(global-set-key (kbd "s-t b") 'battery)
(global-set-key (kbd "s-t s-b") 'notibar)
```


<a id="org4009176"></a>

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


<a id="orgb4dbadd"></a>

## Programming/Typesetting

Bind emacs compile to `C-c C-m`. This allows 2 rapid presses of `C-m` or `RET` to skip the prompt.

"opout" is a script to open the output of a file (e.g. TeX, LilyPond).

```emacs-lisp
(global-set-key (kbd "C-c C-m") 'compile)
(global-set-key (kbd "C-c p") 'opout)
```


<a id="org266bce4"></a>

## Miscellaneous


<a id="org6b38ab0"></a>

### Line numbers

```emacs-lisp
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
```


<a id="org5550f7f"></a>

### Spelling correction

```emacs-lisp
(global-set-key (kbd "C-c s") 'flyspell-mode)
```


<a id="org8437f21"></a>

### Line wrap

```emacs-lisp
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
```


<a id="org6902eae"></a>

### Mouse

```emacs-lisp
(global-set-key (kbd "s-t \\") 'mousetoggle)
```


<a id="orgc527321"></a>

# Mode-line

Just some basic extra stuff in the mode-line. I don't want anything fancy.

```emacs-lisp
(column-number-mode t)
(display-time-mode t)
(setq display-time-24hr-format 1)
```


<a id="orgcad4fbe"></a>

# Email

email settings

```emacs-lisp
(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
```