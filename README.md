- [Packages](#org164a763)
  - [Startup](#org09c21f2)
    - [Repositories and initialisation](#org67ce976)
    - [Use-package](#org364ecad)
  - [Installed Packages](#orge31414e)
    - [Pinentry](#org38167d8)
    - [Org](#org8de6792)
    - [Window management](#orgd1e7e06)
    - [Completion](#org3f6b486)
    - ["Applications"](#orgbd2f16c)
    - [Appearance](#org8aba918)
    - [god-mode](#org74ddff2)
    - [Quality of life](#orga6806c4)
    - [Not really useful](#org60430f9)
    - [To be confirmed](#org9fb0eb0)
    - [PDF-tools](#orgecf4e21)
- [Stuff to do when loading](#org3d13a0f)
  - [Environment Variables](#orgfde04f7)
- [Fixing defaults](#orgfba586c)
  - [Miscellaneous stuff](#orge112d71)
    - [No more pesky extra files, other basics](#orgdc1836f)
    - [Enable all the features, because what's the point in having less?](#org867f6cb)
  - [Aesthetic stuff](#orgb0deae0)
    - [GUI ugliness](#orga4c33fb)
    - [Font and cursor](#orge412ccd)
  - [Tabs](#org93304c7)
  - [Buffers/input](#org06695ec)
    - [ido-mode](#org9f60bbc)
    - [ibuffer](#org69bf243)
  - [desktop-save](#org06fb42e)
- [Custom functions](#orgc18e894)
  - [Resizing windows](#orgff43d86)
  - [Go to config file](#orge080d37)
  - [Ido](#orged80714)
    - [Bookmarks](#org24aac9b)
  - [Reloading config](#org589255c)
  - [Programming](#orge76610e)
    - [Opening Output](#org680d306)
  - [Email](#orgd1579ba)
  - [WM stuff](#orgf0f9b2f)
    - [Notification bar replacement](#org5578466)
    - [dmenu](#orga348815)
    - [Other&#x2026;](#org0c60528)
- [Major mode hooks and variables](#orga098f2f)
  - [Lilypond mode](#org8eb0088)
  - [Electric pairs](#orgfdf434e)
  - [Org Mode](#orga499846)
  - [M-x compile hooks](#org18668c0)
    - [Groff](#org94fabff)
    - [C](#orgff94509)
    - [LiLyPond](#org41b9e54)
    - [LaTeX](#orgf847f4f)
- [Keybindings](#org07b5cf7)
  - [Interaction with Emacs](#org11c71c3)
    - [ido-bookmark-jump (custom function)](#orgac82c6c)
    - [Terminal functionality](#orgd479f3b)
  - [Config](#orge6f18aa)
  - [General WM stuff](#org3948e92)
    - [System information](#orgcafa881)
    - [dmenu scripts](#org7b74207)
  - [Programming/Typesetting](#org4de75ad)
  - [Miscellaneous](#org4e0d73d)
    - [Line numbers](#org7cd0e59)
    - [Spelling correction](#org3d8b018)
    - [Line wrap](#org6c22841)
    - [Mouse](#orgb07bb72)
- [Mode-line](#org182ce6c)
- [Email](#org3125c03)


<a id="org164a763"></a>

# Packages


<a id="org09c21f2"></a>

## Startup


<a id="org67ce976"></a>

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


<a id="org364ecad"></a>

### Use-package

Install use-package if not installed

```emacs-lisp
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
```


<a id="orge31414e"></a>

## Installed Packages


<a id="org38167d8"></a>

### Pinentry

This package lets emacs be used for gpg authentication

```emacs-lisp
(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
```


<a id="org8de6792"></a>

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


<a id="orgd1e7e06"></a>

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


<a id="org3f6b486"></a>

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


<a id="orgbd2f16c"></a>

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


<a id="org8aba918"></a>

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


<a id="org74ddff2"></a>

### god-mode

```emacs-lisp
 (use-package god-mode
   :ensure t
   :config
   (setq god-exempt-major-modes nil
         god-exempt-predicates nil)
(global-set-key (kbd "<left>") 'god-local-mode)
   (global-set-key (kbd "<right>") 'god-local-mode)
   (god-mode))
```


<a id="orga6806c4"></a>

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


<a id="org60430f9"></a>

### Not really useful

1.  Lorem Ipsum

    A 'Lorem ipsum' generator
    
    ```emacs-lisp
    (use-package lorem-ipsum
      :ensure t)
    ```


<a id="org9fb0eb0"></a>

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


<a id="orgecf4e21"></a>

### PDF-tools

Majorly increases performance when viewing pdfs as a file

```emacs-lisp
;; (use-package pdf-tools
;;   :ensure t
;;   :init
;;   (pdf-tools-install))
```


<a id="org3d13a0f"></a>

# Stuff to do when loading


<a id="orgfde04f7"></a>

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


<a id="orgfba586c"></a>

# Fixing defaults


<a id="orge112d71"></a>

## Miscellaneous stuff


<a id="orgdc1836f"></a>

### No more pesky extra files, other basics

```emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
```


<a id="org867f6cb"></a>

### Enable all the features, because what's the point in having less?

```emacs-lisp
(setq disabled-command-function nil)
```


<a id="orgb0deae0"></a>

## Aesthetic stuff


<a id="orga4c33fb"></a>

### GUI ugliness

Disable all the wasteful bars

```emacs-lisp
(when (window-system)
  (scroll-bar-mode -1)
  (fringe-mode 1))
(menu-bar-mode -1)
(tool-bar-mode -1)

```


<a id="orge412ccd"></a>

### Font and cursor

Use system monospace font, red non-blinking cursor

```emacs-lisp
(set-cursor-color "red")
(blink-cursor-mode -1)
```


<a id="org93304c7"></a>

## Tabs

Tabs are 4 spaces wide

```emacs-lisp
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
```


<a id="org06695ec"></a>

## Buffers/input


<a id="org9f60bbc"></a>

### ido-mode

ido-mode is much better than the default for switching buffers and going to files.

```emacs-lisp
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)
```


<a id="org69bf243"></a>

### ibuffer

ibuffer is also a lot better than the default (plus it has colours)

```emacs-lisp
(global-set-key (kbd "C-x C-b") 'ibuffer)
```


<a id="org06fb42e"></a>

## desktop-save

```emacs-lisp
(desktop-save-mode t)
```


<a id="orgc18e894"></a>

# Custom functions


<a id="orgff43d86"></a>

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


<a id="orge080d37"></a>

## Go to config file

Visit your config file. Bound to "C-c e" in `Keybindings` section.

```emacs-lisp
(defun config-visit ()
  "Go to your config.org"
  (interactive)
  (find-file "~/.emacs.d/config.org"))
```


<a id="orged80714"></a>

## Ido


<a id="org24aac9b"></a>

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


<a id="org589255c"></a>

## Reloading config

Reloads this config file. Bound to "C-c r" in Keybindings section.

```emacs-lisp
(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
```


<a id="orge76610e"></a>

## Programming


<a id="org680d306"></a>

### Opening Output

```emacs-lisp
(defun opout ()
  "Opens a pdf file of the same name as the current file"
  (interactive)
  (find-file-other-window (concat
                           (file-name-sans-extension buffer-file-name)
                           ".pdf")))
```


<a id="orgd1579ba"></a>

## Email

```emacs-lisp
(defun mailsync ()
  "Downloads new mail and adds it to the notmuch database"
  (interactive)
  (shell-command "mbsync -a && notmuch new &" "*mailsync*"))
```


<a id="orgf0f9b2f"></a>

## WM stuff


<a id="org5578466"></a>

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


<a id="orga348815"></a>

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


<a id="org0c60528"></a>

### Other&#x2026;

1.  Mouse

    ```emacs-lisp
    (defun mousetoggle ()
      "Toggles touchpad on my laptop"
      (interactive)
      (shell-command "mousetoggle")
      (message "touchpad input toggled"))
    ```


<a id="orga098f2f"></a>

# Major mode hooks and variables


<a id="org8eb0088"></a>

## Lilypond mode

Use lilypond mode for .ly files (taken from lilypond.org)

```emacs-lisp
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
(setq LilyPond-pdf-command "zathura")
```


<a id="orgfdf434e"></a>

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


<a id="orga499846"></a>

## Org Mode

```emacs-lisp
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-src-window-setup 'current-window)
(setq org-src-tab-acts-natively t)
(setq org-ellipsis " ")
```


<a id="org18668c0"></a>

## M-x compile hooks


<a id="org94fabff"></a>

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


<a id="orgff94509"></a>

### C

```emacs-lisp
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "compiler %s" buffer-file-name))))
```


<a id="org41b9e54"></a>

### LiLyPond

```emacs-lisp
(add-hook 'LilyPond-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "lilypond %s" buffer-file-name))))
```


<a id="orgf847f4f"></a>

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


<a id="org07b5cf7"></a>

# Keybindings


<a id="org11c71c3"></a>

## Interaction with Emacs


<a id="orgac82c6c"></a>

### ido-bookmark-jump (custom function)

Open a bookmark with the default keybinding `C-x r b`, but with ido

```emacs-lisp
(global-set-key (kbd "C-x r b") 'ido-bookmark-jump)
```


<a id="orgd479f3b"></a>

### Terminal functionality

Rebinding some useful keys that can't be used in a terminal.

```emacs-lisp
(unless (window-system)
  ;; Comments -- C-x C-;
  (global-set-key (kbd "C-c ;") 'comment-line)
  ;; Indentation -- C-M-\"
  (global-set-key (kbd "C-c \\") 'indent-region))
```


<a id="orge6f18aa"></a>

## Config

```emacs-lisp
(global-set-key (kbd "C-c r") 'config-reload)
```


<a id="org3948e92"></a>

## General WM stuff


<a id="orgcafa881"></a>

### System information

Built-in battery function with `s-t b`. Custom notification script with `s-t s-b`.

```emacs-lisp
(global-set-key (kbd "s-t b") 'battery)
(global-set-key (kbd "s-t s-b") 'notibar)
```


<a id="org7b74207"></a>

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


<a id="org4de75ad"></a>

## Programming/Typesetting

Bind emacs compile to `C-c C-m`. This allows 2 rapid presses of `C-m` or `RET` to skip the prompt.

"opout" is a script to open the output of a file (e.g. TeX, LilyPond).

```emacs-lisp
(global-set-key (kbd "C-c C-m") 'compile)
(global-set-key (kbd "C-c p") 'opout)
```


<a id="org4e0d73d"></a>

## Miscellaneous


<a id="org7cd0e59"></a>

### Line numbers

```emacs-lisp
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
```


<a id="org3d8b018"></a>

### Spelling correction

```emacs-lisp
(global-set-key (kbd "C-c s") 'flyspell-mode)
```


<a id="org6c22841"></a>

### Line wrap

```emacs-lisp
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
```


<a id="orgb07bb72"></a>

### Mouse

```emacs-lisp
(global-set-key (kbd "s-t \\") 'mousetoggle)
```


<a id="org182ce6c"></a>

# Mode-line

Just some basic extra stuff in the mode-line. I don't want anything fancy.

```emacs-lisp
(column-number-mode t)
(display-time-mode t)
(setq display-time-24hr-format 1)
```


<a id="org3125c03"></a>

# Email

email settings

```emacs-lisp
(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
```