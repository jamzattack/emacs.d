- [Packages](#org8caf8c6)
  - [Startup](#orgde0d684)
    - [Repositories and initialisation](#org4db8777)
    - [Use-package](#org3a5922f)
  - [Installed Packages](#org42e32fc)
    - [Pinentry](#orge0c44a2)
    - [Org](#orge5cec7f)
    - [Window management](#orgc8bf708)
    - [Completion](#org7c5177d)
    - ["Applications"](#org23f2617)
    - [Appearance](#orgb41e91b)
    - [god-mode](#org4d3d905)
    - [Quality of life](#orgd44b6d7)
    - [Not really useful](#orga7ff32c)
    - [To be confirmed](#orgb8213c0)
    - [PDF-tools](#org4758953)
- [Stuff to do when loading](#orgc1be37e)
  - [Environment Variables](#org0085533)
- [Fixing defaults](#org99337ea)
  - [Miscellaneous stuff](#orge0e1e01)
    - [No more pesky extra files, other basics](#org0b5d4a0)
    - [Enable all the features, because what's the point in having less?](#orgf4f81c6)
  - [Aesthetic stuff](#org35d6a71)
    - [GUI ugliness](#org023ef1c)
    - [Font and cursor](#orge950693)
  - [Tabs](#org078ebad)
  - [Buffers/input](#org85603e9)
    - [ido-mode](#org369e71e)
    - [ibuffer](#org4dd652e)
  - [desktop-save](#org6d125dd)
- [Custom functions](#org3f51421)
  - [Resizing windows](#org52609a1)
  - [Go to config file](#org20a619a)
  - [Ido](#org8c5df2c)
    - [Bookmarks](#orgc3d84b3)
  - [Reloading config](#org83ba654)
  - [Programming](#orgc5a181c)
    - [Opening Output](#orge50e8b8)
  - [Email](#orgac8bd83)
  - [WM stuff](#org57f8d72)
    - [Notification bar replacement](#org8e316aa)
    - [dmenu](#orge973101)
    - [Other&#x2026;](#org21b5485)
  - [Other](#org998c244)
    - [Xah Lee form feed](#org567a42e)
- [Major mode hooks and variables](#org8924505)
  - [Lilypond mode](#orgb977e82)
  - [Electric pairs](#org26e3e1f)
  - [Org Mode](#org3e7c35e)
  - [M-x compile hooks](#org696576d)
    - [Groff](#org48b71ac)
    - [C](#org3b397ea)
    - [LiLyPond](#orga674eb0)
    - [LaTeX](#org79d6fad)
- [Keybindings](#org55f83cb)
  - [Interaction with Emacs](#orgff77839)
    - [ido-bookmark-jump (custom function)](#org0bf852c)
    - [Terminal functionality](#org6ef3b40)
  - [Config](#orgf1d60f2)
  - [General WM stuff](#org5f652e9)
    - [System information](#org667ee29)
    - [dmenu scripts](#org4de3b5c)
  - [Programming/Typesetting](#org95f249b)
  - [Miscellaneous](#org6dee316)
    - [Line numbers](#org6d5dcf7)
    - [Spelling correction](#org8bbcdaa)
    - [Line wrap](#org13a9bed)
    - [Mouse](#orgbd9361e)
- [Mode-line](#org4228a8d)
- [Email](#org6252db9)


<a id="org8caf8c6"></a>

# Packages


<a id="orgde0d684"></a>

## Startup


<a id="org4db8777"></a>

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


<a id="org3a5922f"></a>

### Use-package

Install use-package if not installed

```emacs-lisp
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
```


<a id="org42e32fc"></a>

## Installed Packages


<a id="orge0c44a2"></a>

### Pinentry

This package lets emacs be used for gpg authentication

```emacs-lisp
(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
```


<a id="orge5cec7f"></a>

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


<a id="orgc8bf708"></a>

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


<a id="org7c5177d"></a>

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


<a id="org23f2617"></a>

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


<a id="orgb41e91b"></a>

### Appearance

1.  Theme

    1.  moe-theme
    
        ```emacs-lisp
        (use-package moe-theme
          :ensure t
          :config
          (require 'moe-theme-switcher)
          (setq moe-theme-highlight-buffer-id nil)
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


<a id="org4d3d905"></a>

### god-mode

```emacs-lisp
(use-package god-mode
  :ensure t
  :bind
  (("<pause>" . 'god-mode-all)
   ("<right>" . 'god-mode-all)
   :map god-local-mode-map
   ("." . 'repeat))
  :init
  (add-hook 'god-mode-enabled-hook
            (lambda ()
              (moe-theme-set-color 'purple)))
  (add-hook 'god-mode-disabled-hook
            (lambda ()
              (moe-theme-set-color 'green)))
  :config
  (setq god-exempt-major-modes nil
        god-exempt-predicates nil)
  (god-mode))
```


<a id="orgd44b6d7"></a>

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


<a id="orga7ff32c"></a>

### Not really useful

1.  Lorem Ipsum

    A 'Lorem ipsum' generator
    
    ```emacs-lisp
    (use-package lorem-ipsum
      :ensure t)
    ```


<a id="orgb8213c0"></a>

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


<a id="org4758953"></a>

### PDF-tools

Majorly increases performance when viewing pdfs as a file

```emacs-lisp
(use-package pdf-tools
  :ensure t)
```


<a id="orgc1be37e"></a>

# Stuff to do when loading


<a id="org0085533"></a>

## Environment Variables

Setting path, email and password variables

```emacs-lisp
(setenv "NOTMUCH_CONFIG"
        (expand-file-name "~/.config/notmuch-config"))
(setenv "PASSWORD_STORE_DIR"
        (expand-file-name "~/.local/share/password-store/"))
```


<a id="org99337ea"></a>

# Fixing defaults


<a id="orge0e1e01"></a>

## Miscellaneous stuff


<a id="org0b5d4a0"></a>

### No more pesky extra files, other basics

```emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
```


<a id="orgf4f81c6"></a>

### Enable all the features, because what's the point in having less?

```emacs-lisp
(setq disabled-command-function nil)
```


<a id="org35d6a71"></a>

## Aesthetic stuff


<a id="org023ef1c"></a>

### GUI ugliness

Disable all the wasteful bars

```emacs-lisp
(when (window-system)
  (scroll-bar-mode -1)
  (fringe-mode 1))
(menu-bar-mode -1)
(tool-bar-mode -1)

```


<a id="orge950693"></a>

### Font and cursor

Use system monospace font, red non-blinking cursor

```emacs-lisp
(set-cursor-color "red")
(blink-cursor-mode -1)
```


<a id="org078ebad"></a>

## Tabs

Tabs are 4 spaces wide

```emacs-lisp
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
```


<a id="org85603e9"></a>

## Buffers/input


<a id="org369e71e"></a>

### ido-mode

ido-mode is much better than the default for switching buffers and going to files.

```emacs-lisp
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)
```


<a id="org4dd652e"></a>

### ibuffer

ibuffer is also a lot better than the default (plus it has colours)

```emacs-lisp
(global-set-key (kbd "C-x C-b") 'ibuffer)
```


<a id="org6d125dd"></a>

## desktop-save

```emacs-lisp
(desktop-save-mode t)
```


<a id="org3f51421"></a>

# Custom functions


<a id="org52609a1"></a>

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


<a id="org20a619a"></a>

## Go to config file

Visit your config file. Bound to "C-c e" in `Keybindings` section.

```emacs-lisp
(defun config-visit ()
  "Go to your config.org"
  (interactive)
  (find-file "~/.emacs.d/config.org"))
```


<a id="org8c5df2c"></a>

## Ido


<a id="orgc3d84b3"></a>

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


<a id="org83ba654"></a>

## Reloading config

Reloads this config file. Bound to "C-c r" in Keybindings section.

```emacs-lisp
(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
```


<a id="orgc5a181c"></a>

## Programming


<a id="orge50e8b8"></a>

### Opening Output

```emacs-lisp
(defun opout ()
  "Opens a pdf file of the same name as the current file"
  (interactive)
  (find-file-other-window (concat
                           (file-name-sans-extension buffer-file-name)
                           ".pdf")))
```


<a id="orgac8bd83"></a>

## Email

```emacs-lisp
(defun mailsync ()
  "Downloads new mail and adds it to the notmuch database"
  (interactive)
  (shell-command "mbsync -a && notmuch new &" "*mailsync*"))
```


<a id="org57f8d72"></a>

## WM stuff


<a id="org8e316aa"></a>

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


<a id="orge973101"></a>

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


<a id="org21b5485"></a>

### Other&#x2026;

1.  Mouse

    ```emacs-lisp
    (defun mousetoggle ()
      "Toggles touchpad on my laptop"
      (interactive)
      (shell-command "mousetoggle")
      (message "touchpad input toggled"))
    ```


<a id="org998c244"></a>

## Other


<a id="org567a42e"></a>

### Xah Lee form feed

```emacs-lisp
(defun xah-show-formfeed-as-line ()
  "Display the formfeed ^L char as line.
URL `http://ergoemacs.org/emacs/emacs_form_feed_section_paging.html'
Version 2018-08-30"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))
```


<a id="org8924505"></a>

# Major mode hooks and variables


<a id="orgb977e82"></a>

## Lilypond mode

Use lilypond mode for .ly files (taken from lilypond.org)

```emacs-lisp
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
(setq LilyPond-pdf-command "zathura")
```


<a id="org26e3e1f"></a>

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


<a id="org3e7c35e"></a>

## Org Mode

```emacs-lisp
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-src-window-setup 'current-window)
(setq org-src-tab-acts-natively t)
(setq org-ellipsis " ")
```


<a id="org696576d"></a>

## M-x compile hooks


<a id="org48b71ac"></a>

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


<a id="org3b397ea"></a>

### C

```emacs-lisp
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "compiler %s" buffer-file-name))))
```


<a id="orga674eb0"></a>

### LiLyPond

```emacs-lisp
(add-hook 'LilyPond-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "lilypond %s" buffer-file-name))))
```


<a id="org79d6fad"></a>

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


<a id="org55f83cb"></a>

# Keybindings


<a id="orgff77839"></a>

## Interaction with Emacs


<a id="org0bf852c"></a>

### ido-bookmark-jump (custom function)

Open a bookmark with the default keybinding `C-x r b`, but with ido

```emacs-lisp
(global-set-key (kbd "C-x r b") 'ido-bookmark-jump)
```


<a id="org6ef3b40"></a>

### Terminal functionality

Rebinding some useful keys that can't be used in a terminal.

```emacs-lisp
(unless (window-system)
  ;; Comments -- C-x C-;
  (global-set-key (kbd "C-c ;") 'comment-line)
  ;; Indentation -- C-M-\"
  (global-set-key (kbd "C-c \\") 'indent-region))
```


<a id="orgf1d60f2"></a>

## Config

```emacs-lisp
(global-set-key (kbd "C-c r") 'config-reload)
```


<a id="org5f652e9"></a>

## General WM stuff


<a id="org667ee29"></a>

### System information

Built-in battery function with `s-t b`. Custom notification script with `s-t s-b`.

```emacs-lisp
(global-set-key (kbd "s-t b") 'battery)
(global-set-key (kbd "s-t s-b") 'notibar)
```


<a id="org4de3b5c"></a>

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


<a id="org95f249b"></a>

## Programming/Typesetting

Bind emacs compile to `C-c C-m`. This allows 2 rapid presses of `C-m` or `RET` to skip the prompt.

"opout" is a script to open the output of a file (e.g. TeX, LilyPond).

```emacs-lisp
(global-set-key (kbd "C-c C-m") 'compile)
(global-set-key (kbd "C-c p") 'opout)
```


<a id="org6dee316"></a>

## Miscellaneous


<a id="org6d5dcf7"></a>

### Line numbers

```emacs-lisp
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
```


<a id="org8bbcdaa"></a>

### Spelling correction

```emacs-lisp
(global-set-key (kbd "C-c s") 'flyspell-mode)
```


<a id="org13a9bed"></a>

### Line wrap

```emacs-lisp
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
```


<a id="orgbd9361e"></a>

### Mouse

```emacs-lisp
(global-set-key (kbd "s-t \\") 'mousetoggle)
```


<a id="org4228a8d"></a>

# Mode-line

Just some basic extra stuff in the mode-line. I don't want anything fancy.

```emacs-lisp
(column-number-mode t)
(display-time-mode t)
(setq display-time-24hr-format 1)
```


<a id="org6252db9"></a>

# Email

email settings

```emacs-lisp
(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
```