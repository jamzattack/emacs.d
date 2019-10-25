- [Packages](#org436cd12)
  - [Startup](#orgf35d3f4)
    - [Repositories and initialisation](#orgf4bb89c)
    - [Use-package](#org295f29e)
  - [Installed Packages](#orgef55dac)
    - [Pinentry](#org2f158a3)
    - [Org](#orgdf655b7)
    - [Window management](#org625c885)
    - [Completion](#org43d7e54)
    - ["Applications"](#orgfa8b3c1)
    - [Appearance](#orge782503)
    - [god-mode](#org1590717)
    - [Quality of life](#org9850a3b)
    - [Not really useful](#org8b14070)
    - [To be confirmed](#org348f175)
    - [PDF-tools](#orga5633fb)
- [Stuff to do when loading](#orgb89d865)
  - [Environment Variables](#org3973c6b)
- [Fixing defaults](#orgddaebf9)
  - [Miscellaneous stuff](#org2bb66c1)
    - [No more pesky extra files, other basics](#org990a511)
    - [Enable all the features, because what's the point in having less?](#org6f583ad)
  - [Aesthetic stuff](#orgd937459)
    - [GUI ugliness](#orgd44a03e)
    - [Font and cursor](#orgeae70b1)
  - [Tabs](#orgcd75e2e)
  - [Buffers/input](#orgd636cba)
    - [ido-mode](#org9714954)
    - [ibuffer](#org4d01fcc)
  - [desktop-save](#org4e967b9)
- [Custom functions](#orgc3c6cd1)
  - [Resizing windows](#org3733114)
  - [Go to config file](#org392ba23)
  - [Ido](#orgc25c020)
    - [Bookmarks](#org32ef667)
  - [Reloading config](#org7414787)
  - [Programming](#org24b99e4)
    - [Opening Output](#org83158d0)
  - [Email](#org3a4e7d3)
  - [WM stuff](#org2ea532a)
    - [Notification bar replacement](#orgd6f5f11)
    - [dmenu](#org8f6bcff)
    - [Other&#x2026;](#org5bf6cbc)
  - [Other](#org6cb7538)
    - [Xah Lee form feed](#org3af278f)
- [Major mode hooks and variables](#org6110fbc)
  - [Lilypond mode](#orgb5a4923)
  - [Electric pairs](#org2aba20a)
  - [Org Mode](#org28130bd)
  - [M-x compile hooks](#org636e753)
    - [Groff](#org6d0975e)
    - [C](#orgb5a471b)
    - [LiLyPond](#org5dfb50e)
    - [LaTeX](#orgb1f9109)
- [Keybindings](#org3790d9d)
  - [Interaction with Emacs](#orgd205d91)
    - [ido-bookmark-jump (custom function)](#org5f887e2)
    - [Terminal functionality](#org7ef38d4)
  - [Config](#org0d997ee)
  - [General WM stuff](#org432e414)
    - [System information](#org500a5ef)
    - [dmenu scripts](#org888e92b)
  - [Programming/Typesetting](#org1d77cfa)
  - [Miscellaneous](#org1061e03)
    - [Line numbers](#orgb16514f)
    - [Spelling correction](#orgb11f273)
    - [Line wrap](#orgd919419)
    - [Mouse](#org8fb16d6)
- [Mode-line](#orgea9fea9)
- [Email](#orgcb7ff30)


<a id="org436cd12"></a>

# Packages


<a id="orgf35d3f4"></a>

## Startup


<a id="orgf4bb89c"></a>

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


<a id="org295f29e"></a>

### Use-package

Install use-package if not installed

```emacs-lisp
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
```


<a id="orgef55dac"></a>

## Installed Packages


<a id="org2f158a3"></a>

### Pinentry

This package lets emacs be used for gpg authentication

```emacs-lisp
(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
```


<a id="orgdf655b7"></a>

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


<a id="org625c885"></a>

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


<a id="org43d7e54"></a>

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


<a id="orgfa8b3c1"></a>

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


<a id="orge782503"></a>

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


<a id="org1590717"></a>

### god-mode

```emacs-lisp
(use-package god-mode
  :ensure t
  :bind
  (("<left>" . 'god-mode-all)
   ("<right>" . 'god-mode-all)
   :map god-local-mode-map
   ("." . 'repeat))
  :init
  (add-hook 'god-mode-enabled-hook (lambda ()
                           (moe-theme-set-color 'purple)))
  (add-hook 'god-mode-disabled-hook (lambda ()
                            (moe-theme-set-color 'green)))
  :config
  (setq god-exempt-major-modes nil
        god-exempt-predicates nil)
  (god-mode))
```


<a id="org9850a3b"></a>

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


<a id="org8b14070"></a>

### Not really useful

1.  Lorem Ipsum

    A 'Lorem ipsum' generator
    
    ```emacs-lisp
    (use-package lorem-ipsum
      :ensure t)
    ```


<a id="org348f175"></a>

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


<a id="orga5633fb"></a>

### PDF-tools

Majorly increases performance when viewing pdfs as a file

```emacs-lisp
;; (use-package pdf-tools
;;   :ensure t
;;   :init
;;   (pdf-tools-install))
```


<a id="orgb89d865"></a>

# Stuff to do when loading


<a id="org3973c6b"></a>

## Environment Variables

Setting path, email and password variables

```emacs-lisp
(setenv "NOTMUCH_CONFIG"
        (expand-file-name "~/.config/notmuch-config"))
(setenv "PASSWORD_STORE_DIR"
        (expand-file-name "~/.local/share/password-store/"))
```


<a id="orgddaebf9"></a>

# Fixing defaults


<a id="org2bb66c1"></a>

## Miscellaneous stuff


<a id="org990a511"></a>

### No more pesky extra files, other basics

```emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
```


<a id="org6f583ad"></a>

### Enable all the features, because what's the point in having less?

```emacs-lisp
(setq disabled-command-function nil)
```


<a id="orgd937459"></a>

## Aesthetic stuff


<a id="orgd44a03e"></a>

### GUI ugliness

Disable all the wasteful bars

```emacs-lisp
(when (window-system)
  (scroll-bar-mode -1)
  (fringe-mode 1))
(menu-bar-mode -1)
(tool-bar-mode -1)

```


<a id="orgeae70b1"></a>

### Font and cursor

Use system monospace font, red non-blinking cursor

```emacs-lisp
(set-cursor-color "red")
(blink-cursor-mode -1)
```


<a id="orgcd75e2e"></a>

## Tabs

Tabs are 4 spaces wide

```emacs-lisp
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
```


<a id="orgd636cba"></a>

## Buffers/input


<a id="org9714954"></a>

### ido-mode

ido-mode is much better than the default for switching buffers and going to files.

```emacs-lisp
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)
```


<a id="org4d01fcc"></a>

### ibuffer

ibuffer is also a lot better than the default (plus it has colours)

```emacs-lisp
(global-set-key (kbd "C-x C-b") 'ibuffer)
```


<a id="org4e967b9"></a>

## desktop-save

```emacs-lisp
(desktop-save-mode t)
```


<a id="orgc3c6cd1"></a>

# Custom functions


<a id="org3733114"></a>

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


<a id="org392ba23"></a>

## Go to config file

Visit your config file. Bound to "C-c e" in `Keybindings` section.

```emacs-lisp
(defun config-visit ()
  "Go to your config.org"
  (interactive)
  (find-file "~/.emacs.d/config.org"))
```


<a id="orgc25c020"></a>

## Ido


<a id="org32ef667"></a>

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


<a id="org7414787"></a>

## Reloading config

Reloads this config file. Bound to "C-c r" in Keybindings section.

```emacs-lisp
(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
```


<a id="org24b99e4"></a>

## Programming


<a id="org83158d0"></a>

### Opening Output

```emacs-lisp
(defun opout ()
  "Opens a pdf file of the same name as the current file"
  (interactive)
  (find-file-other-window (concat
                           (file-name-sans-extension buffer-file-name)
                           ".pdf")))
```


<a id="org3a4e7d3"></a>

## Email

```emacs-lisp
(defun mailsync ()
  "Downloads new mail and adds it to the notmuch database"
  (interactive)
  (shell-command "mbsync -a && notmuch new &" "*mailsync*"))
```


<a id="org2ea532a"></a>

## WM stuff


<a id="orgd6f5f11"></a>

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


<a id="org8f6bcff"></a>

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


<a id="org5bf6cbc"></a>

### Other&#x2026;

1.  Mouse

    ```emacs-lisp
    (defun mousetoggle ()
      "Toggles touchpad on my laptop"
      (interactive)
      (shell-command "mousetoggle")
      (message "touchpad input toggled"))
    ```


<a id="org6cb7538"></a>

## Other


<a id="org3af278f"></a>

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


<a id="org6110fbc"></a>

# Major mode hooks and variables


<a id="orgb5a4923"></a>

## Lilypond mode

Use lilypond mode for .ly files (taken from lilypond.org)

```emacs-lisp
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
(setq LilyPond-pdf-command "zathura")
```


<a id="org2aba20a"></a>

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


<a id="org28130bd"></a>

## Org Mode

```emacs-lisp
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-src-window-setup 'current-window)
(setq org-src-tab-acts-natively t)
(setq org-ellipsis " ")
```


<a id="org636e753"></a>

## M-x compile hooks


<a id="org6d0975e"></a>

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


<a id="orgb5a471b"></a>

### C

```emacs-lisp
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "compiler %s" buffer-file-name))))
```


<a id="org5dfb50e"></a>

### LiLyPond

```emacs-lisp
(add-hook 'LilyPond-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "lilypond %s" buffer-file-name))))
```


<a id="orgb1f9109"></a>

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


<a id="org3790d9d"></a>

# Keybindings


<a id="orgd205d91"></a>

## Interaction with Emacs


<a id="org5f887e2"></a>

### ido-bookmark-jump (custom function)

Open a bookmark with the default keybinding `C-x r b`, but with ido

```emacs-lisp
(global-set-key (kbd "C-x r b") 'ido-bookmark-jump)
```


<a id="org7ef38d4"></a>

### Terminal functionality

Rebinding some useful keys that can't be used in a terminal.

```emacs-lisp
(unless (window-system)
  ;; Comments -- C-x C-;
  (global-set-key (kbd "C-c ;") 'comment-line)
  ;; Indentation -- C-M-\"
  (global-set-key (kbd "C-c \\") 'indent-region))
```


<a id="org0d997ee"></a>

## Config

```emacs-lisp
(global-set-key (kbd "C-c r") 'config-reload)
```


<a id="org432e414"></a>

## General WM stuff


<a id="org500a5ef"></a>

### System information

Built-in battery function with `s-t b`. Custom notification script with `s-t s-b`.

```emacs-lisp
(global-set-key (kbd "s-t b") 'battery)
(global-set-key (kbd "s-t s-b") 'notibar)
```


<a id="org888e92b"></a>

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


<a id="org1d77cfa"></a>

## Programming/Typesetting

Bind emacs compile to `C-c C-m`. This allows 2 rapid presses of `C-m` or `RET` to skip the prompt.

"opout" is a script to open the output of a file (e.g. TeX, LilyPond).

```emacs-lisp
(global-set-key (kbd "C-c C-m") 'compile)
(global-set-key (kbd "C-c p") 'opout)
```


<a id="org1061e03"></a>

## Miscellaneous


<a id="orgb16514f"></a>

### Line numbers

```emacs-lisp
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
```


<a id="orgb11f273"></a>

### Spelling correction

```emacs-lisp
(global-set-key (kbd "C-c s") 'flyspell-mode)
```


<a id="orgd919419"></a>

### Line wrap

```emacs-lisp
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
```


<a id="org8fb16d6"></a>

### Mouse

```emacs-lisp
(global-set-key (kbd "s-t \\") 'mousetoggle)
```


<a id="orgea9fea9"></a>

# Mode-line

Just some basic extra stuff in the mode-line. I don't want anything fancy.

```emacs-lisp
(column-number-mode t)
(display-time-mode t)
(setq display-time-24hr-format 1)
```


<a id="orgcb7ff30"></a>

# Email

email settings

```emacs-lisp
(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
```