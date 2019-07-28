- [Packages](#orgee66018)
  - [Startup](#orgdabeba4)
    - [Repositories and initialisation](#org97a1943)
    - [Use-package](#org40cae66)
    - [Base16-theme](#org6c344b2)
    - [exwm](#org2743eac)
  - [Installed Packages](#org97e66b6)
    - [Pinentry](#orgda4ef86)
    - [Completion](#org004055e)
    - ["Applications"](#org9f34b6a)
    - [Appearance](#org3cb0d4a)
    - [Quality of life](#orgc38d317)
    - [Not really useful](#orgb1929a5)
    - [To be confirmed](#orgc9bb7e4)
    - [PDF-tools](#org8363448)
- [Environment Variables](#orgf2842cf)
- [Fixing defaults](#org732db5f)
  - [Miscellaneous stuff](#orgee36ea2)
    - [No more pesky extra files, other basics](#orged91d6a)
    - [Enable all the features, because what's the point in having less?](#orgfc52869)
  - [Aesthetic stuff](#orga43846f)
    - [GUI ugliness](#orgde62220)
    - [Font and cursor](#orgf4de03c)
  - [Tabs](#org77e32f4)
  - [Buffers/input](#org51db9b7)
    - [ido-mode](#org148c4e4)
    - [ibuffer](#orgbdd67f9)
- [Custom functions](#org2a75bda)
  - [Resizing windows](#orgbbde894)
  - [Go to config file](#orgeb030ef)
  - [Reloading config](#org302edf9)
  - [Programming](#orgc2cd43b)
    - [Compiling](#org36bb660)
    - [Compiling in emacs via lambda](#org13abb08)
    - [Notification bar replacement](#orgd52c625)
    - [Opening Output](#orgd19ad03)
  - [Email](#orgeef64d6)
  - [dmenu](#orgc74d61c)
    - [dmenu launcher](#orgcf2fcf9)
    - [dmenuhandler](#orgab2336a)
    - [pdf-opener](#orgd955a22)
    - [video-opener](#org1f365b6)
  - [Other&#x2026;](#org1d3296e)
    - [Mouse](#org3040a91)
- [Major mode hooks and variables](#org15d82f2)
  - [Lilypond mode](#org10afe62)
  - [Electric pairs](#org85138d0)
  - [Org Mode](#org7c5e300)
  - [M-x compile hooks](#org68cfa57)
    - [Groff](#org2b5ce1f)
    - [C](#org0994209)
    - [LiLyPond](#org3c4dfe9)
    - [LaTeX](#orged4a221)
- [Keybindings](#org36c40c1)
  - [Miscellaneous](#org3842cc8)
    - [Line numbers](#orgb26e99d)
    - [Spelling correction](#org520a9e8)
    - [Line wrap](#org500db4d)
    - [Mouse](#orgc833bb4)
    - [client](#orgf9abd05)
  - [Clipboard](#org19f59a8)
  - [Moving between windows](#orge56f355)
  - [Config file](#org2b9f371)
    - [Visit config file](#org55166ae)
    - [Reload config file](#orgf6887c0)
  - [General WM stuff](#org0472a9f)
    - [Information](#org2a0378c)
    - [dmenu scripts](#org0fb4adf)
  - [Programming/Typesetting](#orgebb4db4)
- [Mode-line](#org8d8ca58)
- [Email](#org900efcf)


<a id="orgee66018"></a>

# Packages


<a id="orgdabeba4"></a>

## Startup


<a id="org97a1943"></a>

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


<a id="org40cae66"></a>

### Use-package

Install use-package if not installed

```emacs-lisp
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
```


<a id="org6c344b2"></a>

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


<a id="org2743eac"></a>

### exwm

Emacs X Window Manager

```emacs-lisp
(use-package exwm
  :ensure t
  :config
  (require 'exwm-config)
  (exwm-config-default))
```


<a id="org97e66b6"></a>

## Installed Packages


<a id="orgda4ef86"></a>

### Pinentry

This package lets emacs be used for gpg authentication

```emacs-lisp
(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
```


<a id="org004055e"></a>

### Completion

1.  Company

    A very good package for auto-completion \#+BEGIN<sub>SRC</sub> emacs-lisp (use-package company :ensure t :init (add-hook 'after-init-hook 'global-company-mode) :config (setq company-idle-delay 0)) \#+END<sub>SRC</sub>\*

2.  Geiser

    A scheme backend
    
    ```emacs-lisp
    (use-package geiser
      :ensure t)
    ```


<a id="org9f34b6a"></a>

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


<a id="org3cb0d4a"></a>

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


<a id="orgc38d317"></a>

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


<a id="orgb1929a5"></a>

### Not really useful

1.  Lorem Ipsum

    A 'Lorem ipsum' generator
    
    ```emacs-lisp
    (use-package lorem-ipsum
      :ensure t)
    ```


<a id="orgc9bb7e4"></a>

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


<a id="org8363448"></a>

### PDF-tools

Majorly increases performance when viewing pdfs as a file

```emacs-lisp
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install))
```


<a id="orgf2842cf"></a>

# Environment Variables

Setting path, email and password variables

```emacs-lisp
(setenv "NOTMUCH_CONFIG" (expand-file-name "~/.config/notmuch-config"))
(setenv "PASSWORD_STORE_DIR" (expand-file-name "~/.local/share/password-store/"))
(setenv "PATH" (concat (getenv "PATH") ":/home/jamzattack/.local/bin"))
(setq exec-path (append exec-path '("/home/jamzattack/.local/bin")))
```


<a id="org732db5f"></a>

# Fixing defaults


<a id="orgee36ea2"></a>

## Miscellaneous stuff


<a id="orged91d6a"></a>

### No more pesky extra files, other basics

```emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
```


<a id="orgfc52869"></a>

### Enable all the features, because what's the point in having less?

```emacs-lisp
(setq disabled-command-function nil)
```


<a id="orga43846f"></a>

## Aesthetic stuff


<a id="orgde62220"></a>

### GUI ugliness

Disable all the wasteful bars

```emacs-lisp
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode 1)
```


<a id="orgf4de03c"></a>

### Font and cursor

Use system monospace font, red non-blinking cursor

```emacs-lisp
(set-cursor-color "red")
(blink-cursor-mode -1)
```


<a id="org77e32f4"></a>

## Tabs

Tabs are 4 spaces wide

```emacs-lisp
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
```


<a id="org51db9b7"></a>

## Buffers/input


<a id="org148c4e4"></a>

### ido-mode

ido-mode is much better than the default for switching buffers and going to files.

```emacs-lisp
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)
```


<a id="orgbdd67f9"></a>

### ibuffer

ibuffer is also a lot better than the default (plus it has colours)

```emacs-lisp
(global-set-key (kbd "C-x C-b") 'ibuffer)
```


<a id="org2a75bda"></a>

# Custom functions


<a id="orgbbde894"></a>

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


<a id="orgeb030ef"></a>

## Go to config file

Visit your config file. Bound to "C-c e" in `Keybindings` section.

```emacs-lisp
(defun config-visit ()
  "Go to your config.org"
  (interactive)
  (find-file "~/.emacs.d/config.org"))
```


<a id="org302edf9"></a>

## Reloading config

Reloads this config file. Bound to "C-c r" in Keybindings section.

```emacs-lisp
(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
```


<a id="orgc2cd43b"></a>

## Programming


<a id="org36bb660"></a>

### Compiling

Compile the file associate with current buffer.

```emacs-lisp
(defun generic-compiler ()
  "Runs my own compile script on the file associated with the
current buffer. Works with:
lilypond, groff (ms, mom), c, tex, python, and go"
  (interactive)
  (shell-command (concat "compiler "
                         (buffer-file-name) " &"))
  (bury-buffer "*Async Shell Command*"))
```


<a id="org13abb08"></a>

### Compiling in emacs via lambda

```emacs-lisp
(require 'compile)
(defun my-compiler-command ()
  "A simple lambda to set compile-command"
  (lambda ()
    (set (make-local-variable 'compile-command)
         (format "compiler %s" buffer-file-name))))
```


<a id="orgd52c625"></a>

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
  (shell-command "notibar"))
```


<a id="orgd19ad03"></a>

### Opening Output

```emacs-lisp
(defun opout ()
  "Opens a pdf file of the same name as the current file"
  (interactive)
  (find-file-other-window (concat
                           (file-name-sans-extension buffer-file-name)
                           ".pdf")))
```


<a id="orgeef64d6"></a>

## Email

```emacs-lisp
(defun mailsync ()
  "Downloads new mail and adds it to the notmuch database"
  (interactive)
  (shell-command "mbsync -a && notmuch new &" "*mailsync*"))
```


<a id="orgc74d61c"></a>

## dmenu


<a id="orgcf2fcf9"></a>

### dmenu launcher

```emacs-lisp
(defun dmenu_recency ()
  "Launch a program with dmenu"
  (interactive)
  (start-process "dmenu_recency" nil
                 "dmenu_recency"))
```


<a id="orgab2336a"></a>

### dmenuhandler

```emacs-lisp
(defun dmenuhandler ()
  "Choose how to handle the url in X11 clipboard"
  (interactive)
  (start-process "dmenuhandler" nil
                 "dmenuhandler"))
```


<a id="orgd955a22"></a>

### pdf-opener

```emacs-lisp
(defun pdf-opener ()
  "Select a .pdf or .ps file to view in zathura"
  (interactive)
  (start-process "pdf-opener" nil
                 "pdf-opener"))
```


<a id="org1f365b6"></a>

### video-opener

```emacs-lisp
(defun video-opener ()
  "Select a downloaded video to watch via dmenu and mpv"
  (interactive)
  (start-process "video-opener" nil
                 "video-opener"))
```


<a id="org1d3296e"></a>

## Other&#x2026;


<a id="org3040a91"></a>

### Mouse

```emacs-lisp
(defun mousetoggle ()
  "Toggles touchpad on my laptop"
  (interactive)
  (shell-command "mousetoggle")
  (message "touchpad input toggled"))
```


<a id="org15d82f2"></a>

# Major mode hooks and variables


<a id="org10afe62"></a>

## Lilypond mode

Use lilypond mode for .ly files (taken from lilypond.org)

```emacs-lisp
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
(setq LilyPond-pdf-command "zathura")
```


<a id="org85138d0"></a>

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


<a id="org7c5e300"></a>

## Org Mode

```emacs-lisp
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-src-window-setup 'current-window)
(setq org-src-tab-acts-natively t)
(setq org-ellipsis " ")
```


<a id="org68cfa57"></a>

## M-x compile hooks

The function ´my-compiler-command´ is defined above. Simply changes variable 'compiler-command.


<a id="org2b5ce1f"></a>

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


<a id="org0994209"></a>

### C

```emacs-lisp
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "compiler %s" buffer-file-name))))
```


<a id="org3c4dfe9"></a>

### LiLyPond

```emacs-lisp
(add-hook 'LilyPond-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "lilypond %s" buffer-file-name))))
```


<a id="orged4a221"></a>

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


<a id="org36c40c1"></a>

# Keybindings


<a id="org3842cc8"></a>

## Miscellaneous


<a id="orgb26e99d"></a>

### Line numbers

```emacs-lisp
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
```


<a id="org520a9e8"></a>

### Spelling correction

```emacs-lisp
(global-set-key (kbd "C-c s") 'flyspell-mode)
```


<a id="org500db4d"></a>

### Line wrap

```emacs-lisp
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
```


<a id="orgc833bb4"></a>

### Mouse

```emacs-lisp
(global-set-key (kbd "s-t \\") 'mousetoggle)
```


<a id="orgf9abd05"></a>

### client

```emacs-lisp
(global-set-key (kbd "C-x C-c") 'delete-frame)
```


<a id="org19f59a8"></a>

## Clipboard

```emacs-lisp
(global-set-key (kbd "C-c w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-c y") 'clipboard-yank)
```


<a id="orge56f355"></a>

## Moving between windows

```emacs-lisp
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-n") 'windmove-down)
(global-set-key (kbd "s-b") 'windmove-left)
(global-set-key (kbd "s-f") 'windmove-right)
(global-set-key (kbd "<M-tab>") 'other-window)
```


<a id="org2b9f371"></a>

## Config file

Both defined in the Custom Functions section


<a id="org55166ae"></a>

### Visit config file

```emacs-lisp
(global-set-key (kbd "C-c e") 'config-visit)
```


<a id="orgf6887c0"></a>

### Reload config file

```emacs-lisp
(global-set-key (kbd "C-c r") 'config-reload)
```


<a id="org0472a9f"></a>

## General WM stuff


<a id="org2a0378c"></a>

### Information

```emacs-lisp
(global-set-key (kbd "s-t b") 'battery)
(global-set-key (kbd "s-t s-b") 'notibar)
```


<a id="org0fb4adf"></a>

### dmenu scripts

All of these are bound to functions written in 'Custom functions'

```emacs-lisp
(global-set-key (kbd "s-t d") 'dmenu_recency)
(global-set-key (kbd "s-t P") 'pdf-opener)
(global-set-key (kbd "s-t V") 'video-opener)
(global-set-key (kbd "s-t D") 'dmenuhandler)
```


<a id="orgebb4db4"></a>

## Programming/Typesetting

```emacs-lisp
(global-set-key (kbd "C-c C-m") 'compile)
(global-set-key (kbd "C-c p") 'opout)
```


<a id="org8d8ca58"></a>

# Mode-line

Just some basic extra stuff in the mode-line. I don't want anything fancy.

```emacs-lisp
(column-number-mode t)
(display-time-mode t)
(setq display-time-24hr-format 1)
```


<a id="org900efcf"></a>

# Email

```emacs-lisp
(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
```