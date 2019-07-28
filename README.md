- [Packages](#org0ede44f)
  - [Startup](#org7c7ba1c)
    - [Repositories and initialisation](#org06cfa5f)
    - [Use-package](#orgebb5271)
    - [Base16-theme](#orgf100bcf)
    - [exwm](#orga253e45)
  - [Installed Packages](#orgf12b25c)
    - [Pinentry](#org32e20b8)
    - [Completion](#orgff494af)
  - ["Applications"](#org9c71b05)
    - [Appearance](#orgc509ee4)
    - [Quality of life](#org6619f4f)
    - [Not really useful](#org2a227fa)
    - [To be confirmed](#org3e01933)
    - [PDF-tools](#orgc7fff73)
- [Environment Variables](#orgcbda003)
- [Fixing defaults](#org5875a51)
  - [Miscellaneous stuff](#orge2f2538)
    - [No more pesky extra files, other basics](#org892d2fd)
    - [Enable all the features, because what's the point in having less?](#org072a315)
  - [Aesthetic stuff](#orgd49b4b5)
    - [GUI ugliness](#orgb899f03)
    - [Font and cursor](#org9de6e14)
  - [Tabs](#orgfb392d1)
  - [Buffers/input](#org415977f)
    - [ido-mode](#orgcf8d0d8)
    - [ibuffer](#org596e3d8)
- [Custom functions](#orgf7fb40e)
  - [Resizing windows](#orgc8a1cf7)
  - [Go to config file](#org5c8d196)
  - [Reloading config](#org6f2ac50)
  - [Programming](#orgb97033e)
    - [Compiling](#org6546249)
    - [Compiling in emacs via lambda](#org59ba1da)
    - [Notification bar replacement](#org10bdade)
    - [Opening Output](#orge01fa4a)
  - [Email](#org8136aff)
  - [dmenu](#org390ffde)
    - [dmenu launcher](#orge36102d)
    - [dmenuhandler](#orgca0fd3f)
    - [pdf-opener](#org892bcf6)
    - [video-opener](#orgbce9526)
  - [Other&#x2026;](#org2dce061)
    - [Mouse](#org4341c34)
- [Major mode hooks and variables](#org8bf073a)
  - [Lilypond mode](#orga793bc0)
  - [Electric pairs](#orgb216f04)
  - [Org Mode](#org52785fc)
  - [M-x compile hooks](#orgebb993e)
    - [Groff](#org561bbaf)
    - [C](#org448dfa4)
    - [LiLyPond](#orgf5f147b)
    - [LaTeX](#org5a7d214)
- [Keybindings](#org9841694)
  - [Miscellaneous](#org2b064a7)
    - [Line numbers](#orga50c189)
    - [Spelling correction](#orge52d005)
    - [Line wrap](#orgd1241da)
    - [Mouse](#org8c2d7d6)
    - [client](#org9bb49f7)
  - [Clipboard](#org201622e)
  - [Moving between windows](#orgbceb506)
  - [Config file](#org58dee1f)
    - [Visit config file](#org86e115f)
    - [Reload config file](#org1f51db9)
  - [General WM stuff](#org658a3ea)
    - [Information](#org39af047)
    - [dmenu scripts](#org0f591ce)
  - [Programming/Typesetting](#orgce1dc59)
- [Mode-line](#org23c1b14)
- [Email](#org30159fb)


<a id="org0ede44f"></a>

# Packages


<a id="org7c7ba1c"></a>

## Startup


<a id="org06cfa5f"></a>

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


<a id="orgebb5271"></a>

### Use-package

Install use-package if not installed

```emacs-lisp
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
```


<a id="orgf100bcf"></a>

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


<a id="orga253e45"></a>

### exwm

Emacs X Window Manager

```emacs-lisp
(use-package exwm
  :ensure t
  :config
  (require 'exwm-config)
  (exwm-config-default))
```


<a id="orgf12b25c"></a>

## Installed Packages


<a id="org32e20b8"></a>

### Pinentry

This package lets emacs be used for gpg authentication

```emacs-lisp
(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
```


<a id="orgff494af"></a>

### Completion

1.  Company

    A very good package for auto-completion \#+BEGIN<sub>SRC</sub> emacs-lisp (use-package company :ensure t :init (add-hook 'after-init-hook 'global-company-mode) :config (setq company-idle-delay 0)) \#+END<sub>SRC</sub>\*

2.  Geiser

    A scheme backend
    
    ```emacs-lisp
    (use-package geiser
      :ensure t)
    ```


<a id="org9c71b05"></a>

## "Applications"

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


<a id="orgc509ee4"></a>

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


<a id="org6619f4f"></a>

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


<a id="org2a227fa"></a>

### Not really useful

1.  Lorem Ipsum

    A 'Lorem ipsum' generator
    
    ```emacs-lisp
    (use-package lorem-ipsum
      :ensure t)
    ```


<a id="org3e01933"></a>

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


<a id="orgc7fff73"></a>

### PDF-tools

Majorly increases performance when viewing pdfs as a file

```emacs-lisp
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install))
```


<a id="orgcbda003"></a>

# Environment Variables

Setting path, email and password variables

```emacs-lisp
(setenv "NOTMUCH_CONFIG" (expand-file-name "~/.config/notmuch-config"))
(setenv "PASSWORD_STORE_DIR" (expand-file-name "~/.local/share/password-store/"))
(setenv "PATH" (concat (getenv "PATH") ":/home/jamzattack/.local/bin"))
(setq exec-path (append exec-path '("/home/jamzattack/.local/bin")))
```


<a id="org5875a51"></a>

# Fixing defaults


<a id="orge2f2538"></a>

## Miscellaneous stuff


<a id="org892d2fd"></a>

### No more pesky extra files, other basics

```emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
```


<a id="org072a315"></a>

### Enable all the features, because what's the point in having less?

```emacs-lisp
(setq disabled-command-function nil)
```


<a id="orgd49b4b5"></a>

## Aesthetic stuff


<a id="orgb899f03"></a>

### GUI ugliness

Disable all the wasteful bars

```emacs-lisp
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode 1)
```


<a id="org9de6e14"></a>

### Font and cursor

Use system monospace font, red non-blinking cursor

```emacs-lisp
(set-cursor-color "red")
(blink-cursor-mode -1)
```


<a id="orgfb392d1"></a>

## Tabs

Tabs are 4 spaces wide

```emacs-lisp
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
```


<a id="org415977f"></a>

## Buffers/input


<a id="orgcf8d0d8"></a>

### ido-mode

ido-mode is much better than the default for switching buffers and going to files.

```emacs-lisp
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)
```


<a id="org596e3d8"></a>

### ibuffer

ibuffer is also a lot better than the default (plus it has colours)

```emacs-lisp
(global-set-key (kbd "C-x C-b") 'ibuffer)
```


<a id="orgf7fb40e"></a>

# Custom functions


<a id="orgc8a1cf7"></a>

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


<a id="org5c8d196"></a>

## Go to config file

Visit your config file. Bound to "C-c e" in `Keybindings` section.

```emacs-lisp
(defun config-visit ()
  "Go to your config.org"
  (interactive)
  (find-file "~/.emacs.d/config.org"))
```


<a id="org6f2ac50"></a>

## Reloading config

Reloads this config file. Bound to "C-c r" in Keybindings section.

```emacs-lisp
(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
```


<a id="orgb97033e"></a>

## Programming


<a id="org6546249"></a>

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


<a id="org59ba1da"></a>

### Compiling in emacs via lambda

```emacs-lisp
(require 'compile)
(defun my-compiler-command ()
  "A simple lambda to set compile-command"
  (lambda ()
    (set (make-local-variable 'compile-command)
         (format "compiler %s" buffer-file-name))))
```


<a id="org10bdade"></a>

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


<a id="orge01fa4a"></a>

### Opening Output

```emacs-lisp
(defun opout ()
  "Opens a pdf file of the same name as the current file"
  (interactive)
  (find-file-other-window (concat
                           (file-name-sans-extension buffer-file-name)
                           ".pdf")))
```


<a id="org8136aff"></a>

## Email

```emacs-lisp
(defun mailsync ()
  "Downloads new mail and adds it to the notmuch database"
  (interactive)
  (shell-command "mbsync -a && notmuch new &" "*mailsync*"))
```


<a id="org390ffde"></a>

## dmenu


<a id="orge36102d"></a>

### dmenu launcher

```emacs-lisp
(defun dmenu_recency ()
  "Launch a program with dmenu"
  (interactive)
  (start-process "dmenu_recency" nil
                 "dmenu_recency"))
```


<a id="orgca0fd3f"></a>

### dmenuhandler

```emacs-lisp
(defun dmenuhandler ()
  "Choose how to handle the url in X11 clipboard"
  (interactive)
  (start-process "dmenuhandler" nil
                 "dmenuhandler"))
```


<a id="org892bcf6"></a>

### pdf-opener

```emacs-lisp
(defun pdf-opener ()
  "Select a .pdf or .ps file to view in zathura"
  (interactive)
  (start-process "pdf-opener" nil
                 "pdf-opener"))
```


<a id="orgbce9526"></a>

### video-opener

```emacs-lisp
(defun video-opener ()
  "Select a downloaded video to watch via dmenu and mpv"
  (interactive)
  (start-process "video-opener" nil
                 "video-opener"))
```


<a id="org2dce061"></a>

## Other&#x2026;


<a id="org4341c34"></a>

### Mouse

```emacs-lisp
(defun mousetoggle ()
  "Toggles touchpad on my laptop"
  (interactive)
  (shell-command "mousetoggle")
  (message "touchpad input toggled"))
```


<a id="org8bf073a"></a>

# Major mode hooks and variables


<a id="orga793bc0"></a>

## Lilypond mode

Use lilypond mode for .ly files (taken from lilypond.org)

```emacs-lisp
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
(setq LilyPond-pdf-command "zathura")
```


<a id="orgb216f04"></a>

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


<a id="org52785fc"></a>

## Org Mode

```emacs-lisp
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-src-window-setup 'current-window)
(setq org-src-tab-acts-natively t)
(setq org-ellipsis " ")
```


<a id="orgebb993e"></a>

## M-x compile hooks

The function ´my-compiler-command´ is defined above. Simply changes variable 'compiler-command.


<a id="org561bbaf"></a>

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


<a id="org448dfa4"></a>

### C

```emacs-lisp
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "compiler %s" buffer-file-name))))
```


<a id="orgf5f147b"></a>

### LiLyPond

```emacs-lisp
(add-hook 'LilyPond-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "lilypond %s" buffer-file-name))))
```


<a id="org5a7d214"></a>

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


<a id="org9841694"></a>

# Keybindings


<a id="org2b064a7"></a>

## Miscellaneous


<a id="orga50c189"></a>

### Line numbers

```emacs-lisp
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
```


<a id="orge52d005"></a>

### Spelling correction

```emacs-lisp
(global-set-key (kbd "C-c s") 'flyspell-mode)
```


<a id="orgd1241da"></a>

### Line wrap

```emacs-lisp
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
```


<a id="org8c2d7d6"></a>

### Mouse

```emacs-lisp
(global-set-key (kbd "s-t \\") 'mousetoggle)
```


<a id="org9bb49f7"></a>

### client

```emacs-lisp
(global-set-key (kbd "C-x C-c") 'delete-frame)
```


<a id="org201622e"></a>

## Clipboard

```emacs-lisp
(global-set-key (kbd "C-c w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-c y") 'clipboard-yank)
```


<a id="orgbceb506"></a>

## Moving between windows

```emacs-lisp
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-n") 'windmove-down)
(global-set-key (kbd "s-b") 'windmove-left)
(global-set-key (kbd "s-f") 'windmove-right)
(global-set-key (kbd "<M-tab>") 'other-window)
```


<a id="org58dee1f"></a>

## Config file

Both defined in the Custom Functions section


<a id="org86e115f"></a>

### Visit config file

```emacs-lisp
(global-set-key (kbd "C-c e") 'config-visit)
```


<a id="org1f51db9"></a>

### Reload config file

```emacs-lisp
(global-set-key (kbd "C-c r") 'config-reload)
```


<a id="org658a3ea"></a>

## General WM stuff


<a id="org39af047"></a>

### Information

```emacs-lisp
(global-set-key (kbd "s-t b") 'battery)
(global-set-key (kbd "s-t s-b") 'notibar)
```


<a id="org0f591ce"></a>

### dmenu scripts

All of these are bound to functions written in 'Custom functions'

```emacs-lisp
(global-set-key (kbd "s-t d") 'dmenu_recency)
(global-set-key (kbd "s-t P") 'pdf-opener)
(global-set-key (kbd "s-t V") 'video-opener)
(global-set-key (kbd "s-t D") 'dmenuhandler)
```


<a id="orgce1dc59"></a>

## Programming/Typesetting

```emacs-lisp
(global-set-key (kbd "C-c C-m") 'compile)
(global-set-key (kbd "C-c p") 'opout)
```


<a id="org23c1b14"></a>

# Mode-line

Just some basic extra stuff in the mode-line. I don't want anything fancy.

```emacs-lisp
(column-number-mode t)
(display-time-mode t)
(setq display-time-24hr-format 1)
```


<a id="org30159fb"></a>

# Email

```emacs-lisp
(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
```