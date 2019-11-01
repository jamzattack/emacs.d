- [Packages](#org878b88d)
  - [Startup](#org37c9810)
    - [Repositories and initialisation](#org5476344)
    - [Use-package](#org2f263bb)
  - [Installed Packages](#org72b6020)
    - [Pinentry](#org8fffff2)
    - [Org](#org7c67a4f)
    - [EXWM - Emacs X Window Manager](#org188d437)
    - [Desktop-environment (useful with EXWM)](#orgf3fcd9f)
    - [Completion](#org808b0cc)
    - ["Applications"](#orga260cde)
    - [Appearance](#orgfd76b07)
    - [god-mode](#orgb5f63b6)
    - [Quality of life](#org9e9b2f4)
    - [Not really useful](#orgc9531cc)
    - [To be confirmed](#org95280ef)
    - [PDF-tools](#org9f0009e)
    - [Mine](#orge6fc08c)
- [Stuff to do when loading](#org1d2d5dd)
  - [Environment Variables](#orgba45fa7)
- [Fixing defaults](#orge3506d9)
  - [Miscellaneous](#org527798a)
    - [Swap yes/no prompt with y/n](#org9bb4f20)
    - [Enable all the features](#org4ee96a6)
  - [Aesthetics](#org7d4cf26)
    - [GUI ugliness](#orgff730e4)
    - [Font and cursor](#orga378163)
    - [Disable audible and visual bell](#org27b3757)
  - [Tabs](#org8095c9a)
  - [Buffers/input](#org686d309)
    - [ido-mode](#org2b03406)
    - [ibuffer](#orgea77062)
  - [desktop-save](#org3f2c84e)
  - [Help](#org894b3a3)
- [Custom functions](#org45cf05a)
  - [Resizing windows](#org9911c15)
  - [Go to config file](#org3f2742a)
  - [Ido](#org0a0ed62)
    - [Bookmarks](#org6d3e086)
  - [Reloading config](#orge12d827)
  - [Programming](#orgac9d372)
    - [Opening Output](#orgfb4ef20)
  - [Email](#org8ad8f56)
  - [WM stuff](#org196d04b)
    - [Notification bar replacement](#org2ab33d6)
    - [dmenu](#org826362a)
  - [Other](#org987f328)
    - [Xah Lee form feed](#org5e958ca)
  - [Fixing packages](#orgaf78e1b)
- [Major mode hooks and variables](#org72a8100)
  - [Lilypond mode](#org2429b12)
  - [Electric pairs](#org3347870)
  - [Org Mode](#orgbf92da3)
  - [M-x compile hooks](#org8b59ff2)
    - [Groff](#orgeb07283)
    - [C](#orgb50a669)
    - [LiLyPond](#org55bf64b)
    - [LaTeX](#org4c22367)
- [Keybindings](#org9d09be7)
  - [Remove `C-z`](#org4105bbe)
  - [Interaction with Emacs](#orgf7e1f79)
    - [ido-bookmark-jump (custom function)](#orgf53ca42)
    - [Terminal functionality](#org70b9d37)
    - [bury-buffer and kill-buffer-and-window](#org1632411)
  - [Config](#orgeae9e1d)
  - [General WM stuff](#org07e8c18)
    - [System information](#orgebf45fb)
    - [dmenu scripts](#orge2978cb)
  - [Programming/Typesetting](#orgf2337da)
  - [Miscellaneous](#org37a4090)
    - [Line numbers](#orgbd9cf7a)
    - [Spelling correction](#orgbc2cb20)
    - [Line wrap](#org05c9718)
- [Mode-line](#org6d96b46)
- [Email](#org9e174ba)


<a id="org878b88d"></a>

# Packages


<a id="org37c9810"></a>

## Startup


<a id="org5476344"></a>

### Repositories and initialisation

Enable melpa archive and making sure packages work

```emacs-lisp
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org-/packages/")
             '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)
```


<a id="org2f263bb"></a>

### Use-package

Install use-package if not installed

```emacs-lisp
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
```


<a id="org72b6020"></a>

## Installed Packages


<a id="org8fffff2"></a>

### Pinentry

This package lets emacs be used for gpg authentication

```emacs-lisp
(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
```


<a id="org7c67a4f"></a>

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


<a id="org188d437"></a>

### EXWM - Emacs X Window Manager

Manipulate X windows as emacs buffers.

```emacs-lisp
(use-package exwm
  :ensure t
  :config
  (require 'exwm-config)
  (exwm-config-default))
```


<a id="orgf3fcd9f"></a>

### Desktop-environment (useful with EXWM)

```emacs-lisp
(use-package desktop-environment
  :ensure t
  :config (desktop-environment-mode))
```


<a id="org808b0cc"></a>

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


<a id="orga260cde"></a>

### "Applications"

1.  vterm

    ```emacs-lisp
    (use-package vterm
      :load-path "~/.emacs.d/emacs-libvterm")
    ```

2.  Mingus

    A nice mpd front-end in emacs (I couldn't get EMMS working with mopidy)
    
    ```emacs-lisp
    (use-package mingus
      :ensure t)
    ```

3.  Notmuch

    A simple email client, with emphasis on searching
    
    ```emacs-lisp
    (use-package notmuch
      :ensure t
      :config (setq notmuch-archive-tags '("-unread" "-inbox")))
    ```

4.  Transmission

    An emacs front-end for the transmission bittorrent daemon
    
    ```emacs-lisp
    (use-package transmission
      :ensure t)
    ```

5.  Elfeed

    ```emacs-lisp
    (use-package elfeed                                         
      :ensure t                                                 
      :config                                                   
      (if (file-exists-p "~/.emacs.d/feeds.el")                 
          (load-file (expand-file-name "~/.emacs.d/feeds.el"))))
    ```


<a id="orgfd76b07"></a>

### Appearance

1.  Theme

    1.  moe-theme
    
        ```emacs-lisp
        (use-package moe-theme
          :ensure t
          :init
          (setq moe-theme-highlight-buffer-id nil)
          :config
          (moe-theme-set-color 'purple)
          (moe-dark))
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


<a id="orgb5f63b6"></a>

### god-mode

```emacs-lisp
(use-package god-mode
  :ensure t
  :bind
  (("<f8>" . 'god-mode-all)
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


<a id="org9e9b2f4"></a>

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


<a id="orgc9531cc"></a>

### Not really useful

1.  Lorem Ipsum

    A 'Lorem ipsum' generator
    
    ```emacs-lisp
    (use-package lorem-ipsum
      :ensure t)
    ```


<a id="org95280ef"></a>

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


<a id="org9f0009e"></a>

### PDF-tools

Majorly increases performance when viewing pdfs as a file

```emacs-lisp
(use-package pdf-tools
  :ensure t)
```


<a id="orge6fc08c"></a>

### Mine

1.  Toggle-touchpad

    A simple package I wrote to toggle the touchpad/trackpoint on my ThinkPad
    
    ```emacs-lisp
    (use-package toggle-touchpad
      :load-path "~/.emacs.d/"
      :bind
      (("<XF86TouchpadToggle>" . 'toggle-touchpad)
       ("C-z \\" . 'toggle-touchpad)))
    ```


<a id="org1d2d5dd"></a>

# Stuff to do when loading


<a id="orgba45fa7"></a>

## Environment Variables

Setting path, email and password variables

```emacs-lisp
(setenv "NOTMUCH_CONFIG"
        (expand-file-name "~/.config/notmuch-config"))
(setenv "PASSWORD_STORE_DIR"
        (expand-file-name "~/.local/share/password-store/"))
```


<a id="orge3506d9"></a>

# Fixing defaults


<a id="org527798a"></a>

## Miscellaneous


<a id="org9bb4f20"></a>

### Swap yes/no prompt with y/n

```emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
```


<a id="org4ee96a6"></a>

### Enable all the features

```emacs-lisp
(setq disabled-command-function nil)
```


<a id="org7d4cf26"></a>

## Aesthetics


<a id="orgff730e4"></a>

### GUI ugliness

Disable all the wasteful bars

```emacs-lisp
(when (window-system)
  (scroll-bar-mode -1)
  (fringe-mode 1))
(menu-bar-mode -1)
(tool-bar-mode -1)

```


<a id="orga378163"></a>

### Font and cursor

Use system monospace font, red non-blinking cursor

```emacs-lisp
(set-face-font 'default "Monospace 8") 
(set-face-font 'fixed-pitch-serif "Monospace Italic")
(set-face-font 'variable-pitch "Sans 10")
(set-cursor-color "red")
(blink-cursor-mode -1)
```


<a id="org27b3757"></a>

### Disable audible and visual bell

```emacs-lisp
(setq ring-bell-function 'ignore)
```


<a id="org8095c9a"></a>

## Tabs

Tabs are 4 spaces wide

```emacs-lisp
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
```


<a id="org686d309"></a>

## Buffers/input


<a id="org2b03406"></a>

### ido-mode

ido-mode is much better than the default for switching buffers and going to files.

```emacs-lisp
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)
```


<a id="orgea77062"></a>

### ibuffer

ibuffer is also a lot better than the default (plus it has colours)

```emacs-lisp
(global-set-key (kbd "C-x C-b") 'ibuffer)
```


<a id="org3f2c84e"></a>

## desktop-save

```emacs-lisp
(desktop-save-mode t)
```


<a id="org894b3a3"></a>

## Help

Use a keybinding for viewing manpages

```emacs-lisp
(global-set-key (kbd "C-h C-m") 'woman)
```


<a id="org45cf05a"></a>

# Custom functions


<a id="org9911c15"></a>

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


<a id="org3f2742a"></a>

## Go to config file

Visit your config file. Bound to "C-c e" in `Keybindings` section.

```emacs-lisp
(defun config-visit ()
  "Go to your config.org"
  (interactive)
  (find-file "~/.emacs.d/config.org"))
```


<a id="org0a0ed62"></a>

## Ido


<a id="org6d3e086"></a>

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


<a id="orge12d827"></a>

## Reloading config

Reloads this config file. Bound to "C-c r" in Keybindings section.

```emacs-lisp
(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
```


<a id="orgac9d372"></a>

## Programming


<a id="orgfb4ef20"></a>

### Opening Output

```emacs-lisp
(defun opout ()
  "Opens a pdf file of the same name as the current file"
  (interactive)
  (find-file-other-window (concat
                           (file-name-sans-extension buffer-file-name)
                           ".pdf")))
```


<a id="org8ad8f56"></a>

## Email

```emacs-lisp
(defun mailsync ()
  "Downloads new mail and adds it to the notmuch database"
  (interactive)
  (shell-command "mbsync -a && notmuch new &" "*mailsync*"))
```


<a id="org196d04b"></a>

## WM stuff


<a id="org2ab33d6"></a>

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


<a id="org826362a"></a>

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
      (shell-command (concat "dmenuhandler " (car kill-ring) " &")))
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


<a id="org987f328"></a>

## Other


<a id="org5e958ca"></a>

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


<a id="orgaf78e1b"></a>

## Fixing packages

```emacs-lisp

(defun transmission ()
  "Open a `transmission-mode' buffer."
  (interactive)
  (let* ((name "*transmission*")
         (buffer (or (get-buffer name)
                     (generate-new-buffer name))))
    (transmission-turtle-poll)
    (unless (eq buffer (current-buffer))
      (with-current-buffer buffer
        (unless (eq major-mode 'transmission-mode)
          (condition-case e
              (progn
                (transmission-mode)
                (transmission-draw)
                (goto-char (point-min)))
            (error
             (kill-buffer buffer)
             (signal (car e) (cdr e))))))
      (switch-to-buffer buffer))))
```


<a id="org72a8100"></a>

# Major mode hooks and variables


<a id="org2429b12"></a>

## Lilypond mode

Use lilypond mode for .ly files (taken from lilypond.org)

```emacs-lisp
(defalias 'lilypond-mode 'LilyPond-mode)
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(load-file "/usr/share/emacs/site-lisp/lilypond-init.el")
(setq auto-mode-alist
      (cons '("\\.ly$" . lilypond-mode) auto-mode-alist))
(setq LilyPond-pdf-command "zathura")
```


<a id="org3347870"></a>

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


<a id="orgbf92da3"></a>

## Org Mode

```emacs-lisp
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-src-window-setup 'current-window)
(setq org-src-tab-acts-natively t)
(setq org-ellipsis " ")
```


<a id="org8b59ff2"></a>

## M-x compile hooks


<a id="orgeb07283"></a>

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


<a id="orgb50a669"></a>

### C

```emacs-lisp
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "compiler %s" buffer-file-name))))
```


<a id="org55bf64b"></a>

### LiLyPond

```emacs-lisp
(add-hook 'LilyPond-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "lilypond %s" buffer-file-name))))
```


<a id="org4c22367"></a>

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


<a id="org9d09be7"></a>

# Keybindings


<a id="org4105bbe"></a>

## Remove `C-z`

```emacs-lisp
(global-unset-key (kbd "C-z"))
```


<a id="orgf7e1f79"></a>

## Interaction with Emacs


<a id="orgf53ca42"></a>

### ido-bookmark-jump (custom function)

Open a bookmark with the default keybinding `C-x r b`, but with ido

```emacs-lisp
(global-set-key (kbd "C-x r b") 'ido-bookmark-jump)
```


<a id="org70b9d37"></a>

### Terminal functionality

Rebinding some useful keys that can't be used in a terminal.

```emacs-lisp
(unless (window-system)
  ;; Comments -- C-x C-;
  (global-set-key (kbd "C-c ;") 'comment-line)
  ;; Indentation -- C-M-\"
  (global-set-key (kbd "C-c \\") 'indent-region))
```


<a id="org1632411"></a>

### bury-buffer and kill-buffer-and-window

```emacs-lisp
(global-set-key (kbd "C-z C-z") 'bury-buffer)
(global-set-key (kbd "C-z z") 'kill-buffer-and-window)
```


<a id="orgeae9e1d"></a>

## Config

```emacs-lisp
(global-set-key (kbd "C-c r") 'config-reload)
```


<a id="org07e8c18"></a>

## General WM stuff


<a id="orgebf45fb"></a>

### System information

Built-in battery function with `C-z b`. Custom notification script with `C-z C-b`.

```emacs-lisp
(global-set-key (kbd "C-z b") 'battery)
(global-set-key (kbd "C-z C-b") 'notibar)
```


<a id="orge2978cb"></a>

### dmenu scripts

I still have some use for dmenu, despite only using emacs&#x2026; All commands are prefixed with `C-z`

| d | enter commands into dmenu       |
| P | select a pdf to open with emacs |
| V | select a video to open with mpv |
| D | choose what to do with a URL    |

```emacs-lisp
(global-set-key (kbd "C-z d") 'dmenu_recency)
(global-set-key (kbd "C-z P") 'pdf-opener)
(global-set-key (kbd "C-z V") 'video-opener)
(global-set-key (kbd "C-z D") 'dmenuhandler)
```


<a id="orgf2337da"></a>

## Programming/Typesetting

Bind emacs compile to `C-c C-m`. This allows 2 rapid presses of `C-m` or `RET` to skip the prompt.

"opout" is a script to open the output of a file (e.g. TeX, LilyPond).

```emacs-lisp
(global-set-key (kbd "C-c C-m") 'compile)
(global-set-key (kbd "C-c p") 'opout)
```


<a id="org37a4090"></a>

## Miscellaneous


<a id="orgbd9cf7a"></a>

### Line numbers

```emacs-lisp
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
```


<a id="orgbc2cb20"></a>

### Spelling correction

```emacs-lisp
(global-set-key (kbd "C-c s") 'flyspell-mode)
```


<a id="org05c9718"></a>

### Line wrap

```emacs-lisp
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
```


<a id="org6d96b46"></a>

# Mode-line

Just some basic extra stuff in the mode-line. I don't want anything fancy.

```emacs-lisp
(column-number-mode t)
(display-time-mode t)
(setq display-time-24hr-format 1)
```


<a id="org9e174ba"></a>

# Email

email settings

```emacs-lisp
(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
```