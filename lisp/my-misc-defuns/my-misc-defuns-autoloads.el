;;; my-misc-defuns-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "my-misc-defuns" "my-misc-defuns.el" (24315
;;;;;;  18897 864585 393000))
;;; Generated autoloads from my-misc-defuns.el

(autoload 'system-apropos "my-misc-defuns" "\
Run the \"apropos\" comamnd with search term SEARCH and
optional arguments ARGS.

\(fn SEARCH &optional ARGS)" t nil)

(autoload 'list-documents "my-misc-defuns" "\
Using `find-dired', list all ps or pdf files in DIR.
If called interactively, prompt for directory.  Else, DIR will
default to ~/Documents/.

\(fn &optional DIR)" t nil)

(autoload 'list-videos "my-misc-defuns" "\
Using `find-dired', list all the videos in DIR.
If called interactively, prompt for directory.  Else, DIR will
default to ~/Downloads/.

\(fn &optional DIR)" t nil)

(autoload 'open-pdf-of-current-file "my-misc-defuns" "\
Opens a pdf file of the same name as the current file" t nil)

(autoload 'eww-open-html-of-current-file "my-misc-defuns" "\
Opens an html file of the same name as the current file" t nil)

(autoload 'indent-region-or-defun-please "my-misc-defuns" "\
Indent region if it is active, otherwise indent defun.  With
prefix arg, indent the whole buffer.

\(fn &optional WHOLE-BUFFER)" t nil)

(autoload 'update-elisp-version "my-misc-defuns" "\
Update a version number in an Elisp file.
Uses the current date formatted as %Y.%m.%d (e.g. 1970.01.01)" t nil)

(autoload 'audacity "my-misc-defuns" "\
Start up audacity, the audio editor.

This runs in a modified environment, with all environment
variables related to input method removed.  This is because
audacity is buggy with these variables.

\(fn &rest ARGS)" t nil)

(fset 'eshell/audacity #'audacity)

;;;***

(provide 'my-misc-defuns-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; my-misc-defuns-autoloads.el ends here
