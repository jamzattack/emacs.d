;;; my-misc-defuns-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "my-misc-defuns" "my-misc-defuns.el" (24191
;;;;;;  18020 485305 16000))
;;; Generated autoloads from my-misc-defuns.el

(autoload 'system-apropos "my-misc-defuns" "\
Run the \"apropos\" comamnd with search term SEARCH and
optional arguments ARGS.

\(fn SEARCH &optional ARGS)" t nil)

(autoload 'list-documents "my-misc-defuns" "\
Using `find-dired', list all the postscript and pdf files a
  specified directory.  If called interactively, prompt for
  Directory. Else, DIR will default to ~/Documents/.

\(fn &optional DIR)" t nil)

(autoload 'list-videos "my-misc-defuns" "\
Using `find-dired', list all the videos a specified directory.
  If called interactively, prompt for Directory. Else, DIR will
  default to ~/Downloads/.

\(fn &optional DIR)" t nil)

(autoload 'open-pdf-of-current-file "my-misc-defuns" "\
Opens a pdf file of the same name as the current file" t nil)

(autoload 'eww-open-html-of-current-file "my-misc-defuns" "\
Opens a pdf file of the same name as the current file" t nil)

(autoload 'notmuch-new-async "my-misc-defuns" "\
Downloads new mail and adds it to the notmuch database" t nil)

(autoload 'indent-region-or-defun-please "my-misc-defuns" "\


\(fn &optional COLUMN)" t nil)

;;;***

(provide 'my-misc-defuns-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; my-misc-defuns-autoloads.el ends here
