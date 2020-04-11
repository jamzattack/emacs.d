;;; selime-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "selime" "selime.el" (0 0 0 0))
;;; Generated autoloads from selime.el

(autoload 'selime-describe-function "selime" "\
If package \"helpful\" is installed, call `helpful-callable',
otherwise call `describe-function'" t nil)

(autoload 'selime-describe-variable "selime" "\
If package \"helpful\" is installed, call `helpful-variable',
otherwise call `describe-variable'" t nil)

(autoload 'selime-describe-symbol "selime" "\
If package \"helpful\" is installed, call `helpful-at-point',
otherwise call `describe-symbol' on the symbol at point" t nil)

(autoload 'selime-disassemble "selime" "\
If point is on a function, disassemble it.  Otherwise prompt
for a function to disassemble.

\(fn FUNCTION)" t nil)

(autoload 'selime-macroexpand "selime" "\
Macro expand the following sexp." t nil)

(autoload 'selime-ielm "selime" "\
Open IELM in another window.

\(fn &optional BUFFER-NAME)" t nil)

(autoload 'selime-compile-file "selime" "\
Compiles the the current file without loading it.  With prefix
arg, read file name from minibuffer.

\(fn &optional FILE)" t nil)

(autoload 'selime-compile-and-load-file "selime" "\
Compiles the file that the current buffer is visiting, or
evals the buffer if it isn't visiting a file.  With prefix
arg, read file name from minibuffer.

\(fn &optional FILE)" t nil)

(autoload 'selime-mode "selime" "\
Enable Slime-style documentation for elisp buffers.

If called interactively, enable Selime mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "selime" '("selime-mode-map")))

;;;***

(provide 'selime-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; selime-autoloads.el ends here
