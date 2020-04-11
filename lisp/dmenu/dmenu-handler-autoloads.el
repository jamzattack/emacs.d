;;; dmenu-handler-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "dmenu-handler" "dmenu-handler.el" (0 0 0 0))
;;; Generated autoloads from dmenu-handler.el

(autoload 'dmenu-handler-stream "dmenu-handler" "\
Plays video with mpv, provided URL is supported by youtube-dl.
Optional argument FLAGS sets mpv flags; interactively, a prefix
arg prompts for these flags.

\(fn URL &optional FLAGS)" t nil)

(autoload 'dmenu-handler-download-video "dmenu-handler" "\
Downloads the audio of URL using youtube-dl.

\(fn URL &optional DIRECTORY)" t nil)

(autoload 'dmenu-handler-audio "dmenu-handler" "\
Downloads the audio of URL using youtube-dl.

\(fn URL &optional DIRECTORY)" t nil)

(autoload 'dmenu-handler-save-to-register "dmenu-handler" "\
Copies the last URL into a register.

\(fn URL)" nil nil)

(autoload 'dmenu-handler-image "dmenu-handler" "\
View URL as an image within emacs

\(fn URL)" nil nil)

(autoload 'dmenu-handler-pdf "dmenu-handler" "\
View URL as a pdf within emacs

\(fn URL)" nil nil)

(autoload 'dmenu-handler-read "dmenu-handler" "\
Reads input for `dmenu-handler'

\(fn PROMPT)" nil nil)

(autoload 'dmenu-handler-get-url "dmenu-handler" "\
Get URL at point or from minibuffer" nil nil)

(autoload 'dmenu-handler "dmenu-handler" "\
Select a way to use a URL

\(fn URL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dmenu-handler" '("dmenu-handler-")))

;;;***

(provide 'dmenu-handler-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dmenu-handler-autoloads.el ends here
