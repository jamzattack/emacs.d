;;; plumb-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "plumb" "plumb.el" (0 0 0 0))
;;; Generated autoloads from plumb.el

(autoload 'plumb-stream "plumb" "\
Plays video with mpv, provided URL is supported by youtube-dl.
Optional argument FLAGS sets mpv flags; interactively, a prefix
arg prompts for these flags.

\(fn URL &optional FLAGS)" t nil)

(autoload 'plumb-download-video "plumb" "\
Downloads the audio of URL using youtube-dl.

\(fn URL &optional DIRECTORY)" t nil)

(autoload 'plumb-audio "plumb" "\
Downloads the audio of URL using youtube-dl.

\(fn URL &optional DIRECTORY)" t nil)

(autoload 'plumb "plumb" "\
Select a way to use a URL

\(fn URL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "plumb" '("plumb-")))

;;;***

(provide 'plumb-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; plumb-autoloads.el ends here
