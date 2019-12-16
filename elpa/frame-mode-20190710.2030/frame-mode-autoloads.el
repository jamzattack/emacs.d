;;; frame-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "frame-mode" "frame-mode.el" (0 0 0 0))
;;; Generated autoloads from frame-mode.el

(defvar frame-mode nil "\
Non-nil if Frame mode is enabled.
See the `frame-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `frame-mode'.")

(custom-autoload 'frame-mode "frame-mode" nil)

(autoload 'frame-mode "frame-mode" "\
Minor mode that uses `display-buffer-alist' to ensure that buffers are
displayed using frames intead of windows.

\(fn &optional ARG)" t nil)

(autoload 'frame-mode-other-window "frame-mode" "\
A version of `other-window' that can jump across frames.

COUNT determines the number of windows to move over.

\(fn COUNT)" t nil)

(autoload 'frame-mode-other-window-or-frame-next-command "frame-mode" "\
Use a new frame no matter what when the next call to `display-buffer' occurs.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "frame-mode" '("frame-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; frame-mode-autoloads.el ends here
