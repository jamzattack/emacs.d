(defun dmenu-handler-stream (url)
  "Plays video with mpv, provided URL is supported by
         youtube-dl."
  (interactive (list
                (if current-prefix-arg
                    (read-file-name "file or url: ")
                  (shr-url-at-point current-prefix-arg))))
  (start-process-shell-command
   "mpv stream" " *mpv stream*"
   "youtube-dl -o - "
   "--all-subs -f 22"
   url
   "| mpv -"))

(defun dmenu-handler-save-to-register (url)
  "Copies the last URL into a register."
  (set-register
   (register-read-with-preview "Copy URL to register: ")
   url))

(defun dmenu-handler-image (url)
  "View URL as an image within emacs"
  (start-process-shell-command "dmenu-handler-image"
                               nil
                               "curl" url "-o"
                               "/tmp/dmenu-handler-image"
                               "&& emacsclient /tmp/dmenu-handler-image"))

(defun dmenu-handler-pdf (url)
  "View URL as a pdf within emacs"
  (start-process-shell-command "dmenu-handler-pdf"
                               nil
                               "curl" url "-o"
                               "/tmp/dmenu-handler-pdf"
                               "&& emacsclient /tmp/dmenu-handler-pdf"))

(defun dmenu-handler-read (prompt)
  "Reads input for `dmenu-handler'"
  (completing-read (concat prompt " : ")
                   '("Save to register"
                     "eww"
                     "external browser"
                     "View as image"
                     "Stream"
                     "Download audio"
                     "View as pdf")))

(defun dmenu-handler-get-url ()
  "Converts fontified kill ring contents into nice plaintext"
  (with-temp-buffer
    (yank)
    (font-lock-mode -1)
    (buffer-string)))

(defun dmenu-handler (url)
  "Select a way to use a URL"
  (interactive (list (dmenu-handler-get-url)))
  (let ((choice (dmenu-handler-read url)))
    (cond
     ((equal "Save to register" choice)
      (dmenu-handler-save-to-register url))
     ((equal "eww" choice)
      (eww url))
     ((equal "external browser" choice)
      (eww-browse-with-external-browser url))
     ((equal "View as image" choice)
      (dmenu-handler-image url))
     ((equal "View as pdf" choice)
      (dmenu-handler-pdf url))
     ((equal "Stream" choice)
      (dmenu-handler-stream))
     ((equal "Download audio" choice)
      (message "not implemented")))))

(provide 'dmenu-handler)
