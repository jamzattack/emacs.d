(defvar dmenu-handler-audio-directory "~/Music"
  "The directory in which to download files using
`dmenu-handler'")

(defvar dmenu-handler-video-directory "~/Videos"
  "The directory in which to download files using
`dmenu-handler'")

(defun dmenu-handler-stream (url &optional flags)
  "Plays video with mpv, provided URL is supported by youtube-dl.
Optional argument FLAGS sets mpv flags; interactively, a prefix
arg prompts for these flags."
  (interactive (list
                (read-string "stream video: " (shr-url-at-point nil))
		(when current-prefix-arg
		  (read-string "mpv flags: "))))
  (start-process-shell-command 
   "mpv stream" " *mpv stream*"
   (concat
    "mpv "
    (if flags
	(concat flags " ")
	"--ytdl-format=22,best --ytdl-raw-options=all-subs= ")
    url))
  (message "%s is being streamed" url))

(defun dmenu-handler-download-video (url &optional directory)
  "Downloads the audio of URL using youtube-dl."
  (interactive (list
                (shr-url-at-point nil)
		(when current-prefix-arg
		  (read-directory-name "Download video where? "
				       dmenu-handler-video-directory))))
  (let ((default-directory (or directory dmenu-handler-video-directory)))
    (start-process-shell-command 
     "youtube-dl download" " *youtube-dl download*"
     (concat "youtube-dl "
	     "--all-subs "
	     "--format=22,best "
	     "--add-metadata "
	     "--output='%(title)s.%(ext)s' "
	     url)))
  (message "%s downloaded in %s" url (or directory dmenu-handler-video-directory)))

(defun dmenu-handler-audio (url &optional directory)
  "Downloads the audio of URL using youtube-dl."
  (interactive (list
                (shr-url-at-point nil)
		(when current-prefix-arg
		  (read-directory-name "Download audio where? "
				       dmenu-handler-audio-directory))))
  (let ((default-directory (or directory dmenu-handler-audio-directory)))
    (start-process-shell-command 
     "youtube-dl audio" " *youtube-dl audio*"
     (concat "youtube-dl "
	     "--extract-audio "
	     "--add-metadata "
	     "--output='%(title)s.%(ext)s' "
	     url)))
  (message "%s downloaded in %s" url (or directory dmenu-handler-audio-directory)))

(defun dmenu-handler-save-to-register (url)
  "Copies the last URL into a register."
  (set-register
   (register-read-with-preview "Copy URL to register: ")
   url))

(defun dmenu-handler-image (url)
  "View URL as an image within emacs"
  (start-process-shell-command
   "dmenu-handler-image" nil
   (concat "curl " url " -o "
	   "/tmp/dmenu-handler-image "
	   "&& emacsclient /tmp/dmenu-handler-image")))

(defun dmenu-handler-pdf (url)
  "View URL as a pdf within emacs"
  (start-process-shell-command
   "dmenu-handler-pdf" nil
   (concat "curl " url " -o "
	   "/tmp/dmenu-handler-pdf "
	   "&& emacsclient /tmp/dmenu-handler-pdf")))

(defun dmenu-handler-read (prompt)
  "Reads input for `dmenu-handler'"
  (completing-read (concat prompt " : ")
                   '("Save to register"
                     "eww"
                     "external browser"
                     "View as image"
                     "Stream"
		     "Download video"
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
  (let ((choice (dmenu-handler-read url))
	(fixed-url (car (split-string url "&"))))
    (cond
     ((equal "Save to register" choice)
      (dmenu-handler-save-to-register fixed-url))
     ((equal "eww" choice)
      (eww fixed-url))
     ((equal "external browser" choice)
      (eww-browse-with-external-browser fixed-url))
     ((equal "View as image" choice)
      (dmenu-handler-image fixed-url))
     ((equal "View as pdf" choice)
      (dmenu-handler-pdf fixed-url))
     ((equal "Stream" choice)
      (dmenu-handler-stream fixed-url))
     ((equal "Download video" choice)
      (dmenu-handler-download-video fixed-url))
     ((equal "Download audio" choice)
      (dmenu-handler-audio fixed-url)))))

(provide 'dmenu-handler)
