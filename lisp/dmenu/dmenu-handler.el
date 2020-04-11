(defvar dmenu-handler-audio-directory "~/Music"
  "The directory in which to download files using
`dmenu-handler'")

(defvar dmenu-handler-video-directory "~/Videos"
  "The directory in which to download files using
`dmenu-handler'")

;;;###autoload
(defun dmenu-handler-stream (url &optional flags)
  "Plays video with mpv, provided URL is supported by youtube-dl.
Optional argument FLAGS sets mpv flags; interactively, a prefix
arg prompts for these flags."
  (interactive (list
                (read-string "stream video: " (or (shr-url-at-point nil)
						  (thing-at-point 'url t)))
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun dmenu-handler-save-to-register (url)
  "Copies the last URL into a register."
  (set-register
   (register-read-with-preview "Copy URL to register: ")
   url))

;;;###autoload
(defun dmenu-handler-image (url)
  "View URL as an image within emacs"
  (start-process-shell-command
   "dmenu-handler-image" nil
   (concat "curl " url " -o "
	   "/tmp/dmenu-handler-image "
	   "&& emacsclient /tmp/dmenu-handler-image")))

;;;###autoload
(defun dmenu-handler-pdf (url)
  "View URL as a pdf within emacs"
  (start-process-shell-command
   "dmenu-handler-pdf" nil
   (concat "curl " url " -o "
	   "/tmp/dmenu-handler-pdf "
	   "&& emacsclient /tmp/dmenu-handler-pdf")))

;;;###autoload
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

;;;###autoload
(defun dmenu-handler-get-url ()
  "Get URL at point or from minibuffer"
  (or (thing-at-point 'url t)
      (read-string "URL: ")))

;;;###autoload
(defun dmenu-handler (url)
  "Select a way to use a URL"
  (interactive (list (dmenu-handler-get-url)))
  (let ((choice (dmenu-handler-read url)))
    (pcase choice
     ("Save to register"
      (dmenu-handler-save-to-register url))
     ("eww"
      (eww url))
     ("external browser"
      (eww-browse-with-external-browser url))
     ("View as image"
      (dmenu-handler-image url))
     ("View as pdf"
      (dmenu-handler-pdf url))
     ("Stream"
      (dmenu-handler-stream url))
     ("Download video"
      (dmenu-handler-download-video url))
     ("Download audio"
      (dmenu-handler-audio url)))))

(provide 'dmenu-handler)
