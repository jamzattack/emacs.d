;;; plumb.el --- Interactively select how to use a URL.  -*- lexical-binding: t; -*- 

;; Copyright (C) 2020  Jamie Beardslee

;; Author: Jamie Beardslee <beardsleejamie@gmail.com>
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package defines a few functions to handle a URL.  The main
;; entry-point `plumb' prompts for an action to run on the URL at
;; point.

;;; Code:

(defvar plumb-audio-directory "~/Music"
  "The directory in which to download files using `plumb'")

(defvar plumb-video-directory "~/Videos"
  "The directory in which to download files using `plumb'")

;;;###autoload
(defun plumb-stream (url &optional flags)
  "Plays video with mpv, provided URL is supported by youtube-dl.
Optional argument FLAGS sets mpv flags; interactively, a prefix
arg prompts for these flags."
  (interactive (list
                (read-string "stream video: " (or (shr-url-at-point nil)
						  (thing-at-point 'url t)))
		(when current-prefix-arg
		  (read-string "mpv flags: "))))
  (start-process "mpv stream" " *mpv stream*"
		 "mpv"
		 (or flags
		     "--ytdl-format=22,best --ytdl-raw-options=all-subs=")
		 url)
  (message "%s is being streamed" url))

;;;###autoload
(defun plumb-download-video (url &optional directory)
  "Downloads the audio of URL using youtube-dl."
  (interactive (list
                (shr-url-at-point nil)
		(when current-prefix-arg
		  (read-directory-name "Download video where? "
				       plumb-video-directory))))
  (let ((default-directory (or directory plumb-video-directory)))
    (start-process "youtube-dl download" " *youtube-dl download*"
		   "youtube-dl"
		   "--all-subs"
		   "--format=22,best"
		   "--add-metadata"
		   "--output=%(title)s.%(ext)s' "
		   url))
  (message "%s downloaded in %s" url (or directory plumb-video-directory)))

;;;###autoload
(defun plumb-audio (url &optional directory)
  "Downloads the audio of URL using youtube-dl."
  (interactive (list
                (shr-url-at-point nil)
		(when current-prefix-arg
		  (read-directory-name "Download audio where? "
				       plumb-audio-directory))))
  (let ((default-directory (or directory plumb-audio-directory)))
    (start-process "youtube-dl audio" " *youtube-dl audio*"
		   "youtube-dl"
		   "--extract-audio"
		   "--add-metadata"
		   "--output=%(title)s.%(ext)s"
		   url))
  (message "%s downloaded in %s" url (or directory plumb-audio-directory)))

;;;###autoload
(defun plumb-save-to-register (url)
  "Copies the last URL into a register."
  (set-register
   (register-read-with-preview "Copy URL to register: ")
   url))

;;;###autoload
(defun plumb-image (url)
  "View URL as an image within emacs"
  (start-process-shell-command
   "plumb-image" nil
   (concat "curl " url " -o "
	   "/tmp/plumb-image "
	   "&& emacsclient /tmp/plumb-image")))

;;;###autoload
(defun plumb-pdf (url)
  "View URL as a pdf within emacs"
  (start-process-shell-command
   "plumb-pdf" nil
   (concat "curl " url " -o "
	   "/tmp/plumb-pdf "
	   "&& emacsclient /tmp/plumb-pdf")))

;;;###autoload
(defun plumb-read (prompt)
  "Reads input for `plumb'"
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
(defun plumb-get-url ()
  "Get URL at point or from minibuffer"
  (or (thing-at-point 'url t)
      (read-string "URL: ")))

;;;###autoload
(defun plumb (url)
  "Select a way to use a URL"
  (interactive (list (plumb-get-url)))
  (let ((choice (plumb-read url)))
    (pcase choice
      ("Save to register"
       (plumb-save-to-register url))
      ("eww"
       (eww url))
      ("external browser"
       (eww-browse-with-external-browser url))
      ("View as image"
       (plumb-image url))
      ("View as pdf"
       (plumb-pdf url))
      ("Stream"
       (plumb-stream url))
      ("Download video"
       (plumb-download-video url))
      ("Download audio"
       (plumb-audio url)))))

(provide 'plumb)
;;; plumb.el ends here
