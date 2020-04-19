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
  "The directory in which to download music using `plumb'")

(defvar plumb-video-directory "~/Videos"
  "The directory in which to download videos using `plumb'")

(defvar plumb-image-directory "~/Pictures"
  "The directory in which do download images using `plumb'")

(defvar plumb-pdf-directory "~/Documents"
  "The directory in which do download pdfs using `plumb'")

(defvar plumb-mpv-default-flags
  '("--ytdl-format=22,best"
    "--ytdl-raw-options=all-subs="
    "--speed=1.30"))

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
  (let ((flags
	 (if (stringp flags)
	     (split-string flags " ")
	   plumb-mpv-default-flags)))
    (eval
     `(start-process "mpv stream" " *mpv stream*"
		     "mpv"
		     ,@flags
		     ,url)))
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

(defun plumb-save-to-register (url)
  "Copies the last URL into a register."
  (set-register
   (register-read-with-preview "Copy URL to register: ")
   url))

(defun plumb-download-and-view (url &optional directory)
  "Download URL in either DIRECTORY or ~/Downloads, and then open it.
Used by `plumb-image' and `plumb-pdf'."
  (let* ((file (expand-file-name
		(file-name-nondirectory url)
		(or directory "~/Downloads"))))
    (url-retrieve url
		  (lambda (_status)
		    (goto-char (point-min))
		    (re-search-forward "\r?\n\r?\n")
		    (write-region (point) (point-max) file)
		    (find-file file)))
    file))

(defun plumb-plain (url)
  "Download and open URL directly.  This should work with any
  filetype in `auto-mode-alist'."
  (plumb-download-and-view url))

(defun plumb-image (url)
  "View URL as an image within Emacs."
  (plumb-download-and-view url plumb-image-directory))

(defun plumb-pdf (url)
  "View URL as a pdf within Emacs."
  (plumb-download-and-view url plumb-pdf-directory))

(defun plumb-read (prompt)
  "Reads input for `plumb'"
  (completing-read (concat prompt " : ")
                   '("Save to register"
                     "eww"
                     "external browser"
                     "View as image"
                     "Stream"
		     "Download and open"
		     "Download video"
                     "Download audio"
                     "View as pdf")))

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
      ("Download and open"
       (plumb-plain url))
      ("Stream"
       (plumb-stream url))
      ("Download video"
       (plumb-download-video url))
      ("Download audio"
       (plumb-audio url)))))

(provide 'plumb)
;;; plumb.el ends here
