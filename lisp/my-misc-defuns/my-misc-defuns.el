;;; my-misc-defuns.el --- My miscellaneous functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Jamie Beardslee

;; Author: Jamie Beardslee <jdb@jamzattack.xyz>
;; Version: 2025-03-20
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the file in which I put various uncategorised functions.

;;; Code:

(require 'simple)
(require 'find-dired)
(require 'eww)
(require 'thingatpt)
(require 'url-util)

;;;###autoload
(defun system-apropos (search)
  "Run the \"apropos\" comamnd with search term SEARCH."
  (interactive (list (read-string "Apropos (regex): ")))
  (let* ((program (or (executable-find "apropos")
		      (user-error "`apropos' must be installed, usually packaged with man")))
	 (buffer-name (format "*System Apropos %s*" search))
	 (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(dolist (line (process-lines program "-l" search))
	  (when (string-match "\\(.*\\) - " line)
	    (insert line)
	    (make-button (line-beginning-position) (line-end-position)
			 'action `(lambda (&rest _ignored)
				    (man ,(match-string 1 line)))
			 'face 'default)
	    (newline))))
      (special-mode))
    (pop-to-buffer-same-window buffer)))


;;; Listing fellas
;; These two list-* functions open up a dired buffer with a list of
;; videos/documents.  The package `openwith' might be nice, but I just
;; use helm to open files externally.

;;;###autoload
(defun list-documents (&optional dir)
  "Using `find-dired', list all ps or pdf files in DIR.
If called interactively, prompt for directory.  Else, DIR will
default to ~/Documents/."
  (interactive (list (read-directory-name "Find videos where: " "~/Documents/")))
  (unless dir
    (setq dir "~/Documents/"))
  (find-dired dir
              "-regex \".*\\\\.\\\\(ps\\\\|pdf\\\\)\"")
  (dired-hide-details-mode t)
  (setq truncate-lines t))

;;;###autoload
(defun list-videos (&optional dir)
  "Using `find-dired', list all the videos in DIR.
If called interactively, prompt for directory.  Else, DIR will
default to ~/Downloads/."
  (interactive (list (read-directory-name "Find videos where: " "~/Downloads/")))
  (unless dir
    (setq dir "~/Downloads/"))
  (find-dired dir
              "-regex  \".*\\\\.\\\\(mkv\\\\|mp4\\\\|webm\\\\|avi\\\\|m4v\\\\)\"")
  (dired-hide-details-mode t)
  (setq truncate-lines t))


;;; Typesetting fellas

;;;###autoload
(defun open-pdf-of-current-file ()
  "Open a pdf file of the same name as the current file.
May be useful for typesetting programs such as LaTeX, lilypond,
ox-latex, etc."
  (interactive)
  (let ((file (concat
	       (file-name-sans-extension buffer-file-name)
	       ".pdf")))
    (if (file-exists-p file)
	(find-file-other-window file)
      (message "File doesn't exist: %s" file))))

;;;###autoload
(defun eww-open-html-of-current-file ()
  "Open an html file of the same name as the current file.
May be useful for writing in org-mode and exporting to html."
  (interactive)
  (eww-open-file (concat
                  (file-name-sans-extension buffer-file-name)
                  ".html")))



;;;###autoload
(defun indent-region-or-defun-please (&optional whole-buffer)
  "Indent region if it is active, otherwise indent defun.
With prefix arg WHOLE-BUFFER, indent the whole buffer."
  (interactive "*P")
  (let ((bounds (cond
  		 (whole-buffer
  		  (cons (point-min) (point-max)))
  		 ((region-active-p)
  		  (car (region-bounds)))
  		 (t (or (bounds-of-thing-at-point 'defun)
			(cons (save-excursion
				(backward-paragraph 1)
				(point))
			      (save-excursion
				(forward-paragraph 1)
				(point))))))))
    (indent-region (car bounds) (cdr bounds))))



;;;###autoload
(defun audacity (&rest args)
  "Start up audacity, the audio editor.
This runs in a modified environment, with all environment
variables related to input method removed.  This is because
audacity is buggy with these variables.  All ARGS are passed onto
Audacity."
  (interactive)
  (let ((process-environment
         (cl-remove-if
          (lambda (string)
            (string-match-p "\\(IM_MODULE\\|XMODIFIERS\\)" string))
          process-environment)))
    (make-process
     :name "audacity"
     :buffer " audacity"
     :command (cons "audacity" args))))

;;;###autoload
(fset 'eshell/audacity #'audacity)



;;;###autoload
(defun copy-gpl-here ()
  "Copy the GPL into this directory."
  (interactive)
  (if (file-exists-p "LICENSE")
      (user-error "File \"LICENSE\" already exists"))
  (with-temp-file "LICENSE"
    (insert-file-contents
     (expand-file-name "COPYING" data-directory))))



;;;###autoload
(defun jamzattack-pastebin (beg end &optional name)
  "Upload the active region to a personal pastebin.
The contents of the region BEG and END will be uploaded, or the
whole buffer if the region is inactive.  The file saved is named
NAME, or randomly generated if left empty.  Save the resulting
url in the kill ring."
  (interactive (let ((string
		      (read-string "Paste name (leave empty for autogen): ")))
		 (if (region-active-p)
		     (list (region-beginning) (region-end) string)
		   (list nil nil string))))
  (let* ((string
	  (buffer-substring (or beg (point-min))
			    (or end (point-max))))
	 (dir "/ssh:jamzattack.xyz:/var/www/html/tmp/")
	 (file
	  (if (string-empty-p name)
	      (make-temp-file dir nil ".txt" string)
	    (expand-file-name (if (string-match-p "\\.[a-zA-Z]+\\'" name)
				  name
				(concat name ".txt"))
			      dir)))
	 (url
	  (url-encode-url
	   (format "https://jamzattack.xyz/tmp/%s"
		   (file-name-nondirectory file)))))
    (with-temp-file file
      (insert string))
    (kill-new url)
    (message "Pasted to: %s" url)))


;;;###autoload
(defun find-file-in-source-directory (file &optional position)
  "Find the current emacs file in the source directory.
This should work for any file installed with emacs."
  (interactive (list (buffer-file-name) (point)))
  (let* ((installation-directory
	  (or installation-directory
	      (expand-file-name (format "../share/emacs/%s" emacs-version)
				invocation-directory)))
	 (relative (file-relative-name file installation-directory))
	 (new (replace-regexp-in-string "\\.gz\\'" ""
					(expand-file-name relative source-directory))))
    (if (file-exists-p new)
	(progn (find-file new)
	       (goto-char position))
      (user-error "File `%s' not exist" new))))

(provide 'my-misc-defuns)
;;; my-misc-defuns.el ends here
