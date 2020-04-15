;;; my-misc-defuns.el --- My miscellaneous functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jamie Beardslee

;; Author: Jamie Beardslee <beardsleejamie@gmail.com>
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

;;; Emacs is sorely missing an interface for "apropos".  This is my
;;; meagre attempt at making it useful within emacs.
;;;###autoload
(defun system-apropos (search &optional args)
  "Run the \"apropos\" comamnd with search term SEARCH and
optional arguments ARGS."
  (interactive (list (read-string "Apropos (regex): ")
		     (when current-prefix-arg
		       (read-string "apropos arguments: "))))
  (let* ((command (or (executable-find "apropos")
		      (user-error "apropos must be installed, usually packaged with man")))
	 (buffer-name (format "*System Apropos %s*" search))
	 (buffer (or (get-buffer buffer-name)
		     (generate-new-buffer buffer-name))))
    (with-current-buffer buffer
      (insert
       (shell-command-to-string (concat command " " args " " search))))
    (switch-to-buffer buffer)))


;;; These two list-* functions open up a dired buffer with a list of
;;; videos/documents.  The package `openwith' might be nice, but I just
;;; use helm to open files externally.
;;;###autoload
(defun list-documents (&optional dir)
  "Using `find-dired', list all the postscript and pdf files a
  specified directory.  If called interactively, prompt for
  Directory. Else, DIR will default to ~/Documents/."
  (interactive (list (read-directory-name "Find videos where: " "~/Documents/")))
  (unless dir
    (setq dir "~/Documents/"))
  (find-dired dir
              "\\( -iname \\*.ps -o -iname \\*.pdf \\)")
  (dired-hide-details-mode t)
  (setq truncate-lines t))

;;;###autoload
(defun list-videos (&optional dir)
  "Using `find-dired', list all the videos a specified directory.
  If called interactively, prompt for Directory. Else, DIR will
  default to ~/Downloads/."
  (interactive (list (read-directory-name "Find videos where: " "~/Downloads/")))
  (unless dir
    (setq dir "~/Downloads/"))
  (find-dired dir
              "\\( -iname \\*.mkv -o -iname \\*.avi -o -iname \\*.mp4 -o -iname \\*.webm -o -iname \\*.m4v \\)")
  (dired-hide-details-mode t)
  (setq truncate-lines t))


;;; Open the pdf file with the same name as the current buffer.
;;; Useful for typesetting programs such as LaTeX, lilypond, ox-latex,
;;; etc.
;;;###autoload
(defun open-pdf-of-current-file ()
  "Opens a pdf file of the same name as the current file"
  (interactive)
  (find-file-other-window (concat
                           (file-name-sans-extension buffer-file-name)
                           ".pdf")))

;;; Similar to `open-pdf-of-current-file' - but open an html file in
;;; eww.  Useful for writing in org-mode and exporting to html.
;;;###autoload
(defun eww-open-html-of-current-file ()
  "Opens an html file of the same name as the current file"
  (interactive)
  (eww-open-file (concat
                  (file-name-sans-extension buffer-file-name)
                  ".html")))


;;; Update locally stored mail with isync and then index it with notmuch.
;;;###autoload 
(defun notmuch-new-async ()
  "Downloads new mail and adds it to the notmuch database"
  (interactive)
  (start-process "notmuch new" "*notmuch new*" "notmuch" "new"))

;;; If region is active, indent it.  Otherwise, indent defun.
;;;###autoload
(defun indent-region-or-defun-please (&optional whole-buffer)
  "Indent region if it is active, otherwise indent defun.  With
prefix arg, indent the whole buffer."
  (interactive "P")
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

(provide 'my-misc-defuns)
;;; my-misc-defuns.el ends here
