;;; my-misc-defuns.el --- My miscellaneous functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jamie Beardslee

;; Author: Jamie Beardslee <jdb@jamzattack.xyz>
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



;;; If region is active, indent it.  Otherwise, indent defun.
;;;###autoload
(defun indent-region-or-defun-please (&optional whole-buffer)
  "Indent region if it is active, otherwise indent defun.  With
prefix arg, indent the whole buffer."
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
(defun update-elisp-version ()
  "Update a version number in an Elisp file.
Uses the current date formatted as %Y.%m.%d (e.g. 1970.01.01)"
  (interactive "P")
  (let ((new-version (format-time-string " %Y.%m.%d")))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(re-search-forward "^;; Version:")
	(kill-line)
	(insert new-version)))))


;;;###autoload
(defun audacity (&rest args)
  "Start up audacity, the audio editor.

This runs in a modified environment, with all environment
variables related to input method removed.  This is because
audacity is buggy with these variables."
  (interactive)
  (let ((process-environment
         (cl-remove-if
          (lambda (string)
            (string-match-p "\\(IM_MODULE\\|XMODIFIERS\\)" string))
          process-environment)))
    (make-process
     :name "audacity"
     :buffer " audacity"
     :command `("audacity" ,@args))))

;;;###autoload
(fset 'eshell/audacity #'audacity)

(provide 'my-misc-defuns)
;;; my-misc-defuns.el ends here
