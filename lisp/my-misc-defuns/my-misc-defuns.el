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
  "Opens a pdf file of the same name as the current file"
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
(defun indent-region-or-defun-please ()
  "Indent region if it is active, otherwise indent defun.  With
prefix arg, indent the whole buffer."
  (interactive)
  (let ((bounds (cond
  		 (current-prefix-arg
  		  (cons (point-min) (point-max)))
  		 ((region-active-p)
  		  (car (region-bounds)))
  		 (t (bounds-of-thing-at-point 'defun)))))
    (indent-region (car bounds) (cdr bounds))))

(provide 'my-misc-defuns)
