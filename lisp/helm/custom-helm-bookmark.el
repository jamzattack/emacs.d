(require 'helm-bookmark)

(defmacro helm-bookmark-create-source-please (name docstring conditions)
  "Create a helm source for helm-filtered-bookmarks.

NAME is a generic name for the source.

DOCSTRING is the name used for (helm-make-source), so it is the
name as seen by (helm-filtered-bookmarks).

CONDITIONS is a single predicate that checks whether to add a
bookmark to a source.  BOOKMARK is the name of the bookmark.  For
convenience, FILENAME is the expanded file name of the bookmark.

An example, for bookmarked info documents in (coreutils):
(helm-bookmark-create-source-please
 coreutils \"Coreutils info pages\"
 (and (string-match-p
       \"\\(coreutils\\).*\"
       (car bookmark)) ; the first element of a bookmark record is its name
      (eq (bookmark-get-handler bookmark) 'Info-bookmark-jump)))

It can then be added to the list `helm-bookmark-default-filtered-sources':
(add-to-list 'helm-bookmark-default-filtered-sources 'helm-source-bookmark-coreutils)

"
  (let* ((name-as-string (symbol-name name))
	  (predicate-name (intern (concat "helm-bookmark-"
					  name-as-string
					  "-p")))
	  (helm-source-name (intern (concat "helm-source-bookmark-" name-as-string)))
	  (alist-function-name (intern (concat "helm-bookmark-"
					       name-as-string
					       "-setup-alist"))))
    `(progn
       (defun ,predicate-name (bookmark)
	 (let* ((filename (expand-file-name (or (bookmark-get-filename bookmark) ""))))
	   ,conditions))
       (defun ,alist-function-name ()
	 ,(concat "Create an alist for " docstring)
	 (helm-bookmark-filter-setup-alist ',predicate-name))
       (defvar ,helm-source-name
	 (helm-make-source ,docstring 'helm-source-filtered-bookmarks
	   :init (lambda ()
		   (bookmark-maybe-load-default-file)
		   (helm-init-candidates-in-buffer
		       'global (,alist-function-name))))))))


;;; University files -- matches anything in ~/Documents/uni
(helm-bookmark-create-source-please
 university "University"
 (string-match-p
  (concat (expand-file-name "~/Documents/uni") ".*")
  filename))

;;; Config files -- matches all org-mode files in ~/org/config/, as
;;; well as anything with "config" in the name.
(helm-bookmark-create-source-please
 config "Org-mode config files"
 (and (string-suffix-p ".org" filename t)
      (or (string-match-p
	   (concat (expand-file-name "~/org/config/") ".*")
	   filename)
	  (string-match-p ".*config.*" (car bookmark)))))

;;; Elisp files -- matches all elisp files
(helm-bookmark-create-source-please
 elisp "Emacs lisp files"
 (string-suffix-p ".el" filename t))

;;; Downloads -- matches anything in ~/Downloads
(helm-bookmark-create-source-please
 downloads "Downloads"
 (string-match-p
  (concat (expand-file-name "~/Downloads/") ".*")
  filename))

;;; Misc. org-mode files -- only org-mode files that aren't already in
;;; one of the sources.
(helm-bookmark-create-source-please
 org-misc "Miscellaneous org files"
 (and (string-suffix-p ".org" filename t)
      (not (helm-bookmark-config-p bookmark))
      (not (helm-bookmark-downloads-p bookmark))
      (not (helm-bookmark-university-p bookmark))))

;;; Directories
(helm-bookmark-create-source-please
 dired "Bookmarked directories"
 (file-directory-p filename))

;;; Files
(helm-bookmark-create-source-please
 other "Other bookmarks"
 (and (not (file-directory-p filename))
      (bookmark-get-filename bookmark)
      (cl-loop for pred in '(helm-bookmark-university-p
			     helm-bookmark-elisp-p
			     helm-bookmark-config-p
			     helm-bookmark-org-misc-p
			     helm-bookmark-downloads-p
			     helm-bookmark-dired-p
			     helm-bookmark-info-bookmark-p
			     helm-bookmark-man-bookmark-p)
               never (funcall pred bookmark))))


(provide 'custom-helm-bookmark)
