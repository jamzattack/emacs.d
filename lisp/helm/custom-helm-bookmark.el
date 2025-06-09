;;; custom-helm-bookmark.el --- Quickly define sources for `helm-bookmark-jump'  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025  Jamie Beardslee

;; Author: Jamie Beardslee <jdb@jamzattack.xyz>
;; Version: 2025.06.09
;; Keywords: convenience, helm, completion

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

;; This package defines a macro to create Helm bookmark sources
;; quickly and easily.  It also uses the macro to create a few sources
;; that I use.

;; Although the macro `helm-bookmark-create-source-please' defines all
;; the necessary functions and variables, they are not enabled by
;; `helm-bookmark-jump'.  The following is how I enable these:
;;
;; (custom-set-variables
;;  '(helm-bookmark-default-filtered-sources
;;    '(helm-source-bookmark-university
;;      helm-source-bookmark-config
;;      helm-source-bookmark-org-misc
;;      helm-source-bookmark-elisp
;;      helm-source-bookmark-downloads
;;      helm-source-bookmark-dired
;;      helm-source-bookmark-info
;;      helm-source-bookmark-man
;;      helm-source-bookmark-other
;;      helm-source-bookmark-set)))


;;; Code:

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
 (or (file-in-directory-p filename "~/Documents/uni")
     (file-in-directory-p filename "~/Documents/UNI")))

;;; Config files -- matches all org-mode files in ~/org/config/, as
;;; well as anything with "config" in the name.
(helm-bookmark-create-source-please
 config "Org-mode config files"
 (and (string-suffix-p ".org" filename t)
      (or (file-in-directory-p
	   filename "~/org/config/")
	  (string-match-p ".*config.*" (car bookmark)))))

;;; Elisp files -- matches all elisp files
(helm-bookmark-create-source-please
 elisp "Emacs lisp files"
 (string-suffix-p ".el" filename t))

;;; Downloads -- matches anything in ~/Downloads or ~/Videos
(helm-bookmark-create-source-please
 downloads "Downloads/Videos"
 (or (file-in-directory-p filename "~/Downloads/")
     (file-in-directory-p filename "~/Videos/")))

;;; Misc. org-mode files -- only org-mode files that aren't already in
;;; one of the sources.
(helm-bookmark-create-source-please
 org-misc "Miscellaneous org files"
 (and (string-suffix-p ".org" filename t)
      (not (helm-bookmark-config-p bookmark))
      (not (helm-bookmark-downloads-p bookmark))
      (not (helm-bookmark-university-p bookmark))))

;;; Magit
(helm-bookmark-create-source-please
 magit "Git repositories"
 (eq (bookmark-get-handler bookmark) 'magit--handle-bookmark))

;;; Directories
(helm-bookmark-create-source-please
 dired "Bookmarked directories"
 (and (file-directory-p filename)
      (not (helm-bookmark-elfeed-p bookmark))
      (not (helm-bookmark-gnus-p bookmark))
      (not (helm-bookmark-magit-p bookmark))))

;;; Elfeed
(helm-bookmark-create-source-please
 elfeed "Elfeed entries and searches"
 (or (eq (bookmark-get-handler bookmark) 'elfeed-show-bookmark-handler)
     (eq (bookmark-get-handler bookmark) 'elfeed-search-bookmark-handler)))

;;; Mail
;; Gnus can create "normal" bookmarks, but it also provides a library
;; specifically for gnus bookmarks.  I'm on the fence about which to use.
(helm-bookmark-create-source-please
 gnus "Gnus Articles"
 (eq (bookmark-get-handler bookmark) 'gnus-summary-bookmark-jump))

;;; Other bookmarks
(helm-bookmark-create-source-please
 other "Other bookmarks"
 (and (not (file-directory-p filename))
      (cl-loop for pred in '(helm-bookmark-gnus-p
			     helm-bookmark-university-p
			     helm-bookmark-elisp-p
			     helm-bookmark-config-p
			     helm-bookmark-org-misc-p
			     helm-bookmark-downloads-p
			     helm-bookmark-dired-p
			     helm-bookmark-magit-p
			     helm-bookmark-info-bookmark-p
			     helm-bookmark-man-bookmark-p
			     helm-bookmark-elfeed-p)
               never (funcall pred bookmark))))


(provide 'custom-helm-bookmark)

;;; custom-helm-bookmark.el ends here
