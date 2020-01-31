;;; ed.el --- a decent editor within GNU Emacs
;; Copyright (C) 2018 Jamie Beardslee

;; Author: Jamie Beardslee <beardsleejamie@gmail.com>
;; Keywords: convenience

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

;; This package provides `ed-find-file', an interface for opening
;; files in ed(1), the standard editor.
;;
;; Recommendation: (global-set-key [remap find-file] 'ed-find-file)

;;; Code:


(defvar ed-use-prompt t
  "If t, change the prompt to \"> \". This will only work with
posix-compliant implementations of ed. Set this to nil if you
use busybox.")


(defun ed-find-file (file)
  "Open a file with ed, the standard editor. See ed(1) for usage."
  (interactive "F")
  (let ((args `(,(when ed-use-prompt
		   "-p> ")
		,file))
	(basename (file-name-base file)))
    (comint-run "ed" args)
    (cd (file-name-directory file))
    (rename-buffer basename t)))

(provide 'ed)
