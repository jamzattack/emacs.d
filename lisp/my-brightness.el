;;; brightness.el --- tiled window manager for emacs

;; Copyright (C) 2019 Jamie Beardslee

;; Author: Jamie beardslee <beardsleejamie@gmail.com>
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

;;

;;; Code:

(defvar brightness-percentage nil)

(defvar brightness-max 15)
(defvar brightness-value-file
  "/sys/class/backlight/acpi_video0/actual_brightness")

(defun brightness-get-value ()
  "reads `brightness-value-file' and changes it to a number"
  (float (string-to-number
          (shell-command-to-string
           (concat "cat " brightness-value-file)))))

(defun brightness-convert-to-percentage ()
  (truncate
   (* 100 (/ (brightness-get-value) brightness-max))))

(defun get-brightness ()
  (interactive)
  "Displays brightess as a percentage"
  (message "Brightness level: %s"
           (concat
            (number-to-string
             (brightness-convert-to-percentage)) "%")))

(provide 'my-brightness)


