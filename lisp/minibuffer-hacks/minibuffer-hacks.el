;;; minibuffer-hacks.el --- Minor minibuffer improvements -*- lexical-binding: t; -*-

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

;; This is a very minimal package.  It just defines two functions at
;; the moment:

;; `increase-minibuffer-size-please': to increase the minibuffer font
;; size
;; `exit-minibuffer-other-window': to exit the minibuffer in another
;; window.

;;; Code:

(defun increase-minibuffer-size-please ()
  "Increase the font size by 1 step."
  (text-scale-increase 1))

(defun exit-minibuffer-other-window ()
  "Exit the minibuffer in another window."
  (interactive)
  (unless (fboundp 'other-window-prefix)
    (error "Emacs 28.0.50 is required"))
  (other-window-prefix)
  (exit-minibuffer))

(provide 'minibuffer-hacks)
;;; minibuffer-hacks.el ends here
