;;; my-bitmaps.el --- Some custom fringe bitmaps     -*- lexical-binding: t; -*-

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

;;

;;; Code:

(defun my-bitmaps-enable ()
  "Enable some custom bitmaps."
  (interactive)
  (define-fringe-bitmap 'right-curly-arrow
    [#b00110000
     #b00011000
     #b00001100
     #b00000110
     #b01001100
     #b01111000
     #b01110000
     #b01111000])
  (define-fringe-bitmap 'left-curly-arrow
    [#b00001100
     #b00011000
     #b00110000
     #b01100000
     #b00110010
     #b00011110
     #b00001110
     #b00011110])
  (define-fringe-bitmap 'right-arrow
    [#b01100000
     #b00110000
     #b00011000
     #b00001100
     #b00001100
     #b00011000
     #b00110000
     #b01100000])
  (define-fringe-bitmap 'left-arrow
    [#b00000110
     #b00001100
     #b00011000
     #b00110000
     #b00110000
     #b00011000
     #b00001100
     #b00000110]))

(provide 'my-bitmaps)
;;; my-bitmaps.el ends here
