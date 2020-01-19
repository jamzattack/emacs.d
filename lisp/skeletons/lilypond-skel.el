;;; lilypond-skel.el --- Provides an auto-insert skeleton for LilyPond-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author: Jamie Beardslee <beardsleejamie@gmail.com>
;; Keywords: abbrev, skeleton, lilypond

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

(require 'skeleton)

(define-skeleton lilypond-auto-insert
  "Insert boilerplate lilypond text."
  nil
  "\\version \"2.19\"" \n
  "\\language \"english\"" \n \n

  "\\header {" \n >
  
  ("Title: "
   "\\title \"" str "\"" \n)
  resume:
  ("Composer: "
   "\\composer \"" str "\"" \n

   "}" \n \n)
  
  ("Instrument: "
   str "= {" \n _ \n 
   "}" \n \n

   "\\score {" \n
   "\\new Staff \\with {" \n \n
   > "} { \\" str " }" \n
   "\\layout { }" \n
   "\\midi { }" \n
   "}"))

(with-eval-after-load 'auto-insert
  (add-to-list 'auto-insert-alist
               '(("\\.ly\\'" . "Lilypond file")
                 . lilypond-auto-insert)))

(provide 'lilypond-skel)
;;; lilypond-skel.el ends here
