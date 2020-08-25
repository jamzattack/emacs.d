;;; shavian-input-method.el --- Shavian input method  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jamie Beardslee

;; Author: Jamie Beardslee <jdb@jamzattack.xyz>
;; Keywords: i18n

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

;; Entirely based on https://gitlab.com/endomain/xkb-shavian-variant/
;; https://shavian.weebly.com/uploads/1/0/2/1/10212142/484851_orig.png

;; I can't even read shavian, so I don't know if this works right...

;; According to this page, this is the most common layout:
;; https://shavian.weebly.com/typing-6664166674666406666266649.html

;;; Code:

(require 'quail)

(quail-define-package
 "shavian" "Shavian" "𐑜" t
 "Shavian input method" nil t nil nil t nil nil nil nil nil t)

(quail-define-rules
 ("M" ?𐑵)
 ("m" ?𐑥)
 ("N" ?𐑙)
 ("n" ?𐑯)
 ("B" ?𐑚)
 ("b" ?𐑚)
 ("V" ?𐑿)
 ("v" ?𐑝)
 ("C" ?𐑽)
 ("c" ?𐑗)
 ("X" ?𐑺)
 ("x" ?𐑻)
 ("Z" ?𐑠)
 ("z" ?𐑟)
 ("L" ?𐑤)
 ("l" ?𐑤)
 ("K" ?𐑒)
 ("k" ?𐑒)
 ("J" ?𐑡)
 ("j" ?𐑘)
 ("H" ?𐑞)
 ("h" ?𐑣)
 ("G" ?·)
 ("g" ?𐑜)
 ("F" ?𐑲)
 ("f" ?𐑓)
 ("D" ?𐑼)
 ("d" ?𐑛)
 ("S" ?𐑖)
 ("s" ?𐑕)
 ("A" ?𐑨)
 ("a" ?𐑩)
 ("P" ?𐑹)
 ("p" ?𐑐)
 ("O" ?𐑴)
 ("o" ?𐑪)
 ("I" ?𐑰)
 ("i" ?𐑦)
 ("U" ?𐑫)
 ("u" ?𐑳)
 ("Y" ?𐑷)
 ("y" ?𐑭)
 ("T" ?𐑔)
 ("t" ?𐑑)
 ("R" ?𐑸)
 ("r" ?𐑮)
 ("E" ?𐑱)
 ("e" ?𐑧)
 ("W" ?𐑾)
 ("w" ?𐑢)
 ("Q" ?𐑬)
 ("q" ?𐑶))

(provide 'shavian-input-method)
;;; shavian-input-method.el ends here

