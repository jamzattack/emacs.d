;;; maori-input-method.el --- Māori input method for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jamie Beardslee

;; Author: Jamie Beardslee <jdb@jamzattack.xyz>
;; Version: 2020.09.18
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

;; This library defines two simple input methods: `maori-prefix' and
;; `maori-postfix'.  They provide a way to insert Māori macrons atop
;; vowels by either using a hyphen or doubling the vowel.  To get a
;; literal hyphen or vowel, double it.
;;
;; For example, using `maori-postfix':
;; a   -> a
;; aa  -> ā
;; a-  -> ā
;; aaa -> aa
;; a-- -> a-

;;; Code:

(quail-define-package
 "maori-prefix" "Māori" "MA>" t
 "Māori input method with postfix modifiers

A prefixed - or doubled letter will produce an accented
character, e.g. aa -> ā oo -> ō -o -> ō.

Tripling the letter inserts two letters, and -- inserts a real
dash, e.g. aaa -> aa --e -> -e.
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("aa" ?\ā)
 ("AA" ?\Ā)
 ("ee" ?\ē)
 ("EE" ?\Ē)
 ("ii" ?\ī)
 ("II" ?\Ī)
 ("oo" ?\ō)
 ("OO" ?\Ō)
 ("uu" ?\ū)
 ("UU" ?\Ū)

 ("-a" ?\ā)
 ("-A" ?\Ā)
 ("-e" ?\ē)
 ("-E" ?\Ē)
 ("-i" ?\ī)
 ("-I" ?\Ī)
 ("-o" ?\ō)
 ("-O" ?\Ō)
 ("-u" ?\ū)
 ("-U" ?\Ū)

 ("aaa" ["aa"])
 ("AAA" ["Aa"])
 ("eee" ["ee"])
 ("EEE" ["EE"])
 ("iii" ["ii"])
 ("III" ["II"])
 ("ooo" ["oo"])
 ("OOO" ["OO"])
 ("uuu" ["uu"])
 ("UUU" ["UU"])

 ("--a" ["-a"])
 ("--A" ["-A"])
 ("--e" ["-e"])
 ("--E" ["-E"])
 ("--i" ["-i"])
 ("--I" ["-I"])
 ("--o" ["-o"])
 ("--O" ["-O"])
 ("--u" ["-u"])
 ("--U" ["-U"]))

(quail-define-package
 "maori-postfix" "Māori" "MA<" t
 "Māori input method with postfix modifiers

A following - or doubled letter will produce an accented
character, e.g. aa -> ā oo -> ō o- -> ō.

Tripling the letter inserts two letters, and -- inserts a real
dash, e.g. aaa -> aa e-- -> e-.
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("aa" ?\ā)
 ("AA" ?\Ā)
 ("ee" ?\ē)
 ("EE" ?\Ē)
 ("ii" ?\ī)
 ("II" ?\Ī)
 ("oo" ?\ō)
 ("OO" ?\Ō)
 ("uu" ?\ū)
 ("UU" ?\Ū)

 ("a-" ?\ā)
 ("A-" ?\Ā)
 ("e-" ?\ē)
 ("E-" ?\Ē)
 ("i-" ?\ī)
 ("I-" ?\Ī)
 ("o-" ?\ō)
 ("O-" ?\Ō)
 ("u-" ?\ū)
 ("U-" ?\Ū)

 ("aaa" ["aa"])
 ("AAA" ["Aa"])
 ("eee" ["ee"])
 ("EEE" ["EE"])
 ("iii" ["ii"])
 ("III" ["II"])
 ("ooo" ["oo"])
 ("OOO" ["OO"])
 ("uuu" ["uu"])
 ("UUU" ["UU"])

 ("a--" ["a-"])
 ("A--" ["A-"])
 ("e--" ["e-"])
 ("E--" ["E-"])
 ("i--" ["i-"])
 ("I--" ["I-"])
 ("o--" ["o-"])
 ("O--" ["O-"])
 ("u--" ["u-"])
 ("U--" ["U-"]))


(provide 'maori-input-method)
;;; maori-input-method.el ends here
