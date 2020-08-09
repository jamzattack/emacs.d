;;; text-mode-abbrevs.el --- My text-mode abbrevs    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jamie Beardslee

;; Author: Jamie Beardslee <jdb@jamzattack.xyz>
;; Keywords: abbrev

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

;; This is just a file containing my text-mode abbrevs

;;; Code:

(dolist (thing '(my-english-words-abbrevs
		 my-english-phrase-abbrevs
		 my-music-dynamic-abbrevs
		 my-music-instrument-abbrevs
		 my-music-harmony-abbrevs
		 my-music-misc-abbrevs))
  (makunbound thing))

(defvar my-music-dynamic-abbrevs
  '(("dyn" "dynamics")
    ("dmp" "/mezzo piano/")
    ("dp" "/piano/")
    ("dpp" "/pianissimo/")
    ("dppp" "/pianississimo/")
    ("dmf" "/mezzo forte/")
    ("df" "/forte/")
    ("dff" "/fortissimo/")
    ("dfff" "/fortississimo/"))
  "Abbrevs for music dynamics.

Dynamics are prefixed with \"d\" and are surrounded by \"/\" for
org-mode italicisation.

e.g. dpp -> /pianissimo/.")

(defvar my-music-instrument-abbrevs
  '(("ins" "instrument")
    ("inss" "instruments")
    ("instn" "instrumentation")
    ("orch" "orchestra")
    ("str" "string")
    ("strs" "strings")
    ("stri" "string instrument")
    ("stris" "string instruments")
    ("strq" "string quartet")

    ("vln" "violin")
    ("vla" "viola")
    ("vc" "cello")
    ("ww" "woodwind")
    ("brs" "brass")

    ("pno" "piano")
    ("satb" "SATB")
    ("S" "soprano")
    ("A" "alto")
    ("T" "tenor")
    ("B" "bass"))
  "Abbrevs for musical instruments.")

(defvar my-music-harmony-abbrevs
  '(("mdn" "modulation")
    ("mdns" "modulations")
    ("mdt" "modulate")
    ("mdts" "modulates")
    ("chrm" "chromatic")
    ("vl" "voice leading")
    ("fph" "four-part harmony")
    ("hmny" "harmony")
    ("hmnc" "harmonic")
    ("mldy" "melody")
    ("mldc" "melodic")

    ("prl" "parallel")
    ("ctry" "contrary")
    ("ped" "pedal")

    ("accd" "accidental")
    ("rsed" "raised")
    ("lwed" "lowered")

    ("dom" "dominant")
    ("sdom" "subdominant")
    ("pdom" "predominant")
    ("fng" "functioning")
    ("fnl" "functional")
    ("fn" "function")
    ("fns" "functions")

    ("susn" "suspension")
    ("susd" "suspended")
    ("sus4" "suspended fourth")
    ("diss" "dissonant")

    ("embn" "embellishing note")
    ("embns" "embellinging notes")
    ("nbn" "neighbour note")
    ("nbns" "neighbour notes")
    ("ant" "anticipate")
    ("antn" "anticipation")
    ("cons" "consonant")

    ("pat" "pattern")
    ("pats" "patterns")
    ("ans" "analysis"))
  "Abbrevs for musical terms related to harmony.")

(defvar my-music-misc-abbrevs
  '(("msc" "music")
    ("clsc" "classical")
    ("ag" "Avant-Garde")
    ("brq" "Baroque")
    ("rns" "Renaissance")
    ("op" "Opus")
    ("nr" "№")
    ("mvmt" "movement"))
  "Miscellaneous music-related abbrevs.")

(defvar my-english-word-abbrevs
  '(("i" "I" )
    ("ds" "does")
    ("n" "and")
    ("r" "are")
    ("rt" "return")
    ("sd" "should")
    ("th" "there")
    ("u" "you")
    ("ur" "your")
    ("ure" "you are")
    ("w" "want")
    ("hv" "have")
    ("k" "know")
    ("ab" "about")
    ("dn" "down")
    ("sts" "sometimes")
    ("thx" "thanks")
    ("misc" "miscellaneous"))
  "Abbrevs for common typos and simplifications of single
words.")

(defvar my-english-phrase-abbrevs
  '(("afaik" "as far as i know")
    ("atm" "at the moment")
    ("btw" "by the way")
    ("cnt" "can't")
    ("dnt" "don't")
    ("ddnt" "didn't")
    ("dsnt" "doesn't")
    ("hnt" "haven't")
    ("hs" "here's")
    ("ie" "i.e.")
    ("iirc" "if I recall correctly")
    ("imo" "in my opinion")
    ("tbf" "to be fair")
    ("im" "I'm")
    ("isnt" "isn't")
    ("pov" "point of view")
    ("sa" "See also:")
    ("shnt" "shouldn't")
    ("ths" "this is")
    ("ti" "that is,")
    ("tr" "there are")
    ("ts" "there is")
    ("ty" "thank you")
    ("uc" "you see")
    ("ull" "you'll")
    ("uv" "you've")
    ("wnt" "won't")
    ("wrt" "with respect to")
    ("wsnt" "wasn't")
    ("wtdb" "What's the difference between")
    ("evt" "every time")))

(define-abbrev-table 'text-mode-abbrev-table
  `(
    ;; English
    ,@my-english-word-abbrevs
    ,@my-english-phrase-abbrevs
    ;; Music
    ,@my-music-dynamic-abbrevs
    ,@my-music-instrument-abbrevs
    ,@my-music-harmony-abbrevs
    ,@my-music-misc-abbrevs
    ))

(provide 'text-mode-abbrevs)
;;; text-mode-abbrevs.el ends here
