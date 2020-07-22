;;; search-query.el --- Search some websites         -*- lexical-binding: t; -*-

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

;; This file defines some functions to search my most commonly used
;; websites.

;;; Code:

(require 'url-util)
(require 'browse-url)

(defun search-query-eww (url &optional buffer-name)
  "Open URL in an eww buffer named BUFFER-NAME."
  (if buffer-name
      (with-current-buffer (get-buffer-create buffer-name)
	(eww-setup-buffer)
	(eww url))
    (eww url current-prefix-arg)))

;;;###autoload
(defun search-torrentz2 (query)
  "Search torrentz2.eu for QUERY."
  (interactive (list (read-string "Torrent: ")))
  (search-query-eww
   (url-encode-url
    (format "https://torrentz2.eu/search?f=%s" query))
   "*eww torrentz2*"))

(defvar search-query-tpb-mirror "thepiratebay.org"
  "The Pirate Bay URL.")

;;;###autoload
(defun search-tpb (query)
  "Search The Pirate Bay for QUERY.

Uses `search-query-tpb-mirror' as the host."
  (interactive (list (read-string "Torrent: ")))
  (search-query-eww
   (url-encode-url
    (format "https://%s/search/%s/0/99/0"
	    search-query-tpb-mirror
	    query))
   "*eww piratebay*"))

;;;###autoload
(defun search-youtube (query)
  "Search YouTube for QUERY."
  (declare (obsolete "YouTube doesn't work with `eww' anymore --
use `search-invidious' instead." "23 Jul 2020"))
  (interactive (list (read-string "Youtube: ")))
  (search-query-eww
   (url-encode-url
    (format "https://youtube.com/search?q=%s&disable_polymer=1" query))
   "*eww youtube*"))

(defvar search-query-invidious-mirror "invidio.us"
  "Your preferred invidious instance.

For a full list of instances, see
https://github.com/iv-org/invidious/wiki/Invidious-Instances")

;;;###autoload
(defun search-invidious (query)
  "Search `search-query-invidious-mirror' for QUERY.

invidio.us is a more eww-friendly frontend for youtube."
  (interactive (list (read-string "Invidious: ")))
  (search-query-eww
   (url-encode-url
    (format "https://%s/search?q=%s"
	    search-query-invidious-mirror
	    query))
   "*eww invidious*"))

;;;###autoload
(defun search-wikipedia (query &optional language)
  "Search wikipedia for QUERY.

With prefix arg, prompt for LANGUAGE."
  (interactive (list (read-string "Wikipedia: ")
		     (when current-prefix-arg
		       (read-string "Language: "))))
  (search-query-eww
   (url-encode-url
    (format "https://%s.wikipedia.org/wiki/Special:Search?search=%s"
	    (or language
		"en")
	    query))
   "*eww wikipedia*"))

;;;###autoload
(defun search-wiktionary (word &optional language)
  "Search wiktionary for a WORD.

With prefix arg, prompt for LANGUAGE."
  (interactive (list (read-string "Wiktionary: "
				  (thing-at-point 'word t))
		     (when current-prefix-arg
		       (read-string "Language: "))))
  (search-query-eww
   (url-encode-url
    (format "https://%s.wiktionary.org/wiki/%s"
	    (or language
		"en")
	    word))
   "*eww wiktionary*"))

;;;###autoload
(defun search-etymonline (word)
  "Search etymonline.com for a WORD.

Called interactively, prompt for a word with the default input
being the word at point."
  (interactive (list (read-string "Etymology: "
				  (thing-at-point 'word t))))
  (search-query-eww
   (url-encode-url
    (concat "https://www.etymonline.com/word/" word))
   "*eww etymology*"))

;;;###autoload
(defun search-nethack (query)
  "Search nethack wiki for QUERY."
  (interactive (list (read-string "Nethack: ")))
  (search-query-eww
   (url-encode-url
    (format "https://nethackwiki.com/wiki/Special:Search?search=%s"
	    query))
   "*eww nethack*"))

;;;###autoload
(defun search-archwiki (query)
  "Search Arch wiki for QUERY."
  (interactive (list (read-string "Arch Wiki: ")))
  (search-query-eww
   (url-encode-url
    (format "https://wiki.archlinux.org/index.php?search=%s"
	    query))
   "*eww archwiki*"))

(provide 'search-query)
;;; search-query.el ends here
