(defun torrentz2-search (query)
  "Search torrentz2.eu. Called interactively, prompt for a
QUERY."
  (interactive (list (read-string "Torrent: ")))
  (browse-url
   (concat "https://torrentz2.eu/search?f=" query)))

(defun tpd-search (query)
  "Search thepiratebay.org for QUERY."
  (interactive (list (read-string "Torrent: ")))
  (browse-url
   (format "https://thepiratebay.org/search/%s/0/99/0" query)))

(defun youtube-search (query)
  "Search youtube. Called interactively, prompt for a QUERY."
  (interactive (list (read-string "Youtube: ")))
  (browse-url
   (concat "https://m.youtube.com/search?q=" query)))

(defun wikipedia-search (query)
  "Search wikipedia. Called interactively, prompt for a QUERY."
  (interactive (list (read-string "Wikipedia: ")))
  (browse-url
   (concat "https://en.wikipedia.org/wiki/Special:Search?search=" query)))

(defun wiktionary-word (word)
  "Search wiktionary for a word. With a prefix arg, prompt for a
word, otherwise use the word at point."
  (interactive (list (if current-prefix-arg
			 (read-string "Wiktionary: ")
		       (thing-at-point 'word t))))
  (browse-url
   (concat "https://en.wiktionary.org/wiki/" word)))

(provide 'search-query)
