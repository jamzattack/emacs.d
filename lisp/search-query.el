(defun torrentz2-search (query)
  (interactive (list (read-string "Torrent: ")))
  (browse-url
   (concat "https://torrentz2.eu/search?f=" query)))

(defun youtube-search (query)
  (interactive (list (read-string "Youtube: ")))
  (browse-url
   (concat "https://m.youtube.com/search?q=" query)))

(defun wikipedia-search (query)
  (interactive (list (read-string "Wikipedia: ")))
  (browse-url
   (concat "https://en.wikipedia.org/wiki/Special:Search?search=" query)))

(defun wiktionary-word (word)
  (interactive (list (thing-at-point 'word t)))
  (browse-url
   (concat "https://en.wiktionary.org/wiki/" word)))

(provide 'search-query)
