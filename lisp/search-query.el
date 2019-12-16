(defun torrentz2-search (query)
  (interactive (list (read-string "Torrent search: ")))
  (browse-url
   (concat "https://torrentz2.eu/search?f=" query)))

(defun youtube-search (query)
  (interactive (list (read-string "Youtube search: ")))
  (browse-url
   (concat "https://m.youtube.com/search?q=" query)))

(defun wikipedia-search (query)
  (interactive (list (read-string "Wikipedia search: ")))
  (browse-url
   (concat "https://wikipedia.org/wiki/" query)))


(provide 'search-query)
