(require 'url-util)
(require 'thing-at-pt)
(require 'browse-url)

;;;###autoload
(defun torrentz2-search (query)
  "Search torrentz2.eu. Called interactively, prompt for a
QUERY."
  (interactive (list (read-string "Torrent: ")))
  (browse-url
   (url-encode-url
    (format "https://torrentz2.eu/search?f=%s" query))))

(defvar tpb-mirror "thepiratebay.org" "The Pirate Bay URL")

;;;###autoload
(defun tpb-search (query)
  "Search The Pirate Bay for QUERY.  Uses `tpb-mirror' as the
host."
  (interactive (list (read-string "Torrent: ")))
  (browse-url
   (url-encode-url
    (format "https://%s/search/%s/0/99/0" tpb-mirror query))))

;;;###autoload
(defun youtube-search (query)
  "Search youtube. Called interactively, prompt for a QUERY."
  (interactive (list (read-string "Youtube: ")))
  (browse-url
   (url-encode-url
    (format "https://m.youtube.com/search?q=%s" query))))

;;;###autoload
(defun wikipedia-search (query &optional language)
  "Search wikipedia.  Called interactively, prompt for a QUERY.
With prefix arg LANGUAGE, prompt for language code."
  (interactive (list (read-string "Wikipedia: ")
		     (when current-prefix-arg
		       (read-string "Language: "))))
  (browse-url
   (url-encode-url
    (format "https://%s.wikipedia.org/wiki/Special:Search?search=%s"
	    (or language
		"en")
	    query))))

;;;###autoload
(defun wiktionary-word (word &optional language)
  "Search wiktionary for a word.  Called interactively, prompt
for a WORD with the default input being the word at point.  With
prefix arg LANGUAGE, prompt for language code."
  (interactive (list (read-string "Wiktionary: " 
				  (thing-at-point 'word t))
		     (when current-prefix-arg
		       (read-string "Language: "))))
  (browse-url
   (url-encode-url
    (format "https://%s.wiktionary.org/wiki/%s"
	    (or language
		"en")
	    word))))

(provide 'search-query)
