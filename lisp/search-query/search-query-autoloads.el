;;; search-query-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "search-query" "search-query.el" (0 0 0 0))
;;; Generated autoloads from search-query.el

(autoload 'torrentz2-search "search-query" "\
Search torrentz2.eu. Called interactively, prompt for a
QUERY.

\(fn QUERY)" t nil)

(autoload 'tpb-search "search-query" "\
Search The Pirate Bay for QUERY.  Uses `tpb-mirror' as the
host.

\(fn QUERY)" t nil)

(autoload 'youtube-search "search-query" "\
Search youtube. Called interactively, prompt for a QUERY.

\(fn QUERY)" t nil)

(autoload 'wikipedia-search "search-query" "\
Search wikipedia.  Called interactively, prompt for a QUERY.
With prefix arg LANGUAGE, prompt for language code.

\(fn QUERY &optional LANGUAGE)" t nil)

(autoload 'wiktionary-word "search-query" "\
Search wiktionary for a word.  Called interactively, prompt
for a WORD with the default input being the word at point.  With
prefix arg LANGUAGE, prompt for language code.

\(fn WORD &optional LANGUAGE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "search-query" '("tpb-mirror")))

;;;***

(provide 'search-query-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; search-query-autoloads.el ends here
