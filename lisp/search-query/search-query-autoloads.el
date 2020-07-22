;;; search-query-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "search-query" "search-query.el" (24344 11903
;;;;;;  705351 684000))
;;; Generated autoloads from search-query.el

(autoload 'search-torrentz2 "search-query" "\
Search torrentz2.eu for QUERY.

\(fn QUERY)" t nil)

(autoload 'search-tpb "search-query" "\
Search The Pirate Bay for QUERY.

Uses `search-query-tpb-mirror' as the host.

\(fn QUERY)" t nil)

(autoload 'search-youtube "search-query" "\
Search YouTube for QUERY.

\(fn QUERY)" t nil)

(make-obsolete 'search-youtube '"YouTube doesn't work with `eww' anymore --\nuse `search-invidious' instead." '"23 Jul 2020")

(autoload 'search-invidious "search-query" "\
Search `search-query-invidious-mirror' for QUERY.

invidio.us is a more eww-friendly frontend for youtube.

\(fn QUERY)" t nil)

(autoload 'search-wikipedia "search-query" "\
Search wikipedia for QUERY.

With prefix arg, prompt for LANGUAGE.

\(fn QUERY &optional LANGUAGE)" t nil)

(autoload 'search-wiktionary "search-query" "\
Search wiktionary for a WORD.

With prefix arg, prompt for LANGUAGE.

\(fn WORD &optional LANGUAGE)" t nil)

(autoload 'search-etymonline "search-query" "\
Search etymonline.com for a WORD.

Called interactively, prompt for a word with the default input
being the word at point.

\(fn WORD)" t nil)

(autoload 'search-nethack "search-query" "\
Search nethack wiki for QUERY.

\(fn QUERY)" t nil)

(autoload 'search-archwiki "search-query" "\
Search Arch wiki for QUERY.

\(fn QUERY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "search-query" '("search-query-")))

;;;***

(provide 'search-query-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; search-query-autoloads.el ends here
