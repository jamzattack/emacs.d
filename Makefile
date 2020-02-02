emacs-eval=emacs -Q --batch --load ob-tangle --eval


lisp/config.elc: lisp/config.el
	$(emacs-eval) '(byte-compile-file "lisp/config.elc")'

lisp/config.el:
	$(emacs-eval) '(org-babel-tangle-file "config.org")'
