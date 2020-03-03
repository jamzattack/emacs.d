(defun system-apropos (search &optional args)
  "Run the \"apropos\" comamnd with search term SEARCH and
optional arguments ARGS."
  (interactive (list (read-string "Apropos (regex): ")
		     (when current-prefix-arg
		       (read-string "apropos arguments: "))))
  (let* ((command (or (executable-find "apropos")
		      (executable-find "man")
		      (user-error "Man or apropos must be installed")))
	 (buffer-name (format "*System Apropos %s*" search))
	 (buffer (or (get-buffer buffer-name)
		     (generate-new-buffer buffer-name))))
    (with-current-buffer buffer
      (insert
       (shell-command-to-string (concat command " " args " " search))))
    (switch-to-buffer buffer)))

(provide 'my-misc-defuns)
