(defun ed-find-file (file)
  "Open a file with ed, the standard editor. See ed(5) for usage."
  (let ((command (format "ed %s" file)))
    (async-shell-command command command)))

(ed-find-file "~/.bashrc")
