(deftheme custom
  "Nothing but changing the background colour to off-white")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'your-mum
   `(border-color ((,class (:background "#FFFFD0"))))
   `(default ((,class (:background "#FFFFD0" :foreground "black"))))
   `(fringe ((,class (:background "#FFFFD0"))))))

(provide-theme 'custom)
