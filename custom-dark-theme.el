(deftheme custom-dark
  "dark theme")

(let ((class '((class color) (min-colors 89)))
      (almost-white "#FFFFD0"))
  (custom-theme-set-faces
   'custom-dark
   `(default ((,class (:background "black" :foreground ,almost-white))))
   `(border-color ((,class (:inherit 'default))))
   `(fringe ((,class (:inherit 'default))))
   `(region ((,class (:background "gray15"))))
   `(line-number-current-line ((,class (:inherit 'secondary-selection))))
   `(secondary-selection ((,class (:background "gray9" :extend t))))
   `(scroll-bar ((,class (:foreground ,almost-white :background "black"))))
   `(helm-source-header ((,class (:inherit 'bold))))))

(provide-theme 'custom-dark)
