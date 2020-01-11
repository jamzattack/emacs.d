(deftheme custom
  "Not much but changing the background colour to off-white")

(let ((class '((class color) (min-colors 89)))
      (almost-white "#FFFFD0"))
  (custom-theme-set-faces
   'custom
   `(default ((,class (:background ,almost-white :foreground "black"))))
   `(border-color ((,class (:inherit 'default))))
   `(fringe ((,class (:inherit 'default))))
   `(line-number-current-line ((,class (:inherit 'secondary-selection))))
   `(secondary-selection ((,class (:background "khaki1" :extend t))))
   `(scroll-bar ((,class (:foreground "peru" :background ,almost-white :box nil))))
   `(helm-source-header ((,class (:inherit 'bold))))))

(provide-theme 'custom)
