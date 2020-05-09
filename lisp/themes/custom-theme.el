(deftheme custom
  "Not much but changing the background colour to off-white")

(let ((class '((class color) (min-colors 89)))
      (almost-white "#FFFFD0"))
  (custom-theme-set-faces
   'custom
   `(default ((,class (:background ,almost-white :foreground "black" :weight normal))))
   `(line-number-current-line ((,class (:inherit 'secondary-selection))))
   `(secondary-selection ((,class (:background "khaki1" :extend t))))
   `(helm-source-header ((,class (:inherit 'bold))))
   `(tooltip ((,class (:inherit 'default))))
   ;; `(scroll-bar ((,class (:foreground "peru" :background ,almost-white :box nil))))
   `(region ((,class (:background "lightgoldenrod2"))))
   `(fringe ((,class (:inherit 'default))))
   `(mode-line ((,class (:background "sky blue"))))
   `(dired-async-mode-message ((,class (:foreground "darkred"))))
   `(tab-bar ((,class (:inherit mode-line-inactive :box nil))))
   `(tab-bar-tab ((,class (:inherit default))))
   `(tab-bar-tab-inactive ((,class (:inherit mode-line-inactive :box nil))))))

(provide-theme 'custom)
