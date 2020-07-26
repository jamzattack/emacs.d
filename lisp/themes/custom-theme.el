(deftheme custom
  "Not much but changing the background colour to off-white")

(let ((class '((class color) (min-colors 89)))
      (almost-white "#FFFFD0"))
  (custom-theme-set-faces
   'custom
   ;; Text
   `(default ((,class (:background ,almost-white :foreground "black" :weight normal))))
   `(line-number-current-line ((,class (:inherit 'secondary-selection))))
   `(secondary-selection ((,class (:background "khaki1" :extend t))))
   `(helm-source-header ((,class (:inherit 'bold))))
   `(font-lock-doc-face ((,class (:slant italic :inherit font-lock-string-face))))
   `(dired-async-mode-message ((,class (:foreground "darkred"))))

   ;; Not really text
   `(tooltip ((,class (:inherit 'default))))
   `(region ((,class (:background "lightgoldenrod2"))))
   `(fringe ((,class (:inherit 'default))))
   `(mode-line ((,class (:background "sky blue"))))
   `(tab-bar ((,class (:inherit mode-line-inactive :box nil))))
   `(tab-bar-tab ((,class (:inherit default))))
   `(tab-bar-tab-inactive ((,class (:inherit mode-line-inactive :box nil))))
   `(ruler-mode-default ((,class (:inherit header-line :box nil))))
   ))

(provide-theme 'custom)
