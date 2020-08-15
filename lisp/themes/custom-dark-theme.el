(deftheme custom-dark
  "dark theme")

(let ((class '((class color) (min-colors 89)))
      (foreground "white")
      (background "black")
      (background2 "gray15")
      (background3 "gray9")
      (modeline "sky blue"))
  (custom-theme-set-faces
   'custom-dark
   ;; Text
   `(default ((,class (:background ,background :foreground ,foreground :weight normal))))
   `(line-number-current-line ((,class (:inherit 'secondary-selection))))
   `(secondary-selection ((,class (:background ,background3 :extend t))))
   `(helm-source-header ((,class (:inherit 'bold))))
   `(font-lock-doc-face ((,class (:slant italic :inherit font-lock-string-face))))
   `(dired-async-mode-message ((,class (:foreground "darkred"))))
   `(org-hide ((,class (:foreground ,background))))

   ;; Not really text
   `(tooltip ((,class (:inherit 'default))))
   `(region ((,class (:background ,background2))))
   `(fringe ((,class (:inherit 'default))))
   `(mode-line ((,class (:background ,modeline :foreground "black"))))
   `(tab-bar ((,class (:inherit mode-line-inactive :box nil))))
   `(tab-bar-tab ((,class (:inherit default))))
   `(tab-bar-tab-inactive ((,class (:inherit mode-line-inactive :box nil))))
   `(ruler-mode-default ((,class (:inherit header-line :box nil))))
   ))

(when (and (member 'dimmer-mode minor-mode-list)
	   (fboundp 'dimmer-mode))
  (dimmer-mode -1)
  (dimmer-mode))

(provide-theme 'custom-dark)
