(deftheme custom
  "Not much but changing the background colour to off-white")

(let ((class '((class color) (min-colors 89)))
      (foreground "#2b0000")
      (background "#FFFFD0")
      (background2 "lightgoldenrod")
      (background3 "khaki1")
      (modeline "sky blue"))
  (custom-theme-set-faces
   'custom
   ;; Text
   `(default ((,class (:background ,background :foreground ,foreground :weight normal))))
   `(line-number-current-line ((,class (:inherit 'secondary-selection))))
   `(secondary-selection ((,class (:background ,background3 :extend t))))
   `(helm-source-header ((,class (:inherit 'bold))))
   `(font-lock-doc-face ((,class (:slant italic :inherit font-lock-string-face))))
   `(dired-async-mode-message ((,class (:foreground "darkred"))))
   `(org-hide ((,class (:foreground ,background))))
   `(erc-current-nick-face ((,class (:foreground "OrangeRed2"))))

   ;; Not really text
   `(tooltip ((,class (:inherit 'default))))
   `(region ((,class (:background ,background2))))
   `(fringe ((,class (:inherit 'default))))
   `(mode-line ((,class (:background ,modeline))))
   `(tab-bar ((,class (:inherit mode-line-inactive))))
   `(tab-bar-tab ((,class (:inherit default :height 80))))
   `(tab-bar-tab-inactive ((,class (:inherit mode-line-inactive))))
   `(ruler-mode-default ((,class (:inherit header-line :box nil))))
   ))

(when (and (fboundp 'dimmer-mode)
	   dimmer-mode)
  (dimmer-mode -1)
  (dimmer-mode))

(provide-theme 'custom)
