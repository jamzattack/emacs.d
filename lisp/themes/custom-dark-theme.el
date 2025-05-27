(deftheme custom-dark
  "dark theme")

(let ((class '((class color) (min-colors 89)))
      (foreground "#ffffd0")
      (background "#0a0000")
      (background2 "gray15")
      (background3 "gray9")
      (fringe "grey90")
      (border "grey40")
      (modeline "#4b004b"))
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
   `(erc-current-nick-face ((,class (:foreground "OrangeRed2"))))
   `(erc-my-nick-face ((,class (:foreground "Lightskyblue3"))))
   `(erc-input-face ((,class (:foreground "Lightskyblue3"))))

   ;; Not really text
   `(tooltip ((,class (:foreground ,background :inherit 'default))))
   `(region ((,class (:background ,background2))))

   ;; Borders: All subtle grey
   `(fringe ((,class (:inherit 'default :foreground ,fringe))))
   `(vertical-border ((,class (:inherit 'fringe))))
   `(mode-line-inactive ((,class (:background ,border))))
   `(mode-line ((,class (:background ,modeline :foreground ,foreground))))
   `(window-divider ((,class (:foreground ,border))))
   `(window-divider-first-pixel ((,class (:inherit 'window-divider))))
   `(window-divider-last-pixel ((,class (:inherit 'window-divider))))

   `(tab-bar ((,class (:inherit mode-line-inactive))))
   `(tab-bar-tab ((,class (:inherit default :height 80))))
   `(tab-bar-tab-inactive ((,class (:inherit mode-line-inactive))))
   `(ruler-mode-default ((,class (:inherit header-line :box nil))))
   ))

(when (and (fboundp 'dimmer-mode)
	   dimmer-mode)
  (dimmer-mode -1)
  (dimmer-mode))

(provide-theme 'custom-dark)
