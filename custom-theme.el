(deftheme custom
  "Nothing but changing the background colour to off-white")
(defun custom-theme-pdf-view ()
  (pdf-info-setoptions
   :render/foreground "black"
   :render/background "#FFFFD0"
   :render/usecolors t))

(add-hook 'pdf-view-mode-hook 'custom-theme-pdf-view)

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'custom
   `(default ((,class (:background "#FFFFD0" :foreground "black"))))
   `(border-color ((,class (:inherit 'default))))
   `(line-number-current-line ((,class (:inherit 'secondary-selection))))
   `(secondary-selection ((,class (:background "khaki1" :extend t))))
   `(helm-source-header ((,class (:inherit 'bold))))
   `(helm-selection-line ((,class (:inherit 'hl-line)))))
  (setq pdf-view-midnight-colors '("white" . "black")))

(provide-theme 'custom)
