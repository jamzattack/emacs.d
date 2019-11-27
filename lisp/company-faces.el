(defun my-company-faces ()
  "Sets some faces to make company look a bit less gaudy"
  (set-face-attribute 'company-tooltip nil
                      :inherit 'default
                      :background "#303030"
                      :foreground nil)

  (set-face-attribute 'company-tooltip-common nil
                      :foreground "#a0a0a0")

  (set-face-attribute 'company-tooltip-selection nil
                      :background "white"
                      :foreground "black")

  (set-face-attribute 'company-scrollbar-fg nil
                      :background "white")

  (set-face-attribute 'company-scrollbar-bg nil
                      :background "#303030")

  (set-face-attribute 'company-preview nil
                      :inherit 'shadow
                      :background nil
                      :foreground nil))

(provide 'company-faces)
