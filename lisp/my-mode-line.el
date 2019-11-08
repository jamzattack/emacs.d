(defun mode-line-green ()
  "Sets the modeline colours to green"
  (interactive)
  (set-face-attribute 'mode-line nil
                      :background "#afdf77"
                      :foreground "black")
  (set-face-attribute 'header-line nil
                      :background "#afdf77"
                      :foreground "black")
  (set-face-attribute 'mode-line-buffer-id nil
                      :background nil
                      :foreground nil))



(defun mode-line-purple ()
  "Sets the modeline colours to purple"
  (interactive)
  (set-face-attribute 'mode-line nil
                      :background "#e6a8df"
                      :foreground "black")
  (set-face-attribute 'header-line nil
                      :background "#e6a8df"
                      :foreground "black")
  (set-face-attribute 'mode-line-buffer-id nil
                      :background nil
                      :foreground nil))

(provide 'my-mode-line)
