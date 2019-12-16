(require 'bookmark)

(defun ido-bookmark-jump ()
  "An ido wrapper for `bookmark-jump'. Designed for interactive
  use, so just use `bookmark-jump' in elisp."
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump
   (ido-completing-read "Open bookmark: "
                        (mapcar (lambda (x) (car x)) bookmark-alist))))

(provide 'ido-bookmark-jump)
