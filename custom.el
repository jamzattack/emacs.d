(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dimmer-fraction 0.3)
 '(exwm-input-prefix-keys
   '([XF86AudioMute]
     [XF86AudioLowerVolume]
     [XF86AudioRaiseVolume]
     [XF86Back]
     [XF86Forward]
     [134217761]
     [134217766]
     [134217824]
     [134217786]
     [134217848]
     [24]
     [26]
     [21]
     [8]
     [8388717]
     [8388705]
     [menu]
     [f8]))
 '(god-exempt-major-modes nil)
 '(god-exempt-predicates nil)
 '(helm-completion-style 'emacs)
 '(helm-describe-function-function 'helpful-callable)
 '(helm-describe-variable-function 'helpful-variable)
 '(inferior-lisp-program "sbcl" t)
 '(man-width 80 t)
 '(nov-text-width 80 t)
 '(org-capture-bookmark nil t)
 '(org-capture-templates
   '(("t" "Todo" entry
      (file+headline "~/org/todo.org" "Tasks")
      "* TODO %?
  %i
  %a")
     ("n" "Notes" entry
      (file+datetree "~/org/notes.org")
      "* %?
Entered on %U
  %i
  %a")
     ("d" "Diary" entry
      (file+datetree "~/org/diary.org")
      "* %?
Entered on %U
  %i
  %a")) t)
 '(org-default-notes-file "~/org/notes.org")
 '(safe-local-variable-values '((org-use-property-inheritance . t)))
 '(scheme-program-name "guile" t)
 '(scroll-bar-mode 'right)
 '(scroll-bar-width 6 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
