(require 'exwm)

(defun custom-exwm-window-setup ()
  "Other configurations."
  ;; Make more room
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; Toggle fullscreen
(defun exwm-fullscreen-or-reset ()
  "Toggle EXWM fullscreen layout"
  (interactive)
  (if (exwm-layout--fullscreen-p)
      (exwm-reset)
    (exwm-layout-set-fullscreen)))

;; Start a program without creating a buffer
(defun exwm-shell-command (command)
  "Executes a shell command, but doesn't create a buffer for the
output."
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

;; Change buffer name to "title <class>"
(defun custom-exwm-buffer-name ()
  "Rename exwm buffers the window class name."
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (exwm-workspace-rename-buffer
	       (format "%s <%s>" exwm-title exwm-class-name)))))



;; Prefix keys
(defun custom-exwm-prefix-keys ()
  "Sets up prefix keys for exwm."
  (custom-set-variables
   '(exwm-input-prefix-keys
     `([XF86AudioMute]
       [XF86AudioLowerVolume]
       [XF86AudioRaiseVolume]
       [XF86Back]
       [XF86Forward]
       [?\M-!]
       [?\M-&]
       [?\M-`]
       [?\M-:]
       [?\M-x]
       [?\C-x]
       [?\C-z]
       [?\C-u]
       [?\C-h]
       [?\s-m]
       [?\s-a]
       [menu]
       [f8]
       
       ;; For edwina-mode
       ,(kbd "s-r")
       ,(kbd "s-j")
       ,(kbd "s-k")
       ,(kbd "s-S-j")
       ,(kbd "s-J")
       ,(kbd "s-S-k")
       ,(kbd "s-K")
       ,(kbd "s-h")
       ,(kbd "s-l")
       ,(kbd "s-d")
       ,(kbd "s-i")
       ,(kbd "s-S-c")
       ,(kbd "s-C")
       ,(kbd "<s-RET>")
       ,(kbd "<s-return>")
       ,(kbd "<s-S-RET>")
       ,(kbd "<s-S-return>")))))


;; Global keybindings.

(defun custom-exwm-input-global-keys ()
  (custom-set-variables
   '(exwm-input-global-keys
     `(;; 's-f': Toggle fullscreen.
       ([?\s-f] . exwm-fullscreen-or-reset)
       ;; 's-w': Switch workspace.
       ([?\s-w] . exwm-workspace-switch)
       ;; 's-&': Launch application.
       ([?\s-&] . exwm-shell-command)
       ;; 's-N': Switch to certain workspace.
       ,@(mapcar (lambda (i)
                   `(,(kbd (format "s-%d" i)) .
                     (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create ,i))))
                 (number-sequence 0 4))))))


;; Line-editing shortcuts
(defun custom-exwm-input-simulation-keys ()
  (custom-set-variables
   '(exwm-input-simulation-keys
     '( ; Basic movement
       ([?\C-b] . [left])
       ([?\C-f] . [right])
       ([?\C-p] . [up])
       ([?\C-n] . [down])
       
       ([?\M-f] . [C-right])
       ([?\M-b] . [C-left])

       ([?\C-a] . [home])
       ([?\C-e] . [end])

       ([?\M-v] . [prior])
       ([?\C-v] . [next])

       ;; Deleting text
       ([?\C-d] . [delete])
       ([?\C-k] . [S-end delete])
       ([?\M-d] . [S-C-right delete])
       ([?\M-\d] . [C-DEL])

       ;; clipboard/kill-ring
       ([?\C-w] . [C-x])
       ([?\M-w] . [C-c])
       ([?\C-y] . [C-v])))))



(defun custom-exwm-config ()
  ;; Don't start with extra workspaces
  (setq exwm-workspace-number 1)
  (custom-exwm-input-global-keys)
  (custom-exwm-input-simulation-keys)
  (custom-exwm-prefix-keys)
  (custom-exwm-buffer-name)
  (custom-exwm-window-setup)
  (global-set-key (kbd "C-x C-c") #'save-buffers-kill-emacs)
  ;; Enable EXWM
  (exwm-enable))



(provide 'custom-exwm-config)

;;; custom-exwm-config.el ends here
