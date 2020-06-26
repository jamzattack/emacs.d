;;; custom-exwm-config.el --- My EXWM configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jamie Beardslee

;; Author: Jamie Beardslee <beardsleejamie@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains my settings for EXWM.  Notable features:
;;   - Functions `exwm-fullscreen-or-reset' and `exwm-quit'
;;   - Keybindings for `eshell' and `helm-mini'
;;   - A bunch of prefix keys for use with Edwina
;;   - All the annoying bars are disabled.

;;; Code:

(require 'exwm)
(require 'exwm-xim)

(defun custom-exwm-window-setup ()
  "Other configurations."
  ;; Make more room
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;;; Toggle fullscreen
(defvar exwm-fullscreen--old-window nil
  "The last window to be focused before
`exwm-fullscreen-or-reset' was called.")

(defun exwm-fullscreen-or-reset ()
  "Toggle EXWM fullscreen layout.  Use either the current exwm
window or the first exwm window found."
  (interactive)
  (let* ((exwm-window (if (eq major-mode 'exwm-mode)
			  (selected-window)
			(let ((ht (make-hash-table)))
			  (dolist (window (window-list))
			    (with-current-buffer (window-buffer window)
			      (puthash major-mode window ht)))
			  (or (gethash 'exwm-mode ht)
			      (user-error "No EXWM windows")))))
	 (exwm-buffer (window-buffer exwm-window))
	 (id (exwm--buffer->id exwm-buffer)))
    (if (exwm-layout--fullscreen-p)
	(progn
	  (exwm-input-grab-keyboard id)
	  (exwm-layout-unset-fullscreen id)
	  (select-window exwm-fullscreen--old-window))
      (progn
	(setq exwm-fullscreen--old-window (selected-window))
	(select-window exwm-window)
	(exwm-layout-set-fullscreen id)))))

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
  (customize-set-variable
   'exwm-input-prefix-keys
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
     [menu]
     [f8]
     [?\C-\\]

     ;; Open up potential for keybindings with super modifier
     ;; (there must be a better way to do this)
     ,@(mapcar (lambda (k)
		 (kbd (format "s-%s" k)))
	       `(,@(number-sequence 0 9)
		 "'" "," "." "p" "y" "f" "g" "c" "r" "l" "/" "=" "\\"
		 "a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-" "RET" "SPC"
		 ";" "q" "j" "k" "x" "b" "m" "w" "v" "z" "[" "]" "DEL")))))

;; Global keybindings.

(defun custom-exwm-input-global-keys ()
  (customize-set-variable
   'exwm-input-global-keys
   `(;; 's-f' and '<f11>': Toggle fullscreen.
     (,(kbd "s-f") . exwm-fullscreen-or-reset)
     (,(kbd "<f11>") . exwm-fullscreen-or-reset)

     ;; 's-&': Launch application.
     (,(kbd "s-&") . exwm-shell-command)

     ;; Tab movement
     (,(kbd "s-r") . tab-next)
     (,(kbd "s-g") . tab-previous)
     ,@(mapcar (lambda (k)
		 (cons (kbd (format "s-%s" k)) 'tab-bar-select-tab))
	       (number-sequence 0 9)))))

;; Line-editing shortcuts
(defun custom-exwm-input-simulation-keys ()
  (customize-set-variable
   'exwm-input-simulation-keys
   `(;; Basic movement
     (,(kbd "C-b") . [left])
     (,(kbd "C-f") . [right])
     (,(kbd "C-p") . [up])
     (,(kbd "C-n") . [down])

     (,(kbd "M-f") . [C-right])
     (,(kbd "M-b") . [C-left])

     (,(kbd "C-a") . [home])
     (,(kbd "C-e") . [end])

     (,(kbd "M-v") . [prior])
     (,(kbd "C-v") . [next])

     ;; Deleting text
     (,(kbd "C-d") . [delete])
     (,(kbd "C-k") . [S-end delete])
     (,(kbd "M-d") . [S-C-right delete])
     (,(kbd "<M-DEL>") . [C-DEL])

     (,(kbd "C-x h") . [C-a])
     (,(kbd "C-/") . [C-z])

     ;; clipboard/kill-ring
     (,(kbd "C-w") . [C-x])
     (,(kbd "M-w") . [C-c])
     (,(kbd "C-y") . [C-v]))))

(defun custom-exwm-input-terminal-keys ()
  "Disable simulation keys for my terminals."
  (when (string-match "\\(st-.*\\|XTerm\\)" exwm-class-name)
    (exwm-input-set-local-simulation-keys nil)))



(defun custom-exwm-config ()
  ;; Don't start with extra workspaces
  (setq exwm-workspace-number 1)
  (exwm-xim--init)
  (add-hook 'exwm-manage-finish-hook 'custom-exwm-input-terminal-keys)
  (custom-exwm-input-global-keys)
  (custom-exwm-input-simulation-keys)
  (custom-exwm-prefix-keys)
  (custom-exwm-buffer-name)
  (custom-exwm-window-setup))

(provide 'custom-exwm-config)
;;; custom-exwm-config.el ends here
