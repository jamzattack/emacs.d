;;; custom-theme.el --- My custom theme              -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Jamie Beardslee

;; Author: Jamie Beardslee <jdb@jamzattack.xyz>
;; Version: 2025.06.09
;; Keywords: theme

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

;; This is a very simple theme, primarily just setting some faces that
;; I have issues with.

;;; Code:

(defvar custom-theme-mode-line-colors
  '("plum1" "orange" "skyblue" "green yellow")
  "A list of colors for the active mode-line.")

(deftheme custom
  "Not much but changing the background colour to off-white.
Active mode-line color will be randomized based on the variable
`custom-theme-mode-line-colors'.")

(let ((class '((class color) (min-colors 89)))
      (foreground "#2b0000")
      (background "#FFFFD0")
      (background2 "lightgoldenrod")
      (background3 "khaki1")
      (fringe "grey40")
      (border "grey90")
      (modeline (nth (random (length custom-theme-mode-line-colors))
		     custom-theme-mode-line-colors)))
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

   ;; Borders: All subtle grey
   `(fringe ((,class (:inherit 'default :foreground ,fringe))))
   `(vertical-border ((,class (:inherit 'fringe))))
   `(mode-line-inactive ((,class (:background ,border))))
   `(mode-line ((,class (:background ,modeline))))
   `(window-divider ((,class (:foreground ,border))))
   `(window-divider-first-pixel ((,class (:inherit 'window-divider))))
   `(window-divider-last-pixel ((,class (:inherit 'window-divider))))

   `(tab-bar ((,class (:inherit mode-line-inactive))))
   `(tab-bar-tab ((,class (:inherit default))))
   `(tab-bar-tab-inactive ((,class (:inherit mode-line-inactive))))
   `(ruler-mode-default ((,class (:inherit header-line :box nil))))
   ))

;;; Restart dimmer-mode if it is loaded and turned on, otherwise
;;; there's some artefacting
(when (bound-and-true-p dimmer-mode)
  (dimmer-mode -1)
  (dimmer-mode))

(provide-theme 'custom)
