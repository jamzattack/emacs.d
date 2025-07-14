;;; custom-dark-theme.el --- My custom theme (dark)  -*- lexical-binding: t; -*-

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
;; I have issues with.  (Dark version)

;;; Code:

(defvar custom-dark-theme-mode-line-colors
  '("dark red" "dark magenta" "dark slate blue" "dark green" "darkorange3")
  "A list of colors for the active mode-line.")

(deftheme custom-dark
  "dark theme")

(let ((class '((class color) (min-colors 89)))
      (foreground "#ffffd0")
      (background "#0a0000")
      (background2 "gray15")
      (background3 "gray9")
      (fringe "grey90")
      (border "grey40")
      (modeline (nth (random (length custom-dark-theme-mode-line-colors))
		     custom-dark-theme-mode-line-colors)))
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
   `(tab-bar-tab ((,class (:background ,background :foreground ,foreground))))
   `(tab-bar-tab-inactive ((,class (:inherit mode-line-inactive))))
   `(ruler-mode-default ((,class (:inherit header-line :box nil))))
   ))

(when (and (fboundp 'dimmer-mode)
	   dimmer-mode)
  (dimmer-mode -1)
  (dimmer-mode))

(provide-theme 'custom-dark)
