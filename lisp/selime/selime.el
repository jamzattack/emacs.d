;;; selime.el --- A better version of elisp-slime-nav  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <beardsleejamie@gmail.com>
;; Keywords: lisp

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

;; My attempt at emulating slime-mode for emacs lisp.  A bunch of
;; things work, but there are still a couple of functions to add and
;; improve.

;;; Code:

(require 'disass)
(require 'ielm)
(require 'help-fns)
(require 'pp)

(defun selime-describe-function ()
  "If package \"helpful\" is installed, call `helpful-callable',
otherwise call `describe-function'"
  (interactive)
  (if (featurep 'helpful)
      (call-interactively #'helpful-callable)
    (call-interactively #'describe-function)))

(defun selime-describe-variable ()
  "If package \"helpful\" is installed, call `helpful-variable',
otherwise call `describe-variable'"
  (interactive)
  (if (featurep 'helpful)
      (call-interactively #'helpful-variable)
    (call-interactively #'describe-variable)))

(defun selime-describe-symbol ()
  "If package \"helpful\" is installed, call `helpful-at-point',
otherwise call `describe-symbol' on the symbol at point"
  (interactive)
  (if (featurep 'helpful)
      (call-interactively #'helpful-at-point)
    (describe-symbol (symbol-at-point))))

(defun selime-disassemble (function)
  "If point is on a function, disassemble it.  Otherwise prompt
for a function to disassemble."
  (interactive (list (let ((fun (symbol-at-point)))
		       (if (fboundp fun)
			   fun
			 (intern
			  (completing-read "Disassemble function: " obarray 'fboundp t nil nil ))))))
  (disassemble function))

(defun selime-macroexpand ()
  "Macro expand the following sexp."
  (interactive)
  (save-excursion
    (forward-sexp)
    (pp-macroexpand-last-sexp nil)))

(defvar selime-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d C-d")	'selime-describe-symbol)
    (define-key map (kbd "C-c C-d d")	'selime-describe-symbol)
    (define-key map (kbd "C-c C-d C-f")	'selime-describe-function)
    (define-key map (kbd "C-c C-d f")	'selime-describe-function)
    (define-key map (kbd "C-c C-d C-v")	'selime-describe-variable)
    (define-key map (kbd "C-c C-d v")	'selime-describe-variable)
    (define-key map (kbd "C-c C-c")	'compile-defun)
    (define-key map (kbd "C-c M-d")	'selime-disassemble)
    (define-key map (kbd "C-c C-m")	'selime-macroexpand)
    (define-key map (kbd "C-c C-k")	'eval-buffer)
    (define-key map (kbd "C-c M-k")	'eval-buffer)
    map))

(define-minor-mode selime-mode
  "Enable Slime-style documentation for elisp buffers."
  nil "" selime-mode-map)

(provide 'selime)
;;; selime.el ends here
