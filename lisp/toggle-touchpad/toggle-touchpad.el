;;; toggle-touchpad.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jamie Beardslee

;; Author: Jamie Beardslee <jdb@jamzattack.xyz>
;; Version: 2020.06.07

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

;; This file defines the function `toggle-touchpad', which uses xinput
;; to disable or enable the mouse in an X session.  This probably only
;; works with my laptop.

;;; Code:

(defvar toggle-touchpad--trackpoint-disabled-p nil
  "Whether the touchpad is disabled. If the value is non-nil, the
touchpad is disabled.")

(defvar toggle-touchpad--touchpad-disabled-p t
  "Whether the touchpad is disabled. If the value is non-nil, the
touchpad is disabled.")

(defvar toggle-touchpad--trackpoint-device "TPPS/2 IBM TrackPoint"
  "The xinput device identifier for the trackpoint.
Use \"xinput list\" to see what devices are available.")

(defvar toggle-touchpad--touchpad-device "SynPS/2 Synaptics TouchPad"
  "The xinput device identifier for the touchpad.
Use \"xinput list\" to see what devices are available.")

(defun toggle-touchpad-xinput-command (command device)
  "Use xinput(1) to run COMMAND on DEVICE."
  (interactive)
  (start-process "touchpad" " *touchpad*"
		 "xinput"
		 command device))


;;; Touchpad

(defun toggle-touchpad-disable-touchpad ()
  "Disables the touchpad."
  (toggle-touchpad-xinput-command
   "disable" toggle-touchpad--touchpad-device)
  (setq toggle-touchpad--touchpad-disabled-p t))

(defun toggle-touchpad-enable-touchpad ()
  "Enables the touchpad."
  (toggle-touchpad-xinput-command
   "enable" toggle-touchpad--touchpad-device)
  (setq toggle-touchpad--touchpad-disabled-p nil))

;;;###autoload
(defun toggle-touchpad-toggle-touchpad ()
  "Toggles the touchpad."
  (interactive)
  (if toggle-touchpad--touchpad-disabled-p
      (toggle-touchpad-enable-touchpad)
    (toggle-touchpad-disable-touchpad)))


;;; Trackpoint

(defun toggle-touchpad-disable-trackpoint ()
  "Disables the trackpoint."
  (toggle-touchpad-xinput-command
   "disable" toggle-touchpad--trackpoint-device)
  (setq toggle-touchpad--trackpoint-disabled-p t))

(defun toggle-touchpad-enable-trackpoint ()
  "Enables the trackpoint."
  (toggle-touchpad-xinput-command
   "enable" toggle-touchpad--trackpoint-device)
  (setq toggle-touchpad--trackpoint-disabled-p nil))

;;;###autoload
(defun toggle-touchpad-toggle-trackpoint ()
  "Toggles the trackpoint."
  (interactive)
  (if toggle-touchpad--trackpoint-disabled-p
      (toggle-touchpad-enable-trackpoint)
    (toggle-touchpad-disable-trackpoint)))


;;; Both

(defun toggle-touchpad-enable-both ()
  "Enables both the trackpoint and the touchpad."
  (toggle-touchpad-enable-touchpad)
  (toggle-touchpad-enable-trackpoint))

(defun toggle-touchpad-disable-both ()
  "Disables both the trackpoint and the touchpad."
  (toggle-touchpad-disable-touchpad)
  (toggle-touchpad-disable-trackpoint))

;;;###autoload
(defun toggle-touchpad (&optional arg)
  "Toggles both the trackpoint and the touchpad.

With prefix arg ARG, use my preferred settings (touchpad disabled
and trackpoint enabled)."
  (interactive "P")
  (cond
   (arg (toggle-touchpad-disable-touchpad)
	(toggle-touchpad-enable-trackpoint))
   ((or toggle-touchpad--trackpoint-disabled-p
	toggle-touchpad--touchpad-disabled-p)
    (toggle-touchpad-enable-both))
   (t (toggle-touchpad-disable-both))))

(provide 'toggle-touchpad)

;;; toggle-touchpad.el ends here
