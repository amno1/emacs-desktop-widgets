;;; emacs-metro-clock.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
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

;; https://www.deviantart.com/satya164/art/Conky-Metro-Clock-245432929

;;; Code:
(require 'time-stamp)

(defgroup emacs-metro-clock nil
  "Conky like clock application written in Emacs Lisp."
  :prefix "emc--"
  :prefix "emacs-metro-clock-"
  :tag "Conky metro style clock for Emacs"
  :group 'applications)

;;; Customize
(defcustom emacs-metro-clock-x -200
  "Frame width"
  :type 'fixnum
  :group 'emacs-metro-clock)

(defcustom emacs-metro-clock-y 600
  "Frame width"
  :type 'fixnum
  :group 'emacs-metro-clock)

(defface emacs-metro-clock-day-face
  '((t :weight light :family "Open Sans" :default nil
       :foreground "white" :height 400))
  "Default face for emacs-metro-clock frame."
  :group 'emacs-metro-clock)

(defface emacs-metro-clock-month-face
  '((t :weight light :family "Open Sans" :default nil
       :foreground "white" :height 400))
  "Default face for emacs-metro-clock frame."
  :group 'emacs-metro-clock)

(defface emacs-metro-clock-time-face
  '((t :weight normal :family "Open Sans" :default nil
       :foreground "white" :height 1000))
  "Default face for emacs-metro-clock frame."
  :group 'emacs-metro-clock)

;;; Implementation
(defvar emc--frame nil)
(defvar emc--timer nil)
(defvar emc--buffer nil)
(defvar emc--day-face 'emacs-metro-clock-day-face)
(defvar emc--time-face 'emacs-metro-clock-time-face)
(defvar emc--month-face 'emacs-metro-clock-month-face)

(defun emc--time ()
  (propertize (time-stamp-string "%H:%M") 'face emc--time-face))

(defun emc--day ()
  (propertize (capitalize (time-stamp-string "%A")) 'face emc--day-face))

(defun emc--month ()
  (propertize (capitalize (time-stamp-string "%B %d")) 'face emc--month-face))

(defun emc--make-frame ()
  (make-frame `((name . "*emacs-metro-clock*")
                (left . ,emacs-metro-clock-x)
                (top . ,emacs-metro-clock-y)
                (alpha-background . 0)
                (tool-bar-lines . 0)
                (menu-bar-lines . 0)
                (left-fringe . 0)
                (right-fringe . 0)
                (minibuffer . nil)
                (horizontal-scroll-bars . nil)
                (vertical-scroll-bars . nil)
                (border-width . 0)
                (internal-border-width . 0)
                (visibility . nil)
                (z-group . below)
                (auto-raise . nil)
                (skip-taskbar . t)
                (no-focus-on-map . t)
                (no-accept-focus . t)
                (unsplittable . t)
                (undecorated . t))))

(defun emc--update ()
  (select-frame emc--frame)
  (with-current-buffer emc--buffer
    (goto-char (point-min))
    (re-search-forward "[0-9][0-9]:[0-9][0-9]" nil t)
    (replace-match (emc--time))))

(defun emc--make-buffer ()
  (with-current-buffer (get-buffer-create "  *emacs-metro-clock*")
    (setq cursor-in-non-selected-windows nil mode-line-format nil)
    (erase-buffer)
    (insert (emc--day) "\n" (emc--month) "\n" (emc--time))
    (let ((fill-column (- (line-end-position) (line-beginning-position))))
      (goto-char (point-min))
      (center-line))
    (setq emc--buffer (current-buffer))))

(defun emc--make-widget ()
  (let ((frame (emc--make-frame)))
    (emc--make-buffer)
    (select-frame frame)
    (switch-to-buffer emc--buffer)
    (delete-other-windows)
    (fit-frame-to-buffer)
    (set-frame-width emc--frame (1+ (frame-width)))
    (make-frame-visible frame)
    (set-window-dedicated-p (selected-window) t)
    (setq emc--frame frame emc--timer (run-at-time nil 60 #'emc--update))))

;;; Public commands
(defun emacs-metro-clock-run ()
  (interactive)
  (unless emc--frame
    (emc--make-widget)))

(defun emacs-metro-clock-stop ()
  (interactive)
  (if emc--timer (cancel-timer emc--timer))
  (if emc--frame (delete-frame emc--frame))
  (if emc--buffer (kill-buffer emc--buffer))
  (setq emc--buffer nil emc--frame nil emc--timer nil))

(provide 'emacs-metro-clock)
;;; emacs-metro-clock.el ends here
