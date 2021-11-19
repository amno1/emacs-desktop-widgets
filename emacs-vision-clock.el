;;; emacs-vision-clock.el ---  -*- lexical-binding: t; -*-

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

;; https://github.com/BobbyBakes/conky-Vision

;;; Code:
(require 'time-stamp)

(defgroup emacs-vision-clock nil
  "Conky like clock application written in Emacs Lisp."
  :prefix "evc--"
  :prefix "emacs-vision-clock-"
  :tag "Conky-Vision style clock as Emacs application."
  :group 'applications)

;;; Customize
(defcustom emacs-vision-clock-x -100
  "Frame width"
  :type 'fixnum
  :group 'emacs-vision-clock)

(defcustom emacs-vision-clock-y 100
  "Frame width"
  :type 'fixnum
  :group 'emacs-vision-clock)

(defface emacs-vision-clock-day-face
  '((t :weight light :family "Poiret One" :default nil
       :foreground "white" :height 340))
  "Default face for emacs-vision-clock frame."
  :group 'emacs-vision-clock)

(defface emacs-vision-clock-month-face
  '((t :weight light :family "Poiret One" :default nil
       :foreground "white" :height 340))
  "Default face for emacs-vision-clock frame."
  :group 'emacs-vision-clock)

(defface emacs-vision-clock-time-face
  '((t :weight light :family "Poiret One" :default nil
       :foreground "white" :height 900))
  "Default face for emacs-vision-clock frame."
  :group 'emacs-vision-clock)

;;; Implementation
(defvar evc--frame nil)
(defvar evc--timer nil)
(defvar evc--buffer nil)
(defvar evc--day-face 'emacs-vision-clock-day-face)
(defvar evc--time-face 'emacs-vision-clock-time-face)
(defvar evc--month-face 'emacs-vision-clock-month-face)

(defun evc--time ()
  (propertize (time-stamp-string "%H:%M") 'face evc--time-face))

(defun evc--day ()
  (propertize (capitalize (time-stamp-string "%A")) 'face evc--day-face))

(defun evc--month ()
  (propertize (capitalize (time-stamp-string "%B %d")) 'face evc--month-face))

(defun evc--make-frame ()
  (make-frame `((name . "*emacs-vision-clock*")
                (left . ,emacs-vision-clock-x)
                (top . ,emacs-vision-clock-y)
                (alpha-background . 0)
                (tool-bar-lines . 0)
                (menu-bar-lines . 0)
                (minibuffer . nil)
                (right-fringe . 0)
                (left-fringe . 0)
                (horizontal-scroll-bars . nil)
                (vertical-scroll-bars . nil)
                (border-width . 0)
                (internal-border-width . 0)
                (no-special-glyphs . t)
                (visibility . nil)
                (z-group . below)
                (auto-raise . nil)
                (skip-taskbar . t)
                (no-focus-on-map . t)
                (no-accept-focus . t)
                (unsplittable . t)
                (undecorated . t))))

(defun evc--update ()
  (select-frame evc--frame)
  (with-current-buffer evc--buffer
    (goto-char (point-min))
    (re-search-forward "[0-9][0-9]:[0-9][0-9]" nil t)
    (replace-match (evc--time))))

(defun evc--make-buffer ()
  (with-current-buffer (get-buffer-create "  *emacs-vision-clock*")
    (setq cursor-in-non-selected-windows nil mode-line-format nil)
    (erase-buffer)
    (insert (evc--time) "\n " (evc--day) ". " (evc--month))
    (let ((fill-column (- (line-end-position) (line-beginning-position))))
      (center-line)
      (forward-line -1)
      (center-line))
    (setq evc--buffer (current-buffer))))

(defun evc--make-widget ()
  (let ((frame (evc--make-frame)))
    (evc--make-buffer)
    (select-frame frame)
    (switch-to-buffer evc--buffer)
    (delete-other-windows)
    (fit-frame-to-buffer)
    (set-frame-width evc--frame (1+ (frame-width)))
    (make-frame-visible frame)
    (set-window-dedicated-p (selected-window) t)
    (setq evc--frame frame evc--timer (run-at-time nil 60 #'evc--update))))

;;; Public commands
(defun emacs-vision-clock-run ()
  (interactive)
  (unless evc--frame
    (evc--make-widget)))

(defun emacs-vision-clock-stop ()
  (interactive)
  (if evc--timer (cancel-timer evc--timer))
  (if evc--frame (delete-frame evc--frame))
  (if evc--buffer (kill-buffer evc--buffer))
  (setq evc--buffer nil evc--frame nil evc--timer nil))

(provide 'emacs-vision-clock)
;;; emacs-vision-clock.el ends here
