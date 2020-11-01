;; learn-timer.el 	-*- lexical-binding: t -*-

;; Author: Firslov
;; URL: https://github.com/firslov/learn-timer/

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; This is a countdown clock based on modeline.

;;; Code:
(require 'org-agenda)

(defgroup learn-time nil
  "A countdown tool."
  :group 'learn-time)

(defvar timer-lt nil)

(defcustom learn-time "2025-10-21T00:00:00+0800"
  "The countdown deadline."
  :group 'learn-time
  :type 'string)

(defcustom lt-todo-files nil
  "List todo files."
  :group 'learn-time
  :type 'cons)

(defun learn-timer--buf ()
  (let ((buf (get-buffer "*Learn Timer*")))
    (if buf
        (with-current-buffer buf
          (let* ((seconds (truncate
                           (- (time-to-seconds (date-to-time learn-time))
                              (time-to-seconds))))
                 (days    (/ seconds (* 24 60 60)))
                 (hours   (progn (setq seconds (- seconds(* days (* 24 60 60))))
                                 (/ seconds (* 60 60))))
		 (minutes (progn (setq seconds (- seconds (* hours (* 60 60))))
				 (/ seconds 60)))
		 (seconds (setq seconds (- seconds (* minutes 60)))))
		 (erase-buffer)
		 (insert (propertize (format "  %s 天 %s 时 %s 分 %s 秒"
                                             days hours minutes seconds)
                                     'face '(:height 2.0))))
	    (unless timer-lt
	      (setq timer-lt
		    (run-at-time t 1 'learn-timer--buf))))
	  (when timer-lt
	    (cancel-timer timer-lt)
	    (setq timer-lt nil)))))

  (defun learn-timer--time ()
    (let* ((seconds (truncate
		     (- (time-to-seconds (date-to-time learn-time))
			(time-to-seconds))))
	   (days    (/ seconds (* 24 60 60)))
	   (hours   (progn (setq seconds (- seconds (* days (* 24 60 60))))
			   (/ seconds (* 60 60))))
	   (minutes (progn (setq seconds (- seconds (* hours (* 60 60))))
			   (/ seconds 60)))
	   (seconds (setq seconds (- seconds (* minutes 60)))))
      (format "→%s天%s时←" days hours)))

  (defun todo-num ()
    (save-excursion
      (let* ((today (org-today))
	     (date (calendar-gregorian-from-absolute today))
	     (file (car lt-todo-files)))
	(if file
	    (let*
		((rtn (org-agenda-get-day-entries file date :todo))
		 (num (length rtn)))
	      (format "Todo:%s" num))
	  "Todo:~"))))

  (defun timer ()
    (interactive)
    (with-current-buffer (get-buffer-create "*Learn Timer*")
      (buffer-disable-undo)
      (setq cursor-type nil)
      (learn-timer--buf)
      (display-buffer (current-buffer))))

  (defun awesome-tray-module-timer-info ()
    (learn-timer--time))

  (defface awesome-tray-module-timer-face
    '((((background light))
       :foreground "#cc2444" :bold t)
      (t
       :foreground "#ff2d55" :bold t))
    "timer face."
    :group 'awesome-tray)

  (defun awesome-tray-module-todo-info ()
    (todo-num))

  (defface awesome-tray-module-todo-face
    '((((background light))
       :foreground "#cc2444" :bold t)
      (t
       :foreground "#ffff00" :bold t))
    "todo face."
    :group 'awesome-tray)

  (add-to-list 'awesome-tray-module-alist
	       '("timer" . (awesome-tray-module-timer-info awesome-tray-module-timer-face)))

  (add-to-list 'awesome-tray-module-alist
	       '("todo" . (awesome-tray-module-todo-info awesome-tray-module-todo-face)))

  (provide 'learn-timer)
