;;; company-box-util.el --- Util for company-box  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Jen-Chieh Shen

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Util for company-box

;;; Code:

(require 'frame)
(require 'subr-x)

(declare-function company-box--make-frame "company-box.el")
(declare-function company-box--set-frame "company-box.el")

(defmacro company-box--with-no-redisplay (&rest body)
  "Execute BODY without any redisplay execution."
  (declare (indent 0) (debug t))
  `(let ((inhibit-redisplay t)
         (inhibit-modification-hooks t)
         buffer-list-update-hook
         display-buffer-alist
         window-configuration-change-hook
         after-focus-change-function)
     ,@body))

(defmacro company-box--with-buffer-valid (buffer &rest body)
  "Execute BODY inside BUFFER and make sure disable read-only."
  (declare (indent 1) (debug t))
  `(with-current-buffer ,buffer (let (buffer-read-only) ,@body)))

(defmacro company-box--with-buffer (suffix &rest body)
  "Execute BODY inside buffer with SUFFIX."
  (declare (indent 1) (debug t))
  `(company-box--with-buffer-valid (company-box--get-buffer ,suffix) ,@body))

(defmacro company-box--with-buffer-window (suffix &rest body)
  "Execute BODY inside selected window with buffer SUFFIX."
  (declare (indent 1) (debug t))
  `(when-let* ((buf-name (company-box--get-buffer ,suffix))
               (window (get-buffer-window buf-name t))
               ((window-live-p window)))
     (with-selected-window window (let (buffer-read-only) ,@body))))

(defmacro company-box--with-selected-frame (frame  &rest body)
  "Execute BODY inside a selected frame."
  (declare (indent 1) (debug t))
  `(when-let (((frame-live-p ,frame))) (with-selected-frame ,frame ,@body)))

(defun company-box--kill-delay (buffer)
  (when (buffer-live-p buffer) (kill-buffer buffer)))

(defun company-box--kill-timer (timer)
  "Kill TIMER the safe way."
  (when (timerp timer) (cancel-timer timer))
  (when-let (((symbolp timer))
             (val (ignore-errors (symbol-value timer))))
    (cancel-timer val)))

(defun company-box--frame-show (show frame timer)
  "Show the frame if SHOW is non-nil; else we hide it."
  (if-let (((frame-live-p frame)))
      (let ((visible (frame-visible-p frame))
            (func (if show #'make-frame-visible #'make-frame-invisible)))
        (unless (eq show visible) (funcall func frame))
        (when show (raise-frame frame)))
    (unless frame
      (company-box--set-frame (company-box--make-frame)))
    (company-box--start-frame-timer show frame timer)))

(defun company-box--start-frame-timer (show frame timer)
  "Delay show/hide frame to prevent GUI bug.

Argument SHOW, see function `company-box--frame-show' description."
  (company-box--kill-timer timer)
  (set timer (run-with-idle-timer 0 nil #'company-box--frame-show show frame timer)))

(provide 'company-box-util)
;;; company-box-util.el ends here
