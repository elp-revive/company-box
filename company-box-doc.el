;;; company-box-doc.el --- Company front-end  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Sebastien Chapuis
;; Copyright (C) 2021-2022 Jen-Chieh Shen

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
;; Display candidate's documentation in another frame

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'face-remap)
(require 'subr-x)

(require 'dash)
(require 'company)
(require 'frame-local)

(require 'company-box-util)

(defgroup company-box-doc nil
  "Display documentation popups alongside company-box"
  :group 'company)

(defcustom company-box-doc-enable t
  "Enable company-box-doc by default."
  :type 'boolean
  :safe #'booleanp
  :group 'company-box-doc)

(defcustom company-box-doc-delay 0.5
  "The number of seconds to wait before displaying the popup."
  :type 'number
  :group 'company-box-doc)

(defcustom company-box-doc-no-wrap nil
  "Specify whether or not to wrap the documentation box at the edge of the
Emacs frame."
  :type 'boolean
  :group 'company-box-doc)

(defcustom company-box-doc-text-scale-level 0
  "Text scale amount for doc buffer."
  :type 'integer
  :group 'company-box-doc)

(defvar company-box-doc-frame-parameters
  '((internal-border-width  . 10)
    (vertical-scroll-bars   . nil)
    (horizontal-scroll-bars . nil))
  "Frame parameters to use on the doc frame.
`company-box-frame-parameters' is then append to this variable.")

(declare-function company-box--get-frame 'company-box)
(declare-function company-box--set-frame 'company-box)
(declare-function company-box--get-buffer 'company-box)
(declare-function company-box--make-frame 'company-box)

(defvar company-box-frame-parameters)
(defvar company-box--bottom)
(defvar company-box-scrollbar)

(defvar-local company-box-doc--timer nil)

(defun company-box-doc--fetch-doc-buffer (candidate)
  "Return doc for CANDIDATE."
  (let (company-mode-hook)
    (company-box--mute-apply
      (--> (while-no-input
             ;; XXX: By calling `company-call-backend' with `doc-buffer' will
             ;; enable `company-mode' once... not sure why!
             ;;
             ;; Let's temporarily set `company-mode-hook' to `nil' (on top) to
             ;; prevent other possible side effects.
             (-some-> (company-call-backend 'doc-buffer candidate)
               (get-buffer)))
           (if (eq t it) nil it)))))

(defun company-box-doc--set-frame-position (frame)
  "Update frame position."
  (-let* ((box-position (frame-position (company-box--get-frame)))
          (box-width (frame-pixel-width (company-box--get-frame)))
          (window (frame-root-window frame))
          (frame-resize-pixelwise t)
          ((width . height)
           (if company-box-doc-no-wrap
               (window-text-pixel-size window nil nil 10000 10000)
             (window-text-pixel-size
              window nil nil
              ;; Use the widest space available (left or right of the box frame)
              (let ((space-right (- (frame-native-width) (+ 40 (car box-position) box-width)))
                    (space-left (- (car box-position) 40)))
                (if (< space-right space-left) space-left space-right))
              (- (frame-native-height) 40))))
          (bottom (+ company-box--bottom (window-pixel-top) (frame-border-width)))
          (x (+ (car box-position) box-width (/ (frame-char-width) 2)))
          (y (cdr box-position))
          (y (if (> (+ y height 20) bottom)
                 (- y (- (+ y height) bottom) 20)
               y))
          (space-right (- (frame-pixel-width) x))
          (space-left (car box-position))
          (x (or (let ((border (* (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0)
                                  2)))
                   (and (> width space-right)
                        (> space-left (+ width border (/ (frame-char-width) 2)))
                        (- (car box-position) width border (/ (frame-char-width) 2))))
                 x)))
    (set-frame-position frame (max x 0) (max y 0))
    (set-frame-size frame width height t)))

(defun company-box-doc--make-buffer (object)
  "Create doc buffer."
  (company-box--with-no-redisplay
    (when-let*
        ((string (cond ((stringp object) object)
                       ((bufferp object) (company-box--with-buffer-valid object (buffer-string)))))
         (string (string-trim string))
         ((> (length string) 0)))
      (company-box--with-buffer "doc"
        (erase-buffer)
        (insert string)
        (let ((text-scale-mode-step 1.1))
          (text-scale-set company-box-doc-text-scale-level))
        (setq mode-line-format nil
              display-line-numbers nil
              header-line-format nil
              tab-line-format nil
              show-trailing-whitespace nil
              truncate-lines nil
              cursor-in-non-selected-windows nil)
        (when (bound-and-true-p tab-bar-mode)
          (set-frame-parameter (company-box-doc--get-frame) 'tab-bar-lines 0))
        (current-buffer)))))

(defun company-box-doc--make-frame (buffer)
  "Creat doc frame."
  (let* ((company-box-frame-parameters
          (append company-box-doc-frame-parameters
                  company-box-frame-parameters))
         (frame (company-box--make-frame buffer)))
    ;;(set-face-background 'internal-border "white" frame)
    (set-frame-parameter frame 'name "")
    frame))

(defun company-box-doc--get-frame ()
  "Return company-box-doc frame."
  (frame-local-getq company-box-doc-frame))

(defun company-box-doc--show (selection frame)
  (company-box--with-no-redisplay
    (cl-letf (((symbol-function 'completing-read) #'company-box-completing-read))
      (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                     company-box--bottom
                                     company-selection
                                     (company-box--get-frame)
                                     (frame-visible-p (company-box--get-frame))))
                   (candidate (nth selection company-candidates))
                   (doc (or (company-call-backend 'quickhelp-string candidate)
                            (company-box-doc--fetch-doc-buffer candidate)))
                   (doc (company-box-doc--make-buffer doc)))
        (unless (frame-live-p (frame-local-getq company-box-doc-frame))
          (frame-local-setq company-box-doc-frame (company-box-doc--make-frame doc)))
        (company-box-doc--set-frame-position (frame-local-getq company-box-doc-frame))
        (company-box-doc--show-frame t)))))

(defun company-box-completing-read (_prompt candidates &rest _)
  "`cider', and probably other libraries, prompt the user to
resolve ambiguous documentation requests.  Instead of failing we
just grab the first candidate and press forward."
  (car candidates))

(defun company-box-doc (selection frame)
  (when company-box-doc-enable
    (company-box-doc--hide frame)
    (company-box--kill-timer company-box-doc--timer)
    (setq company-box-doc--timer
          (run-with-idle-timer
           company-box-doc-delay nil
           (lambda nil
             (company-box-doc--show selection frame)
             (company-ensure-emulation-alist))))))

(defvar company-box-doc--frame-timer nil)

(defun company-box-doc--show-frame (show)
  "Start the timer to SHOW frame."
  (company-box--start-frame-timer show (company-box-doc--get-frame) 'company-box-doc--frame-timer))

(defun company-box-doc--hide (frame)
  "Hide the doc FRAME."
  ;; TODO: we can't use `company-box-doc--show-frame' function here; it seems
  ;; like it will enter an infinite loop and freezes Emacs.
  (company-box--kill-timer company-box-doc--frame-timer)
  (when-let* ((local-frame (frame-local-getq company-box-doc-frame frame))
              ((frame-live-p local-frame))
              ((frame-visible-p local-frame)))
    (make-frame-invisible local-frame)))

(defun company-box-doc--delete-frame ()
  "Delete the child frame if it exists."
  (-when-let (frame (frame-local-getq company-box-doc-frame))
    (and (frame-live-p frame)
         (delete-frame frame))
    (frame-local-setq company-box-doc-frame nil)))

(defun company-box-doc-manually ()
  (interactive)
  (company-box-doc--show company-selection (or (frame-parent) (selected-frame))))

(define-key company-active-map [remap company-show-doc-buffer] 'company-box-doc-manually)

(provide 'company-box-doc)
;;; company-box-doc.el ends here
