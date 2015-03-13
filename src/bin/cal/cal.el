;;; cal.el --- /bin/cal

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'calendar)
(require 'cl-lib)

(defun -current-month ()
  (let ((now (decode-time)))
    (nth 4 now)))

(defun -current-year ()
  (let ((now (decode-time)))
    (nth 5 now)))

(defun -month-string (year month indent)
  (with-temp-buffer
    (calendar-generate-month month year indent)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun -print-month (year month)
  (princ (-month-string year month 0))
  (princ "\n\n"))

(defun -3month-string (year month)
  (with-temp-buffer
    (dotimes (j 3)
      (let ((month (+ month (1+ j)))
            (indent (* calendar-month-width j)))
        (calendar-generate-month month year indent)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun -insert-header-year (year)
  (dotimes (_i (* calendar-month-width 1.25))
    (insert " "))
  (insert (number-to-string year))
  (insert "\n"))

(defun -print-year (year)
  (with-temp-buffer
    (-insert-header-year year)
    (cl-loop for i from 0 to 3
             do
             (let ((str (-3month-string year (* i 3))))
               (insert str)
               (insert "\n\n")))
    (insert "\n")
    (princ (buffer-substring-no-properties (point-min) (point-max)))))

(defun main ()
  (let* ((args (length argv))
         (month (cond ((zerop args) (-current-month))
                      ((= args 2) (string-to-number (cl-first argv)))
                      (t nil)))
         (year (cond ((= args 2) (string-to-number (cl-second argv)))
                     ((= args 1) (string-to-number (cl-first argv)))
                     (t (-current-year)))))
    (if (and year month)
        (-print-month year month)
      (-print-year year))))

(unless (boundp '--unit-test)
  (main)
  (setq argv nil))

;;; cal.el ends here
