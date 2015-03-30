;;; wc.el --- /usr/bin/wc

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/
;; Version: 0.01

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

(require 'cl-lib)

(defvar -cflag nil)
(defvar -lflag nil)
(defvar -mflag nil)
(defvar -wflag nil)

(defun -not-set-flags-p ()
  (not (or -cflag -lflag -mflag -wflag)))

(defun -set-default-flag ()
  (setq -cflag t -wflag t -lflag t))

(defun -getopt (args)
  (dolist (arg argv)
    (cond ((string= arg "-l")
           (setq -lflag t))
          ((string= arg "-w")
           (setq -wflag t))
          ((string= arg "-c")
           (setq -cflag t))
          ((string= arg "-m")
           (setq -mflag t)))))

(defun -files (args)
  (cl-loop for arg in argv
           unless (string-prefix-p "-" arg)
           collect arg))

(defun -read-bytes (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally file)
    (- (point-max) (point-min))))

(defun -wc (file)
  (with-current-buffer (find-file-noselect file nil t)
    (let ((counts (make-vector 5 0)))
      (when -lflag
        (aset counts 0 (count-lines (point-min) (point-max))))

      (when -wflag
        (aset counts 1 (count-words (point-min) (point-max))))

      (when -cflag
        (aset counts 2 (- (point-max) (point-min))))

      (when -mflag
        (aset counts 3 (-read-bytes file)))
      (aset counts 4 (abbreviate-file-name file))
      counts)))

(defun -sum (total count)
  (dotimes (i 4)
    (let ((curval (aref total i)))
      (aset total i (+ curval (aref count i))))))

(defun -max-width-columns (counts)
  (cl-loop for i from 0 to 3
           collect
           (cl-loop for count in counts
                    maximize (length (format "%d" (aref count i))))))

(defun -format (counts file-p)
  (let ((max-widths (-max-width-columns counts)))
    (cl-loop for val in (list -lflag -wflag -cflag -mflag)
             for i = 0 then (1+ i)
             when val
             collect (format "%%%dd" (nth i max-widths)) into fmts
             finally
             return
             (if file-p
                 (mapconcat 'identity (append fmts '("%s")) " ")
               (mapconcat 'identity fmts " ")))))

(defun -count-to-format-params (count file-p)
  (let (params)
    (when -lflag
      (push (aref count 0) params))

    (when -wflag
      (push (aref count 1) params))

    (when -cflag
      (push (aref count 2) params))

    (when -mflag
      (push (aref count 3) params))

    (when file-p
      (push (aref count 4) params))
    (nreverse params)))

(defun -print-results (counts total files)
  (let* ((file-p (> (length files) 0))
         (fmt (concat " " (-format (if total (cons total counts) counts)  file-p))))
    (cl-loop for count in counts
             for params = (-count-to-format-params count file-p)
             do
             (progn
               (apply 'message fmt params)))
    (when (>= (length files) 2)
      (aset total 4 "total")
      (apply 'message fmt (-count-to-format-params total file-p)))))

(defun main ()
  (let ((options (-getopt argv))
        (files (-files argv)))
    (when (-not-set-flags-p)
      (-set-default-flag))
    (if (not files)
        (error "Read from STDIN is not implemented")
      (let ((total (make-vector 5 0)))
        (cl-loop for file in files
                 for count = (-wc file)
                 collect count into counts
                 and
                 do
                 (-sum total count)
                 finally
                 (-print-results counts total files))))))

(unless (boundp '--unit-test)
  (main)
  (setq argv nil))

;;; wc.el ends here
