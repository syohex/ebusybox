;;; echo.el --- /bin/echo

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

(defvar echo-escaped-map
  '(("a" . "\a")
    ("b" . "\b")
    ("c" . "\c")
    ("f" . "\f")
    ("n" . "\n")
    ("r" . "\r")
    ("t" . "\t")
    ("v" . "\v")
    ("\\" . "\\")))

(defun echo-process-escaped-character (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "\\\\\\([abcfnrtv\\\\]\\|0[0-7]+\\)" nil t)
      (let* ((escaped (match-string-no-properties 1))
             (replaced (assoc-default escaped echo-escaped-map)))
        (unless replaced
          (setq replaced (char-to-string (string-to-number escaped 8))))
        (replace-match replaced)))
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun main ()
  (let ((args (cl-copy-list argv))
        nflag)
    (setq argv nil)
    (when (string= (car args) "-n")
      (setq nflag t
            args (cdr args)))
    (princ (mapconcat 'echo-process-escaped-character args " "))
    (unless nflag
      (princ "\n"))))

(unless (boundp '--unit-test)
  (main))
;;; echo.el ends here
