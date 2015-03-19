;;; basename.el --- /usr/bin/basename

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

;; See also
;;  - http://pubs.opengroup.org/onlinepubs/009695399/utilities/basename.html

;;; Code:

(require 'cl-lib)

(defun -basename (name)
  (cond ((string= name "\0") ".")
        ((string= name "//") "/")
        ((string-match-p "\\`/+\\'" name) "/")
        ((string-match "/+\\'" name)
         (replace-match "" t nil name))
        (t
         (file-name-nondirectory name))))

(defun main ()
  (if (= (length argv) 0)
      (error "Usage: basename.el NAME [SUFFIX]")
    (let ((name (-basename (cl-first argv)))
          (suffix (and (>= (length argv) 2) (cl-second argv))))
      (when suffix
        (let ((regexp (concat suffix "\\'")))
          (when (string-match regexp name)
            (setq name (replace-match "" t nil name)))))
      (princ name)
      (princ "\n"))))

(unless (boundp '--unit-test)
  (main)
  (setq argv nil))

;;; cal.el ends here
