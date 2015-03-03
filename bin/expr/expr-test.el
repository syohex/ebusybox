;;; expr-test.el --- Test of /bin/expr

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

(require 'ert)

(ert-deftest infix-to-rpn ()
  "Convert infix to RPN notation"

  (let ((got (-infix-to-rpn '("1"))))
    (should (equal got '("1"))))

  (let ((got (-infix-to-rpn '("1" "+" "2"))))
    (should (equal got '("1" "2" "+"))))

  (let ((got (-infix-to-rpn '("5" "+" "(" "(" "1" "+" "2" ")" "*" "4" ")" "-" "3"))))
    (should (equal got '("5" "1" "2" "+" "4" "*" "+" "3" "-")))))

(ert-deftest eval ()
  "Evaluate RPN expression"

  (let ((got (-eval-rpn '("1"))))
    (should (= got 1)))

  (let ((got (-eval-rpn '("1" "2" "+"))))
    (should (= got 3)))

  (let ((got (-eval-rpn '("5" "1" "2" "+" "4" "*" "+" "3" "-"))))
    (should (equal got 14))))

;;; expr-test.el ends here
