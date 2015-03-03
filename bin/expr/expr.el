;;; expr.el --- /bin/expr

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

(defconst -operator-priority
  '(("^" .  6)
    ("*" .  5)
    ("/" .  5)
    ("%" .  5)
    ("+" .  4)
    ("-" .  4)
    ("<" .  3)
    ("<=" . 3)
    (">"  . 3)
    (">=" . 3)
    ("="  . 2)
    ("!=" . 2)
    ("||" . 1)
    ("&&" . 1)))

(defun -string-to-number (str)
  (cond ((string-match "\\`0x\\(.+\\)" str)
	 (string-to-number (match-string-no-properties 16)))
	((string-match-p "\\`0" str)
	 (string-to-number str 8))
	(t
	 (string-to-number str))))

(defun -looks-like-number (str)
  (string-match-p "\\`\\(?:0x?\\)?[1-9][0-9]*\\'" str))

(defun -is-operator (str)
  (assoc str -operator-priority))

(defun -is-left-assoc-p (op)
  (not (string= op "^")))

(defun -pop-stack-p (op1 op2)
  (let ((prio1 (assoc-default op1 -operator-priority))
	(prio2 (assoc-default op2 -operator-priority)))
    (or (and (-is-left-assoc-p op2) (<= prio1 prio2))
	(< prio1 prio2))))

(defun -infix-to-rpn (args)
  (let (rpn op-stack)
    (dolist (arg args)
      (cond ((-looks-like-number arg)
	     (push arg rpn))
	    ((-is-operator arg)
	     (when (>= (length op-stack) 1)
	       (let ((stack-top (car op-stack))
		     finish)
		 (while (and (not finish) (-is-operator stack-top))
		   (if (-pop-stack-p arg stack-top)
		       (progn
			 (push stack-top rpn)
                         (pop op-stack)
			 (setq stack-top (car op-stack)))
		     (setq finish t)))))
             (push arg op-stack))
	    ((string= arg "(")
	     (push arg op-stack))
	    ((string= arg ")")
	     (let (finish)
	       (while (not finish)
		 (when (zerop (length op-stack))
		   (error "Left paren is not found"))
		 (let ((stack-top (car op-stack)))
		   (if (string= stack-top "(")
		       (progn
			 (pop op-stack)
			 (setq finish t))
		     (push (pop op-stack) rpn))))))))
    (while (not (null op-stack))
      (let ((stack-top (pop op-stack)))
	(push stack-top rpn)))
    (reverse rpn)))

(defconst -op-table
  '(("+" . +)
    ("-" . -)
    ("*" . *)
    ("/" . /)
    ("%" . mod)
    ("^" . expt)
    ("<" . <)
    ("<=" . <=)
    (">" . >)
    (">=" . >=)
    ("=" . =)
    ("!=" . /=)
    ("||" . or)
    ("&&" . and)))

(defun -eval-rpn (rpn)
  (let (stack)
    (while (not (null rpn))
      (let ((stack-top (pop rpn)))
        (if (-is-operator stack-top)
            (let* ((op2 (pop stack))
                   (op1 (pop stack))
                   (op (assoc-default stack-top -op-table)))
              (push (or (funcall op op1 op2) 0) stack))
          (push (-string-to-number stack-top) stack))))
    (car stack)))

(defun main ()
  (when (zerop (length argv))
    (error "Usage: expr expression..."))
  (let ((rpn (-infix-to-rpn argv)))
    (princ (-eval-rpn rpn))
    (princ "\n")))

(unless (boundp '--unit-test)
  (main)
  (setq argv nil))
;;; expr.el ends here
