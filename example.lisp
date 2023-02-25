;;; * -- (C) 2022  M E Leypold ------------------------------------------------------------*- common-lisp -*-|
;;;
;;;   de.m-e-leypold.cl-specificiation --- Marrying specifications and tests
;;;   Copyright (C) 2022  M E Leypold
;;;
;;;   This program is free software: you can redistribute it and/or modify
;;;   it under the terms of the GNU General Public License as published by
;;;   the Free Software Foundation, either version 3 of the License, or
;;;   (at your option) any later version.
;;;
;;;   This program is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;   GNU General Public License for more details.
;;;
;;;   You should have received a copy of the GNU General Public License
;;;   along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;
;;;   For alternative licensing options, see README.md
;;;
;;;
;;;   This is the system under test used in the example.
;;;
;;; * -- Options --------------------------------------------------------------------------------------------|
;;;

(declaim (optimize (speed 0) (debug 3) (safety 3)))

;;; * -- Package definition & documentation -----------------------------------------------------------------|

(defpackage :de.m-e-leypold.cl-specification/example

  (:use :common-lisp)
  (:export
   :fac
   :square
   ))

(in-package :de.m-e-leypold.cl-specification/example)

;;; * -- The functions we want to test ----------------------------------------------------------------------|
;;;
;;;   TODO: Find better examples

(defun fac (n)
  "
  Calculate faculty of N.

  - Defined for positive numbers as (FAC N) => 1 * 2 ... (N - 1) * N. See test
    `TEST:fac-satisfies-spec'.

  - Signals error on negative number, see test `TEST:fac-signal-on-non-positive-input'. The
    error says 'Argument must be positive', see test `TEST:fac-error-message-as-expected'.
"

  (assert (< 0 n) () (format nil "Argument must be positive, is ~S" n))
  (if (= 1 n)
      1
      (* n (fac (1- n)))))


;;; The implementation of SQUARE is defective for demonstration purposes. It's test will fail.

(defun square (n)
  "
  Calculate the sqare of N.
"
  (* n n n))
