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
   :getcars
   :getcars/1
   :getcars/2
   ))

(in-package :de.m-e-leypold.cl-specification/example)

;;; * -- The functions we want to test ----------------------------------------------------------------------|
;;;

(defun getcars (items)
  "
  TBD
"
  (let ((cars '()))
    (dolist (item items)
      ;; TBD error handling, restart handler
      (push (car item) cars))
    (reverse cars)))
