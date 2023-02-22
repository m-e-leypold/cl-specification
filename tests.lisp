;;; ------------------------------------------------------------------------*- common-lisp -*-|
;;;
;;;   de.m-e-leypold.cl-specification -- Couples specification and testing
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
;;;   For altermative licensing options, see README.md
;;;
;;; * Options -----------------------------------------------------------------------------------------------|

(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3) (safety 3)))

;;; * Define package ----------------------------------------------------------------------------------------|

(defpackage :de.m-e-leypold.cl-specification/tests
  (:documentation "Testing cl-specification")
  (:use
   :common-lisp
   :de.m-e-leypold.cl-specification
   :de.m-e-leypold.cl-simple-utils
   :de.m-e-leypold.cl-simple-utils/wrapped-streams)

  (:import-from
   :de.m-e-leypold.cl-simple-utils/basic-test
   :assert! :run-tests! :deftest!
   :deftest-registry!
   :end-test-registry!
   :set-flag :clear-flags :*flags*
   :explain :trace-expr
   :test-failure)

  (:export
   :run-tests!
   ))

(in-package :de.m-e-leypold.cl-specification/tests)

(inject-package-local-nickname "TEST"
			       :de.m-e-leypold.cl-specification/tests
			       :de.m-e-leypold.cl-specification)

(deftest-registry!)

;;; * The tests ---------------------------------------------------------------------------------------------|

;;; * Package epilog ----------------------------------------------------------------------------------------|

(end-test-registry!)

;;; * -------------------------------------------------------------------------------------------------------|
;;;   WRT the outline-* and comment-* variables, see the comment in test.lisp
;;;
;;; Local Variables:
;;; eval: (progn (outshine-mode 1) (column-enforce-mode 1) (toggle-truncate-lines 1))
;;; fill-column: 110
;;; column-enforce-column: 110
;;; outline-regexp: ";;; [*]\\{1,8\\} "
;;; comment-add: 2
;;; End:
