;;; ------------------------------------------------------------------------*- common-lisp -*-|
;;;
;;;   de.m-e-leypold.cl-specification -- Marry specifications and tests
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
;;;   This are example of how to apply CL-SPECIFICATION.
;;;

;;; * Options -----------------------------------------------------------------------------------------------|

(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3) (safety 3)))

;;; * Define package ----------------------------------------------------------------------------------------|
;;;
;;;   We define a package as seperate namespace for the tests in order to achieve some isolation.
;;;   We need to export the whole API from cl-specification and the example.

(defpackage :de.m-e-leypold.cl-specification/example-tests

  (:documentation
   "
   These are the sample tests. Run by loading 'example-test.lisp'. Some tests will fail (intentionally,
   this is a demonstration), so at the end an error will be signalled \"1 of 4 tests failed:
   (SQUARE-IS-MATHEMATICALLY-CORRECT).\". Just choose `ABORT` in the debugger.

   To understand the example look at the implementation of the single tests.

   - Their documentation strings
     explain the test in the domain (i.e. they talk about about the function(s) to be tested not about
     cl-specification).

   - The comments around the tests explain what feature of c-simple-test a test demonstrates.

   Therefore do not look only at the documentation string, instead look at the implementations (in SLIME by
   using `M-.` or `M-x slime-edit-definition`).

   TODO: More doc.
")

  (:use
   :common-lisp
   :de.m-e-leypold.cl-simple-utils             ;; Some infrastructure, see FN.1
   :de.m-e-leypold.cl-specification            ;; The framework we use/demonstrate here.
   :de.m-e-leypold.cl-specification/example)   ;; The system under test.

  (:export

   ;; It is not necessary to export all specficiations, but useful for invoking the tests from
   ;; the REPL and also to document that these symbols are the existing specifications.

   :fac-satisfies-spec
   :fac-signal-on-non-positive-input
   :fac-error-message-as-expected
   :square-is-mathematically-correct))

;;; FN.1: From cl-cimple-utils we use `inject-package-local-nickname' (see below), which is not necessary, but
;;; convenient.

(in-package :de.m-e-leypold.cl-specification/example-tests)


;;; The following form adds the package local nickname "SPEC" for this package (the tests) to the system under
;;; test. That has the advantage, that after loading the tests, it is then possible to refer to tests as e.g.
;;; `test::square-is-mathematically-correct' from the system under test. E.g. in order to refer from a
;;; function implementation to a more detailled specification of the function in the comment string of a
;;; tests. As an example, (...)
;;; TODO: Complete this.


(inject-package-local-nickname "SPEC"
			       :de.m-e-leypold.cl-specification/example-tests
			       :de.m-e-leypold.cl-specification/example)

;; The following binds the package documentation string to a symbol in the package, so we can refere to it in
;; other documentation strings as a symbol. This is strictly optional and might be useful to describe a test
;; suite (a package with tests) on a higher level. Here we also use it to provide navigation from the
;; documentation of cl-specification to the the example.

(defpackage-doc -doc-)   ;; -doc- will contain package doc string (from above)

;;; * The specifications ------------------------------------------------------------------------------------|

(specifications *SPECIFICATIONS*)

(specification contracts  "Contracts of the basic functions in EXAMPLE"
  "
  Basic primitives are

  - `GETCARS' -- get the first element of all items in a list. See specification `GETCARS/'
")

(contract getcars/  "Contract of `GETCAR' (cars of all items in a list)"
  "
  TBD
"
  )

(clause getcars/1  "`GETCAR' returns the cars of list elements"
  "
  `GETCARS' returns a list of the first elements of every item in a given list in the same
  order as the original items in the list.

  Calling (getcars SOME-LIST) iterates over all items ITEM in SOME-LIST and collects (car ITEM)
  in a result list which it then returns.
"
  (let ((test-input '((a 1) (b 2) (c 3))))
    (assert (equal (getcars test-input)
		   '(a b c)))))


(clause getcars/2  "Error if argument is not `consp'"
  "
  The argument to `getcars' needs to satisfy `consp'.

  If the argument to `getcars' is not satisfying `consp' an `error' will be signaled.

  TODO: Proper list clause.
"

  )





;;; * File epilog -------------------------------------------------------------------------------------------|

(end-of-load-file)
