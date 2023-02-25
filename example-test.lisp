;;; ------------------------------------------------------------------------*- common-lisp -*-|
;;;
;;;   de.m-e-leypold.cl-specification -- marrying tests and specification
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
;;;   This file is the test loader for the example.
;;;
;;;
;;; * Introduction ------------------------------------------------------------------------------------------|
;;;
;;;   This is a "driver" file, that is loaded with (slime-load-file) (i.e. C-c C-l) or from the Makefile in
;;;   order to run the tests.
;;;
;;;   A file like this is --- strictly speaking --- not necessary, but it is definitely convenient.
;;;
;;;   Without such such a driver file it is sufficient to load the system in which the tests are defined (in
;;;   this case :de.m-e-leypold.cl-specification/example-tests) and then invoke
;;;   `de.m-e-leypold.cl-specification:run-tests'. But for interactive development, especially with a flat
;;;   directory structure, it is much more convenient to just load a driver file which contains all those
;;;   instructions, instead of typing them at the REPL prompt.
;;;
;;;   The driver might also contain the setting of global parameters for test execution (like
;;;   `*DROP-INTO-DEBUGGER*')

;;; * Options -----------------------------------------------------------------------------------------------|

(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3) (safety 3)))

;;; * Load system to be tested & the tests ------------------------------------------------------------------|
;;;
;;;   First we load the system under test via ASDF.
;;;

(asdf:load-system "de.m-e-leypold.cl-specification/example-tests")

;;; * Define sandbox for tests ------------------------------------------------------------------------------|
;;;
;;;   Then we define a package in which the driver code is defined. If you don't define new symbols, it is
;;;   technically possible to do without a package definition here, but is safer to already have a package
;;;   definition, just in case a defun or defvar later needs to be added to the driver.

(defpackage :de.m-e-leypold.cl-specification/example-run-tests
  (:documentation "Loader + Driver to run EXAMPLE-TESTS")
  (:use
   :common-lisp
   :de.m-e-leypold.cl-simple-test
   :de.m-e-leypold.cl-specification
   ))

(in-package :de.m-e-leypold.cl-specification/example-run-tests)

;;; * Actually executing the tests --------------------------------------------------------------------------|
;;;
;;;   You can invoke `RUN-TESTS' (DE.M-E-LEYPOLD.CL-SPECIFICATION:RUN-TESTS) directly here. If you want to set
;;;   parameters for test execution first, it is convenient to define a local `RUN-TESTS*' and and invoke this
;;;   instead.
;;;
;;;   Here we set `*DROP-INTO-DEBUGGER*' to nil in order to execute all tests regardless wether they passed or
;;;   not and only fail with a signal after all tests have been run.
;;;
;;;   This is actually the RUN-TEST procedure from cl-simple-test.

(defun run-tests* ()
  (let ((*drop-into-debugger* nil))
    (run-tests)))

(run-tests*)
