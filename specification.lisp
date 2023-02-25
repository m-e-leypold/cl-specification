;;; * -- (C) 2022  M E Leypold ------------------------------------------------------------*- common-lisp -*-|
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

;;;   For user and technical documentation start with the documentatin string of Symbol
;;;   `de.m-e-leypold.cl-specification::DOC'.
;;;
;;; * -- Options --------------------------------------------------------------------------------------------|
;;;
;;;   Will be changed to defaults when cl-specification has reached sufficient maturity.

(declaim (optimize (speed 0) (debug 3) (safety 3)))

;;; * -- Package definition & documentation -----------------------------------------------------------------|

(defpackage :de.m-e-leypold.cl-specification
  (:documentation "

   DE.M-E-LEYPOLD.CL-SPECIFICATION (...)

   TODO: Complet package docstring
   ")

  (:use :common-lisp :cl-ppcre :de.m-e-leypold.cl-simple-test)
  (:import-from :de.m-e-leypold.cl-simple-utils
   :defpackage-doc
   :defrestart
   :with-gensyms
   )

  (:export

   :run-tests  ;; a re-export for convenience

   :specifications
   ))

(in-package :de.m-e-leypold.cl-specification)
(defpackage-doc -doc-)                          ;; -doc- will contain package doc string


;;; * -- Development infrastructure -------------------------------------------------------------------------|

(defun load-tests ()
  (asdf:load-system "de.m-e-leypold.cl-specificationy/tests"))

;;; * -- Defining specifications ----------------------------------------------------------------------------|

(defmacro specifications (varname)
  )
