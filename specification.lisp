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
  (:import-from
   :de.m-e-leypold.cl-simple-utils

   :defpackage-doc
   :defrestart
   :with-gensyms
   :define-documentation-node
   :base-documentation-node
   :make-docstring
   )

  (:export

   :run-tests  ;; a re-export for convenience

   :specifications
   :specification
   :contract
   :clause
   ))

(in-package :de.m-e-leypold.cl-specification)
(defpackage-doc -doc-)                          ;; -doc- will contain package doc string


;;; * -- Development infrastructure -------------------------------------------------------------------------|

(defun load-tests ()
  (asdf:load-system "de.m-e-leypold.cl-specificationy/tests"))

;;; * -- Defining specifications ----------------------------------------------------------------------------|

(defmacro specifications (varname)
  "
  Define a specifications bundle in VARNAME. This will be an instance of `SPECIFICATION-BUNDLE'.

  It also establishes (...)

  TODO: docstring
"
  `(progn
     (define-documentation-node ,varname specification-bundle)))

(defmacro specification (name heading &body body-text)
  "
  TODO: docstring
"
  (assert (not (cdr body-text)) nil "Argument body-text to `SPECIFICATION' needs to be a single string")
  (setf body-text (car body-text))
  `(progn
     (defun ,name ()
       ,(format nil "~a (specification) -- ~a.~%~%~a~&" name heading body-text)
       nil
       )
     ;; TODO: Now this needs to become a docnode, too.
     ))

(defmacro contract (name heading &body body-text)
  "
  TODO: docstring
"
  (assert (not (cdr body-text)) nil "Argument body-text to `CONTRACT' needs to be a single string")
  (setf body-text (car body-text))
  `(progn
     (defun ,name ()
       ,(format nil "~a (specification contract) -- ~a.~%~%~a~&" name heading body-text)
       nil
       )
  ;; TODO: Now this needs to become a docnode, too.
  ))

(defmacro clause (name heading &body body-text+body)
  "
  TODO: docstring
"
  (destructuring-bind (body-text &optional body) body-text+body
    `(progn
       (deftest ,name ()
	   ,(format nil "~a (specification clause) -- ~a.~%~%~a~&" name heading body-text)
	 ,body)
       ;; TODO: Now this needs to become a docnode, too.
       )))

;;; * -- Specification bundles ------------------------------------------------------------------------------|

(defclass documentation-object (base-documentation-node)
  ())

(defmethod make-docstring ((node documentation-object))
  "TBD -- not yet implemented")

(defclass specification-bundle (base-documentation-node)
  ())

(defmethod make-docstring ((node specification-bundle))
  "TBD -- not yet implemented")

(defun close-specification-bundle ()
  (format t "closing spec~%"))
