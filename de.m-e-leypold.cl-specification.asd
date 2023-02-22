;;; ------------------------------------------------------------------------*- common-lisp -*-|
;;;
;;;   de.m-e-leypold.cl-specification -- Marries specification and testing
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
;;; ---- System ---------------------------------------------------------------------------

(defsystem "de.m-e-leypold.cl-specification"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Marries specification and testing"
  :depends-on ("cl-ppcre" "de.m-e-leypold.cl-simple-utils")
  :components ((:file "specification")))

(defsystem "de.m-e-leypold.cl-specification/tests"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :depends-on ("cl-ppcre"
	       "de.m-e-leypold.cl-specification"
	       "de.m-e-leypold.cl-simple-utils"
	       "de.m-e-leypold.cl-simple-utils/basic-test")
  :description "Tests and specifications for CL-SPECIFICATION"
  :components ((:file "tests")))

(defsystem "de.m-e-leypold.cl-specification/prerequisites"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :depends-on ("cl-ppcre"
	       "de.m-e-leypold.cl-simple-utils"
	       "de.m-e-leypold.cl-simple-utils/basic-test")
  :description "Just all external prerequisites"
  :components ())

(defsystem "de.m-e-leypold.cl-specification/load-all"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Load all systems in CL-SPECIFICATION"
  :depends-on ("de.m-e-leypold.cl-specification" "de.m-e-leypold.cl-specification/tests"))

;;; ---- Example --------------------------------------------------------------------------

(defsystem "de.m-e-leypold.cl-specification/example"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "System under test for EXAMPLE-TESTS"
  :depends-on ("cl-ppcre" "de.m-e-leypold.cl-simple-utils")
  :components ((:file "example")))

(defsystem "de.m-e-leypold.cl-specification/example-tests"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Demonstration for applying CL-SPECIFICATION"
  :depends-on ("cl-ppcre"
	       "de.m-e-leypold.cl-specification"
	       "de.m-e-leypold.cl-specification/example")
  :description "Tests and specifications for CL-SPECIFICATION"
  :components ((:file "example-tests")))

