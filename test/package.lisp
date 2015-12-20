(in-package :cl-user)

(defpackage :ccs-test
  (:use
   :cl
   :lift
   :ccs)
  (:export
   :root)
  (:documentation
   "This package contains unit tests of the CCS system."))

(in-package :ccs-test)

(deftestsuite root ()
  ()
  (:documentation
   "Root unit test suite for the CCS system."))
