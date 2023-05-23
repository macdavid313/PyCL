;;;; package.cl
(in-package #:cl-user)

(defpackage #:pycl.sys
  (:use #:cl
        #:excl
        #:ff)
  (:export #:+PY_LOCK_FAILURE+
           #:+PY_LOCK_ACQUIRED+
           #:+PY_LOCK_INTR+
           #:+PyGILState_LOCKED+
           #:+PyGILState_UNLOCKED+
           #:+libpython-foreign-pointers+))
