;;;; package.cl
(in-package #:cl-user)

(defpackage #:pycl.sys
  (:use #:cl
        #:excl
        #:ff
        #:util.string)
  (:export
   ;; prelude
   #:+PY_LOCK_FAILURE+
   #:+PY_LOCK_ACQUIRED+
   #:+PY_LOCK_INTR+
   #:+PyGILState_LOCKED+
   #:+PyGILState_UNLOCKED+
   #:pyptr
   #:pyobject
   #:*pynull*
   ;; capi
   #:+libpython-extern-variables+
   ;; sys
   ;; --- "lost and found"
   #:PyObject_DelAttrString
   #:check-python-gil
   #:PyRun_SimpleString
   #:from-pyunicode!
   ;; --- Conditions
   #:pycl-condition
   #:simple-pycl-error
   ;; --- Utilities
   #:pyimport!
   #:pyhasattr!
   #:pyattr!
   ;; --- Python and its GIL
   #:*python*
   #:python
   #:make-python
   #:python-p
   #:python-exe
   #:python-version
   #:python-libpython
   #:python-home
   #:pyglobalptr
   #:with-python-gil
   ;; --- pyobject
   ;; APIs and Utilities
   #:pyptr-eq
   #:pynull
   #:pyincref
   #:pydecref
   #:pydecref*
   #:pystealref
   #:pymarkgc
   #:pygc
   ;; -- python exception
   #:python-exception
   #:python-exception-type
   #:python-exception-msg
   #:python-exception-occurred
   #:make-python-exception
   #:pyexcept
   #:pyerror
   #:pycheckn
   #:pycheckz))
