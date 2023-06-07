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
   #:pyobject
   #:*pynull*
   #:make-pyobject
   ;; capi
   #:+libpython-extern-variables+
   ;; sys
   ;; --- "lost and found"
   #:PyObject_DelAttrString
   #:PyGILState_Check
   #:PyRun_SimpleString
   #:PyUnicode_AsUTF8
   ;; --- Conditions
   #:pycl-condition
   #:simple-pycl-error
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
   #:pyobject-eq
   #:pynull
   #:pyincref
   #:pydecref
   #:pydecref*
   #:pystealref
   ;; -- python exception
   #:python-exception
   #:python-exception-type
   #:python-exception-msg
   #:python-exception-occurred
   #:make-python-exception
   #:pyerror
   #:pycheckn
   #:pycheckz))
