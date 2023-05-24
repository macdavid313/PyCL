;;;; package.cl
(in-package #:cl-user)

(defpackage #:pycl
  (:use #:cl
        #:excl
        #:ff
        #:util.string
        #:pycl.sys)
  (:export
   ;; init
   #:*python*
   #:python
   #:python-p
   #:python-libpython
   #:python-exe
   #:python-program
   #:python-home
   #:python-version
   #:start-python
   #:shutdown-python
   ;; conditions
   #:pycl-condition
   #:python-error
   #:pycl-error
   ))
