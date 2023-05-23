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
   #:*libpython-loaded-p*
   #:python
   #:python-p
   #:python-lib
   #:python-exe
   #:python-program
   #:python-home
   #:python-version
   #:start-python
   #:shutdown-python
   ;; pyobject
   ;; #:pyobject #:to-pyobject #:from-pyobject #:pyobject-eq #:pynull-p
   ;; #:pyincref #:pydecref #:pytypep #:pytype-of
   ))
