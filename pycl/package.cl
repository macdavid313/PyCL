;;;; package.cl
(in-package #:cl-user)

(defpackage #:pycl
  (:use #:cl
        #:excl
        #:ff
        #:util.string
        #:pycl.sys)
  (:export
   ;; configuration
   #:pycontext
   #:pycontext-p
   #:pycontext-lib
   #:pycontext-exe
   #:pycontext-program
   #:pycontext-home
   #:pycontext-version
   #:pycontext-initialized-p
   #:pycontext-finalized-p
   #:init-pycontext
   #:finalize-pycontext
   #:*pycontext*
   ;; pyobject
   ;; #:pyobject #:to-pyobject #:from-pyobject #:pyobject-eq #:pynull-p
   ;; #:pyincref #:pydecref #:pytypep #:pytype-of
   ))
