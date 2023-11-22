;;;; pkg.cl
(in-package #:cl-user)

(defpackage #:pycl.pkg
  (:use #:cl
        #:excl
        #:util.string)
  (:export #:compile-and-load-pycl
           #:build-pycl))

(in-package #:pycl.pkg)

(eval-when (:load-toplevel :execute)
  (defparameter *pycl-src-files*
    (let ((files '(;; start of sys module
                   "sys/package"
                   "sys/prelude"
                   "sys/capi"
                   "sys/sys"
                   ;; start of pycl module
                   "pycl/package"
                   "pycl/consts"
                   "pycl/init"
                   "pycl/objects"
                   ;; files list ends here
                   )))
      (mapcar (lambda (file)
                (string+ (directory-namestring *load-pathname*) file))
              files)))

  (defparameter *pycl-fasl-output*
    (string+ (directory-namestring *load-pathname*) "pycl.fasl")))

(defun compile-and-load-pycl ()
  (dolist (file *pycl-src-files*)
    (compile-file (string+ file ".cl")
                  :load-after-compile t)))

(defun build-pycl ()
  (compile-and-load-pycl)
  (with-open-file (out *pycl-fasl-output* :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
    (dolist (file *pycl-src-files*)
      (sys:copy-file (string+ file ".fasl") out))))


