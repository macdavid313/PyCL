;;;; pkg.cl
(in-package #:cl-user)

(use-package 'util.string)

(eval-when (:load-toplevel :execute)
  (defparameter *pycl-src-files*
    (let ((files '(;; start of sys module
                   "sys/package"
                   "sys/prelude"
                   "sys/capi"
                   "sys/sys"
                   ;; start of pycl module
                   "pycl/package"
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
  (let (fasls)
    (dolist (file *pycl-src-files*)
      (push (compile-file (string+ file ".cl")
                          :load-after-compile t)
            fasls))
    (nreverse fasls)))

(defun build-pycl ()
  (with-open-file (out *pycl-fasl-output* :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
    (dolist (fasl (compile-and-load-pycl))
      (sys:copy-file fasl out))))
