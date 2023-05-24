;;;; pkg.cl
(in-package #:cl-user)

#-asdf (require :asdf)

(use-package 'util.string)

(eval-when (:load-toplevel :execute)
  (defparameter *pycl-src-files*
    (let ((files '(;; start of files list
                   "sys/package"
                   "sys/types"
                   "sys/conversion"
                   "sys/capi"
                   "pycl/package"
                   "pycl/init"
                   "pycl/conditions"
                   "pycl/pyobject"
                   ;; files list ends here
                   )))
      (mapcar (lambda (file)
                (string+ (directory-namestring *load-pathname*) file))
              files)))

  (defparameter *pycl-fasl-output*
    (string+ (directory-namestring *load-pathname*) "pycl.fasl")))

(defun compile-and-load-pycl ()
  (dolist (file *pycl-src-files*)
    (load
     (compile-file-if-needed (string+ file ".cl")
                             :load-after-compile nil))))

(defun build-pycl ()
  (compile-and-load-pycl)
  (uiop:concatenate-files (mapcar (lambda (f) (string+ f ".fasl")) *pycl-src-files*)
                          *pycl-fasl-output*))
