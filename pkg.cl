;;;; pkg.cl
(in-package #:cl-user)

#-asdf (require :asdf)

(use-package 'util.string)

(eval-when (:load-toplevel :execute)
  (defparameter *pycl-src-files*
    (let ((files '(;; start of files list
                   "sys/package"
                   "sys/definitions"
                   "sys/pyptr"
                   "sys/capi"
                   "pycl/package"
                   "pycl/init"
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
  (uiop:concatenate-files (mapcar (lambda (f) (string+ f ".fasl")) *pycl-src-files*)
                          *pycl-fasl-output*))
