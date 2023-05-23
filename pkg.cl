;;;; pkg.cl
(in-package #:cl-user)

(use-package 'util.string)

(eval-when (:load-toplevel :execute)
  (defparameter *pycl-src-files*
    (let ((files '(;; start of files list
                   "sys/sys"
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
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-open-file (fasl-out *pycl-fasl-output*
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (dolist (file *pycl-src-files*)
        (with-open-file (fasl-in (string+ file ".fasl")
                                 :direction :input)
          (loop for offset = (read-sequence buffer fasl-in)
                while (not (zerop offset))
                do (write-sequence buffer fasl-out :end offset)))))))
