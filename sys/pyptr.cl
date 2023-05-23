;;;; pyptr.cl
(in-package #:pycl.sys)

(defstruct (pyptr (:print-function print-pyptr))
  (ptr 0 :type #+32bit (unsigned-byte 32)
               #+64bit (unsigned-byte 64))
  (ctype nil :type (or null symbol)))

(defun print-pyptr (o stream depth)
  (declare (ignore depth))
  (with-slots (ptr ctype) o
    (if* (zerop ptr)
       then (format stream "#<~a NULL>" ctype)
       else (let ((*print-base* 16))
              (format stream "#<~a @ #x~a>" ctype ptr)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun convert-python-ff-call/arg (action val ctype ltype)
    "Convert a foreign pointer to the corresponding ALIGNED foreign address."
    (declare (ignore ctype ltype))
    (case action
      (:convert (pyptr-ptr val))
      (:convert-type 'integer)
      (:identify :arg)
      (:check (check-type val pyptr))))

  (defun convert-python-ff-call/ret (action val ctype ltype)
    "Convert a foriegn address to a foreign pointer."
    (declare (ignore ltype))
    (case action
      (:convert (make-pyptr :ptr val :ctype (when (listp ctype) (second ctype))))
      (:convert-type 'integer)
      (:identify :return)
      (:allocate nil)
      (:will-allocate nil))))
