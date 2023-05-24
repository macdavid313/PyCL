;;;; conversion.cl
(in-package #:pycl.sys)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun convert-python-ff-call/arg (action fp ctype ltype)
    "Convert a foreign pointer to the corresponding foreign address."
    (declare (ignore ctype ltype)
             (optimize (speed 3) (safety 0) (space 0)))
    (case action
      (:convert (if* (foreign-pointer-p fp)
                   then (foreign-pointer-address fp)
                   else fp))
      (:convert-type 'integer)
      (:identify :arg)
      (:check (check-type fp (or (unsigned-byte #+32bit 32 #+64bit 64) foreign-pointer)))))

  (defun convert-python-ff-call/ret (action fp ctype ltype)
    "Convert a foriegn address to a foreign pointer."
    (declare (ignore ltype)
             (type #+32bit (unsigned-byte 32) #+64bit (unsigned-byte 64) fp)
             (optimize (speed 3) (safety 0) (space 0)))
    (case action
      (:convert (make-foreign-pointer :foreign-address fp
                                      :foreign-type (if (listp ctype) (second ctype) ctype)))
      (:convert-type 'integer)
      (:identify :return)
      (:allocate nil)
      (:will-allocate nil))))
