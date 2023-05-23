;;;; conversion.cl
(in-package #:pycl.sys)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun convert-python-ff-call/arg (action val ctype ltype)
    "Convert a foreign pointer to the corresponding ALIGNED foreign address."
    (declare (ignore ctype ltype))
    (case action
      (:convert (foreign-pointer-address val))
      (:convert-type 'integer)
      (:identify :arg)
      (:check (check-type val foreign-pointer))))

  (defun convert-python-ff-call/ret (action val ctype ltype)
    "Convert a foriegn address to a foreign pointer."
    (declare (ignore ltype))
    (case action
      (:convert (make-foreign-pointer :foreign-address val
                                      :foreign-type (if (listp ctype) (second ctype) ctype)))
      (:convert-type 'integer)
      (:identify :return)
      (:allocate nil)
      (:will-allocate nil))))
