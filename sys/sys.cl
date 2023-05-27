;;;; sys.cl
(in-package :pycl.sys)

#-os-threads
(defmacro with-python-gil ((&key safe) &body body)
  (declare (ignore safe))
  `(progn ,@body))

#+os-threads
(defmacro with-python-gil ((&key (safe t)) &body body)
  (let ((g (gensym "g"))
        (res (gensym "res")))
    (if* safe
       then `(let ((,g (PyGILState_Ensure))
                   ,res)
               (declare (type (mod 1) ,g)
                        (dynamic-extent ,g))
               (setq ,res (progn ,@body))
               (PyGILState_Release ,g)
               ,res)
       else `(let ((,g (PyGILState_Ensure)))
               (unwind-protect (progn ,@body)
                 (PyGILState_Release))))))
