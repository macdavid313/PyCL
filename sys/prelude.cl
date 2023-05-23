;;;; prelude.cl
(in-package #:cl-user)

(defpackage #:pycl.sys
  (:use #:cl
        #:excl)
  (:export #:+PY_LOCK_FAILURE+
           #:+PY_LOCK_ACQUIRED+
           #:+PY_LOCK_INTR+
           #:+PyGILState_LOCKED+
           #:+PyGILState_UNLOCKED+
           #:+libpython-foreign-pointers+))

(in-package #:pycl.sys)

(ff:def-foreign-type Py_ssize_t :nat)
(ff:def-foreign-type Py_hash_t Py_ssize_t)
(ff:def-foreign-type Py_UCS4 :unsigned-int)
(ff:def-foreign-type Py_UCS2 :unsigned-short)
(ff:def-foreign-type Py_UCS1 :unsigned-char)
(ff:def-foreign-type PyCFunction (* :void))
(ff:def-foreign-type PyCapsule_Destructor (* :void))
(ff:def-foreign-type PyThread_type_lock (* :void))
(ff:def-foreign-type PyOS_sighandler_t (* :void))

(ff:def-foreign-type PyLockStatus :int)
(defconstant +PY_LOCK_FAILURE+ 0)
(defconstant +PY_LOCK_ACQUIRED+ 1)
(defconstant +PY_LOCK_INTR+ 2)

(ff:def-foreign-type PyGILState_STATE :int)
(defconstant +PyGILState_LOCKED+ 0)
(defconstant +PyGILState_UNLOCKED+ 1)

(ff:def-foreign-type PyObject
    ;; a mirror of PyObject C struct (for non-debugging Python builds)
    ;; we will not directly access the slots
    ;; and it will only be used for type annotation
    (:struct
     (ob_refcnt Py_ssize_t)
     (ob_type (* :void))))

(defstruct (pyptr (:print-function (lambda (o stream depth)
                                     (declare (ignore depth))
                                     (with-slots (ptr ctype) o
                                       (if* (zerop ptr)
                                          then (format stream "#<~a NULL>" ctype)
                                          else (let ((*print-base* 16))
                                                 (format stream "#<~a @ #x~a>" ctype ptr)))))))
  (ptr 0 :type #+32bit (unsigned-byte 32)
               #+64bit (unsigned-byte 64))
  (ctype nil :type (or null symbol)))

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
